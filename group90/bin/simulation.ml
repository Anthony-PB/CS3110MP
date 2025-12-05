(** Frontend simulation module - handles rendering and user interaction.

    This module is responsible for:
    - Main game loop coordination
    - User input processing (keyboard, mouse, sliders)
    - Camera management
    - Rendering orchestration
    - UI event handling

    Backend logic lives in the lib folder:
    - Simulation_state: state management
    - Physics_system: physics calculations
    - Scenario: scenario creation *)

open Raylib
open Group90

(** Main simulation loop - handles rendering and input *)
let rec simulation_loop state camera theta phi radius =
  (* Handle slider interactions for planet parameters *)
  let state_after_sliders = handle_slider_input state in

  (* Handle keyboard input for simulation control *)
  let state_after_keyboard = handle_keyboard_input state_after_sliders in

  (* Update physics if not paused *)
  let state_after_physics =
    if state_after_keyboard.Simulation_state.paused then state_after_keyboard
    else update_physics_step state_after_keyboard
  in

  (* Update camera based on input *)
  let new_camera, new_theta, new_phi, new_radius =
    Cameracontrol.update_camera camera theta phi radius
  in

  (* Check for exit condition *)
  let should_exit = Ui.check_exit_button () || window_should_close () in

  if should_exit then ()
  else begin
    (* Render everything *)
    render_frame state_after_physics new_camera;

    (* Frame timing *)
    Unix.sleepf 0.016;

    (* Continue loop *)
    simulation_loop state_after_physics new_camera new_theta new_phi new_radius
  end

(** Handle slider input for adjusting planet parameters *)
and handle_slider_input state =
  let sidebar_x = 600 in
  let rec process_sliders i state_acc =
    if i >= 3 then state_acc (* Process 3 planets *)
    else begin
      let base_y = 55 + (i * 145) in

      (* Handle density slider *)
      let state_after_density =
        match
          Ui.check_slider_drag (sidebar_x + 50) (base_y + 60) 130 1e10 6e10
        with
        | Some new_density ->
            Simulation_state.update_planet_density state_acc i new_density
        | None -> state_acc
      in

      (* Handle radius slider *)
      let state_after_radius =
        match
          Ui.check_slider_drag (sidebar_x + 50) (base_y + 110) 130 10. 40.
        with
        | Some new_radius ->
            Simulation_state.update_planet_radius state_after_density i
              new_radius
        | None -> state_after_density
      in

      process_sliders (i + 1) state_after_radius
    end
  in
  process_sliders 0 state

(** Handle keyboard input for simulation control *)
and handle_keyboard_input state =
  (* Check for scenario switching (1-7 keys) *)
  let state_after_scenario =
    if is_key_pressed Key.One then load_scenario_by_index state 0
    else if is_key_pressed Key.Two then load_scenario_by_index state 1
    else if is_key_pressed Key.Three then load_scenario_by_index state 2
    else if is_key_pressed Key.Four then load_scenario_by_index state 3
    else if is_key_pressed Key.Five then load_scenario_by_index state 4
    else if is_key_pressed Key.Six then load_scenario_by_index state 5
    else if is_key_pressed Key.Seven then load_scenario_by_index state 6
    else state
  in

  (* Check for reset (R key) - reset scenario with current slider values *)
  let state_after_reset =
    if is_key_pressed Key.R then begin
      (* Use current pending_params (slider values) to create new bodies *)
      let new_bodies =
        Scenario.create_three_body_system
          ~custom_params:
            (Some
               (extract_params_tuple
                  state_after_scenario.Simulation_state.pending_params))
          ()
      in
      let state_with_bodies =
        Simulation_state.set_world state_after_scenario new_bodies
      in
      let state_with_trails =
        Simulation_state.set_trails state_with_bodies []
      in
      let state_with_anims =
        Simulation_state.set_collision_anims state_with_trails []
      in
      (* Mark current params as applied *)
      Simulation_state.apply_params state_with_anims
    end
    else state_after_scenario
  in

  (* Check for time scale adjustments (Z and X keys) *)
  let state_after_timescale =
    if is_key_pressed Key.Z then
      Simulation_state.set_time_scale state_after_reset
        (min (state_after_reset.time_scale *. 1.5) 20.0)
    else if is_key_pressed Key.X then
      Simulation_state.set_time_scale state_after_reset
        (max (state_after_reset.time_scale /. 1.5) 0.1)
    else state_after_reset
  in

  (* Check for pause toggle (P key) *)
  if is_key_pressed Key.P then
    Simulation_state.toggle_pause state_after_timescale
  else state_after_timescale

(** Load a scenario by index *)
and load_scenario_by_index state index =
  if index >= 0 && index < List.length Scenario.all_scenarios then
    let scenario_name = List.nth Scenario.all_scenarios index in
    let scenario = Scenario.get_scenario_by_name scenario_name in
    let state_loaded = Simulation_state.load_scenario state scenario.name in
    Simulation_state.set_world state_loaded scenario.bodies
  else state

(** Extract params as tuple for scenario creation *)
and extract_params_tuple params_list =
  match params_list with
  | (d1, r1) :: (d2, r2) :: (d3, r3) :: _ -> (d1, r1, d2, r2, d3, r3)
  | _ -> (3.5747e10, 20., 2.6810e10, 18., 1.7873e10, 16.)

(** Update physics for one step *)
and update_physics_step state =
  (* Get current time for animations *)
  let current_time = Unix.gettimeofday () in

  (* Store old body colors before physics update as RGBA tuples *)
  let old_body_colors =
    List.mapi
      (fun i _ ->
        let c = Ui.get_body_color i in
        (Raylib.Color.r c, Raylib.Color.g c, Raylib.Color.b c, Raylib.Color.a c))
      state.Simulation_state.world
  in

  (* Update physics *)
  let new_world, all_collisions =
    Physics_system.update_physics ~time_scale:state.time_scale
      ~world:state.world
  in

  (* Get new positions for trail updates *)
  let new_positions = List.map Body.pos new_world in

  (* Update state with new world *)
  let state_with_world = Simulation_state.set_world state new_world in

  (* Update trails with fading for removed bodies *)
  let state_with_trails =
    Simulation_state.update_trails_with_fading state_with_world new_positions
      current_time
  in

  (* Prune empty orphaned trails *)
  let state_pruned = Simulation_state.prune_empty_trails state_with_trails in

  (* Add collision animations if any collisions occurred *)
  let state_with_collisions =
    if List.length all_collisions > 0 then
      Simulation_state.add_collision_animations state_pruned all_collisions
        old_body_colors current_time
    else state_pruned
  in

  (* Remove expired animations *)
  Simulation_state.prune_expired_animations state_with_collisions current_time

(** Render a single frame *)
and render_frame state camera =
  begin_drawing ();
  clear_background (Ui.color 5 5 15 255);

  (* Dark space background *)

  (* Restrict 3D rendering to left side (non-sidebar area) *)
  begin_scissor_mode 0 0 600 600;

  (* 3D rendering *)
  begin_mode_3d camera;

  (* Draw starbox background *)
  Render.draw_starbox camera;

  (* Draw trails with fading for orphaned trails *)
  let current_time = Unix.gettimeofday () in
  let base_trail_colors =
    [ (255, 200, 100, 100); (100, 150, 255, 100); (255, 100, 100, 100) ]
  in
  List.iteri
    (fun i trail ->
      let positions, alpha =
        Simulation_state.get_trail_render_info trail current_time
      in
      if List.length positions > 0 then begin
        let r, g, b, a =
          if i < List.length base_trail_colors then List.nth base_trail_colors i
          else (200, 200, 200, 100)
        in
        let faded_alpha = int_of_float (float_of_int a *. alpha) in
        let trail_color = Ui.color r g b faded_alpha in
        Render.draw_trail positions trail_color
      end)
    state.Simulation_state.trails;

  (* Draw bodies - only use as many colors as we have bodies *)
  let all_body_colors =
    [
      Ui.color 255 200 100 255;
      Ui.color 100 150 255 255;
      Ui.color 255 100 100 255;
    ]
  in
  let num_bodies = List.length state.world in
  let body_colors = List.filteri (fun i _ -> i < num_bodies) all_body_colors in
  List.iter2 Render.draw_body state.world body_colors;

  (* Draw collision animations *)
  let current_time = Unix.gettimeofday () in
  let render_scale = 0.1 in
  (* Match render.ml's render_scale *)
  List.iter
    (fun anim ->
      let age = current_time -. anim.Simulation_state.start_time in
      let progress = age /. anim.duration in
      if progress < 1.0 then begin
        let current_radius = anim.max_radius *. progress *. render_scale in
        let alpha = int_of_float (255.0 *. (1.0 -. progress)) in
        let r, g, b, _ = anim.color in
        let fade_color = Raylib.Color.create r g b alpha in
        let pos_vec3 =
          Raylib.Vector3.create
            (render_scale *. Vec3.x anim.position)
            (render_scale *. Vec3.y anim.position)
            (render_scale *. Vec3.z anim.position)
        in
        Raylib.draw_sphere pos_vec3 current_radius fade_color
      end)
    state.collision_anims;

  end_mode_3d ();
  end_scissor_mode ();

  (* Draw 2D UI overlay *)
  let is_colliding = Physics_system.is_colliding state.world in
  let has_changes = Simulation_state.has_pending_changes state in
  let num_alive = Simulation_state.num_bodies state in
  Ui.draw_ui is_colliding state.time_scale state.paused state.pending_params
    has_changes num_alive state.current_scenario;

  end_drawing ()
