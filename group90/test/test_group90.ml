open OUnit2
open Group90

(* ---------- Helpers ---------- *)

(** [vec x y z] is a 3D vector with components [x], [y], and [z]. *)
let vec x y z = Vec3.make x y z

(** [approx_equal ?eps a b] asserts that [a] and [b] are approximately equal
    within epsilon [eps] (default 1e-6). *)
let approx_equal ?(eps = 1e-6) a b =
  assert_bool (Printf.sprintf "Expected %f ≈ %f" a b) (Float.abs (a -. b) <= eps)

(** [approx_vec ?eps v1 v2] asserts that vectors [v1] and [v2] are
    approximately equal component-wise within epsilon [eps]. *)
let approx_vec ?(eps = 1e-6) v1 v2 =
  approx_equal ~eps (Vec3.x v1) (Vec3.x v2);
  approx_equal ~eps (Vec3.y v1) (Vec3.y v2);
  approx_equal ~eps (Vec3.z v1) (Vec3.z v2)

(** [make_body ~pos ~vel ~mass ?radius ()] creates a test body with the given
    position, velocity, mass, and optional radius (default 1.0). *)
let make_body ~pos ~vel ~mass ?(radius = 1.0) () =
  let density = mass /. (4.0 /. 3.0 *. Float.pi *. (radius ** 3.0)) in
  Body.make ~pos ~vel ~density ~radius ~color:(255., 255., 255., 255.)

(** [string_of_int i] converts integer [i] to a string for test output. *)
let string_of_int = string_of_int

(* ---------- g ---------- *)

let test_g_value _ =
  assert_bool "g > 0" (Engine.g > 0.);
  approx_equal 6.67e-11 Engine.g

(* ---------- gravitational_force ---------- *)

let test_grav_same_position_zero _ =
  let p = vec 0. 0. 0. in
  let v = vec 0. 0. 0. in
  let b1 = make_body ~pos:p ~vel:v ~mass:1. () in
  let b2 = make_body ~pos:p ~vel:v ~mass:2. () in
  let f = Engine.gravitational_force b1 ~by:b2 in
  approx_vec f Vec3.zer0

let test_grav_direction _ =
  let b1 = make_body ~pos:(vec 0. 0. 0.) ~vel:(vec 0. 0. 0.) ~mass:3. () in
  let b2 = make_body ~pos:(vec 1. 0. 0.) ~vel:(vec 0. 0. 0.) ~mass:4. () in
  let f = Engine.gravitational_force b1 ~by:b2 in
  assert_bool "force should be in +x direction" (Vec3.x f > 0.);
  approx_equal 0. (Vec3.y f);
  approx_equal 0. (Vec3.z f)

let test_grav_newton3 _ =
  let b1 = make_body ~pos:(vec (-1.) 0. 0.) ~vel:(vec 0. 0. 0.) ~mass:5. () in
  let b2 = make_body ~pos:(vec 1. 0. 0.) ~vel:(vec 0. 0. 0.) ~mass:5. () in
  let f12 = Engine.gravitational_force b1 ~by:b2 in
  let f21 = Engine.gravitational_force b2 ~by:b1 in
  approx_vec
    (vec
       (Vec3.x f12 +. Vec3.x f21)
       (Vec3.y f12 +. Vec3.y f21)
       (Vec3.z f12 +. Vec3.z f21))
    Vec3.zer0

(* ---------- net_force_on ---------- *)

let test_net_force_two_body _ =
  let b1 = make_body ~pos:(vec 0. 0. 0.) ~vel:(vec 0. 0. 0.) ~mass:1. () in
  let b2 = make_body ~pos:(vec 1. 0. 0.) ~vel:(vec 0. 0. 0.) ~mass:2. () in
  let net = Engine.net_force_on b1 [ b1; b2 ] in
  let expected = Engine.gravitational_force b1 ~by:b2 in
  approx_vec net expected

let test_net_force_three_body _ =
  let vel0 = vec 0. 0. 0. in
  let b_left = make_body ~pos:(vec (-2.) 0. 0.) ~vel:vel0 ~mass:2. () in
  let b_mid = make_body ~pos:(vec 0. 0. 0.) ~vel:vel0 ~mass:3. () in
  let b_right = make_body ~pos:(vec 2. 0. 0.) ~vel:vel0 ~mass:4. () in
  let world = [ b_left; b_mid; b_right ] in
  let net = Engine.net_force_on b_mid world in
  let f1 = Engine.gravitational_force b_mid ~by:b_left in
  let f2 = Engine.gravitational_force b_mid ~by:b_right in
  approx_vec net
    (vec
       (Vec3.x f1 +. Vec3.x f2)
       (Vec3.y f1 +. Vec3.y f2)
       (Vec3.z f1 +. Vec3.z f2))

(* ---------- step ---------- *)

let test_step_single_body _ =
  let p0 = vec 0. 0. 0. in
  let v0 = vec 1. 2. 0. in
  let b = make_body ~pos:p0 ~vel:v0 ~mass:1. () in
  let dt = 0.5 in
  match Engine.step ~dt [ b ] with
  | [ b' ] ->
      approx_vec (Body.vel b') v0;
      (* no other bodies -> no acceleration *)
      let expected_p =
        vec
          (Vec3.x p0 +. (Vec3.x v0 *. dt))
          (Vec3.y p0 +. (Vec3.y v0 *. dt))
          (Vec3.z p0 +. (Vec3.z v0 *. dt))
      in
      approx_vec (Body.pos b') expected_p
  | _ -> assert_failure "Expected one body"

let test_step_two_body_moves_toward_each_other _ =
  let v0 = vec 0. 0. 0. in
  (* Use smaller radius to avoid collision at distance 2 *)
  let b1 = make_body ~pos:(vec (-1.) 0. 0.) ~vel:v0 ~mass:10. ~radius:0.5 () in
  let b2 = make_body ~pos:(vec 1. 0. 0.) ~vel:v0 ~mass:10. ~radius:0.5 () in
  let world' = Engine.step ~dt:0.1 [ b1; b2 ] in
  match world' with
  | [ b1'; b2' ] ->
      assert_bool "b1 should accelerate right" (Vec3.x (Body.vel b1') > 0.);
      assert_bool "b2 should accelerate left" (Vec3.x (Body.vel b2') < 0.)
  | _ -> assert_failure "Expected two bodies"

(* ---------- Collision Tests ---------- *)

let test_collision_touching _ =
  let b1 =
    make_body ~pos:(vec 0. 0. 0.) ~vel:(vec 0. 0. 0.) ~mass:1. ~radius:5. ()
  in
  let b2 =
    make_body ~pos:(vec 10. 0. 0.) ~vel:(vec 0. 0. 0.) ~mass:1. ~radius:5. ()
  in
  assert_bool "bodies touching should collide" (Engine.check_collision b1 b2)

let test_collision_overlapping _ =
  let b1 =
    make_body ~pos:(vec 0. 0. 0.) ~vel:(vec 0. 0. 0.) ~mass:1. ~radius:10. ()
  in
  let b2 =
    make_body ~pos:(vec 5. 0. 0.) ~vel:(vec 0. 0. 0.) ~mass:1. ~radius:10. ()
  in
  assert_bool "overlapping bodies should collide" (Engine.check_collision b1 b2)

let test_collision_not_touching _ =
  let b1 =
    make_body ~pos:(vec 0. 0. 0.) ~vel:(vec 0. 0. 0.) ~mass:1. ~radius:5. ()
  in
  let b2 =
    make_body ~pos:(vec 20. 0. 0.) ~vel:(vec 0. 0. 0.) ~mass:1. ~radius:5. ()
  in
  assert_bool "separate bodies should not collide"
    (not (Engine.check_collision b1 b2))

let test_find_collisions_none _ =
  let b1 =
    make_body ~pos:(vec 0. 0. 0.) ~vel:(vec 0. 0. 0.) ~mass:1. ~radius:1. ()
  in
  let b2 =
    make_body ~pos:(vec 10. 0. 0.) ~vel:(vec 0. 0. 0.) ~mass:1. ~radius:1. ()
  in
  let collisions = Engine.find_collisions [ b1; b2 ] in
  assert_equal ~printer:string_of_int 0 (List.length collisions)

let test_find_collisions_one_pair _ =
  let b1 =
    make_body ~pos:(vec 0. 0. 0.) ~vel:(vec 0. 0. 0.) ~mass:1. ~radius:10. ()
  in
  let b2 =
    make_body ~pos:(vec 5. 0. 0.) ~vel:(vec 0. 0. 0.) ~mass:1. ~radius:10. ()
  in
  let b3 =
    make_body ~pos:(vec 100. 0. 0.) ~vel:(vec 0. 0. 0.) ~mass:1. ~radius:1. ()
  in
  let collisions = Engine.find_collisions [ b1; b2; b3 ] in
  assert_equal ~printer:string_of_int 1 (List.length collisions)

  (* ---------- Scenario Tests ---------- *)

let test_scenario_get_by_name_known_names _ =
  List.iter
    (fun name ->
      let s = Scenario.get_scenario_by_name name in
      assert_equal
        ~printer:(fun s -> s)
        name s.Scenario.name)
    Scenario.all_scenarios

let test_scenario_get_by_name_unknown_uses_default _ =
  let s = Scenario.get_scenario_by_name "Not A Real Scenario" in
  (* default_scenario name *)
  assert_equal
    ~printer:(fun s -> s)
    "Three-Body Problem" s.Scenario.name

let test_scenario_collision_course_two_bodies _ =
  let s = Scenario.get_scenario_by_name "Collision Course" in
  assert_equal ~printer:string_of_int 2 (List.length s.Scenario.bodies)

  (* ---------- Simulation_state Tests ---------- *)

let test_state_initial_defaults _ =
  let st = Simulation_state.create_initial () in
  approx_equal 1.0 st.time_scale;
  assert_bool "initial not paused" (not st.paused);
  assert_equal
    ~printer:(fun s -> s)
    "Three-Body Problem" st.current_scenario;
  assert_equal ~printer:string_of_int 0 (Simulation_state.num_bodies st);
  assert_bool "no pending changes initially"
    (not (Simulation_state.has_pending_changes st))

let test_state_time_scale_and_pause _ =
  let st0 = Simulation_state.create_initial () in
  let st1 = Simulation_state.set_time_scale st0 2.5 in
  approx_equal 2.5 st1.time_scale;
  let st2 = Simulation_state.toggle_pause st1 in
  assert_bool "paused after toggle" st2.paused

let test_state_pending_changes_after_update _ =
  let st0 = Simulation_state.create_initial () in
  let st1 = Simulation_state.update_planet_density st0 0 9.99e9 in
  assert_bool "now has pending changes"
    (Simulation_state.has_pending_changes st1);
  (* pending_params[0] density should be updated *)
  let d1, _r1 = List.nth st1.pending_params 0 in
  approx_equal 9.99e9 d1

let test_state_apply_params_clears_pending _ =
  let st0 = Simulation_state.create_initial () in
  let st1 = Simulation_state.update_planet_radius st0 1 30.0 in
  assert_bool "pending before apply"
    (Simulation_state.has_pending_changes st1);
  let st2 = Simulation_state.apply_params st1 in
  assert_bool "no pending after apply"
    (not (Simulation_state.has_pending_changes st2));
  (* applied_params should match pending_params *)
  assert_equal
    ~printer:(fun _ -> "params list")
    st2.pending_params st2.applied_params

let test_state_cycle_selected_planet_wraps _ =
  let st0 = Simulation_state.create_initial () in
  assert_equal ~printer:string_of_int 0 st0.selected_planet;
  let st1 = Simulation_state.cycle_selected_planet st0 (-1) in
  (* (0 - 1 + 3) mod 3 = 2 *)
  assert_equal ~printer:string_of_int 2 st1.selected_planet;
  let st2 = Simulation_state.cycle_selected_planet st1 1 in
  assert_equal ~printer:string_of_int 0 st2.selected_planet

let test_state_trail_render_info_active_and_orphaned _ =
  let p1 = vec 0. 0. 0. in
  let active = Simulation_state.Active [ p1 ] in
  let positions_a, alpha_a =
    Simulation_state.get_trail_render_info active 10.0
  in
  assert_equal ~printer:string_of_int 1 (List.length positions_a);
  approx_equal 1.0 alpha_a;
  let orphaned =
    Simulation_state.Orphaned { positions = [ p1 ]; orphaned_at = 0.0 }
  in
  let _positions_o, alpha_o =
    Simulation_state.get_trail_render_info orphaned 1.0
  in
  (* alpha_o should be between 0 and 1 *)
  assert_bool "orphaned alpha in (0,1]"
    (alpha_o <= 1.0 && alpha_o >= 0.0)

    (* ---------- Physics_system Tests ---------- *)

let test_physics_update_small_time_scale_single_step _ =
  let b =
    make_body ~pos:(vec 0. 0. 0.) ~vel:(vec 1. 0. 0.) ~mass:1. ()
  in
  let world0 = [ b ] in
  let time_scale = 0.5 in
  (* fixed_dt is internal, so we use 0.1 which is the known value *)
  let dt = time_scale *. 0.1 in
  let world1, collisions1 =
    Physics_system.update_physics ~time_scale ~world:world0
  in
  let world2, collisions2 =
    Engine.step_with_collisions ~dt world0
  in
  match (world1, world2) with
  | [ b1 ], [ b2 ] ->
      approx_vec (Body.pos b1) (Body.pos b2);
      assert_equal ~printer:string_of_int
        (List.length collisions2)
        (List.length collisions1)
  | _ -> assert_failure "Expected single-body worlds"

let test_physics_update_multi_step_matches_three_steps _ =
  let b =
    make_body ~pos:(vec (-10.) 0. 0.) ~vel:(vec 0. 0. 0.) ~mass:5. ()
  in
  let world0 = [ b ] in
  let time_scale = 3.0 in
  let world_physics, _ =
    Physics_system.update_physics ~time_scale ~world:world0
  in
  let rec three_steps w n =
    if n = 0 then w
    else
      let w', _ =
        Engine.step_with_collisions ~dt:0.1 w
      in
      three_steps w' (n - 1)
  in
  let world_expected = three_steps world0 3 in
  match (world_physics, world_expected) with
  | [ b1 ], [ b2 ] ->
      approx_vec (Body.pos b1) (Body.pos b2)
  | _ -> assert_failure "Expected single-body worlds"

let test_physics_update_trails_adds_and_limits_length _ =
  let body =
    make_body ~pos:(vec 1. 0. 0.) ~vel:(vec 0. 0. 0.) ~mass:1. ()
  in
  let world = [ body ] in
  let initial_trail = [ vec 0. 0. 0. ] in
  let trails, _ =
    Physics_system.update_trails [ initial_trail ] world []
  in
  (match trails with
  | [ t ] ->
      assert_equal ~printer:string_of_int 2 (List.length t);
      approx_vec (List.hd t) (Body.pos body)
  | _ -> assert_failure "Expected one trail");


  let long_trail =
    List.init 105 (fun i -> vec (float_of_int i) 0. 0.)
  in
  let trails2, _ =
    Physics_system.update_trails [ long_trail ] world []
  in
  match trails2 with
  | [ t ] -> assert_equal ~printer:string_of_int 100 (List.length t)
  | _ -> assert_failure "Expected one trail"

let test_physics_is_colliding_wrapper _ =
  (* Non-colliding case *)
  let b1 =
    make_body ~pos:(vec 0. 0. 0.) ~vel:(vec 0. 0. 0.) ~mass:1. ~radius:1. ()
  in
  let b2 =
    make_body ~pos:(vec 10. 0. 0.) ~vel:(vec 0. 0. 0.) ~mass:1. ~radius:1. ()
  in
  assert_bool "no collision"
    (not (Physics_system.is_colliding [ b1; b2 ]));

  (* Colliding case *)
  let b3 =
    make_body ~pos:(vec 0. 0. 0.) ~vel:(vec 0. 0. 0.) ~mass:1. ~radius:5. ()
  in
  let b4 =
    make_body ~pos:(vec 8. 0. 0.) ~vel:(vec 0. 0. 0.) ~mass:1. ~radius:5. ()
  in
  assert_bool "collision detected"
    (Physics_system.is_colliding [ b3; b4 ])

(* ---------- Body Tests ---------- *)

let test_body_set_density_updates_mass _ =
  let b = make_body ~pos:(vec 0. 0. 0.) ~vel:(vec 0. 0. 0.) ~mass:10. ~radius:5. () in
  let initial_mass = Body.mass b in
  Body.set_density 1e11 b;
  let new_mass = Body.mass b in
  assert_bool "mass should change after density update" (new_mass <> initial_mass);
  approx_equal 1e11 (Body.density b)

let test_body_set_radius_updates_mass _ =
  let b = make_body ~pos:(vec 0. 0. 0.) ~vel:(vec 0. 0. 0.) ~mass:10. ~radius:5. () in
  let initial_mass = Body.mass b in
  Body.set_radius 10. b;
  let new_mass = Body.mass b in
  assert_bool "mass should change after radius update" (new_mass <> initial_mass);
  approx_equal 10. (Body.radius b)

let test_body_color _ =
  let color = (255., 100., 50., 255.) in
  let b_with_color = Body.make ~density:1e10 ~pos:(vec 0. 0. 0.) ~vel:(vec 0. 0. 0.)
                                ~radius:1. ~color in
  let r, g, b_val, a = Body.color b_with_color in
  approx_equal 255. r;
  approx_equal 100. g;
  approx_equal 50. b_val;
  approx_equal 255. a

(* ---------- Engine Step with Collisions Tests ---------- *)

let test_engine_step_with_collisions_merges_bodies _ =
  (* Two overlapping bodies should merge *)
  let b1 = make_body ~pos:(vec 0. 0. 0.) ~vel:(vec 0. 0. 0.) ~mass:5. ~radius:10. () in
  let b2 = make_body ~pos:(vec 5. 0. 0.) ~vel:(vec 0. 0. 0.) ~mass:5. ~radius:10. () in
  let b3 = make_body ~pos:(vec 100. 0. 0.) ~vel:(vec 0. 0. 0.) ~mass:5. ~radius:1. () in
  let world = [b1; b2; b3] in
  let new_world, collisions = Engine.step_with_collisions ~dt:0.01 world in
  (* b1 and b2 should merge into one body, plus b3 = 2 bodies *)
  assert_equal ~printer:string_of_int 2 (List.length new_world);
  assert_equal ~printer:string_of_int 1 (List.length collisions)

let test_engine_step_with_collisions_no_collision _ =
  (* Distant bodies should not merge *)
  let b1 = make_body ~pos:(vec 0. 0. 0.) ~vel:(vec 0. 0. 0.) ~mass:1. ~radius:1. () in
  let b2 = make_body ~pos:(vec 100. 0. 0.) ~vel:(vec 0. 0. 0.) ~mass:1. ~radius:1. () in
  let world = [b1; b2] in
  let new_world, collisions = Engine.step_with_collisions ~dt:0.01 world in
  assert_equal ~printer:string_of_int 2 (List.length new_world);
  assert_equal ~printer:string_of_int 0 (List.length collisions)

(* ---------- More Simulation_state Tests ---------- *)

let test_state_set_world _ =
  let st = Simulation_state.create_initial () in
  let b = make_body ~pos:(vec 0. 0. 0.) ~vel:(vec 0. 0. 0.) ~mass:1. () in
  let st2 = Simulation_state.set_world st [b] in
  assert_equal ~printer:string_of_int 1 (Simulation_state.num_bodies st2)

let test_state_load_scenario _ =
  let st = Simulation_state.create_initial () in
  let st2 = Simulation_state.load_scenario st "Binary Star" in
  assert_equal ~printer:(fun s -> s) "Binary Star" st2.current_scenario;
  assert_equal ~printer:string_of_int 0 (List.length st2.trails);
  assert_equal ~printer:string_of_int 0 (List.length st2.collision_anims)

let test_state_reset_to_defaults _ =
  let st = Simulation_state.create_initial () in
  let st2 = Simulation_state.update_planet_density st 0 9e10 in
  let st3 = Simulation_state.reset_to_defaults st2 in
  assert_bool "should not have pending changes after reset"
    (not (Simulation_state.has_pending_changes st3))

let test_state_set_sidebar_and_selected _ =
  let st = Simulation_state.create_initial () in
  let st2 = Simulation_state.set_sidebar_visible st false in
  assert_bool "sidebar should be hidden" (not st2.sidebar_visible);
  let st3 = Simulation_state.set_selected_planet st2 2 in
  assert_equal ~printer:string_of_int 2 st3.selected_planet

let test_state_collision_anims _ =
  let st = Simulation_state.create_initial () in
  let b1 = make_body ~pos:(vec 0. 0. 0.) ~vel:(vec 0. 0. 0.) ~mass:1. () in
  let b2 = make_body ~pos:(vec 1. 0. 0.) ~vel:(vec 0. 0. 0.) ~mass:1. () in
  let collision_pairs = [(b1, b2)] in
  let colors = [(255, 255, 255, 255); (200, 200, 200, 255)] in
  let st2 = Simulation_state.add_collision_animations st collision_pairs colors 0.0 in
  assert_equal ~printer:string_of_int 1 (List.length st2.collision_anims);
  let st3 = Simulation_state.prune_expired_animations st2 10.0 in
  (* Should be expired after 10 seconds *)
  assert_equal ~printer:string_of_int 0 (List.length st3.collision_anims)

let test_state_trails_fading _ =
  let st = Simulation_state.create_initial () in
  let b = make_body ~pos:(vec 5. 5. 5.) ~vel:(vec 0. 0. 0.) ~mass:1. () in
  let st2 = Simulation_state.set_world st [b] in
  (* Update trails with the body's position *)
  let st3 = Simulation_state.update_trails_with_fading st2 [vec 5. 5. 5.] 0.0 in
  assert_bool "should have at least one trail" (List.length st3.trails > 0);
  (* Test orphaned trails - need to wait for fade duration *)
  let st4 = Simulation_state.update_trails_with_fading st3 [vec 6. 6. 6.] 1.0 in
  assert_bool "trails should still exist with active body" (List.length st4.trails > 0)

let test_state_update_planet_params_bounds _ =
  let st = Simulation_state.create_initial () in
  (* Test updating beyond list bounds *)
  let st2 = Simulation_state.update_planet_density st 0 5e10 in
  let d, _ = List.nth st2.pending_params 0 in
  approx_equal 5e10 d;
  let st3 = Simulation_state.update_planet_radius st2 1 25.0 in
  let _, r = List.nth st3.pending_params 1 in
  approx_equal 25.0 r

(* ---------- Vec3 edge cases ---------- *)

let test_vec3_normalize_zero _ =
  let v = Vec3.zer0 in
  let normalized = Vec3.normalize v in
  approx_vec normalized Vec3.zer0

(* ---------- Suite ---------- *)

let suite =
  "engine tests"
  >::: [
         "g_value" >:: test_g_value;
         "grav_same_pos" >:: test_grav_same_position_zero;
         "grav_direction" >:: test_grav_direction;
         "grav_newton3" >:: test_grav_newton3;
         "net_two_body" >:: test_net_force_two_body;
         "net_three_body" >:: test_net_force_three_body;
         "step_single" >:: test_step_single_body;
         "step_two_body" >:: test_step_two_body_moves_toward_each_other;
         "collision_touching" >:: test_collision_touching;
         "collision_overlapping" >:: test_collision_overlapping;
         "collision_not_touching" >:: test_collision_not_touching;
         "find_collisions_none" >:: test_find_collisions_none;
         "find_collisions_one_pair" >:: test_find_collisions_one_pair;

         (* Scenario tests *)
         "scenario_get_by_name_known"
         >:: test_scenario_get_by_name_known_names;
         "scenario_get_by_name_unknown_default"
         >:: test_scenario_get_by_name_unknown_uses_default;
         "scenario_collision_two_bodies"
         >:: test_scenario_collision_course_two_bodies;

         (* Simulation_state tests *)
         "state_initial_defaults" >:: test_state_initial_defaults;
         "state_time_scale_and_pause"
         >:: test_state_time_scale_and_pause;
         "state_pending_changes_after_update"
         >:: test_state_pending_changes_after_update;
         "state_apply_params_clears_pending"
         >:: test_state_apply_params_clears_pending;
         "state_cycle_selected_planet_wraps"
         >:: test_state_cycle_selected_planet_wraps;
         "state_trail_render_info"
         >:: test_state_trail_render_info_active_and_orphaned;

         (* Physics_system tests *)
         "physics_update_small_time_scale"
         >:: test_physics_update_small_time_scale_single_step;
         "physics_update_multi_step"
         >:: test_physics_update_multi_step_matches_three_steps;
         "physics_update_trails"
         >:: test_physics_update_trails_adds_and_limits_length;
         "physics_is_colliding_wrapper"
         >:: test_physics_is_colliding_wrapper;

         (* Body tests *)
         "body_set_density_updates_mass" >:: test_body_set_density_updates_mass;
         "body_set_radius_updates_mass" >:: test_body_set_radius_updates_mass;
         "body_color" >:: test_body_color;

         (* Engine step with collisions tests *)
         "engine_step_with_collisions_merges"
         >:: test_engine_step_with_collisions_merges_bodies;
         "engine_step_with_collisions_no_collision"
         >:: test_engine_step_with_collisions_no_collision;

         (* More simulation_state tests *)
         "state_set_world" >:: test_state_set_world;
         "state_load_scenario" >:: test_state_load_scenario;
         "state_reset_to_defaults" >:: test_state_reset_to_defaults;
         "state_set_sidebar_and_selected" >:: test_state_set_sidebar_and_selected;
         "state_collision_anims" >:: test_state_collision_anims;
         "state_trails_fading" >:: test_state_trails_fading;
         "state_update_planet_params_bounds"
         >:: test_state_update_planet_params_bounds;

         (* Vec3 edge cases *)
         "vec3_normalize_zero" >:: test_vec3_normalize_zero;
       ]


let () = run_test_tt_main suite