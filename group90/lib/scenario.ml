(** Scenario module - handles creation of different physics scenarios.

    This module provides functions for creating various celestial body
    configurations for simulation. *)

type scenario = {
  name : string;
  description : string;
  bodies : Body.b list;
  recommended_camera : float * float * float; (* theta, phi, radius *)
}

(** Gravitational constant *)
let g = 6.67e-11

(** Color palette for celestial bodies *)
let sun_color = (255., 220., 80., 255.) (* Bright yellow *)

let planet_colors = [|
  (255., 200., 100., 255.); (* Orange *)
  (100., 150., 255., 255.); (* Blue *)
  (255., 100., 100., 255.); (* Red *)
  (120., 180., 255., 255.); (* Light blue *)
  (180., 120., 255., 255.); (* Purple *)
  (100., 255., 150., 255.); (* Green *)
  (255., 150., 200., 255.); (* Pink *)
  (200., 200., 100., 255.); (* Yellow-green *)
|]

(** Get a planet color by index *)
let get_planet_color index =
  planet_colors.(index mod Array.length planet_colors)

(** Get a random planet color *)
let random_planet_color () =
  planet_colors.(Random.int (Array.length planet_colors))

(** Helper: Calculate mass from density and radius *)
let mass_from_density_radius ~density ~radius =
  4.0 /. 3.0 *. Float.pi *. (radius ** 3.0) *. density

(** Helper: Calculate orbital velocity for circular orbit *)
let orbital_velocity ~central_mass ~orbital_radius =
  Float.sqrt (g *. central_mass /. orbital_radius)

(** Helper: Calculate center-of-mass positions for binary system *)
let binary_com_positions ~mass1 ~mass2 ~separation =
  let r1 = separation *. mass2 /. (mass1 +. mass2) in
  let r2 = separation *. mass1 /. (mass1 +. mass2) in
  (r1, r2)

(** Helper: Calculate binary orbital velocities *)
let binary_velocities ~mass1 ~mass2 ~separation =
  let total_mass = mass1 +. mass2 in
  let v_rel = Float.sqrt (g *. total_mass /. separation) in
  let v1 = v_rel *. mass2 /. total_mass in
  let v2 = v_rel *. mass1 /. total_mass in
  (v1, v2)

(** Helper: Random float in range *)
let rand_range min max = min +. Random.float (max -. min)

(** Create a three-body orbital system with custom parameters. Parameters:
    (density1, radius1, density2, radius2, density3, radius3) *)
let create_three_body_system ?(custom_params = None) () =
  (* Default densities and radii - deterministic *)
  let default_density1 = 3.5747e10 in
  let default_radius1 = 20. in
  let default_density2 = 2.6810e10 in
  let default_radius2 = 18. in
  let default_density3 = 1.7873e10 in
  let default_radius3 = 16. in

  (* Use custom params if provided, otherwise use defaults *)
  let density1, radius1, density2, radius2, density3, radius3 =
    match custom_params with
    | Some params -> params
    | None ->
        ( default_density1,
          default_radius1,
          default_density2,
          default_radius2,
          default_density3,
          default_radius3 )
  in

  (* Separation for binary pair *)
  let separation = 120. in

  (* Calculate masses using helper *)
  let mass1 = mass_from_density_radius ~density:density1 ~radius:radius1 in
  let mass2 = mass_from_density_radius ~density:density2 ~radius:radius2 in

  (* Get positions and velocities for binary system *)
  let r1, r2 = binary_com_positions ~mass1 ~mass2 ~separation in
  let v1, v2 = binary_velocities ~mass1 ~mass2 ~separation in

  (* Body 1 - Heavy star orbiting in YZ plane *)
  let body1 =
    Body.make ~density:density1
      ~pos:(Vec3.make 0. (-.r1) 0.)
      ~vel:(Vec3.make 0. 0. v1) ~radius:radius1 ~color:(get_planet_color 0)
  in
  (* Body 2 - Medium companion orbiting opposite direction *)
  let body2 =
    Body.make ~density:density2
      ~pos:(Vec3.make 0. r2 0.)
      ~vel:(Vec3.make 0. 0. (-.v2)) ~radius:radius2 ~color:(get_planet_color 1)
  in
  (* Body 3 - Interloper approaching at an angle *)
  let body3 =
    Body.make ~density:density3 ~pos:(Vec3.make 180. 60. 100.)
      ~vel:(Vec3.make (-1.0) (-0.4) (-0.6))
      ~radius:radius3 ~color:(get_planet_color 2)
  in
  [ body1; body2; body3 ]

(** Create randomized three-body system *)
let create_randomized_three_body () =
  let base_mass = 7000. /. g in
  let mass_variation = 0.3 in
  let mass1 = base_mass *. rand_range (1. -. mass_variation) (1. +. mass_variation) in
  let mass2 = base_mass *. rand_range (1. -. mass_variation) (1. +. mass_variation) in
  let mass3 = base_mass *. rand_range (1. -. mass_variation) (1. +. mass_variation) in

  let base_radius = 18. in
  let radius_variation = 0.2 in
  let radius1 = base_radius *. rand_range (1. -. radius_variation) (1. +. radius_variation) in
  let radius2 = base_radius *. rand_range (1. -. radius_variation) (1. +. radius_variation) in
  let radius3 = base_radius *. rand_range (1. -. radius_variation) (1. +. radius_variation) in

  (* Calculate densities from target masses and radii *)
  let density1 = mass1 /. (4.0 /. 3.0 *. Float.pi *. (radius1 ** 3.0)) in
  let density2 = mass2 /. (4.0 /. 3.0 *. Float.pi *. (radius2 ** 3.0)) in
  let density3 = mass3 /. (4.0 /. 3.0 *. Float.pi *. (radius3 ** 3.0)) in

  let separation = 150. in
  let r1, r2 = binary_com_positions ~mass1 ~mass2 ~separation in

  (* Random offsets and velocities *)
  let offset1 = Vec3.make (rand_range (-15.) 15.) (rand_range (-15.) 15.) (rand_range (-15.) 15.) in
  let offset2 = Vec3.make (rand_range (-15.) 15.) (rand_range (-15.) 15.) (rand_range (-15.) 15.) in
  let offset3 = Vec3.make (rand_range (-20.) 20.) (rand_range (-20.) 20.) (rand_range (-20.) 20.) in

  let speed_max = 6.0 in
  let vel1 = Vec3.make (rand_range (-.speed_max) speed_max) (rand_range (-.speed_max) speed_max) (rand_range (-.speed_max) speed_max) in
  let vel2 = Vec3.make (rand_range (-.speed_max) speed_max) (rand_range (-.speed_max) speed_max) (rand_range (-.speed_max) speed_max) in
  let vel3 = Vec3.make (rand_range (-.speed_max) speed_max) (rand_range (-.speed_max) speed_max) (rand_range (-.speed_max) speed_max) in

  let body1 =
    Body.make ~density:density1
      ~pos:Vec3.(offset1 + make 0. (-.r1) 0.)
      ~vel:vel1 ~radius:radius1 ~color:(random_planet_color ())
  in
  let body2 =
    Body.make ~density:density2
      ~pos:Vec3.(offset2 + make 0. r2 0.)
      ~vel:vel2 ~radius:radius2 ~color:(random_planet_color ())
  in
  let third_distance = rand_range 180. 240. in
  let third_angle = rand_range 0. (2. *. Float.pi) in
  let body3 =
    Body.make ~density:density3
      ~pos:Vec3.(offset3 + make (third_distance *. Float.cos third_angle) (rand_range 40. 80.) (third_distance *. Float.sin third_angle))
      ~vel:vel3 ~radius:radius3 ~color:(random_planet_color ())
  in
  [ body1; body2; body3 ]

(** Randomized three-body scenario *)
let randomized_three_body_scenario () =
  {
    name = "Randomized 3-Body";
    description = "Three bodies with random masses, positions, and velocities";
    bodies = create_randomized_three_body ();
    recommended_camera = (0.0, 0.0, 400.0);
  }

(** Create default three-body scenario *)
let default_scenario () =
  {
    name = "Three-Body Problem";
    description = "A binary star system with an interloping third body";
    bodies = create_three_body_system ();
    recommended_camera = (0.0, 0.0, 400.0);
    (* theta, phi, radius *)
  }

(** Create a scenario from custom planet parameters *)
let scenario_from_params params_list =
  (* Support variable number of bodies - currently handles 3 for backward compatibility *)
  let d1, r1, d2, r2, d3, r3 =
    match params_list with
    | (d1, r1) :: (d2, r2) :: (d3, r3) :: _ -> (d1, r1, d2, r2, d3, r3)
    | _ -> (3.5747e10, 20., 2.6810e10, 18., 1.7873e10, 16.)
    (* fallback *)
  in
  {
    name = "Custom Three-Body System";
    description = "User-configured three-body system";
    bodies =
      create_three_body_system ~custom_params:(Some (d1, r1, d2, r2, d3, r3)) ();
    recommended_camera = (0.0, 0.0, 400.0);
  }

(** Binary Star System - two stars in stable orbit *)
let binary_star_scenario () =
  let star_density = 5e10 in
  let star1_radius = 25. in
  let star2_radius = 22. in

  let mass1 = mass_from_density_radius ~density:star_density ~radius:star1_radius in
  let mass2 = mass_from_density_radius ~density:star_density ~radius:star2_radius in

  let separation = 150. in
  let r1, r2 = binary_com_positions ~mass1 ~mass2 ~separation in
  let v1, v2 = binary_velocities ~mass1 ~mass2 ~separation in

  let star1 =
    Body.make ~density:star_density ~pos:(Vec3.make 0. (-.r1) 0.)
      ~vel:(Vec3.make v1 0. 0.) ~radius:star1_radius ~color:(get_planet_color 0)
  in
  let star2 =
    Body.make ~density:star_density ~pos:(Vec3.make 0. r2 0.)
      ~vel:(Vec3.make (-.v2) 0. 0.) ~radius:star2_radius ~color:(get_planet_color 1)
  in

  {
    name = "Binary Star";
    description = "Two stars in stable circular orbit";
    bodies = [ star1; star2 ];
    recommended_camera = (0.0, 0.0, 400.0);
  }

(** Solar System-like - central star with orbiting planets *)
let solar_system_scenario () =
  let sun_density = 8e10 in
  let sun_radius = 30. in
  let sun =
    Body.make ~density:sun_density ~pos:(Vec3.make 0. 0. 0.)
      ~vel:(Vec3.make 0. 0. 0.) ~radius:sun_radius ~color:sun_color
  in

  let sun_mass = mass_from_density_radius ~density:sun_density ~radius:sun_radius in

  (* Inner planet *)
  let planet1_radius = 12. in
  let planet1_density = 2e10 in
  let orbit1 = 100. in
  let v1 = orbital_velocity ~central_mass:sun_mass ~orbital_radius:orbit1 in
  let planet1 =
    Body.make ~density:planet1_density ~pos:(Vec3.make orbit1 0. 0.)
      ~vel:(Vec3.make 0. 0. v1) ~radius:planet1_radius ~color:(get_planet_color 0)
  in

  (* Outer planet *)
  let planet2_radius = 15. in
  let planet2_density = 1.8e10 in
  let orbit2 = 180. in
  let v2 = orbital_velocity ~central_mass:sun_mass ~orbital_radius:orbit2 in
  let planet2 =
    Body.make ~density:planet2_density
      ~pos:(Vec3.make (-.orbit2) 0. 0.)
      ~vel:(Vec3.make 0. 0. (-.v2)) ~radius:planet2_radius ~color:(get_planet_color 1)
  in

  {
    name = "Solar System";
    description = "Central star with two orbiting planets";
    bodies = [ sun; planet1; planet2 ];
    recommended_camera = (0.0, 0.0, 450.0);
  }

(** Collision Course - two bodies heading for collision *)
let collision_scenario () =
  let body1_density = 4e10 in
  let body1_radius = 22. in
  let body2_density = 3.5e10 in
  let body2_radius = 20. in

  let body1 =
    Body.make ~density:body1_density ~pos:(Vec3.make (-150.) 0. 0.)
      ~vel:(Vec3.make 2.0 0. 0.) ~radius:body1_radius ~color:(get_planet_color 0)
  in
  let body2 =
    Body.make ~density:body2_density ~pos:(Vec3.make 150. 0. 0.)
      ~vel:(Vec3.make (-2.0) 0. 0.) ~radius:body2_radius ~color:(get_planet_color 1)
  in

  {
    name = "Collision Course";
    description = "Two bodies on collision trajectory";
    bodies = [ body1; body2 ];
    recommended_camera = (0.0, 0.0, 400.0);
  }

(** Figure-8 Orbit - special chaotic three-body configuration *)
let figure_eight_scenario () =
  let body_density = 3e10 in
  let body_radius = 18. in

  (* Figure-8 requires very specific initial conditions *)
  let body1 =
    Body.make ~density:body_density
      ~pos:(Vec3.make 97.0 (-24.3) 0.)
      ~vel:(Vec3.make 0.466 0.433 0.) ~radius:body_radius ~color:(get_planet_color 0)
  in
  let body2 =
    Body.make ~density:body_density
      ~pos:(Vec3.make (-97.0) 24.3 0.)
      ~vel:(Vec3.make 0.466 0.433 0.) ~radius:body_radius ~color:(get_planet_color 1)
  in
  let body3 =
    Body.make ~density:body_density ~pos:(Vec3.make 0. 0. 0.)
      ~vel:(Vec3.make (-0.932) (-0.866) 0.)
      ~radius:body_radius ~color:(get_planet_color 2)
  in

  {
    name = "Figure-8 Orbit";
    description = "Chaotic three-body figure-8 configuration";
    bodies = [ body1; body2; body3 ];
    recommended_camera = (0.0, 0.0, 400.0);
  }

(** Real Solar System - accurate recreation with 8 planets and major moons *)
let real_solar_system_scenario () =
  (* Scale factor: divide real distances by this to fit in simulation
     Real solar system is huge, so we scale distances down significantly
     while keeping relative proportions accurate *)
  let distance_scale = 1e9 in (* 1 billion meters = 1 simulation unit *)
  let radius_scale = 1e6 in (* 1 million meters = 1 simulation unit *)
  
  (* Sun - center of the solar system *)
  let sun_radius = 696000e3 /. radius_scale in (* 696,000 km *)
  let sun_density = 1408. in (* kg/m³ *)
  let sun = Body.make ~density:sun_density ~pos:(Vec3.make 0. 0. 0.)
    ~vel:(Vec3.make 0. 0. 0.) ~radius:sun_radius ~color:sun_color in
  let sun_mass = mass_from_density_radius ~density:sun_density ~radius:sun_radius in
  
  (* Mercury *)
  let mercury_orbit = 57.9e9 /. distance_scale in
  let mercury_radius = 2439.7e3 /. radius_scale in
  let mercury_density = 5427. in
  let mercury_vel = orbital_velocity ~central_mass:sun_mass ~orbital_radius:mercury_orbit in
  let mercury = Body.make ~density:mercury_density ~pos:(Vec3.make mercury_orbit 0. 0.)
    ~vel:(Vec3.make 0. 0. mercury_vel) ~radius:mercury_radius ~color:(get_planet_color 0) in
  
  (* Venus *)
  let venus_orbit = 108.2e9 /. distance_scale in
  let venus_radius = 6051.8e3 /. radius_scale in
  let venus_density = 5243. in
  let venus_vel = orbital_velocity ~central_mass:sun_mass ~orbital_radius:venus_orbit in
  let venus = Body.make ~density:venus_density ~pos:(Vec3.make (-.venus_orbit) 0. 0.)
    ~vel:(Vec3.make 0. 0. (-.venus_vel)) ~radius:venus_radius ~color:(get_planet_color 1) in
  
  (* Earth *)
  let earth_orbit = 149.6e9 /. distance_scale in
  let earth_radius = 6371e3 /. radius_scale in
  let earth_density = 5514. in
  let earth_vel = orbital_velocity ~central_mass:sun_mass ~orbital_radius:earth_orbit in
  let earth = Body.make ~density:earth_density ~pos:(Vec3.make 0. earth_orbit 0.)
    ~vel:(Vec3.make earth_vel 0. 0.) ~radius:earth_radius ~color:(get_planet_color 2) in
  
  (* Moon (Earth's) *)
  let moon_orbit_from_earth = 384400e3 /. distance_scale in
  let moon_radius = 1737.4e3 /. radius_scale in
  let moon_density = 3344. in
  let earth_mass = mass_from_density_radius ~density:earth_density ~radius:earth_radius in
  let moon_vel_relative = orbital_velocity ~central_mass:earth_mass ~orbital_radius:moon_orbit_from_earth in
  let moon = Body.make ~density:moon_density
    ~pos:(Vec3.make moon_orbit_from_earth earth_orbit 0.)
    ~vel:(Vec3.make earth_vel 0. moon_vel_relative) ~radius:moon_radius ~color:(get_planet_color 3) in
  
  (* Mars *)
  let mars_orbit = 227.9e9 /. distance_scale in
  let mars_radius = 3389.5e3 /. radius_scale in
  let mars_density = 3933. in
  let mars_vel = orbital_velocity ~central_mass:sun_mass ~orbital_radius:mars_orbit in
  let mars = Body.make ~density:mars_density ~pos:(Vec3.make 0. (-.mars_orbit) 0.)
    ~vel:(Vec3.make (-.mars_vel) 0. 0.) ~radius:mars_radius ~color:(get_planet_color 4) in
  
  (* Jupiter *)
  let jupiter_orbit = 778.5e9 /. distance_scale in
  let jupiter_radius = 69911e3 /. radius_scale in
  let jupiter_density = 1326. in
  let jupiter_vel = orbital_velocity ~central_mass:sun_mass ~orbital_radius:jupiter_orbit in
  let jupiter = Body.make ~density:jupiter_density ~pos:(Vec3.make jupiter_orbit 0. 0.)
    ~vel:(Vec3.make 0. 0. jupiter_vel) ~radius:jupiter_radius ~color:(get_planet_color 5) in
  
  (* Io (Jupiter's moon) *)
  let io_orbit = 421700e3 /. distance_scale in
  let io_radius = 1821.6e3 /. radius_scale in
  let io_density = 3528. in
  let jupiter_mass = mass_from_density_radius ~density:jupiter_density ~radius:jupiter_radius in
  let io_vel_relative = orbital_velocity ~central_mass:jupiter_mass ~orbital_radius:io_orbit in
  let io = Body.make ~density:io_density
    ~pos:(Vec3.make (jupiter_orbit +. io_orbit) 0. 0.)
    ~vel:(Vec3.make 0. 0. (jupiter_vel +. io_vel_relative)) ~radius:io_radius ~color:(get_planet_color 6) in
  
  (* Europa (Jupiter's moon) *)
  let europa_orbit = 671100e3 /. distance_scale in
  let europa_radius = 1560.8e3 /. radius_scale in
  let europa_density = 3013. in
  let europa_vel_relative = orbital_velocity ~central_mass:jupiter_mass ~orbital_radius:europa_orbit in
  let europa = Body.make ~density:europa_density
    ~pos:(Vec3.make (jupiter_orbit -. europa_orbit) 0. 0.)
    ~vel:(Vec3.make 0. 0. (jupiter_vel -. europa_vel_relative)) ~radius:europa_radius ~color:(get_planet_color 7) in
  
  (* Saturn *)
  let saturn_orbit = 1433.5e9 /. distance_scale in
  let saturn_radius = 58232e3 /. radius_scale in
  let saturn_density = 687. in
  let saturn_vel = orbital_velocity ~central_mass:sun_mass ~orbital_radius:saturn_orbit in
  let saturn = Body.make ~density:saturn_density ~pos:(Vec3.make (-.saturn_orbit) 0. 0.)
    ~vel:(Vec3.make 0. 0. (-.saturn_vel)) ~radius:saturn_radius ~color:(get_planet_color 0) in
  
  (* Titan (Saturn's moon) *)
  let titan_orbit = 1221870e3 /. distance_scale in
  let titan_radius = 2574.7e3 /. radius_scale in
  let titan_density = 1880. in
  let saturn_mass = mass_from_density_radius ~density:saturn_density ~radius:saturn_radius in
  let titan_vel_relative = orbital_velocity ~central_mass:saturn_mass ~orbital_radius:titan_orbit in
  let titan = Body.make ~density:titan_density
    ~pos:(Vec3.make (-.saturn_orbit -. titan_orbit) 0. 0.)
    ~vel:(Vec3.make 0. 0. (-.saturn_vel -. titan_vel_relative)) ~radius:titan_radius ~color:(get_planet_color 1) in
  
  (* Uranus *)
  let uranus_orbit = 2872.5e9 /. distance_scale in
  let uranus_radius = 25362e3 /. radius_scale in
  let uranus_density = 1271. in
  let uranus_vel = orbital_velocity ~central_mass:sun_mass ~orbital_radius:uranus_orbit in
  let uranus = Body.make ~density:uranus_density ~pos:(Vec3.make 0. uranus_orbit 0.)
    ~vel:(Vec3.make uranus_vel 0. 0.) ~radius:uranus_radius ~color:(get_planet_color 2) in
  
  (* Neptune *)
  let neptune_orbit = 4495.1e9 /. distance_scale in
  let neptune_radius = 24622e3 /. radius_scale in
  let neptune_density = 1638. in
  let neptune_vel = orbital_velocity ~central_mass:sun_mass ~orbital_radius:neptune_orbit in
  let neptune = Body.make ~density:neptune_density ~pos:(Vec3.make 0. (-.neptune_orbit) 0.)
    ~vel:(Vec3.make (-.neptune_vel) 0. 0.) ~radius:neptune_radius ~color:(get_planet_color 3) in
  
  (* Triton (Neptune's moon) *)
  let triton_orbit = 354759e3 /. distance_scale in
  let triton_radius = 1353.4e3 /. radius_scale in
  let triton_density = 2061. in
  let neptune_mass = mass_from_density_radius ~density:neptune_density ~radius:neptune_radius in
  let triton_vel_relative = orbital_velocity ~central_mass:neptune_mass ~orbital_radius:triton_orbit in
  let triton = Body.make ~density:triton_density
    ~pos:(Vec3.make triton_orbit (-.neptune_orbit) 0.)
    ~vel:(Vec3.make (-.neptune_vel) 0. (-.triton_vel_relative)) ~radius:triton_radius ~color:(get_planet_color 4) in
  
  {
    name = "Real Solar System";
    description = "Accurate scale model of our solar system with 8 planets and major moons";
    bodies = [
      sun; mercury; venus; earth; moon; mars; jupiter; io; europa;
      saturn; titan; uranus; neptune; triton
    ];
    recommended_camera = (0.0, 0.0, 6000.0); (* Zoomed out to see outer planets *)
  }

(** Get scenario by name *)
let get_scenario_by_name name =
  match name with
  | "Randomized 3-Body" -> randomized_three_body_scenario ()
  | "Binary Star" -> binary_star_scenario ()
  | "Solar System" -> solar_system_scenario ()
  | "Real Solar System" -> real_solar_system_scenario ()
  | "Collision Course" -> collision_scenario ()
  | "Figure-8 Orbit" -> figure_eight_scenario ()
  | "Three-Body Problem" | _ -> default_scenario ()

(** List of all available scenarios *)
let all_scenarios =
  [
    "Three-Body Problem";
    "Randomized 3-Body";
    "Binary Star";
    "Solar System";
    "Real Solar System";
    "Collision Course";
    "Figure-8 Orbit";
  ]
