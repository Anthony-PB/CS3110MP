open OUnit2
open Group90

(* ---------- Helpers ---------- *)

let vec x y z = Vec3.make x y z

let approx_equal ?(eps = 1e-6) a b =
  assert_bool (Printf.sprintf "Expected %f ≈ %f" a b) (Float.abs (a -. b) <= eps)

let approx_vec ?(eps = 1e-6) v1 v2 =
  approx_equal ~eps (Vec3.x v1) (Vec3.x v2);
  approx_equal ~eps (Vec3.y v1) (Vec3.y v2);
  approx_equal ~eps (Vec3.z v1) (Vec3.z v2)

(* Updated helper to include radius and color *)
let make_body ~pos ~vel ~mass ?(radius = 1.0) ?(color = (255., 255., 255., 255.)) () =
  let density = mass /. (4.0 /. 3.0 *. Float.pi *. (radius ** 3.0)) in
  Body.make ~pos ~vel ~density ~radius ~color

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
  let b1 = make_body ~pos:(vec (-1.) 0. 0.) ~vel:v0 ~mass:10. () in
  let b2 = make_body ~pos:(vec 1. 0. 0.) ~vel:v0 ~mass:10. () in
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
  assert_equal 0 (List.length collisions)

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
  assert_equal 1 (List.length collisions)

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
       ]

let () = run_test_tt_main suite
