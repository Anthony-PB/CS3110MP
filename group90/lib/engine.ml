let g = 6.67e-11

type w = Body.b list

let merge b1 b2 =
  let m1 = Body.mass b1 in
  let m2 = Body.mass b2 in
  let m = m1 +. m2 in

  let pos1 = Body.pos b1 in
  let pos2 = Body.pos b2 in

  (*Center of mass*)
  let pos = Vec3.(1. /. m *~ ((m1 *~ pos1) + (m2 *~ pos2))) in
  (*new velocity, perfectly inelastic collision (momentum)*)
  let v1 = Body.vel b1 in
  let v2 = Body.vel b2 in
  let vel = Vec3.(1. /. m *~ ((m1 *~ v1) + (m2 *~ v2))) in
  let rho1 = Body.density b1 in
  let rho2 = Body.density b2 in
  let volume1 = m1 /. rho1 in
  let volume2 = m2 /. rho2 in
  let v = volume1 +. volume2 in

  let density = m /. v in
  let radius = (3. *. v /. (4. *. Float.pi)) ** (1. /. 3.) in

  (* Blend colors based on mass ratio *)
  let r1, g1, b1, a1 = Body.color b1 in
  let r2, g2, b2, a2 = Body.color b2 in
  let ratio1 = m1 /. m in
  let ratio2 = m2 /. m in
  let color = (
    r1 *. ratio1 +. r2 *. ratio2,
    g1 *. ratio1 +. g2 *. ratio2,
    b1 *. ratio1 +. b2 *. ratio2,
    a1 *. ratio1 +. a2 *. ratio2
  ) in

  (*creating new merged body*)
  Body.make ~density ~pos ~vel ~radius ~color

let check_collision b1 b2 =
  let sum_of_radii = Body.radius b1 +. Body.radius b2 in
  sum_of_radii >= Vec3.norm Vec3.(Body.pos b1 - Body.pos b2)

let find_collisions world =
  let rec aux acc = function
    | [] -> acc
    | b1 :: rest ->
        let new_collisions =
          List.filter_map
            (fun b2 -> if check_collision b1 b2 then Some (b1, b2) else None)
            rest
        in
        aux (new_collisions @ acc) rest
  in
  aux [] world

let resolve_collisions world =
  let collisions = find_collisions world in

  let merged, to_remove =
    List.fold_left
      (fun (merged_acc, remove_acc) (b1, b2) ->
        let merged_body = merge b1 b2 in
        (merged_body :: merged_acc, b1 :: b2 :: remove_acc))
      ([], []) collisions
  in
  let remaining = List.filter (fun b -> not (List.mem b to_remove)) world in

  merged @ remaining

let resolve_collisions_with_info world =
  let collisions = find_collisions world in

  let merged, to_remove =
    List.fold_left
      (fun (merged_acc, remove_acc) (b1, b2) ->
        let merged_body = merge b1 b2 in
        (merged_body :: merged_acc, b1 :: b2 :: remove_acc))
      ([], []) collisions
  in
  let remaining = List.filter (fun b -> not (List.mem b to_remove)) world in

  (merged @ remaining, collisions)

let gravitational_force b1 ~by:b2 =
  let p1 = Body.pos b1 in
  let p2 = Body.pos b2 in
  let dx = Vec3.x p2 -. Vec3.x p1 in
  let dy = Vec3.y p2 -. Vec3.y p1 in
  let dz = Vec3.z p2 -. Vec3.z p1 in
  let r2 = (dx *. dx) +. (dy *. dy) +. (dz *. dz) in
  if r2 = 0. then Vec3.zer0
  else
    let magnitude = g *. Body.mass b1 *. Body.mass b2 /. r2 in
    let dir = Vec3.make dx dy dz |> Vec3.normalize in
    Vec3.(magnitude *~ dir)

let net_force_on b1 world =
  List.fold_left
    (fun acc b2 ->
      if b1 == b2 then acc else Vec3.(acc + gravitational_force b1 ~by:b2))
    Vec3.zer0 world

let step ~dt world =
  let update =
    List.map
      (fun b ->
        let f = net_force_on b world in
        let a = Vec3.(1. /. Body.mass b *~ f) in
        let old_vel = Body.vel b in
        let new_vel = Vec3.(old_vel + (dt *~ a)) in
        let new_pos = Vec3.(Body.pos b + (dt *~ old_vel)) in
        Body.(b |> with_vel new_vel |> with_pos new_pos))
      world
  in
  resolve_collisions update

let step_with_collisions ~dt world =
  let update =
    List.map
      (fun b ->
        let f = net_force_on b world in
        let a = Vec3.(1. /. Body.mass b *~ f) in
        let old_vel = Body.vel b in
        let new_vel = Vec3.(old_vel + (dt *~ a)) in
        let new_pos = Vec3.(Body.pos b + (dt *~ old_vel)) in
        Body.(b |> with_vel new_vel |> with_pos new_pos))
      world
  in
  resolve_collisions_with_info update
