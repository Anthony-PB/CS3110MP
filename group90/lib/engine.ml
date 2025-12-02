let g = 6.67e-11


type w = Body.b list


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


(* Single physics integration step with fixed dt *)
let step_fixed fixed_dt world =
 List.map
   (fun b ->
     let f = net_force_on b world in
     let a = Vec3.(1. /. Body.mass b *~ f) in
     let new_vel = Vec3.(Body.vel b + (fixed_dt *~ a)) in
     let new_pos = Vec3.(Body.pos b + (fixed_dt *~ new_vel)) in
     Body.(b |> with_vel new_vel |> with_pos new_pos))
   world


(* Main step function with fixed timestep subdivision *)
let step ~dt world =
 (* Always use fixed internal timestep for consistent physics *)
 let fixed_dt = 0.016 in (* ~60 FPS physics rate *)
 let num_steps = max 1 (int_of_float (dt /. fixed_dt)) in
 let actual_dt = dt /. float_of_int num_steps in


 (* Run multiple small fixed timesteps *)
 let rec substep remaining_steps current_world =
   if remaining_steps <= 0 then current_world
   else substep (remaining_steps - 1) (step_fixed actual_dt current_world)
 in
 substep num_steps world
