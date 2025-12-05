type color = float * float * float * float

type b = {
  density : float ref;
  pos : Vec3.v;
  vel : Vec3.v;
  radius : float ref;
  mass : float ref;
  color : color;
}

let make ~density:d ~pos:p ~vel:v ~radius:r ~color:c =
  {
    density = ref d;
    pos = p;
    vel = v;
    radius = ref r;
    mass = ref (4.0 /. 3.0 *. Float.pi *. (r ** 3.0) *. d);
    color = c;
  }

let mass b = !(b.mass)
let pos b = b.pos
let vel b = b.vel
let radius b = !(b.radius)
let density b = !(b.density)
let with_pos p b = { b with pos = p }
let with_vel v b = { b with vel = v }
let color b = b.color

let set_density d b =
  b.density := d;
  let r = !(b.radius) in
  let volume = 4.0 /. 3.0 *. Float.pi *. (r ** 3.0) in
  b.mass := d *. volume

let set_radius r b =
  b.radius := r;
  let d = !(b.density) in
  let volume = 4.0 /. 3.0 *. Float.pi *. (r ** 3.0) in
  b.mass := d *. volume
