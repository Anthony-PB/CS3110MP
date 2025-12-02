(** N-body physics simulation engine.
    Implements gravitational interactions and numerical integration for multiple bodies. *)

(** [g] is the gravitational constant in SI units: 6.67e-11 N⋅m²/kg².
    Used in Newton's law of universal gravitation: F = G * m1 * m2 / r² *)
val g : float

(** The type representing a world (collection of celestial bodies).
    A world is a list of bodies that interact gravitationally. *)
type w = Body.b list 

(** [gravitational_force b1 ~by:b2] computes the gravitational force vector
    exerted on body [b1] by body [b2].
    Uses Newton's law: F = G * m1 * m2 / r² in the direction from [b1] to [b2].
    Returns [Vec3.zer0] if bodies occupy the same position to avoid singularity.
    @param b1 The body experiencing the force.
    @param by The body exerting the force.
    @return Force vector in Newtons pointing from [b1] toward [b2]. *)
val gravitational_force : Body.b -> by:Body.b -> Vec3.v

(** [net_force_on b world] computes the total gravitational force acting on body [b]
    from all other bodies in [world].
    Sums the pairwise gravitational forces from each body in the world.
    @param b The body to compute net force for.
    @param world The collection of all bodies (including [b]).
    @return Net force vector in Newtons. *)
val net_force_on : Body.b -> w -> Vec3.v

(** [step ~dt world] advances the simulation by time [dt] using fixed timestep subdivision.
    Uses Euler integration with automatic substeps: subdivides [dt] into multiple
    small fixed timesteps (~60 FPS physics rate) for consistent behavior.
    For each substep and body: computes net force, then acceleration (F=ma),
    updates velocity (v' = v + a*dt), and updates position (p' = p + v'*dt).
    @param dt Total time to advance in seconds. Larger values speed up simulation.
    @param world The current state of all bodies.
    @return New world state after advancing by time [dt].
    Note: This uses explicit Euler method with fixed internal timesteps.
          Increasing [dt] speeds up simulation without affecting physics accuracy. *)
val step : dt:float -> w -> w

(** [check_collision b1 b2] checks if two bodies are colliding based on their positions and radii.
    A collision occurs if the distance between the centers of the bodies is less than
    or equal to the sum of their radii.
    @param b1 The first body.
    @param b2 The second body.
    @return [true] if the bodies are colliding, [false] otherwise. *)
val check_collision : Body.b -> Body.b -> bool

(** [find_collisions world] returns a list of all pairs of bodies in [world] that are colliding.
    Iterates through all unique pairs of bodies and checks for collisions using [check_collision].
    @param world The collection of all bodies.
    @return A list of tuples, each containing a pair of colliding bodies. *)
val find_collisions : w -> (Body.b * Body.b) list