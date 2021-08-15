namespace RayTracer

[<Struct>]
type Ray = { Origin: Point3; Direction: Vec3 }

module Ray =
    let inline init orig dir = { Origin = orig; Direction = dir }

    let inline at (t: float) ray = ray.Origin + ray.Direction * t
