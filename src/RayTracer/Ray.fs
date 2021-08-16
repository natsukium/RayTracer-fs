namespace RayTracer

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Ray =
    let inline init orig dir = { Origin = orig; Direction = dir }

    let inline at (t: float) (ray: Ray) = ray.Origin + ray.Direction * t
