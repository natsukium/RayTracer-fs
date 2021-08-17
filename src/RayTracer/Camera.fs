namespace RayTracer

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Camera =
    let inline init (lookFrom: Point3) (lookAt: Point3) (vup: Vec3) vfov aspectRatio =
        Camera.Init(lookFrom, lookAt, vup, vfov, aspectRatio)

    let getRay (s: float) (t: float) (camera: Camera) =
        Ray.init
            camera.Origin
            (camera.LowerLeftCorner
             + s * camera.Horizontal
             + t * camera.Vertical
             - camera.Origin)
