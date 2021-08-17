namespace RayTracer

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Camera =
    let inline init (lookFrom: Point3) (lookAt: Point3) (vup: Vec3) vfov aspectRatio aperture focusDist =
        Camera.Init(lookFrom, lookAt, vup, vfov, aspectRatio, aperture, focusDist)

    let getRay (s: float) (t: float) (camera: Camera) =
        let rd =
            camera.LensRadius * Vec3.randomInUnitDisk ()

        let offSet = camera.U * rd.X + camera.V * rd.Y

        Ray.init
            (camera.Origin + offSet)
            (camera.LowerLeftCorner
             + s * camera.Horizontal
             + t * camera.Vertical
             - camera.Origin
             - offSet)
