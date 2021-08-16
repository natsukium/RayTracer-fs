namespace RayTracer

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Camera =
    let inline init viewportHeight viewportWidth focalLength =
        Camera.Init(viewportHeight, viewportWidth, focalLength)

    let inline primary () =
        let aspectRatio = 16.0 / 9.0
        let viewportHeight = 2.0
        let viewportWidth = aspectRatio * viewportHeight
        let focalLength = 1.0
        init viewportHeight viewportWidth focalLength

    let getRay (u: float) (v: float) (camera: Camera) =
        Ray.init
            camera.Origin
            (camera.LowerLeftCorner
             + u * camera.Horizontal
             + v * camera.Vertical
             - camera.Origin)
