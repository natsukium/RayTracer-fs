namespace RayTracer

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Color =
    let init = Vec3.init

    let map = Vec3.map

    let writeColor samplesPerPixel (pixelColor: Color) : string =
        let scale = 1.0 / float samplesPerPixel

        let { Color.X = r
              Color.Y = g
              Color.Z = b } =
            pixelColor * scale
            |> map (sqrt >> (clamp 0.0 0.999) >> (*) 256.0)

        sprintf "%d %d %d\n" (int r) (int g) (int b)
