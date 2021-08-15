namespace RayTracer

type Color = Vec3

module Color =
    let init = Vec3.init

    let writeColor (pixelColor: Color) : string =
        let { Color.X = r
              Color.Y = g
              Color.Z = b } =
            pixelColor * 255.999

        sprintf "%d %d %d\n" (int r) (int g) (int b)
