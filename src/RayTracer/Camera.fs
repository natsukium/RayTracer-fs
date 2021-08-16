namespace RayTracer

type Camera =
    { Origin: Point3
      LowerLeftCorner: Point3
      Horizontal: Vec3
      Vertical: Vec3 }
    static member inline Init(viewportHeight, viewportWidth, focalLength) =
        let origin = Vec3.init 0.0 0.0 0.0
        let horizontal = Vec3.init viewportWidth 0.0 0.0
        let vertical = Vec3.init 0.0 viewportHeight 0.0

        { Origin = origin
          LowerLeftCorner =
              origin
              - horizontal / 2.0
              - vertical / 2.0
              - Vec3.init 0.0 0.0 focalLength
          Horizontal = horizontal
          Vertical = vertical }


module Camera =
    let inline init viewportHeight viewportWidth focalLength =
        Camera.Init(viewportHeight, viewportWidth, focalLength)

    let inline primary () =
        let aspectRatio = 16.0 / 9.0
        let viewportHeight = 2.0
        let viewportWidth = aspectRatio * viewportHeight
        let focalLength = 1.0
        init viewportHeight viewportWidth focalLength

    let getRay (u: float) (v: float) camera =
        Ray.init
            camera.Origin
            (camera.LowerLeftCorner
             + u * camera.Horizontal
             + v * camera.Vertical
             - camera.Origin)
