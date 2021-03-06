namespace RayTracer

type Color = Vec3

type Point3 = Vec3

[<Struct>]
type Ray = { Origin: Point3; Direction: Vec3 }

type Material =
    | Lambertian of Albedo: Color
    | Metal of Albedo: Color * Fuzz: float
    | Dielectric of Ir: float

[<Struct>]
type Face =
    | Front
    | Back

[<Struct>]
type HitRecord =
    { Point: Point3
      Normal: Vec3
      T: float
      Face: Face
      Material: Material }
    static member inline Init(point, normal, t, face, material) =
        { Point = point
          Normal = normal
          T = t
          Face = face
          Material = material }

[<Struct>]
type Sphere =
    { Center: Point3
      Radius: float
      Material: Material }
    static member inline Init(c, r, m) =
        { Center = c; Radius = r; Material = m }

type Camera =
    { Origin: Point3
      LowerLeftCorner: Point3
      Horizontal: Vec3
      Vertical: Vec3
      U: Vec3
      V: Vec3
      W: Vec3
      LensRadius: float }
    static member inline Init(lookFrom: Point3, lookAt: Point3, vUp, vfov, aspectRatio, aperture, focusDist) =
        let θ = degreesToRadians vfov
        let h = tan (θ / 2.0)
        let viewportHeight = 2.0 * h
        let viewportWidth = aspectRatio * viewportHeight

        let w = lookFrom - lookAt |> Vec3.unit
        let u = Vec3.cross vUp w
        let v = Vec3.cross w u

        let origin = lookFrom
        let horizontal = focusDist * viewportWidth * u
        let vertical = focusDist * viewportHeight * v

        let lowerLeftCorner =
            origin
            - horizontal / 2.0
            - vertical / 2.0
            - focusDist * w

        let lensRadius = aperture / 2.0

        { Origin = origin
          LowerLeftCorner = lowerLeftCorner
          Horizontal = horizontal
          Vertical = vertical
          U = u
          V = v
          W = w
          LensRadius = lensRadius }
