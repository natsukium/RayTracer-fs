namespace RayTracer

type Color = Vec3

type Point3 = Vec3

[<Struct>]
type Ray = { Origin: Point3; Direction: Vec3 }

type Material =
    | Lambertian of Albedo: Color
    | Metal of Albedo: Color * Fuzz: float

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
