namespace RayTracer

[<Struct>]
type Vec3 =
    { X: float
      Y: float
      Z: float }
    static member inline Init(x, y, z) = { X = x; Y = y; Z = z }

    static member inline Zero() = Vec3.Init(0.0, 0.0, 0.0)

    static member inline Map f v = Vec3.Init(f v.X, f v.Y, f v.Z)

    static member inline Map2 f u v =
        Vec3.Init(f u.X v.X, f u.Y v.Y, f u.Z v.Z)

    static member inline (~-) v = Vec3.Map(~-) v

    static member inline (+)(u, v) = Vec3.Map2(+) u v

    static member inline (-)(u, v) = Vec3.Map2(-) u v

    static member inline (*)(v, t: float) = Vec3.Map2(*) v (Vec3.Init(t, t, t))

    static member inline (*)(t: float, v: Vec3) = v * t

    static member inline (*)(u, v) = Vec3.Map2(*) u v

    static member inline (/)(v: Vec3, t: float) = v * (1.0 / t)

    static member inline Dot u v = u.X * v.X + u.Y * v.Y + u.Z * v.Z

    static member inline Cross u v =
        Vec3.Init(u.Y * v.Z - u.Z * v.Y, u.Z * v.X - u.X * v.Z, u.X * v.Y - u.Y * v.X)

    member inline this.Unit() = this / this.Length()

    member this.LengthSquared() =
        this.X ** 2.0 + this.Y ** 2.0 + this.Z ** 2.0

    member this.Length() = sqrt (this.LengthSquared())

module Vec3 =
    let init x y z = Vec3.Init(x, y, z)

    let zero = Vec3.Zero

    let map f v = Vec3.Map f v

    let map2 f u v = Vec3.Map2 f u v

    let dot u v = Vec3.Dot u v

    let cross u v = Vec3.Cross u v

    let unit (v: Vec3) = v.Unit()

    let lengthSquared (v: Vec3) = v.LengthSquared()

    let length (v: Vec3) = v.Length()

    let random min max =
        init (random min max) (random min max) (random min max)

    let randomInUnitSphere () =
        Seq.initInfinite (fun _ -> random -1.0 1.0)
        |> Seq.find (fun p -> lengthSquared p < 1.0)

    let randomUnitVector () = unit (randomInUnitSphere ())

    let nearZero (v: Vec3) =
        let s = 1e-8
        abs v.X < s && abs v.Y < s && abs v.Z < s

    let reflect (v: Vec3) (n: Vec3) = v - 2.0 * (dot v n) * n

    let refract (uv: Vec3) (n: Vec3) (ηiOverηt: float) =
        let cosθ = min (dot -uv n) 1.0
        let rOutPerp = ηiOverηt * (uv + cosθ * n)

        let rOutParallel =
            - sqrt(abs (1.0 - lengthSquared rOutPerp)) * n

        rOutPerp + rOutParallel

    let randomInUnitDisk () =
        Seq.initInfinite (fun _ -> init (Utils.random -1.0 1.0) (Utils.random -1.0 1.0) 0.0)
        |> Seq.find (fun p -> lengthSquared p < 1.0)
