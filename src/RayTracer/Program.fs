open RayTracer

let rayColor world ray : Color =
    match Hittable.hit ray 0.0 infinity world with
    | Some record -> 0.5 * (record.Normal + Color.init 1.0 1.0 1.0)
    | None ->
        let unitDirection = Vec3.unit ray.Direction
        let t = 0.5 * unitDirection.Y + 1.0

        (1.0 - t) * Color.init 1.0 1.0 1.0
        + t * Color.init 0.5 0.7 1.0

let aspectRatio = 16.0 / 9.0

[<Literal>]
let ImageWidth = 400

let ImageHeight = float ImageWidth / aspectRatio |> int

let world =
    [ Sphere.init (Vec3.init 0.0 0.0 -1.0) 0.5
      Sphere.init (Vec3.init 0.0 -100.5 -1.0) 100.0 ]

let viewportHeight = 2.0
let viewportWidth = aspectRatio * viewportHeight
let focalLength = 1.0

let origin = Vec3.zero ()
let horizontal = Vec3.init viewportWidth 0.0 0.0
let vertical = Vec3.init 0.0 viewportHeight 0.0

let lowerLeftCorner =
    origin
    - horizontal / 2.0
    - vertical / 2.0
    - Vec3.init 0.0 0.0 focalLength

let render (w, h) =
    let u = float w / float (ImageWidth - 1)
    let v = float h / float (ImageHeight - 1)

    Ray.init
        origin
        (lowerLeftCorner + u * horizontal + v * vertical
         - origin)
    |> rayColor world
    |> Color.writeColor

[<EntryPoint>]
let main _ =
    sprintf "P3\n%d %d\n255" ImageWidth ImageHeight
    |> printfn "%s"

    seq {
        for j in List.rev [ 0 .. ImageHeight - 1 ] do
            for i in [ 0 .. ImageWidth - 1 ] -> i, j
    }
    |> Seq.mapi
        (fun count (i, j) ->
            if count % ImageWidth = 0 then
                eprintf "\rScanlines remaining: %d" (ImageHeight - 1 - (count / ImageWidth))

            render (i, j))
    |> String.concat ""
    |> printfn "%s"

    eprintfn "\ndone"
    0
