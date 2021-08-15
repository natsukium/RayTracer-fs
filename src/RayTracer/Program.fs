open RayTracer

let rayColor ray : Color =
    let unitDirection = Vec3.unit ray.Direction
    let t = 0.5 * unitDirection.Y + 1.0

    (1.0 - t) * Color.init 1.0 1.0 1.0
    + t * Color.init 0.5 0.7 1.0

let aspectRatio = 16.0 / 9.0

[<Literal>]
let ImageWidth = 400

let ImageHeight = float ImageWidth / aspectRatio |> int

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

            let u = float i / float (ImageWidth - 1)
            let v = float j / float (ImageHeight - 1)

            Ray.init
                origin
                (lowerLeftCorner + u * horizontal + v * vertical
                 - origin)
            |> rayColor
            |> Color.writeColor)
    |> String.concat ""
    |> printfn "%s"

    eprintfn "\ndone"
    0
