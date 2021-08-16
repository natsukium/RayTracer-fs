open RayTracer

let rec rayColor world depth ray : Color =
    if depth <= 0 then
        Color.init 0.0 0.0 0.0
    else
        match Hittable.hit ray 0.001 infinity world with
        | Some record ->
            match Material.scatter ray record with
            | Some (scattered, attenuation) -> attenuation * rayColor world (depth - 1) scattered
            | None -> Color.init 0.0 0.0 0.0
        | None ->
            let unitDirection = Vec3.unit ray.Direction
            let t = 0.5 * unitDirection.Y + 1.0

            (1.0 - t) * Color.init 1.0 1.0 1.0
            + t * Color.init 0.5 0.7 1.0

let aspectRatio = 16.0 / 9.0

[<Literal>]
let ImageWidth = 400

let ImageHeight = float ImageWidth / aspectRatio |> int

[<Literal>]
let SamplesPerPixel = 100

[<Literal>]
let MaxDepth = 10

module Scene =
    let materialGround = Lambertian(Color.init 0.8 0.8 0.0)
    let materialCenter = Lambertian(Color.init 0.1 0.2 0.5)
    let materialLeft = Dielectric 1.5
    let materialRight = Metal(Color.init 0.8 0.6 0.2, 0.0)

    let world =
        [ Sphere.init (Vec3.init 0.0 -100.5 -1.0) 100.0 materialGround
          Sphere.init (Vec3.init 0.0 0.0 -1.0) 0.5 materialCenter
          Sphere.init (Vec3.init -1.0 0.0 -1.0) 0.5 materialLeft
          Sphere.init (Vec3.init -1.0 0.0 -1.0) -0.4 materialLeft
          Sphere.init (Vec3.init 1.0 0.0 -1.0) 0.5 materialRight ]

let camera = Camera.primary ()

let render (w, h) =
    [ 0 .. SamplesPerPixel - 1 ]
    |> List.fold
        (fun acc _ ->
            let u =
                (float w + random 0.0 1.0)
                / float (ImageWidth - 1)

            let v =
                (float h + random 0.0 1.0)
                / float (ImageHeight - 1)

            acc
            + (rayColor Scene.world MaxDepth
               <| Camera.getRay u v camera))
        (Color.init 0.0 0.0 0.0)

    |> Color.writeColor SamplesPerPixel

[<EntryPoint>]
let main _ =
    sprintf "P3\n%d %d\n255" ImageWidth ImageHeight
    |> printfn "%s"

    [| for j in List.rev [ 0 .. ImageHeight - 1 ] do
           for i in [ 0 .. ImageWidth - 1 ] -> i, j |]
    |> Array.Parallel.mapi
        (fun count (i, j) ->
            if count % ImageWidth = 0 then
                eprintf "\rScanlines remaining: %d" (ImageHeight - 1 - (count / ImageWidth))

            render (i, j))
    |> String.concat ""
    |> printfn "%s"

    eprintfn "\ndone"
    0
