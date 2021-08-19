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

let aspectRatio = 3.0 / 2.0

[<Literal>]
let ImageWidth = 1200

let ImageHeight = float ImageWidth / aspectRatio |> int

[<Literal>]
let SamplesPerPixel = 500

[<Literal>]
let MaxDepth = 50

module Scene =
    let randomScene () =
        let materialGround = Lambertian(Color.init 0.5 0.5 0.5)

        let randomSphere (a, b) =
            let chooseMat = random 0.0 1.0

            let center =
                Vec3.init (a + 0.9 * random 0.0 1.0) 0.2 (b + 0.9 * random 0.0 1.0)

            if Vec3.length (center - Vec3.init 4.0 0.2 0.0) > 0.9 then
                match chooseMat with
                | diffuse when diffuse < 0.8 ->
                    let albedo =
                        Vec3.random 0.0 1.0 * Vec3.random 0.0 1.0

                    let material = Lambertian albedo
                    Some(Sphere.init center 0.2 material)
                | metal when metal < 0.95 ->
                    let albedo = Vec3.random 0.5 1.0
                    let fuzz = random 0.0 0.5
                    let material = Metal(albedo, fuzz)
                    Some(Sphere.init center 0.2 material)
                | _ ->
                    let material = Dielectric 1.5
                    Some(Sphere.init center 0.2 material)
            else
                None

        let spheres =
            [ for a in [ -11 .. 10 ] do
                  for b in [ -11 .. 10 ] -> float a, float b ]
            |> List.map randomSphere
            |> List.filter Option.isSome
            |> List.map Option.get

        let material1 = Dielectric 1.5
        let material2 = Lambertian(Color.init 0.4 0.2 0.1)
        let material3 = Metal(Color.init 0.7 0.6 0.5, 0.0)

        [ Sphere.init (Vec3.init 0.0 -1000.0 -0.0) 1000.0 materialGround
          Sphere.init (Vec3.init 0.0 1.0 0.0) 1.0 material1
          Sphere.init (Vec3.init -4.0 1.0 0.0) 1.0 material2
          Sphere.init (Vec3.init 4.0 1.0 0.0) 1.0 material3 ]
        @ spheres


module Camera =
    let lookFrom = Vec3.init 13.0 2.0 3.0
    let lookAt = Vec3.init 0.0 0.0 0.0
    let vUp = Vec3.init 0.0 1.0 0.0
    let distToFocus = 10.0
    let aperture = 0.1

    let camera =
        Camera.init lookFrom lookAt vUp 20.0 aspectRatio aperture distToFocus

let render scene (w, h) =
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
            + (rayColor scene MaxDepth
               <| Camera.getRay u v Camera.camera))
        (Color.init 0.0 0.0 0.0)

    |> Color.writeColor SamplesPerPixel

[<EntryPoint>]
let main _ =
    let scene = Scene.randomScene ()

    sprintf "P3\n%d %d\n255" ImageWidth ImageHeight
    |> printfn "%s"

    [| for j in List.rev [ 0 .. ImageHeight - 1 ] do
           for i in [ 0 .. ImageWidth - 1 ] -> i, j |]
    |> Array.Parallel.mapi
        (fun count (i, j) ->
            if count % ImageWidth = 0 then
                eprintf "\rScanlines remaining: %d" (ImageHeight - 1 - (count / ImageWidth))

            render scene (i, j))
    |> String.concat ""
    |> printfn "%s"

    eprintfn "\ndone"
    0
