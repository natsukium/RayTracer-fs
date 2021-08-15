open RayTracer

[<Literal>]
let ImageWidth = 256

[<Literal>]
let ImageHeight = 256

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

        Color.init (float i / float (ImageWidth - 1)) (float j / float (ImageHeight - 1)) 0.25
        |> Color.writeColor)
|> String.concat ""
|> printfn "%s"

eprintfn "\ndone"
