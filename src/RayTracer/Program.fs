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

        let r = float i / float (ImageWidth - 1)
        let g = float j / float (ImageHeight - 1)
        let b = 0.25
        let ir = int (255.999 * r)
        let ig = int (255.999 * g)
        let ib = int (255.999 * b)
        sprintf "%d %d %d\n" ir ig ib)
|> String.concat ""
|> printfn "%s"

eprintfn "\ndone"
