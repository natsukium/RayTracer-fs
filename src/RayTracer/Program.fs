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
|> Seq.map
    (fun (i, j) ->
        let r = float i / float (ImageWidth - 1)
        let g = float j / float (ImageHeight - 1)
        let b = 0.25
        let ir = int (255.999 * r)
        let ig = int (255.999 * g)
        let ib = int (255.999 * b)
        sprintf "%d %d %d\n" ir ig ib)
|> String.concat ""
|> printfn "%A"
