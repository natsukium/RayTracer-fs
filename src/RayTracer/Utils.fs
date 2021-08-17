namespace RayTracer

open System

[<AutoOpen>]
module Utils =
    let random min max =
        let rng = Random()
        min + (max - min) * rng.NextDouble()

    let clamp min max =
        function
        | x when x < min -> min
        | x when x > max -> max
        | x -> x

    let degreesToRadians degrees = degrees * System.Math.PI / 180.0
