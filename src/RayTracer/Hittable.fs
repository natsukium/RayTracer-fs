namespace RayTracer

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module HitRecord =
    let inline init p n t f = HitRecord.Init(p, n, t, f)

    let setFaceNormal ray outwardNormal =
        if Vec3.dot ray.Direction outwardNormal < 0.0 then
            outwardNormal, Front
        else
            -outwardNormal, Back

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Sphere =
    let inline init c r = Sphere.Init(c, r)

    let hit (ray: Ray) tMin tMax sphere =
        let oc = ray.Origin - sphere.Center
        let a = Vec3.lengthSquared ray.Direction
        let bHalf = Vec3.dot oc ray.Direction

        let c =
            Vec3.lengthSquared oc - sphere.Radius ** 2.0

        let discriminant = bHalf ** 2.0 - a * c

        if discriminant < 0.0 then
            None
        else
            let sqrtD = sqrt discriminant
            let rootMinus = (-bHalf - sqrtD) / a
            let rootPlus = (-bHalf + sqrtD) / a

            match rootMinus < tMin || tMax < rootMinus, rootPlus < tMin || tMax < rootPlus with
            | true, true -> None
            | isPlus, _ ->
                let t = if isPlus then rootPlus else rootMinus
                let point = Ray.at t ray

                let normal, face =
                    HitRecord.setFaceNormal ray ((point - sphere.Center) / sphere.Radius)

                Some(HitRecord.init point normal t face)

module Hittable =
    let hit ray tMin tMax (hittables: Sphere list) =
        hittables
        |> List.fold
            (fun (hitRecord, tMax') hittable ->
                match Sphere.hit ray tMin tMax' hittable with
                | Some r -> Some r, r.T
                | None -> hitRecord, tMax')
            (None, tMax)
        |> fst
