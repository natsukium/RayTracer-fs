namespace RayTracer

module Lambertian =
    let scatter record albedo =
        let scatterDirection =
            match record.Normal + Vec3.randomUnitVector () with
            | d when Vec3.nearZero d -> record.Normal
            | d -> d

        let scattered = Ray.init record.Point scatterDirection
        let attenuation = albedo
        Some(scattered, attenuation)

module Metal =
    let scatter rayIn record albedo fuzz =
        let reflected =
            Vec3.reflect (Vec3.unit rayIn.Direction) record.Normal

        let scattered =
            Ray.init record.Point (reflected + fuzz * Vec3.randomInUnitSphere ())

        let attenuation = albedo

        if Vec3.dot scattered.Direction record.Normal > 0.0 then
            Some(scattered, attenuation)
        else
            None

module Dielectric =
    let reflectance (cos: float) (refIdx: float) =
        let r0 = (1.0 - refIdx) / (1.0 + refIdx)
        r0 ** 2.0 + (1.0 - r0 ** 2.0) * (1.0 - cos) ** 5.0

    let scatter rayIn record ir =
        let attenuation = Color.init 1.0 1.0 1.0

        let refractionRatio =
            match record.Face with
            | Front -> 1.0 / ir
            | Back -> ir

        let unitDirection = Vec3.unit rayIn.Direction

        let cosθ =
            min (Vec3.dot -unitDirection record.Normal) 1.0

        let sinθ = sqrt (1.0 - cosθ ** 2.0)

        let cannotRefract = refractionRatio * sinθ > 1.0

        let direction =
            if cannotRefract
               || reflectance cosθ refractionRatio > random 0.0 1.0 then
                Vec3.reflect unitDirection record.Normal
            else
                Vec3.refract unitDirection record.Normal refractionRatio

        let scattered = Ray.init record.Point direction
        Some(scattered, attenuation)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Material =
    let scatter rayIn (record: HitRecord) =
        match record.Material with
        | Lambertian l -> Lambertian.scatter record l
        | Metal (m, f) -> Metal.scatter rayIn record m f
        | Dielectric ir -> Dielectric.scatter rayIn record ir
