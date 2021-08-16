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

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Material =
    let scatter rayIn (record: HitRecord) =
        match record.Material with
        | Lambertian l -> Lambertian.scatter record l
        | Metal (m, f) -> Metal.scatter rayIn record m f
