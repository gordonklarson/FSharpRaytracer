module Hitable
open Sphere
open Material
open HitRecord
open Ray
open Vector3

type  Hitable = 
    | HitableSphere of Sphere

let rand = System.Random()

let testHit (r:Ray) t_min acc x=
    let (hit, max) = acc
    match x with
    |HitableSphere s ->
        let hit = s.hitSphere r t_min max
        match hit with
        |None -> acc
        |Some y -> (Some y, y.T)

let hit r t_min t_max world =
      let (h, _) =  Seq.fold(fun acc x -> let (_, max) = acc
                                          match x with
                                          |HitableSphere s ->
                                             let sphereHit = s.hitSphere r t_min max
                                             match sphereHit with
                                             |None -> acc
                                             |Some y -> (Some y, y.T)) (None, t_max) world
      h
    
// returns (scattered ray, attenuation)
let scatter rayIn hitRecord = 
    match hitRecord.Mat with
    |Lambertian x -> let target = hitRecord.P + hitRecord.Normal + randomInUnitSphere()
                     Some({Origin= hitRecord.P; Direction = target - hitRecord.P}, x)
    |Metal (x, fuzz) -> let scattered = {Origin = hitRecord.P; Direction = reflect rayIn.Direction.getUnitVector hitRecord.Normal + fuzz * randomInUnitSphere()}
                        match scattered with
                        | s when s.Direction.dot hitRecord.Normal > 0.0 -> 
                            Some(scattered, x)
                        | _ -> None
    |Dialectric refIdx -> 
                        let (outwardNormal, niOverNt, cosine) =
                            let sameDirection = rayIn.Direction.dot hitRecord.Normal
                            match sameDirection with 
                            | d when d > 0.0 -> (-hitRecord.Normal, refIdx, (refIdx * d)/(rayIn.Direction.length()))
                            | d -> (hitRecord.Normal, 1.0/refIdx, (-d)/(rayIn.Direction.length()))
                        let refracted = refract rayIn.Direction outwardNormal niOverNt
                        let reflectProbe = 
                            match refracted with
                            |Some r -> schlick cosine refIdx
                            | None -> 1.0
                        match rand.NextDouble() < reflectProbe with
                        | true -> 
                                let reflected = reflect rayIn.Direction hitRecord.Normal
                                Some({Origin = hitRecord.P; Direction = reflected}, {X = 1.0; Y = 1.0; Z = 1.0})
                        | false -> Some({Origin = hitRecord.P; Direction = Option.get refracted}, {X = 1.0; Y = 1.0; Z = 1.0})
