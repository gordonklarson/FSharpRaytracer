module Sphere
open Vector3
open Ray
open HitRecord
open System
open Material

type Sphere  =
    {
        Center : Vec3;
        Radius : float;
        Mat : Material
    }

    member this.hitSphere (r: Ray) (t_min: float) (t_max: float)  =
        let oc = r.Origin - this.Center
        let a = r.Direction.dot r.Direction
        let b = oc.dot r.Direction
        let c = (oc.dot oc) - (this.Radius*this.Radius)
    
        match (b*b - a*c) > 0.0 with
        |true ->
            let temp = (-b - sqrt(b*b-a*c))/a
            match temp with
            | x when ((x < t_max) && (x > t_min))->
                    let p = pointAtParameter r x
                    let normal = (p - this.Center) / this.Radius
                    Some {T= x; P = p; Normal=normal; Mat = this.Mat}
            |_ ->
                let temp1 = (-b + sqrt(b*b-a*c))/a
                match temp1 with
                | x when ((x < t_max) && (x > t_min))->
                    let p = pointAtParameter r x
                    let normal = (p - this.Center) / this.Radius
                    Some {T= x; P = p; Normal = normal; Mat = this.Mat}
                | _ -> None
        |false -> None

let rand = System.Random()

let rec randomInUnitSphere () =
        let p = 2.0 * {X = rand.NextDouble(); Y =  rand.NextDouble(); Z =  rand.NextDouble()} - {X = 1.0; Y =  1.0; Z = 1.0}
        match p with 
        | x when x.squaredLength < 1.0 ->
            x
        |_ -> randomInUnitSphere()
    