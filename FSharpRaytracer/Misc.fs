module Misc
open System
open Vector3
open Ray
open HitRecord
open Hitable
open Sphere
open Camera


let nx = 200.0
let ny = 100.0

let camera = 
    {
        VFOV = 20.0;
        Aspect = nx/ny;
        LookFrom = {X = -2.0; Y = 2.0; Z = 1.0};
        LookAt = { X = 0.0; Y = 0.0; Z = -1.0};
        VUp = {X = 0.0; Y = 1.0; Z = 0.0}
    }


let colorToInt (color: Vec3) =
    (color.X * 255.99 |> int, color.Y * 255.99 |> int, color.Z * 255.99 |> int)
    
// generate list of Ray lists containing rays offset by random values for use in antialiasing
let genAARays col row width height passes =
    let rand = System.Random()
    let rec genPasses = function
        | 0.0 -> [getCameraRay ((col + rand.NextDouble())/width) ((row + rand.NextDouble())/height) camera]
        | p -> (getCameraRay ((col + rand.NextDouble())/width) ((row + rand.NextDouble())/height) camera) :: genPasses (p - 1.0)
    genPasses (passes - 1.0)

let calcColorRow width height row passesAA= 
    let rec calcRays col =
        match col with
        | x when x = width -> []
        | n -> [genAARays n row width height passesAA] @ calcRays (n + 1.0)
    calcRays 0.0




let calcWindow width height passesAA=
    let rec calcColors row =
        match row with
        | x when x = height -> []
        | n -> calcColors (n + 1.0) @ (calcColorRow width height n passesAA)
    calcColors 0.0

let printColor color =
    let (r, g, b) = color
    sprintf "%i %i %i\n" r g b

        

//color function in book
let rec rayToColor (r:Ray) world depth =
    let h = hit r 0.001 System.Double.MaxValue world
    match h with 
    | None -> 
        let t = 0.5 * ((r.Direction.getUnitVector.Y) + 1.0)
        (1.0 - t) * {X = 1.0; Y = 1.0; Z = 1.0} + t* {X = 0.5; Y =  0.7; Z = 1.0}
    | Some record -> 
            let scattered = scatter r record 
            match scattered with
            |Some (scatterRay, attenuation) when depth < 50 -> 
                attenuation * (rayToColor scatterRay world (depth + 1))
            |_->{X=0.0; Y=0.0; Z=0.0}
           



            
let world = [HitableSphere {Center = {X = 0.0; Y =  0.0; Z =  -1.0}; Radius = 0.5; Mat = Material.Lambertian{X = 0.1; Y = 0.2; Z = 0.5}};
             HitableSphere {Center = {X = 0.0; Y =  -100.5; Z =  -1.0}; Radius = 100.0; Mat = Material.Lambertian{X= 0.8; Y = 0.8; Z= 0.0}};
             HitableSphere {Center = {X = 1.0; Y = 0.0; Z = -1.0}; Radius = 0.5; Mat = Material.Metal({X = 0.8; Y = 0.6; Z = 0.2}, 1.0)};
             HitableSphere {Center = {X = -1.0; Y = 0.0; Z = -1.0}; Radius = 0.5; Mat = Material.Dialectric 1.5};
             HitableSphere {Center = {X = -1.0; Y = 0.0; Z = -1.0}; Radius = -0.45; Mat = Material.Dialectric 1.5}]
