// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open System
open System.IO
open Misc
open Vector3

[<EntryPoint>]
let main argv = 
    let ns = 100.0
    let header = sprintf "P3%s%i %i%s255%s" Environment.NewLine (int nx) (int ny) Environment.NewLine Environment.NewLine
    let windowString = 
        calcWindow nx ny ns
        |> List.map (fun y -> List.fold (fun acc x -> acc + (rayToColor x world 0)) {X = 0.0; Y = 0.0; Z = 0.0} y)
        |> List.map (fun x-> x / ns)
        |> List.map (fun x -> { X= sqrt x.X; Y = sqrt x.Y; Z = sqrt x.Z})
        |> List.map colorToInt           
        |> List.map printColor

    File.WriteAllLines("image.ppm", header :: windowString)
    0 // return an integer exit code
