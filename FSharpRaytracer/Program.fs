// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open System
open System.IO
open System.Diagnostics
open Misc
open Vector3
open System.Speech.Synthesis
open FSharp.Collections.ParallelSeq

let colorSum acc x = 
    acc + (rayToColor x world 0)

[<EntryPoint>]
let main argv = 
    let ns = 100.0
    let header = sprintf "P3%s%i %i%s255%s" Environment.NewLine (int nx) (int ny) Environment.NewLine Environment.NewLine
    let windowString = 
        calcWindow nx ny ns
        |> Seq.map (fun y -> Seq.fold colorSum {X = 0.0; Y = 0.0; Z = 0.0} y)
        |> Seq.map (fun x-> x / ns)
        |> Seq.map (fun x -> { X= sqrt x.X; Y = sqrt x.Y; Z = sqrt x.Z})
        |> Seq.map colorToInt           
        |> Seq.map printColor
        |> Seq.append [|header|]
    File.WriteAllLines("image.ppm", windowString)
    0 // return an integer exit code
