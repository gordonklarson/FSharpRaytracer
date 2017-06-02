module Camera
open Ray
open Vector3

type Camera =
    {
        LowerLeftCorner : Vec3;
        Horizontal : Vec3;
        Vertical : Vec3;
        Origin : Vec3
    }
let getCameraRay (u:float) (v:float) camera=
    {Origin = camera.Origin; Direction = camera.LowerLeftCorner + (u * camera.Horizontal) + (v * camera.Vertical) - camera.Origin}