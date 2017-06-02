module Ray
open Vector3

type Ray =
    {
        Origin:Vec3;
        Direction:Vec3
    }

let pointAtParameter r (t:float) =
        r.Origin + t*r.Direction
    