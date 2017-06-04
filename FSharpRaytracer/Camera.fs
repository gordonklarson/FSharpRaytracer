module Camera
open Ray
open Vector3
open System


type Camera =
    {
        VFOV : float;
        Aspect : float
        LookFrom : Vec3;
        LookAt : Vec3;
        VUp : Vec3
    }

    member this.theta = 
            this.VFOV * Math.PI/180.0
    
    member this.halfHeight =
            Math.Tan (this.theta/2.0)

    member this.halfWidth = 
            this.Aspect * this.halfHeight

    member this.origin = this.LookFrom

    member this.w = (this.LookFrom - this.LookAt).getUnitVector

    member this.u = (this.VUp.cross this.w).getUnitVector

    member this.v = this.w.cross this.u
    

    member this.lowerLeftCorner = this.origin - this.halfWidth * this.u - this.halfHeight * this.v - this.w


    member this.horizontal = 2.0 * this.halfWidth * this.u

    member this.vertical = 2.0 * this.halfHeight * this.v

let getCameraRay (s:float) (t:float) (camera: Camera) =
    {Origin = camera.origin; Direction = camera.lowerLeftCorner + (s * camera.horizontal) + (t * camera.vertical) - camera.origin}