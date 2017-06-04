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
        VUp : Vec3;
        Aperture : float;
        FocusDist : float
    }

    member this.lensRadius = this.Aperture / 2.0

    member this.theta = 
            this.VFOV * (Math.PI/180.0)
    
    member this.halfHeight =
            Math.Tan (this.theta/2.0)

    member this.halfWidth = 
            this.Aspect * this.halfHeight

    member this.origin = this.LookFrom

    member this.w = (this.LookFrom - this.LookAt).getUnitVector

    member this.u = (this.VUp.cross this.w).getUnitVector

    member this.v = this.w.cross this.u
    

    member this.lowerLeftCorner = this.origin - (this.halfWidth * this.u* this.FocusDist) - (this.halfHeight * this.v * this.FocusDist) - (this.w * this.FocusDist)


    member this.horizontal = 2.0 * this.halfWidth * this.u * this.FocusDist

    member this.vertical = 2.0 * this.halfHeight * this.v * this.FocusDist

let randDouble = System.Random().NextDouble

let rec randomInUnitDisk() =
        let p = 2.0 * {X = randDouble(); Y = randDouble(); Z = 0.0} - {X = 1.0; Y = 1.0; Z = 0.0}
        match p with 
        | point when (point.dot point) >= 1.0 -> randomInUnitDisk()
        | _ -> p

let getCameraRay (s:float) (t:float) (camera: Camera) =
    let rd = camera.lensRadius * randomInUnitDisk()
    let offset = (camera.u * rd.X) + (camera.v * rd.Y)
    {Origin = camera.origin + offset;
    Direction = camera.lowerLeftCorner + (s * camera.horizontal) + (t * camera.vertical) - camera.origin - offset}