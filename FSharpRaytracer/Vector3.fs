module Vector3

    type Vec3 =
        {
            X:float;
            Y:float;
            Z:float
        }

        override this.ToString () = 
            sprintf "%f %f %f" this.X this.Y this.Z
    
        member this.length () =
            sqrt(this.X*this.X + this.Y*this.Y + this.Z*this.Z)

        static member (+) (a:Vec3, b:Vec3) =
            {X = a.X + b.X; Y = a.Y + b.Y; Z = a.Z + b.Z}
        
        static member (~-) (a:Vec3) = 
            {X = -a.X; Y = -a.Y; Z = -a.Z}

        static member (-) (a:Vec3, b:Vec3) =
            {X = a.X - b.X; Y = a.Y - b.Y; Z = a.Z - b.Z}

        static member (*) (a:Vec3, b:Vec3) =
            {X = a.X * b.X; Y =  a.Y * b.Y; Z =  a.Z * b.Z}

        static member(/) (a:Vec3, b:Vec3) : Vec3 =
            {X = a.X / b.X; Y = a.Y / b.Y; Z = a.Z / b.Z}

        static member(*) (a:Vec3, b:float) =
            {X = a.X * b; Y = a.Y * b; Z = a.Z * b}

        static member (*) (a:float, b:Vec3) :Vec3 =
            {X = a * b.X; Y = a * b.Y; Z = a * b.Z}
        
        static member (/) (a:Vec3, b:float) =
            {X = a.X / b; Y =  a.Y / b; Z =  a.Z / b}

        member this.dot (b:Vec3) =
                this.X * b.X + this.Y * b.Y + this.Z * b.Z

        member this.cross (b:Vec3) =
                {X = this.Y*b.Z - this.Z*b.Y; Y = -(this.X * b.Z - this.Z * b.X); Z = this.X*b.Y - this.Y*b.X}

        member this.getUnitVector =
                let k = 1.0 / this.length()
                {X = this.X*k; Y =  this.Y*k; Z =  this.Z*k}
        
        member this.squaredLength =
                this.X*this.X + this.Y*this.Y + this.Z*this.Z

    let reflect (v: Vec3) (n: Vec3) =
        v - 2.0 * (v.dot n) * n

    let refract (v:Vec3) (n:Vec3) (niOverNt:float) =
        let uv = v.getUnitVector
        let un = n.getUnitVector
        let dt = uv.dot un
        let discriminant = 1.0 - niOverNt * niOverNt*(1.0 - dt*dt)
        match discriminant with
        | d when d > 0.0 -> Some(niOverNt*(uv - un * dt) - un * sqrt(d))
        | _ -> None

    let schlick (cosine: float) (refIdx:float) =
        let r0 = ((1.0 - refIdx) / (1.0 + refIdx) ** 2.0)
        r0 + (1.0-r0) * ((1.0 - cosine) ** 5.0)