module HitRecord
open Ray
open Vector3
open Material

type HitRecord =
    {
        T:float;
        P:Vec3;
        Normal:Vec3;
        Mat : Material
    }
    


        

    