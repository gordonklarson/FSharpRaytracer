module Material
open Vector3


type Material = 
    |Lambertian of Vec3
    |Metal of Vec3*float // attenuation * fuzziness
    |Dialectric of float

