import Numeric.Noise.Perlin
import System.Random
main =  do
    	seed        <- randomIO :: IO Int
        let octaves     = 5
        let scale       = 0.05
        let persistance = 0.5
        let perlinNoise = perlin seed octaves scale persistance
        let x           = noiseValue perlinNoise (1, 2, 3)
        putStrLn ("Noise value at (1, 2, 3): " ++ show perlinNoise)