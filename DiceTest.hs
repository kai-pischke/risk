import Battle 
import System.Random

doMany :: StdGen -> [(Int, Int)]
doMany g = (x,y) : doMany g'
    where (x,y,g') = doBattle ThreeAtt OneDef g

main :: IO()
main = do 
    g <- getStdGen 
    let xs = take 10000000 $ doMany g
    print $ length $ filter ( (==1) . snd ) xs 
    print $ length $ filter ( (==0) . snd ) xs 
    