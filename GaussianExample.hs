import Control.Monad
import GaussianDistribution

main = do
  forM_ results $ \(x,n,v,e) ->
    putStrLn $ "x :" ++ show x ++ ", n: " ++ show n ++ ", value: " ++ show v ++ ", error bound: " ++ show e

results =
  [(x,n,integral x n,error_term x n) | x<-[1.96,5],n<-[1,2,10,30,50,100,200]]
