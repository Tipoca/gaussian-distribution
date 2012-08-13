{- The function integral calculates the value of the cumulative
   distribution function of the Gaussian distribution. Due to
   rounding errors, we abort the computation for extreme values.
   Mathematical details will be published an arxiv soon.

   Frank Recker, 2012
-}

module GaussianDistribution(integral,error_term) where
-- x: Argument for the Gaussian integral
-- n: Number of steps for the approximation
-- return value: approximation of Phi(x)
integral :: Double -> Int -> Double
integral x n
  | n>=1 && x>(-6.2) && x<6.2 = 0.5 + x * ((liste!!(n-1)) + 1) / sqrt(2*pi)
  where
    liste = 0:rest -- the last element is d_1
    rest = zipWith d_next_term liste [n-1,n-2..]
    d_next_term d j =
      -x^2 * (d + 1 / (2 * fromIntegral j + 1)) / (2 * fromIntegral j)

-- x: Argument for the Gaussian integral
-- n: Number of steps for the approximation
-- An upper bound for the absolute difference of the result and Phi(x)
error_term :: Double -> Int -> Maybe Double
error_term x n
  | 2 * fromIntegral n >= x^2 = Just $ x * product [x^2 / (2*fromIntegral j) | j<-[1..n]] / (2*fromIntegral n+1) / sqrt(2*pi)
  | otherwise = Nothing
