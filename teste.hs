
--pdelay :: Stop -> Stop -> Dist Delay
--pdelay a b = somatorio $  uncurry zip $  split id tail $  enumFromTo a b


-- S0 S4 
-- S0,S1,S2,S3,S4
-- [(S0,S1),(S1,S2),(S2,S3)]

--somatorio = cataList $ either (const instantaneous) (uncurry (joinWith (+) . delay))


f x 0 = (x,x,1)
f x i = (l+ (a/b),a,b ) where (l,a,b) =  (l,(x ** 2 * x_) , ( (2*i +1 ) * (2*i) * y_ ) ) where (l,x_,y_) = f x (i-1) 


f x i = (l+ (a/b),a,b ) where (l,a,b) =  (l,(x ** 2 * x_) , ( 2* (i**2 + i ) * y_ ) ) where (l,x_,y_) = f x (i-1) 
