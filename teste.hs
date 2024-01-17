import Cp
import Nat
import List
import BTree
import LTree
import Probability
import FTree
import ListUtils



pdelay :: Stop -> Stop -> Dist Delay
pdelay a b = somatorio $  uncurry zip $  split id tail $  enumFromTo a b


-- S0 S4 
-- S0,S1,S2,S3,S4
-- [(S0,S1),(S1,S2),(S2,S3)]

somatorio = cataList $ either (const instantaneous) (uncurry (joinWith (+) . delay))