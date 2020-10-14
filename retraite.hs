{-# LANGUAGE ScopedTypeVariables #-}
valpoint::Double    = 1.2714
plafond ::Double    =3428
hpoints ::[Double]  = [727.06,741.69,762.49,818.95,776.96,791.07,766.69]
tauxcomp::[Double]  = [0.78,0.7925,0.805,0.8175,0.83,0.8425,0.855,0.8675,0.88,0.89,0.9,0.91,0.92,0.93,0.94,0.95,0.96,0.97,0.98,0.99,1]
t0      ::Double    = 22148.52
rgtaux  ::Int->Double;          rgtaux n = 0.5 - (fromIntegral(n)*(0.00625))
cptaux  ::Int->Double;          cptaux n = tauxcomp!!(20-n)
rg      ::Int->Double;              rg n = plafond * (rgtaux n) * (167-fromIntegral(n))/167
comp    ::Double-> Int -> Double;  comp cap n = (cap*valpoint)*(cptaux n)
total   ::Double-> Int -> Double; total cap manque = (rg manque) + (comp cap manque)
apoints ::Double;apoints = (sum hpoints) / fromIntegral(length hpoints)
tpoints ::Double= apoints / 4
cap     ::Double->Double;cap n = (t0 + (n * tpoints))
valcomp ::(Double->Double)->Int->Double;valcomp cap n = comp (cap (fromIntegral n)) n