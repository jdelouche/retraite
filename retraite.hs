{-# LANGUAGE ScopedTypeVariables #-}
rgplafond ::Double   = 3428
covalpoint::Double   = 1.2714
cohpoints ::[Double] = [727.06,741.69,762.49,818.95,776.96,791.07,766.69]
cotaux    ::[Double] = [0.78,0.7925,0.805,0.8175,0.83,0.8425,0.855,0.8675,0.88,0.89,0.9,0.91,0.92,0.93,0.94,0.95,0.96,0.97,0.98,0.99,1]
cot0      ::Double   = 22148.52
tpoints   ::Double;                        tpoints             = apoints / 4
rgtaux    ::Int->Double;                   rgtaux n            = 0.5 - (fromIntegral(n)*(0.00625))
cptaux    ::Int->Double;                   cptaux n            = cotaux!!(20-n)
rg        ::Int->Double;                   rg n                = rgplafond * (rgtaux n) * (167-fromIntegral(n))/167
comp      ::Double-> Int -> Double;        comp cap n          = (cap*covalpoint)*(cptaux n)
retraite  ::Double-> Int -> Double;        retraite cap manque = (12*(rg manque)) + (comp cap manque)
apoints   ::Double;                        apoints             = (sum cohpoints) / fromIntegral(length cohpoints)
cap       ::Double->Double;                cap a               = (cot0 + (4*(a-60) * tpoints))
valcomp   ::(Double->Double)->Int->Double; valcomp cap n       = comp (cap (fromIntegral n)) n