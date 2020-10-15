import Prelude
rgplafond   ::Double  ;rgplafond   = 3428
covalpoint  ::Double  ;covalpoint  = 1.2714
jpcohpoints ::[Double];jpcohpoints = [727.06,741.69,762.49,818.95,776.96,791.07,766.69]
jocohpoints ::[Double];jocohpoints = [2882.49,2939.69,2564.36,2394.77+175.05,2394.77,2186.97,1952.32,1575.01+515.85]
cohistotaux ::[Double];cohistotaux = [0.78,0.7925,0.805,0.8175,0.83,0.8425,0.855,0.8675,0.88,0.89,0.9,0.91,0.92,0.93,0.94,0.95,0.96,0.97,0.98,0.99,1]
jpcot0      ::Double;  jpcot0      = 22148.52
jocot0      ::Double;  jocot0      = 61402.48
coannpoints ::Double;  coannpoints = (sum jpcohpoints) / fromIntegral(length jpcohpoints)
cotrimpoints::Double;  cotrimpoints        = coannpoints / 4
cotaux      ::Int->Double;                   cotaux n            = cohistotaux!!(20-n)
comp        ::Double-> Int -> Double;        comp cap n          = (cap*covalpoint)*(cotaux n)
cap         ::Double->Double;                cap a               = (jpcot0 + (4*(a-60) * cotrimpoints))
valcomp     ::(Double->Double)->Int->Double; valcomp cap n       = comp (cap (fromIntegral n)) n
rgtaux      ::Int->Double;                   rgtaux n            = 0.5 - (fromIntegral(n)*(0.00625))
rg          ::Int->Double;                   rg n                = 12*rgplafond * (rgtaux n) * (167-fromIntegral(n))/167
retraite    ::Double-> Int -> Double;        retraite cap manque = (rg manque) + (comp cap manque)
