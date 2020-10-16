import Prelude
rgplafond   ::Double  ;rgplafond    = 3428
covalpoint  ::Double  ;covalpoint   = 1.2714
cohistotaux ::[Double];cohistotaux  = [0.78,0.7925,0.805,0.8175,0.83,0.8425,0.855,0.8675,0.88,0.89,0.9,0.91,0.92,0.93,0.94,0.95,0.96,0.97,0.98,0.99,1]
rgtrim::Int;           rgtrim       = 167
jptrimusa::Int;        jptrimusa    = 12
jptrim0::Int;          jptrim0      = 137
jptrim0usa::Int;       jptrim0usa   = jptrim0+jptrimusa
jotrim0::Int;          jotrim0      = 162
jpcohpoints ::[Double];jpcohpoints  = [727.06,741.69,762.49,818.95,776.96,791.07,766.69]
avg ::[Double]->Double;  avg  l = (sum l)/(fromIntegral (length l))
jpcotrimpts ::Double;  jpcotrimpts  = (avg jpcohpoints)
jocohpoints ::[Double];jocohpoints  = [2882.49,2939.69,2564.36,2394.77+175.05,2394.77,2186.97,1952.32,1575.01+515.85]
jocotrimpts::Double; jocotrimpts = (avg jocohpoints)
jpcot0      ::Double;  jpcot0       = 22148.52
jocot0      ::Double;  jocot0       = 61402.48
-- cap         ::Double->Double->Double;        cap trim0 cot0 t    = cot0 + ((t-trim0) * cotrimpoints)
-- valcomp     ::(Double->Double)->Int->Double; valcomp cap n       = comp (cap (fromIntegral n)) n
-- rgtaux      ::Int->Double;                   rgtaux n            = 0.5 - (fromIntegral(n)*(0.00625))
-- rg          ::Int->Double;                   rg n                = 12*rgplafond * (rgtaux n) * (167-fromIntegral(n))/167
-- retraite    ::Double-> Int -> Double;        retraite cap manque = (rg manque) + (comp cap manque)
-- test1 = (cap jptrim0usa jpcot0 jptrim0usa) == (22148.52)
main = print tests
tests = and [
        test1,
        test2,
        test3,
        test4,
        test5,
        test6,
        True
        ]
cotaux      ::Int->Double;                   cotaux n
                                                    | n >  20            = 0.78
                                                    | n <  0             = 1
                                                    | otherwise          = cohistotaux!!(20-n)
test1 = (cotaux (-1)) == 1
test2 = (cotaux 0)  == 1
test3 = (cotaux 20) == 0.78
test4 = (cotaux 21) == 0.78
jptrim63 = jptrim0usa + 13
test5 = (cotaux (rgtrim - jptrim63)) ==  0.95
jotrim61 = 167
test6 = (cotaux (rgtrim - jotrim61)) == 1
capjp63 = jpcot0 + 13 * jpcotrimpts
cap::Double->Double->Double->Double; cap pts t0 n = t0 + n * pts
test7 = (cap jpcotrimpts jpcot0 13) == capjp63