part1 stream = f stream 0 0 False where
    f ""         lvl pnts _    = pnts
    f ('!':_:cs) lvl pnts ignr = f cs lvl     pnts       ignr
    f ('>':cs)   lvl pnts True = f cs lvl     pnts       False
    f (_:cs)     lvl pnts True = f cs lvl     pnts       True
    f ('<':cs)   lvl pnts _    = f cs lvl     pnts       True
    f ('{':cs)   lvl pnts _    = f cs (lvl+1) pnts       False
    f ('}':cs)   lvl pnts _    = f cs (lvl-1) (pnts+lvl) False
    f (_:cs)     lvl pnts _    = f cs lvl     pnts       False

part2 stream = f stream False 0 where
    f ""         _    grbg = grbg
    f ('!':_:cs) ignr grbg = f cs ignr  grbg
    f ('>':cs)   True grbg = f cs False grbg
    f (_:cs)     True grbg = f cs True  grbg+1
    f ('<':cs)   _    grbg = f cs True  grbg
    f (_:cs)     _    grbg = f cs False grbg

main :: IO ()
main = do
    stream <- getLine

    print $ part1 stream
    print $ part2 stream
