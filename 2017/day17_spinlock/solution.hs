data Spinlock = Spinlock [Int] [Int] deriving Show

singleton e = Spinlock [e] []
selected (Spinlock (a:_) _) = a
insert e (Spinlock feb aft) = Spinlock (e:feb) aft
step (Spinlock feb []) = let b:ef = reverse feb in Spinlock [b] ef
step (Spinlock feb (a:ft)) = Spinlock (a:feb) ft

stepN n = last . take (n+1) . iterate step
find e sl = if selected sl == e then sl else find e (step sl)
after = selected . step

spin count stepSize = foldr ($) (singleton 0) fs where
    f x = insert x . stepN stepSize
    fs = map (f$) [count, count-1.. 1]

part1 = after . spin 2017
part2 = after . find 0 . spin 50000000 -- TODO optimize

main = do
    input <- getContents
    let stepSize = read input

    print $ part1 stepSize
    print $ part2 stepSize
