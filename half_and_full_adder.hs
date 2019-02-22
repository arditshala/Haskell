binar :: Int -> Int -> [Int]
binar 0 _ = []
binar m n = binar (m-1) (n `div` 2) ++ [mod n 2]

bin :: Int -> Int -> [Bool]
bin x y = map (\x-> if x == 1 then True else False) (binar x y)

dech :: [Int] -> Int -> Int
dech [] _ = 0
dech (x:xs) idx = x * 2^idx + dech xs (idx + 1)


dec :: [Bool] -> Int
dec xs = dech (reverse (map (\x-> if x then 1 else 0) xs)) 0

fa_c :: Bool -> Bool -> Bool -> Bool
fa_c x y z = (x && y) || (z && ((x && not y) || (not x && y)))

fa_s :: Bool -> Bool -> Bool -> Bool
fa_s a b c = (a && b && c) || (a && not b && not c) || (not a && b && not c) || (not a && not b && c)

rc_addhelp :: [Bool] -> [Bool] -> Bool -> [Bool]
rc_addhelp [] l b = l
rc_addhelp l [] b = l
rc_addhelp (l:ls) (r:rs) b = rc_addhelp ls rs (fa_c l r b) ++ [fa_s l r b]

rc_add :: [Bool] -> [Bool] -> [Bool]
rc_add l r = rc_addhelp (reverse l) (reverse r) False


ha_c :: Bool -> Bool -> Bool
ha_c x y = x && y

ha_s :: Bool -> Bool -> Bool
ha_s x y = (x && not y) || (not x && y)


cla_addhelp :: [Bool] -> [Bool] -> [Bool]
cla_addhelp [] u = u
cla_addhelp (u:us) (o:os) = rc_addhelp us os (ha_c u o) ++ [ha_s u o]

cla_add :: [Bool] -> [Bool] -> [Bool]
cla_add j k = cla_addhelp (reverse j) (reverse k)
