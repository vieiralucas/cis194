type Peg = String

type Move = (Peg, Peg)

fromTo :: Peg -> Peg -> Integer -> [Move] -> [Move]
fromTo _ _ 0 _ = []
fromTo from to n moves = moves ++ [(from, to)] ++ (fromTo from to (n - 1) moves)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n p1 p2 p3 = p1ToP3 (n - 1) [] ++ [(p1, p2)] ++ p3ToP2 (n - 1) []
  where
    p1ToP3 = fromTo p1 p3
    p3ToP2 = fromTo p3 p2

works = hanoi 2 "a" "b" "c" == [("a", "c"), ("a", "b"), ("c", "b")]
