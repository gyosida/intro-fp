-- create `replicate` function to produce a list of identical elements
replicate x y = [y | _ <- [1..x]]

-- create `pyths` function to decide if a triple is pythagorean
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n],
   x ^ 2 + y ^ 2 == z ^ 2]

-- create `perfects`function, that return a list of perfect numbers, excluding.
-- the passed in number. A number will be perfect if it is equals the sum of its factors
factors n = [x | x <- [1..n], n `mod` x == 0]
perfects n = [x | x <- [1..n], isPerfect x]
    where isPerfect num = sum (init (factors num)) == num

-- re expressed list comprehension
-- concat [[(x, y) | y <- [4, 5, 6]] | x <- [1, 2, 3]]

-- create `positions`function that returns a list of positions where x numbers
-- was found at xs list
find k xs = [v | (k', v) <- xs, k'== k]
positions x xs = find x (zip xs [0..n])
   where n = length xs - 1

-- create `scalarproduct` function that returns the scalar product of two
-- list of n numbers
scalarproduct xs ys = sum [x * y | (x, y) <- xs `zip`ys]

-- create `riffle` function that interleaves element order from two lists with
-- the same length
riffle xs ys = concat [[x, y] | (x, y) <- zip xs ys]
