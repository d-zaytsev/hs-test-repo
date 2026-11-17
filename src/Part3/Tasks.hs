module Part3.Tasks where

import Util (notImplementedYet)

-- Функция finc принимает на вход функцию f и число n и возвращает список чисел [f(n), f(n + 1), ...]
finc :: (Int -> a) -> Int -> [a]
finc f n = map f [n ..]

-- Функция ff принимает на вход функцию f и элемент x и возвращает список [x, f(x), f(f(x)), f(f(f(x))) ...]
ff :: (a -> a) -> a -> [a]
ff f x = x : ff f (f x)

-- Дан список чисел. Вернуть самую часто встречающуюся *цифру* в этих числах (если таковых несколько -- вернуть любую)
mostFreq :: [Int] -> Int
mostFreq [] = 0
mostFreq lst@(x : _) = find_max count_lst 0 0
  where
    find_max :: [(Int, Int)] -> Int -> Int -> Int
    find_max [] best _ = best
    find_max ((digit, freq) : rest) best bestFreq
      | freq > bestFreq = find_max rest digit freq
      | otherwise = find_max rest best bestFreq

    count_lst :: [(Int, Int)]
    count_lst = map (\x -> (x, count x digits_lst)) digits_lst

    count :: Int -> [Int] -> Int
    count x = length . filter ((==) x)

    digits_lst :: [Int]
    digits_lst = break_lst lst

    break_lst :: [Int] -> [Int]
    break_lst = concatMap $ break_num []

    break_num :: [Int] -> Int -> [Int]
    break_num [] 0 = [0]
    break_num acc 0 = acc
    break_num acc num
      | num > 0 = break_num (mod num 10 : acc) (div num 10)
      | otherwise = break_num acc (-num)

-- Дан список lst. Вернуть список элементов из lst без повторений, порядок может быть произвольным.
uniq :: (Eq a) => [a] -> [a]
uniq = foldl (\acc el -> if not (contains acc el) then el : acc else acc) []
  where
    contains [] x = False
    contains (hd : tl) x
      | hd == x = True
      | otherwise = contains tl x

-- Функция grokBy принимает на вход список Lst и функцию F и каждому возможному
-- значению результата применения F к элементам Lst ставит в соответствие список элементов Lst,
-- приводящих к этому результату. Результат следует представить в виде списка пар.
grokBy :: (Eq k) => (a -> k) -> [a] -> [(k, [a])]
grokBy f lst = map (\k -> foldl (\acc el -> if f el == k then (k, el : snd acc) else acc) (k, []) lst) img
  where
    img = uniq $ map f lst
