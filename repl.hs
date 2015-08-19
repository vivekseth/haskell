import Data.List.Split

init_arr :: Int -> [Int]
init_arr n 
    | n <= 1 = [0]
    | otherwise = [0] ++ init_arr (n - 1)

reset_arr :: [Int] -> [Int]
reset_arr arr = map (\x -> 0) arr

slice_arr :: Int -> Int -> [Int] -> [Int]
slice_arr from to xs = take (to - from + 1) (drop from xs)

set_arr :: [Int] -> Int -> Int -> [Int]
set_arr list index elem 
    | index < 0 = [] 
    | index >= (length list) = [] 
    | otherwise = (slice_arr 0 (index - 1) list) ++ [elem] ++ (slice_arr (index + 1) (length list - 1) list)

repl_eval :: [Int] -> String -> [Int]
repl_eval lst cmd  
    | ((length cmd) >= 3) = ((\x -> (set_arr lst (x !! 0) (x !! 1))) (map (\x -> read x :: Int) (splitOn "," cmd)))
    | cmd == "r" = (init_arr 5)
    | otherwise = lst

repl_loop :: ([Int] -> String -> [Int]) -> [Int] ->  IO [Int]
repl_loop eval lst = do
    -- READ
    user_input <- getLine
    -- EVALUATE
    next_lst <- return (eval lst user_input)
    -- PRINT
    print next_lst
    -- LOOP
    repl_loop eval next_lst

main = return (init_arr 5) >>= repl_loop repl_eval