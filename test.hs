
-- Constant Strings

_INPUT_STRING = "Please input a number: "
_OUTPUT_STRING = "Answer: "

-- IO Funtions

append :: (Eq a) => [a] -> a -> [a]
append list a = list ++ [a]

getNumber :: String -> IO Int
getNumber str = putStrLn str >>= (\_ -> readLn :: IO Int)

printAnswer :: Int -> IO ()
printAnswer n = putStrLn _OUTPUT_STRING >> print n

operateOnArray :: [Int] -> IO [Int]
operateOnArray arr = getNumber _INPUT_STRING >>= (\x -> return (append arr x))

storeNumbers :: Int -> IO [Int]
storeNumbers n = composeMonadicFunc n operateOnArray []

-- Utility

composeMonadicFunc :: Int -> ([Int] -> IO [Int]) -> ([Int] -> IO [Int])
composeMonadicFunc n f 
    | n <= 1 = f
    | otherwise = \x -> f =<< composeMonadicFunc (n - 1) f x

-- Main



main = getNumber _INPUT_STRING >>= storeNumbers >>= print >> main