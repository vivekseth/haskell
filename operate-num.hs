
-- Constant Strings

_ASK_STRING = "Operate on how many numbers?"
_INPUT_STRING = "Please input a number: "
_OUTPUT_STRING = "Answer: "

-- IO Funtions

getNumber :: String -> IO Int
getNumber str = putStrLn str >>= (\_ -> readLn :: IO Int)

runningOperation :: (Int -> Int -> Int) -> Int -> IO Int
runningOperation op a = getNumber _INPUT_STRING >>= (\x -> return (op a x))

aggregateOperation :: Int -> IO Int
aggregateOperation n = composeMonadicFunc n (runningOperation (*)) 1

printAnswer :: Int -> IO ()
printAnswer n = putStrLn _OUTPUT_STRING >> print n

-- Utility

composeMonadicFunc :: Int -> (Int -> IO Int) -> (Int -> IO Int)
composeMonadicFunc n f 
    | n <= 1 = f
    | otherwise = \x -> f =<< composeMonadicFunc (n - 1) f x

-- Main

main = 
    getNumber _ASK_STRING
    >>= aggregateOperation 
    >>= printAnswer 
    >> main