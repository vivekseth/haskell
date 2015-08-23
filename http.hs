import Network.HTTP
import System.Environment
import Text.HTML.TagSoup

getArgIndex :: Int -> IO String
getArgIndex i = do
    args <- getArgs
    return $ args !! i

httpGetRequestByURL :: String -> IO String
httpGetRequestByURL url = (simpleHTTP . getRequest) url >>= getResponseBody

mapi :: (Int -> a -> b) -> Int -> [a] -> [b]  
mapi _ _ [] = []
mapi f i (x:xs) = f i x : mapi f (i + 1) xs

-- appendLineNumber :: Int -> String -> String
appendLineNumber i str = (show i) ++ ". " ++ str

(!=) a b = not (a == b)

hasPrefix (a:a_tail) (b:b_tail) 
    | (a:a_tail) == [] = True
    | (a:a_tail) != [] && (b:b_tail) == [] = False
    | a_tail == [] = (a == b)
    | a_tail != [] && b_tail == [] = False
    | otherwise = (a == b) && (hasPrefix a_tail b_tail)

isTagType a = (flip (~==) $ "<" ++ a ++ ">")

linksFromTags tags = (filter $ hasPrefix "http" ) . (map $ fromAttrib "href") . (filter $ isTagType "a") $ tags

putNumberedLines arr = (putStrLn . unlines . (mapi appendLineNumber 1)) arr

main_do = do
    url <- getArgIndex 0
    tags <- fmap parseTags $ httpGetRequestByURL url
    putNumberedLines $ linksFromTags tags

main = main_do

{-

You can think of using monads like progressively applying "filters" to an input to get a desired output. 

haskell: 
main = A >>= B >>= C 

A's job is to output something that B can use. B's job is to use that data to output something that C can use. 

Lets say we want to:
1. get arguments to program
2. select first argument
3. print "hello" + first argument

Steps 1, 2, 3 are exactly what A, B, C will do. 

-- hello.hs
main = getArgs >>= (\args -> return (args !! 0)) >>= (\firstString -> print "hello " ++ firstString) 

> ghc hello.hs
> ./hello Vivek
hello vivek
> 

Syntax Explanation:
A >>= B is our way of feeding the output of A into B
\x -> ... is a function that has 1 inputs, x, and ouputs whatever the evaluated value of ... is. 
Array !! i gets the i'th index of Array
a ++ b concatenates strings a and b

Lets see how this code compares to an imperative version that produces the same output.

String[] args = getArgs(); // A
String firstString = args[0]; // B
print("Hello" + firstString); // C

or even 

print("hello" + getArgs()[0]); // C

In the imperative version we take the output of each function and feed it into the next one directly. 
We don't need to use the strange >>= symbol. 

This approach cannot work in Haskell. We cannot just write:

print ("hello" ++ (getArgs !! 0))

We need the >>= to join our steps together.

On a fundamental level, we cannot compile the last example because there will be a type mismatch. 

getArgs :: IO [String]
print :: String -> IO ()

!! can operate on a type of [String], but not on a type of IO [String]
print takes String as an input but not IO String

What is IO? 

IO is a monad and monads are containers. IO [String] is an array of strings 
contained within an IO monad. Similarly, IO () is an empty tuple contained 
within an IO monad. More generally M A refers to a type A contained within 
a M monad. One example of M is IO, but there are others.

In short, we use monads to accomplish any and all kinds of meaningful work that a program can do. 
Because functions in Haskell are required to be absolutely stateless, they cannot do things like file IO, network requests, accessing databses, or computer graphics. 
Monads provide a controlled escape from haskell's statelessness to accomplish some kind of meaningful work. 

Haskell provides 1 and only 1 way to convert from IO A to A : >>= 
Haskell provides 1 and only 1 way to convert from A to IO A : return

Lets examine >>= further. 

in the code

f >>= g

>>= requires that 
1. f output a type of IO A
2. g have input of type A
3. and that g output a type of IO B

f :: IO A
g :: A -> IO B

if these conditions are met, >>= will do the work necessary to convert from IO A to A and feed it into g. 

We can use the `return` function to promote A to IO A to meet condition 1. 


-}