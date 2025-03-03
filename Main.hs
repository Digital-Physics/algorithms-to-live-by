-- The Main module, where execution starts.
module Main where

-- Import the `bayes` function from our custom `Lib` module.
import Lib (bayes)

-- The `main` function is the entry point of a Haskell program.
-- This line declares that main is an IO action that returns () (unit type, meaning no useful return value).
-- Haskell is a purely functional language, meaning that functions should not have side effects. However, for things like printing, reading files, or getting user input, we use the IO monad.
-- The do notation allows sequencing multiple IO actions in a more readable way.
main :: IO ()
main = do
    putStrLn "Enter P(A), your prior probability:"
    pA <- readLn  -- First IO action: Read a number from user input
    putStrLn "Enter P(B | A):"
    pBGivenA <- readLn  -- Second IO action: Read another number
    putStrLn "Enter P(B):"
    pB <- readLn  -- Third IO action: Read another number

    let result = bayes pA pBGivenA pB -- This is a pure function, not an IO action
    -- ++ is the string concatenation operator in Haskell.
    -- show result converts result (a number) into a string.
    putStrLn $ "P(A | B) = " ++ show result -- Fourth IO action: Print result