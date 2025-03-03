-- This module defines the Bayes' rule
-- We declare that this module is named "Lib" and explicitly export `bayes`
-- so it can be used in other files.
module Lib (bayes) where

-- `bayes` computes P(A | B) using Bayes' theorem.
-- It takes three probabilities as input:
--   pA: P(A) - Prior probability of A
--   pBGivenA: P(B | A) - Likelihood of B given A
--   pB: P(B) - Total probability of B
-- It returns P(A | B).
-- The type signature shows that bayes takes three floating-point numbers via currying and returns a floating-point number.
--equivalent: bayes :: (Floating a) => a -> (a -> (a -> a))
bayes :: (Floating a) => a -> a -> a -> a
bayes pA pBGivenA pB = (pBGivenA * pA) / pB
