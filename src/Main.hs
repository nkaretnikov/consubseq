module Main where

import System.IO

import ConSubSeq

-- XXX: Use conduit or alternatives if lazy IO is a problem.
main :: IO ()
main = run stdin stdout stderr
