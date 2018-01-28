{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.Process (callProcess)
import Criterion.Main 

mkStack (lrsz, rlsz) =
  callProcess "C:\\Users\\steven\\OneDrive\\Projects\\DRAEquiv\\.stack-work\\install\\3573363d\\bin\\stacks-exe.exe" ["--lrsz=" ++ show lrsz, "--rlsz=" ++ show rlsz]

main :: IO ()
main = 
  defaultMain [
    bgroup "stack" [ bench (show i) (nfIO $ mkStack (i,i)) | i <- [200,400,600,800,1000] ]
  ]

