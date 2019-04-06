#! /usr/bin/env nix-shell
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/18.09.tar.gz --pure -i runhaskell -p "python27.withPackages (ps: with ps; [numpy pandas])" -p "(pkgs.haskellPackages.extend (self: super: with haskell; rec { pyfi = haskell.lib.doJailbreak super.pyfi; })).ghcWithPackages (ps: with ps; [aeson pyfi])"
{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
module Pyfi where

import "pyfi" Python

pandasExample :: [[Int]] -> IO [Int]
pandasExample = defVV [str|
import pandas as pd
def export(matrix):
    df = pd.DataFrame(matrix)
    print "------------"
    print "From Python:"
    print df
    return list(df.sum())
|]

main :: IO ()
main = do
    xs <- pandasExample [ [1,2,3]
                        , [4,5,6]
                        , [7,8,9]
                        ]
    print "-------------"
    print "From Haskell:"
    print xs
