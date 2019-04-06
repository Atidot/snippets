#! /usr/bin/env nix-shell
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/18.09.tar.gz --pure -i ghcjs -p "(pkgs.haskell.packages.ghcjs.extend (self: super: with haskell; {})).ghcWithPackages (ps: with ps; [parallel])"
{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE PackageImports #-}
module Main where

import "parallel" Control.Parallel.Strategies

-- fails on runtime
main :: IO ()
main = do
    let x = map (+1) [(1 :: Int) .. 1000000] `using` rpar
    print x

