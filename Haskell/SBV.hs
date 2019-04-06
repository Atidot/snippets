#! /usr/bin/env nix-shell
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/18.09.tar.gz --pure -i runhaskell -p z3 -p "pkgs.haskellPackages.ghcWithPackages (pkgs: with pkgs; [sbv])"
{-# OPTIONS_GHC -Wall -Werror #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SBV where

import "sbv" Data.SBV

formula :: Symbolic SBool
formula = do
    x :: SInteger <- exists "x"
    y :: SInteger <- exists "y"
    constrain $ x^2 + y^2 .== 25
    return $ 3 * x + 4 * y .== 0

main :: IO ()
main = do
   result <- sat formula
   print result
