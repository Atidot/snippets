#! /usr/bin/env nix-shell
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/20.03-beta.tar.gz --pure -i runhaskell -p z3 -p "pkgs.haskellPackages.ghcWithPackages (pkgs: with pkgs; [sbv])"
{-# OPTIONS_GHC -Wall -Werror           #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SBV where

import           "sbv" Data.SBV
import qualified "sbv" Data.SBV.Internals as SI

newtype LengthInIntegerMetres
    = LengthInIntegerMetres Integer
    deriving (Real, Integral, Num, Enum, Eq, Ord)

type SLenIntMetres = SBV LengthInIntegerMetres

instance HasKind LengthInIntegerMetres where
    kindOf _ = KUnbounded

instance SymVal LengthInIntegerMetres where
    mkSymVal = SI.genMkSymVar KUnbounded
    literal  = SI.genLiteral  KUnbounded
    fromCV   = SI.genFromCV

newtype LengthInFloatingMetres
    = LengthInFloatingMetres Float
    deriving (Real, Num, Enum, Eq, Ord, Fractional, Floating, RealFrac, RealFloat)

type SLenFMetres = SBV LengthInFloatingMetres

instance HasKind LengthInFloatingMetres where
    kindOf _ = KFloat

instance SymVal LengthInFloatingMetres where
    mkSymVal = SI.genMkSymVar KFloat
    literal (LengthInFloatingMetres l) = SI.SBV . SI.SVal KFloat . Left . SI.CV KFloat . SI.CFloat $ l
    fromCV (SI.CV _ (SI.CFloat a)) = LengthInFloatingMetres a
    fromCV c                 = error $ "SymVal.LengthInFloatingMetres: Unexpected non-float value: " ++ show c

instance IEEEFloating LengthInFloatingMetres

newtype LengthInDoubleMetres
    = LengthInDoubleMetres Double
    deriving (Real, Num, Enum, Eq, Ord, Fractional, Floating, RealFrac, RealFloat)

type SLenDMetres = SBV LengthInDoubleMetres

instance HasKind LengthInDoubleMetres where
    kindOf _ = KDouble

instance SymVal LengthInDoubleMetres where
    mkSymVal = SI.genMkSymVar KDouble
    literal (LengthInDoubleMetres l) = SI.SBV . SI.SVal KDouble . Left . SI.CV KDouble . SI.CDouble $ l
    fromCV (SI.CV _ (SI.CDouble a)) = LengthInDoubleMetres a
    fromCV c                 = error $ "SymVal.LengthInDoubleMetres: Unexpected non-float value: " ++ show c

instance IEEEFloating LengthInDoubleMetres

formula :: Symbolic SBool
formula = do
    n :: SLenIntMetres <- exists "n"
    x :: SLenFMetres   <- exists "x"
    y :: SLenDMetres   <- exists "y"
    constrain $ y .< literal 3.14
    return $ (n .== literal 3) .&& (sFromIntegral n .== x)

main :: IO ()
main = print =<< sat formula
