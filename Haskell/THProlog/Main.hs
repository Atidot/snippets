#! /usr/bin/env nix-shell
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/19.09.tar.gz -i runhaskell -p swiProlog -p "pkgs.haskell.packages.ghc864.ghcWithPackages (ps: with ps; [template-haskell process lens])"
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE StandaloneDeriving #-}

import           "base"             Control.Monad (void)
import           "base"             Data.Char (toUpper)
import           "lens"             Control.Lens
import           "process"          System.Process (readProcess)
import           "template-haskell" Language.Haskell.TH
import           "template-haskell" Language.Haskell.TH.Syntax
import           "template-haskell" Language.Haskell.TH.Quote
import           "template-haskell" Language.Haskell.TH.Ppr

import qualified THProlog as PL

$(PL.deriveTypes "insurance.pl")

testFunction :: Term -> Universal -> Variable
testFunction t w = undefined


$(PL.testCompileTimeCheck 'testFunction)

main :: IO ()
main = do
    return ()

