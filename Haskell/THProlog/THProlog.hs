#! /usr/bin/env nix-shell
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/19.09.tar.gz -i runhaskell -p swiProlog -p "pkgs.haskell.packages.ghc864.ghcWithPackages (ps: with ps; [template-haskell process lens])"
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE StandaloneDeriving #-}
module THProlog where

import           "base"             Control.Monad (void)
import           "base"             Data.Char (toUpper)
import           "lens"             Control.Lens
import           "process"          System.Process (readProcess)
import           "template-haskell" Language.Haskell.TH
import           "template-haskell" Language.Haskell.TH.Syntax
import           "template-haskell" Language.Haskell.TH.Quote
import           "template-haskell" Language.Haskell.TH.Ppr

--
deriveTypes :: FilePath -> DecsQ
deriveTypes prologPath = do
    xs <- runIO
        $ readProcess "swipl"
                      [ "-f", prologPath
                      --
                      , "-g", "forall(type(X, life), writeln(X))."
                      --
                      , "-t", "halt."
                      ]
                      ""
    let typesNames = map (over _head toUpper)
                   . filter (not . elem '(')
                   . lines
                   $ xs

    let dataTypes = map mkDataType typesNames
    return dataTypes
    where
        mkDataType name
            = DataD []
                    (mkName name)
                    []
                    Nothing
                    []
                    []

--
testCompileTimeCheck :: Name -> DecsQ
testCompileTimeCheck name = do
    (VarI name' type' _) <- reify name
    runIO $ putStrLn $ "--------------------------------------------------------------"
    runIO $ putStrLn $ "Function:     " <> nameBase name'
    runIO $ putStrLn $ "Type:         " <> show type'
    runIO $ putStrLn $ "Pretty Type:  " <> pprint type'
    runIO $ putStrLn $ "--------------------------------------------------------------"
    runIO $ putStrLn $ "TODO:     analyze this type using Prolog, does it make sense?"
    return []


--
main :: IO ()
main = do
    xs <- runQ $ deriveTypes "insurance.pl"
    putStrLn $ pprint xs
