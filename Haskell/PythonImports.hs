#! /usr/bin/env nix-shell
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/19.03.tar.gz -i runhaskell -p "(pkgs.haskell.packages.ghc864.extend (self: super: with haskell; { language-python = lib.doJailbreak super.language-python; })).ghcWithPackages (ps: with ps; [language-python uniplate])"
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where
import           "base"             Data.Monoid ((<>))
import           "base"             Data.List (intercalate)
import           "base"             System.IO (hGetContents, stdin)
import           "uniplate"         Data.Generics.Uniplate.Data (universeBi, childrenBi)
import           "language-python"  Language.Python.Common hiding ((<>), Assignment)
import           "language-python"  Language.Python.Version3.Parser

--
type PythonSourceCode  = String
type PythonPackageName = String

getImports :: PythonSourceCode -> Maybe [PythonPackageName]
getImports source = do
    case parseModule source "<PATH>" of
        Left _ -> Nothing
        Right (pyModule, _) -> do
            Just $ concat $ imports pyModule <> froms pyModule

    where
        --
        imports :: Module SrcSpan -> [[String]]
        imports pyModule = do
            let getImportItems = universeBi :: (Module SrcSpan -> [ImportItem SrcSpan])
            let getIdentifiers = childrenBi :: ([ImportItem SrcSpan] -> [[Ident SrcSpan]])
            let packageNames = map (intercalate "." . map ident_string)
                             . getIdentifiers
                             . getImportItems
                             $ pyModule
            return packageNames

        --
        froms :: Module SrcSpan -> [[String]]
        froms pyModule = do
            let getImportRelatives = universeBi :: (Module SrcSpan -> [ImportRelative SrcSpan])
            let getIdentifiers     = childrenBi :: ([ImportRelative SrcSpan] -> [[Ident SrcSpan]])
            let packageNames = map (intercalate "." . map ident_string)
                             . getIdentifiers
                             . getImportRelatives
                             $ pyModule
            return packageNames

--
main :: IO ()
main = do
    source <- hGetContents stdin
    let ps = getImports source
    print ps
