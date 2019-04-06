#! /usr/bin/env nix-shell
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/18.09.tar.gz --pure -i runhaskell -p "pkgs.haskellPackages.ghcWithPackages (pkgs: with pkgs; [conduit conduit-extra resourcet])"
{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
module Conduits where

import           "base"          System.IO (stdin, stdout)
import           "base"          Data.Monoid ((<>))
import           "base"          Data.Char (toUpper)
import qualified "bytestring"    Data.ByteString.Char8 as B8
import           "resourcet"     Control.Monad.Trans.Resource (runResourceT)
import           "conduit"       Data.Conduit
import qualified "conduit"       Data.Conduit.Combinators as CC (map)
import qualified "conduit-extra" Data.Conduit.Binary as CB

main :: IO ()
main = runResourceT . runConduit
     $ CB.sourceHandle stdin
    .| CB.lines
    .| CC.map (B8.map toUpper)
    .| CC.map (<> "\n")
    .| CB.sinkHandle stdout
