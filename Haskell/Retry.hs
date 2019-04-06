#! /usr/bin/env nix-shell
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/18.09.tar.gz --pure -i runhaskell -p "pkgs.haskellPackages.ghcWithPackages (pkgs: with pkgs; [retry])"
{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Retry where

import "retry"     Control.Retry
import "directory" System.Directory (doesFileExist)

myPolicy :: RetryPolicy
myPolicy =  exponentialBackoff 1000000 <> limitRetries 5

predicate :: RetryStatus -> a -> IO Bool
predicate _ _ = not <$> doesFileExist "/tmp/nonexist"

main :: IO ()
main = do
    _ <- retrying myPolicy predicate $ \_ -> do
        print "Still waiting for file..."
        return False
    return ()
