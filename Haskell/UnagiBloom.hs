#! /usr/bin/env nix-shell
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/18.03.tar.gz --pure -i ghcjs -p "(pkgs.haskell.packages.ghcjsHEAD.extend (self: super: with haskell; {})).ghcWithPackages (ps: with ps; [unagi-bloomfilter text bytestring])"
{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import "unagi-bloomfilter" Control.Concurrent.BloomFilter as Bloom
import "text"              Data.Text (Text)

main :: IO ()
main = do
    b_5_20 <- Bloom.new (Bloom.SipKey 1 1) 5 20
    _ <- Bloom.insert b_5_20 "test"
    p <- Bloom.lookup b_5_20 ("test" :: Text)
    print p

    b_5_20' <- deserialize (Bloom.SipKey 1 1) =<< serialize b_5_20
    p' <- Bloom.lookup b_5_20' ("test" :: Text)
    print p'
