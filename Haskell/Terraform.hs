#! /usr/bin/env nix-shell
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/19.03.tar.gz -i runhaskell -p "(pkgs.haskell.packages.ghc864.extend (self: super: with haskell; { language-python = lib.doJailbreak super.language-python; terraform-hs = self.callPackage({ mkDerivation, base, containers, data-default, fetchgit, filepath, stdenv, text, transformers}: mkDerivation {pname = \"terraform-hs\";version = \"0.1.0.0\";src = fetchgit {  url = \"https://github.com/Atidot/terraform-hs\";  sha256 = \"1sb962lfbl08bd9bf39c41lc9p8vx4vzzrn8c2z03p8l2dg4hwg3\";  rev = \"4eae2b437092a97a433dbe658efff948496b9115\";  fetchSubmodules = true;};libraryHaskellDepends = [  base containers data-default filepath text transformers];homepage = \"https://github.com/timbod7/terraform-hs#readme\";description = \"Initial project template from stack\";license = stdenv.lib.licenses.bsd3;}); })).ghcWithPackages (ps: with ps; [terraform-hs])"
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where


main :: IO ()
main = putStrLn "hello world"


-- { mkDerivation, base, containers, data-default, fetchgit, filepath, stdenv, text, transformers}: mkDerivation {pname = "terraform-hs";version = "0.1.0.0";src = fetchgit {  url = "https://github.com/Atidot/terraform-hs";  sha256 = "1sb962lfbl08bd9bf39c41lc9p8vx4vzzrn8c2z03p8l2dg4hwg3";  rev = "4eae2b437092a97a433dbe658efff948496b9115";  fetchSubmodules = true;};libraryHaskellDepends = [  base containers data-default filepath text transformers];homepage = "https://github.com/timbod7/terraform-hs#readme";description = "Initial project template from stack";license = stdenv.lib.licenses.bsd3;}
