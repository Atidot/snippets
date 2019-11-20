#! /usr/bin/env nix-shell
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/19.03.tar.gz -i runhaskell -p "(pkgs.haskell.packages.ghc864.extend (self: super: with haskell; { language-python = lib.doJailbreak super.language-python; terraform-hs = lib.doJailbreak (self.callPackage({ mkDerivation, base, containers, data-default, fetchgit, filepath, stdenv, text, transformers}: mkDerivation {pname = \"terraform-hs\";version = \"0.1.0.0\";src = fetchgit {  url = \"https://github.com/Atidot/terraform-hs\";  sha256 = \"1bxfpi686b6278bln5bdj57jw3cizyvwjb0vnq08swvg4l5j2z9w\";  rev = \"1bxfpi686b6278bln5bdj57jw3cizyvwjb0vnq08swvg4l5j2z9w\";  fetchSubmodules = true;};libraryHaskellDepends = [  base containers data-default filepath text transformers];homepage = \"https://github.com/timbod7/terraform-hs#readme\";description = \"Initial project template from stack\";license = stdenv.lib.licenses.bsd3;}) {}); })).ghcWithPackages (ps: with ps; [containers lens ps.terraform-hs])"
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import "base"                   Control.Monad(void)
import "base"                   Data.Traversable(for)
import "base"                   Data.Monoid
import "lens"                   Control.Lens
import "terraform-hs"           Language.Terraform.Core
import "terraform-hs"           Language.Terraform.Aws

import qualified "containers"   Data.Map as M
import qualified "text"         Data.Text as T
import qualified "terraform-hs" Language.Terraform.Util.Text as T


-- generates simple configuration as used in the tutorial for terrform
-- https://learn.hashicorp.com/terraform/getting-started/build
-- the difference is only in instances name, due to the terraform-hs name labeling
simpleConfig :: TF AwsInstance
simpleConfig = awsInstance' "example" "ami-2757f631" "t2.micro"

main :: IO ()
main = generateFiles "/home/talz/development/snippets/Haskell" $ do
  withNameScope "example" $ do
    newAws (makeAwsParams "us-east-1"){aws_profile="default"}
    simpleConfig
  return ()
