#! /usr/bin/env nix-shell
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/19.03.tar.gz -i runhaskell -p "pkgs.haskell.packages.ghc864.ghcWithPackages (ps: with ps; [stratosphere])"
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as B
import Stratosphere

main :: IO ()
main = B.putStrLn $ encodeTemplate instanceTemplate

instanceTemplate :: Template
instanceTemplate
    = template [ resource "EC2Instance"
                 ( EC2InstanceProperties
                 $ ec2Instance
                 & eciImageId ?~ "ami-22111148"
                 & eciKeyName ?~ (Ref "KeyName")
                 )
               & resourceDeletionPolicy ?~ Retain
               ]
    & templateDescription ?~ "Sample template"
    & templateParameters ?~
      [ parameter "KeyName" "AWS::EC2::KeyPair::KeyName"
      & parameterDescription ?~ "Name of an existing EC2 KeyPair to enable SSH access to the instance"
      & parameterConstraintDescription ?~ "Must be the name of an existing EC2 KeyPair."
      ]
