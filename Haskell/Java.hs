#! /usr/bin/env nix-shell
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/18.09.tar.gz --pure -i ghc -p jdk8 -p gradle -p "pkgs.haskellPackages.ghcWithPackages (pkgs: with pkgs; [text jvm inline-java])"
{-# OPTIONS_GHC -Wall -Werror #-}
{-# OPTIONS_GHC -fplugin=Language.Java.Inline.Plugin #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import "text"        Data.Text (Text)
import "jvm"         Language.Java (withJVM)
import "jvm"         Language.Java (reflect)
import "inline-java" Language.Java.Inline (java)


main :: IO ()
main = withJVM [] $ do
    message <- reflect ("Hello World!" :: Text)
    [java| {
      javax.swing.JOptionPane.showMessageDialog(null, $message);
      }
    |]
