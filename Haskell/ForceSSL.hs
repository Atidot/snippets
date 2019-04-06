#! /usr/bin/env nix-shell
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/18.09.tar.gz --pure -i runhaskell -p "pkgs.haskellPackages.ghcWithPackages (pkgs: with pkgs; [wai warp warp-tls http-types wai-extra])"
{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import "base"       Control.Concurrent
import "http-types" Network.HTTP.Types
import "wai"        Network.Wai
import "wai-extra" Network.Wai.Middleware.ForceSSL
import "warp"       Network.Wai.Handler.Warp
import "warp-tls"   Network.Wai.Handler.WarpTLS


serveApp :: IO ()
serveApp = do
    -- HTTPS
    _ <- forkIO $ do
        let tls = tlsSettings "test.crt"
                              "test.key"
        runTLS tls (setPort 443 defaultSettings) (forceSSL app)
    -- HTTP
    run 80 $ forceSSL $ app


app :: p -> (Response -> t) -> t
app _ respond = do
    respond $ responseLBS status200 [] "Hello World"


main :: IO ()
main = do
    serveApp
