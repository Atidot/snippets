#! /usr/bin/env nix-shell
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/18.09.tar.gz --pure -i runhaskell -p "pkgs.haskellPackages.ghcWithPackages (ps: with ps; [aeson text lucid shakespeare clay])"
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}

import           "text"        Data.Text (Text)
import           "text"        Data.Text.Lazy (toStrict)
import qualified "text"        Data.Text.IO as T (putStrLn)
import           "lucid"       Lucid
import           "shakespeare" Text.Julius


main :: IO ()
main = do
    T.putStrLn . toStrict $ renderText blog


blog :: Html ()
blog = html_ $ do
    blogHead
    body_ $ do
        intro
        part1
        part2
        conclusion


blogHead :: Html ()
blogHead = head_ $ do
    title_ "The Title"
    meta_ [charset_ "utf-8"]
    meta_ [ name_ "viewport"
          ,content_ "width=device-width, initial-scale=1"
          ]
    script' "https://cdnjs.cloudflare.com/ajax/libs/d3/5.9.1/d3.min.js"
    link'   "https://cdnjs.cloudflare.com/ajax/libs/c3/0.6.12/c3.min.css"
    script' "https://cdnjs.cloudflare.com/ajax/libs/c3/0.6.12/c3.min.js"


intro :: Html ()
intro = do
    h1_ "Intorduction"
    p_ $ do
        "lorem ipsum"
        "lorem ipsum"
        "lorem ipsum"
        "lorem ipsum"


part1 :: Html ()
part1 = do
    h2_ "Part 1"
    p_ $ do
        "lorem ipsum"
        "lorem ipsum"
        "lorem ipsum"
        newline
        newline
        "lorem ipsum"
        ul_ $ do
            li_ $ "A" --> "http://www.wikipedia.org"
            li_ $ "B"  --> "http://www.wikipedia.org"
            li_ $ "C"  --> "http://www.wikipedia.org"


part2 :: Html ()
part2 = do
    h2_ "Part 2"
    p_ $ do
        "lorem ipsum"
        "lorem ipsum"
        "lorem ipsum" >> b_ "lorem ipsum" >> "lorem ipsum"
        newline
        "lorem ipsum" >> b_ "lorem ipsum" >> "lorem ipsum"
        ul_ $ do
            li_ $ "X"  --> "http://www.wikipedia.org"
            li_ $ "Y"  --> "http://www.wikipedia.org"
            li_ $ "Z"  --> "http://www.wikipedia.org"
    div_ [id_ "chart"] ""
    barChart
    where


conclusion :: Html ()
conclusion = do
    h2_ $ "Conslusion"
    p_ $ do
        "lorem ipsum"
        "lorem ipsum"
        newline
        "lorem ipsum"
        newline


barChart :: Html ()
barChart = do
    scriptJs [js|
        var chart = c3.generate({
            bindto: "#chart",
            grid: {
                x: {
                    show: true
                },
                y: {
                    show: true
                }
            },
            axis: {
              y : {
                tick: {
                  format: d3.format("$,")
                }
              }
            },
            data: {
                type: "bar",
                x: "x",
                json: {
                    x: [2010, 2011, 2012, 2013, 2014, 2015],
                    A: [30, 20, 50, 40, 60, 50],
                    B: [200, 130, 90, 240, 130, 220],
                    C: [300, 200, 160, 400, 250, 250]
                }
            }
        });
    |]


-- helpers
text --> uri = a_ [href_ uri] text
script' uri  = script_ [src_ uri] ("" :: Text)
scriptJs js  = script_ [] $ renderJs js
link'   uri  = link_ [ rel_ "stylesheet", href_ uri ]

newline :: Html ()
newline = br_ []

renderJs = renderJavascriptUrl (\_ _ -> undefined)
