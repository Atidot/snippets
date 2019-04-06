#! /usr/bin/env nix-shell
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/18.09.tar.gz --pure -i runhaskell -p "pkgs.haskell.packages.ghc843.ghcWithPackages (ps: with ps; [template-haskell split process containers casing wreq lens tagsoup bytestring text])"
{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE StandaloneDeriving #-}

import           "base"             Data.Monoid ((<>))
{-import           "base"             System.Environment (getArgs)-}
import           "base"             Data.List (groupBy)
import           "bytestring"       Data.ByteString.Lazy.Char8 (toStrict)
import           "text"             Data.Text (Text)
import qualified "text"             Data.Text as T (strip, unwords, words, unpack, isPrefixOf, filter)
import           "text"             Data.Text.Encoding (decodeUtf8)
import           "lens"             Control.Lens
import           "casing"           Text.Casing
import           "tagsoup"          Text.HTML.TagSoup
import           "wreq"             Network.Wreq
import           "template-haskell" Language.Haskell.TH

type Command    = Text
type Subcommand = Text
type Flag       = Text

analyzeAzureCLIDocumentation :: String -> IO [(Command, Subcommand, [Flag])]
analyzeAzureCLIDocumentation uri = do
    response <- get uri
    let body = response ^. responseBody
    return $ -- take definition (not example)
             map head
           . groupBy (\(_, subC1, _) (_, subC2, _) -> subC1 == subC2)

           -- turn to command list
           . map toCommand

           -- partition by "code" "lang-azurecli"
           . partitions (~== TagOpen ("code" :: Text) [("class","lang-azurecli")])
           . parseTags

           -- turn to Text
           . decodeUtf8
           . toStrict
           $ body
    where
        toCommand :: [Tag Text] -> (Command, Subcommand, [Flag])
        toCommand tags
            = (\(command : subCommand : flags) -> (command, subCommand, flags))
            . drop 1 -- drop "az"
            . T.words
            . T.unwords
            . map T.strip
            . map fromTagText

            -- filter only "TagText" until "TagClose"
            . filter isTagText
            . takeWhile (~/= TagClose ("code" :: Text))

            -- drop the "TagOpen" "code"
            . drop 1
            $ tags


deriveCommand :: String -> DecsQ
deriveCommand uri = do
    cmds <- runIO $ analyzeAzureCLIDocumentation uri
    let name = mkName . fix . T.unpack . (\(cmd, _, _) -> cmd) . head $ cmds
    let dataType = DataD []
                         name
                         []
                         Nothing
                         (map toCon cmds)
                         [ DerivClause Nothing  -- [DerivClause]
                                       [ ConT $ mkName "Show"
                                       , ConT $ mkName "Read"
                                       , ConT $ mkName "Eq"
                                       ]
                         ]
    return [dataType]
    where
        fix :: String -> String
        fix = toPascal . fromAny

        toField command subCommand flag
            = ( mkName fieldName
              , Bang NoSourceUnpackedness SourceStrict
              , type'
              )
            where
                fieldName = "_" <> (toCamel . fromAny . T.unpack $ command)
                         <> (toPascal . fromAny . T.unpack $ subCommand)
                         <> "_" <> (toCamel . fromAny . T.unpack . T.filter (not . (`elem` ("[]" :: String))) $ flag)

                type' | "[" `T.isPrefixOf` flag = AppT (ConT $ mkName "Maybe") (ConT $ mkName "String")
                      | otherwise = (ConT $ mkName "String")

        toCon (command, subCommand, flags)
            = RecC (mkName . fix . T.unpack $ subCommand)
                   (map (toField command subCommand) flags)




main :: IO ()
main = do
    -- (uri:_) <- getArgs
    let uri = "https://docs.microsoft.com/en-us/cli/azure/group?view=azure-cli-latest#az-group-list"
    xs <- runQ $ deriveCommand uri
    putStrLn $ pprint xs
