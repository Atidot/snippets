#! /usr/bin/env nix-shell
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/19.03.tar.gz -i runhaskell -p "(pkgs.haskell.packages.ghc864.extend (self: super: with haskell; rec { stratosphere = self.callPackage ( { mkDerivation, aeson, aeson-pretty, base, bytestring, containers , fetchgit, hashable, hpack, hspec, hspec-discover, lens, stdenv , template-haskell, text, unordered-containers }: mkDerivation { pname = \"stratosphere\"; version = \"0.50.0\"; src = fetchgit { url = \"https://github.com/freckle/stratosphere\"; sha256 = \"045x6rw7b85zxxv4mm3i3dz21n5cc5212maxckl1bvkdywwadqmp\"; rev = \"8981ed145a0e582981403fb7ee7e10d5be5de508\"; fetchSubmodules = true; }; isLibrary = true; isExecutable = true; libraryHaskellDepends = [ aeson aeson-pretty base bytestring containers hashable lens template-haskell text unordered-containers ]; libraryToolDepends = [ hpack ]; testHaskellDepends = [ aeson aeson-pretty base bytestring containers hashable hspec hspec-discover lens template-haskell text unordered-containers ]; testToolDepends = [ hspec-discover ]; preConfigure = \"hpack\"; homepage = \"https://github.com/frontrowed/stratosphere#readme\"; description = \"EDSL for AWS CloudFormation\"; license = stdenv.lib.licenses.mit; }) {}; })).ghcWithPackages (ps: with ps; [stratosphere heredoc])"
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import           "lens"         Control.Lens
import           "aeson"        Data.Aeson (Value (Array), object)
import qualified "bytestring"   Data.ByteString.Lazy.Char8 as B
import           "text"         Data.Text (Text)
import           "stratosphere" Stratosphere
import           "heredoc"      Text.Heredoc


--
main :: IO ()
main = B.putStrLn $ encodeTemplate myTemplate


-- Helpers
arn :: Resource -> Val Text
arn = flip GetAtt "Arn" . _resourceName


-- Template
myTemplate :: Template
myTemplate
    = template
    [ role'
    , ecs
    , apiGateway
    , apiGatewayRootMethod
    , apiGatewayDeployment'
    , lambda
    , lambdaApiGatewayInvoke
    ]
    & templateDescription   ?~ "Lambda example"
    & templateFormatVersion ?~ "2010-09-09"
    & templateParameters ?~
        [ apiGatewayStageName
        , lambdaFunctionName
        ]
    & templateOutputs ?~
        [ lambdaArn
        , apiGatewayInvokeUrl
        ]


-- Parameters
apiGatewayStageName :: Parameter
apiGatewayStageName
    = parameter "apiGatewayStageName"
                "String"
    & parameterAllowedPattern ?~ "^[a-z0-9]+$"
    & parameterDefault'       ?~ "call"

lambdaFunctionName :: Parameter
lambdaFunctionName
    = parameter "lambdaFunctionName"
                "String"
    & parameterAllowedPattern ?~ "^[a-zA-Z0-9]+[a-zA-Z0-9-]+[a-zA-Z0-9]+$"
    & parameterDefault'       ?~ "my-function"


-- Outputs
lambdaArn :: Output
lambdaArn
    = output "lambdaArn" $ arn lambda

apiGatewayInvokeUrl :: Output
apiGatewayInvokeUrl
    = output "apiGatewayInvokeUrl"
    $ Sub uri Nothing
    where
        uri :: Text
        uri = "https://${" <> gw <> "}.execute-api.${AWS::Region}.amazonaws.com/${" <> gwStageName <> "}"
        gw :: Text
        gw = apiGateway ^. resourceName

        gwStageName :: Text
        gwStageName = apiGatewayStageName ^. parameterName


-- Resources
ecs :: Resource
ecs = resource "ECSCluster" ecsCluster

apiGateway :: Resource
apiGateway
    = resource "apiGateway"
    $ apiGatewayRestApi
    & agraName        ?~ "my-api"
    & agraDescription ?~ "My API"


apiGatewayRootMethod :: Resource
apiGatewayRootMethod
    = resource "apiGatewayRootMethod" method
    where
        method :: ApiGatewayMethod
        method = apiGatewayMethod (Literal POST)
                                 (GetAtt (apiGateway ^. resourceName) "RootResourceId")
                                 (Ref $ apiGateway ^. resourceName)
               & agmeAuthorizationType ?~ Literal NONE
               & agmeIntegration       ?~ integration

        integration :: ApiGatewayMethodIntegration
        integration = apiGatewayMethodIntegration
                    & agmiType                  ?~ Literal AWS_PROXY
                    & agmiIntegrationHttpMethod ?~ Literal POST
                    & agmiUri                   ?~ uri

        uri :: Val Text
        uri = Sub "arn:aws:apigateway:${AWS::Region}:lambda:path/2015-03-31/functions/${lambdaArn}/invocations"
                  (Just [("lambdaArn", arn lambda)])


apiGatewayDeployment' :: Resource
apiGatewayDeployment'
    = ( resource "apiGatewayDeployment"
      $ apiGatewayDeployment (Ref $ apiGateway ^. resourceName)
      )
    & resourceDependsOn ?~ [apiGatewayRootMethod ^. resourceName]


lambda :: Resource
lambda
    = resource "LambdaFunction" function
    & resourceDependsOn ?~ [ role' ^. resourceName ]
    where
        function :: LambdaFunction
        function = lambdaFunction lambdaCode
                                  "index.handler"
                                  (arn role')
                                  (Literal NodeJS12x)

        lambdaCode :: LambdaFunctionCode
        lambdaCode = lambdaFunctionCode
                   & lfcZipFile ?~ code

        code :: Val Text
        code = [str|exports.handler = (event, context, callback) => {
                   |  const name = event.name || 'World';
                   |  const response = {greeting: `Hello, ${name}!`};
                   |  callback(null, response);
                   |};
                   |]


lambdaApiGatewayInvoke :: Resource
lambdaApiGatewayInvoke
    = resource "lambdaApiGatewayInvoke" permission
    where
        permission :: LambdaPermission
        permission = lambdaPermission "lambda:InvokeFunction"
                                      (arn lambda)
                                      "apigateway.amazonaws.com"
                   & lpSourceArn ?~ arn'

        arn' :: Val Text
        arn' = Sub ("arn:aws:execute-api:${AWS::Region}:${AWS::AccountId}:${" <> apiGateway ^. resourceName <> "}/*/POST/") Nothing


role' :: Resource
role' =
  resource "IAMRole" $
  iamRole
  rolePolicyDocumentObject
  & iamrPolicies ?~ [ executePolicy ]
  & iamrRoleName ?~ "MyLambdaBasicExecutionRole"
  & iamrPath ?~ "/"
    where
        executePolicy =
          iamRolePolicy
          [ ("Version", "2012-10-17")
          , ("Statement", statement)
          ]
          "MyLambdaExecutionPolicy"

          where
            statement = object
              [ ("Effect", "Allow")
              , ("Action", actions)
              , ("Resource", "*")
              ]

            actions = Array
              [ "logs:CreateLogGroup"
              , "logs:CreateLogStream"
              , "logs:PutLogEvents"
              ]

        rolePolicyDocumentObject =
          [ ("Version", "2012-10-17")
          , ("Statement", statement)
          ]

          where
            statement = object
              [ ("Effect", "Allow")
              , ("Principal", principal)
              , ("Action", "sts:AssumeRole")
              ]

            principal = object
              [ ("Service", "lambda.amazonaws.com") ]
