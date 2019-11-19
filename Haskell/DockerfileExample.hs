#!/usr/bin/env nix-shell
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/19.09.tar.gz --pure -i runhaskell -p "pkgs.haskell.packages.ghc881.ghcWithPackages (ps: with ps; [ dockerfile containers ])"
{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

import "base"       Data.Foldable (foldl')
import "base"       GHC.Generics (Generic)
import "base"       Data.Typeable (Typeable)
import "base"       Data.Data (Data)
import "containers" Data.Map.Strict (Map, empty, assocs)
import "dockerfile" Data.Docker
import "directory"  System.Directory (getCurrentDirectory)

data OS = Ubuntu
        | RedHat
        | CentOS
        deriving (Show, Read, Eq, Ord, Enum, Bounded, Data, Typeable, Generic)

data User = Root
          | User { _user_name :: !String }
          deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

type Entrypoint = (String, [String])

data ContainerEnv = ContainerEnv 
          { _containerEnv_OS :: !OS 
          , _containerEnv_users :: ![User]
          , _containerEnv_image :: !String
          , _containerEnv_installations :: ![(String, [String])]
          , _containerEnv_env :: !(Map String String)
          , _containerEnv_runCmds :: ![String]
          , _containerEnv_entrypoint :: !(Maybe Entrypoint)
          , _containerEnv_command :: !(Maybe [String])
          } deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)   

aptInstalls :: (String, [String])
aptInstalls = ("apt-get install -yq", ["python3", "python3-pip", "wget", "tar"])

pipInstalls :: (String, [String])
pipInstalls = ("pip3 install --user", ["jupyter"])

runCmds :: [String]
runCmds = ["wget http://apache.mirrors.hoobly.com/drill/drill-1.16.0/apache-drill-1.16.0.tar.gz", "tar -xzvf apache-drill-1.16.0.tar.gz"]

jupyter :: Entrypoint
jupyter = ("jupyter", ["notebook", "--ip-0.0.0.0", "--no-browser"])

example :: ContainerEnv
example = ContainerEnv Ubuntu 
                       [User "atidot"] 
                       "ubuntu" 
                       [aptInstalls, pipInstalls]
                       empty
                       runCmds
                       (Just jupyter)
                       Nothing

toDocker :: ContainerEnv -> Docker ()
toDocker (ContainerEnv os users img pkgs environment runCmds entrypoint' command) = do
    from img
    foreach (makeUser os) users
    foreach (uncurry installPkgs) pkgs
    foreach (uncurry env) $ assocs environment
    foreach run runCmds
    doIfJust (uncurry entrypoint) entrypoint'
    doIfJust cmd command
        where doIfJust f Nothing = return ()
              doIfJust f (Just x) = f x

foreach :: (a -> Docker ()) 
        -> [a] 
        -> Docker ()
foreach f xs = foldl' (>>) (return ()) (map f xs)

installPkgs :: String
            -> [String] 
            -> Docker ()
installPkgs installer pkgs = run installation
    where installation = foldl' (++) (installer ++ endl1) (map instLine pkgs)
          instLine pkg = "    " ++ pkg ++ endl pkg
          endl pkg     = replicate (maxLength - paddedLength pkg) ' ' ++ " \\\n"
          paddedLine1  = "RUN " ++ installer ++ " \\"
          endl1        = replicate (maxLength - length paddedLine1) ' ' ++ " \\\n"
          maxLength    = maximum $ map length (paddedLine1 : map pad pkgs)
          paddedLength = length . pad
          pad pkg      = "    " ++ pkg ++ " \\"

addUserProgram :: OS -> String
addUserProgram Ubuntu = "adduser"
addUserProgram CentOS = "adduser"
addUserProgram RedHat = "useradd"

makeUser :: OS 
         -> User 
         -> Docker ()
makeUser _  Root         = return ()
makeUser os (User uname) = do
    run (addUserProgram os ++ " " ++ uname)
    user uname
    workdir ("/home/" ++ uname)
    env "PATH" "/home/atidot/.local/bin:${PATH}"

main :: IO ()
main = do
    pwd <- getCurrentDirectory
    let dockerPath = pwd ++ "/DockerfileExample"
    dockerfileWrite dockerPath $ toDocker example
