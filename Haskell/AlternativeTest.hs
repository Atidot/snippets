#! /usr/bin/env nix-shell
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/18.09.tar.gz --pure -i runhaskell -p "pkgs.haskellPackages.ghcWithPackages (pkgs: with pkgs; [])"
{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE PackageImports #-}
module Main where
import           Control.Monad

-- Maybe has Alternative instance
-- no IFs no nested code
testWhenYourMonadHasAlternative1 :: Maybe Int
testWhenYourMonadHasAlternative1 = do
    x <- Just 5
    y <- Nothing
    let w = x + y
    guard $ w `mod` 2 == 0
    return w

-- List has Alternative instance
-- no IFs no nested code
testWhenYourMonadHasAlternative2 :: [Int]
testWhenYourMonadHasAlternative2 = do
    x <- [1 .. 5]
    y <- [1 .. 5]
    let w = x + y
    guard $ w `mod` 2 == 0
    return w

-- IO doesn't have alternative instance
-- can't use "guard" to short-circut run and "return something"
testNoAlternative1 :: IO (Maybe Int)
testNoAlternative1 = do
    guard $ False -- exception = AlternativeTest.hs: user error (mzero)
    print "shouldn't get here"
    return Nothing

-- IO doesn't have alternative instance
testNoAlternative2 :: IO (Maybe Int)
testNoAlternative2 = do
    when False $ do
        return ()
    print "shouldn't get here"
    return Nothing

-- IO doesn't have alternative instance
-- using "Maybe"s as return values doesn't change the monad
-- it's still IO (and still no Alternative in your monadic-do actions)

testNoAlternative3 :: IO (Maybe Int)
testNoAlternative3 = do
    x <- return (Just 5)
    y <- return (Just 6)

    -- let w = x + y -- won't compile no `Num (Maybe)`
    -- guard $ w `mod` 2 == 0 -- won't compile, `w` is a Maybe

    let w = (+) <$> x <*> y
    -- Python / C :
    case w of
        Nothing -> return (Just 0)
        Just x' -> return (Just x')


main :: IO ()
main = do
    print testWhenYourMonadHasAlternative1
    print testWhenYourMonadHasAlternative2
    print =<< testNoAlternative3
    _ <- testNoAlternative2
    _ <- testNoAlternative1
    return ()
