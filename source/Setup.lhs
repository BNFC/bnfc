#!/usr/bin/env runghc

> module Main where
>
> import Distribution.Extra.Doctest (defaultMainWithDoctests)
>
> main :: IO ()
> main = defaultMainWithDoctests "doctests"

This generates the module Build_doctests.
