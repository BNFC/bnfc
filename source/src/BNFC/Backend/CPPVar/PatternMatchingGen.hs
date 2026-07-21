{-# LANGUAGE QuasiQuotes #-}

{-|
  Module      : BNFC.Backend.CPPVar.PatternMatchingGen
  Description : The pipe "|" operator overload.

  The pipe "|" operator overload.
-}

module BNFC.Backend.CPPVar.PatternMatchingGen
  (
    -- * The entrypoint
    patternMatchingHpp

    -- * File naming
  , patternMatchingFilename
  ) where

import Data.String.QQ (s)
import Text.PrettyPrint (Doc)

import BNFC.Backend.CPPVar.CPPUtil

-- | The name of the header file.
patternMatchingFilename :: String
patternMatchingFilename = "PatternMatching.hpp"

-- | The contents of the header file.
patternMatchingHpp :: Doc
patternMatchingHpp = unlinesToText [s|
/************************** Pattern Matching for C++ ***************************
* You are highly encouraged to include this file to enable the following syntax:

    variant<A, B> v1;
    variant<C, D, E> v2;
    //...
    v1 | PatternMatch{
        [](A a) { cout << "Got A"; },
        [](B b) { cout << "Got B"; },
    };
    cout << (v2 | PatternMatch{
        [](C c) { return "Got C"; },
        [](...) { return "Got D or E"; },
    });
    cout << (make_tuple(v1, v2) | PatternMatch{
        [](A, C) { return "Got AC"; },
        [](auto&&, C) { return "Got _C"; },  // will match (B, C)
        [](...) { return "Something else"; },
    });

* The pipe "|" operator also works with visitor classes:

    struct Leaf; struct Branch;
    using Tree = variant<Leaf, Branch>;
    struct Leaf { char Value = '.'; };
    struct Branch { unique_ptr<Tree> left, right; };

    // class Flip, class Linearize - visitors

    string chain(Tree tree) {
        return tree | Flip() | Linearize();  // flip and then linearize
    }

* (The visitors in the above example could be defined as follows:)

    class Flip {
    public:
        Tree operator()(const Leaf& l) { return l; }
        Tree operator()(const Branch& b) {
            return Branch{
                make_unique<Tree>(*b.right | *this),
                make_unique<Tree>(*b.left | *this)
            };
        }
    };
    class Linearize {  // (depth-first traversal)
    public:
        string operator()(const Leaf& l) { return string(1, l.Value); }
        string operator()(const Branch& b) {
            return (*b.left | *this) + (*b.right | *this);
        }
    };

*/

#pragma once
#include <utility>
#include <variant>
#include <tuple>

template <class... TCase>
class PatternMatch : TCase... {
public:
    PatternMatch(TCase&&... closure)
        : TCase(std::forward<TCase>(closure))... {}
    using TCase::operator()...;
};

template <class... TVariants, class TMatcher>
decltype(auto) operator|(std::variant<TVariants...>&& variant, TMatcher&& vis) {
    return std::visit(std::forward<TMatcher>(vis), std::move(variant));
}

template <class... TVariants, class TMatcher>
decltype(auto) operator|(const std::variant<TVariants...>& variant,
        TMatcher&& vis) {
    return std::visit(std::forward<TMatcher>(vis), variant);
}

template <class... TVariants, class TMatcher>
decltype(auto) operator|(std::variant<TVariants...>& variant, TMatcher&& vis) {
    return std::visit(std::forward<TMatcher>(vis), variant);
}

template <class... TVariant, class TMatcher>
decltype(auto) operator|(std::tuple<TVariant...>&& vars, TMatcher&& vis) {
    return std::apply(
        [&vis](auto&&... args) {
            return std::visit(std::forward<TMatcher>(vis),
                              std::forward<decltype(args)>(args)...);
        },
        std::move(vars));
}
|]
