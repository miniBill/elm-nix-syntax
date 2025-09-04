# elm-nix-syntax

Nix Syntax in Elm: for parsing and writing Nix in Elm.

The structure of modules, much of the documentation and some of the code is from [elm-syntax](https://github.com/stil4m/elm-syntax).

## How does this work?

When Nix code is parsed, it's converted into an Abstract Syntax Tree (AST).
The AST lets us represent the code in a way that's much easier to work with when programming.

Here's an example of that:
Code: `3 + 4 * 2`
AST:
```elm
OperatorApplicationExpr
    (IntExpr 3)
    "+"
    (OperatorApplicationExpr
        (IntExpr 4)
        "*"
        (IntExpr 2)
    )
```

Notice how it forms a tree structure where we first multiply together 4 and 2, and then add the result with 3.
That's where the "tree" part of AST comes from.

## Getting Started

```elm
import Nix.Parser
import Html exposing (Html)

src : String
src =
    """
{ lib
, pkgs
, ...
}:
lib.getName pkgs.vim
"""

parse : String -> String
parse input =
    case Nix.Parser.parse input of
        Err e ->
            "Failed: " ++ Debug.toString e

        Ok v ->
            "Success: " ++ Debug.toString v

main : Html msg
main =
    Html.text (parse src)
```
