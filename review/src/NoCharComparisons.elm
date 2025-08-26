module NoCharComparisons exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node exposing (Node(..))
import Hex
import Review.Fix as Fix
import Review.Rule as Rule exposing (Rule)


{-| Reports... REPLACEME

    config =
        [ NoCharComparisons.rule
        ]


## Fail

    a =
        "REPLACEME example to replace"


## Success

    a =
        "REPLACEME example to replace"


## When (not) to enable this rule

This rule is useful when REPLACEME.
This rule is not useful when REPLACEME.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template undefined/example --rules NoCharComparisons
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchemaUsingContextCreator "NoCharComparisons" initialContext
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


type alias Context =
    {}


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator
        (\() ->
            {}
        )


expressionVisitor : Node Expression -> Context -> ( List (Rule.Error {}), Context )
expressionVisitor node context =
    case node of
        Node range (Expression.OperatorApplication "==" _ (Node lrange (Expression.FunctionOrValue [] _)) (Node rrange (Expression.CharLiteral char))) ->
            ( [ Rule.errorWithFix
                    { message = "Comparing to char literals is slow"
                    , details =
                        [ "The Elm compiler, in debug mode, doesn't optimize char comparisons, so they go through _Utils_eq and they're slow."
                        , "Use `==` between numbers instead."
                        ]
                    }
                    range
                    [ Fix.insertAt lrange.start "Char.toCode "
                    , Fix.insertAt rrange.start ("0x" ++ Hex.toString (Char.toCode char) ++ " {- ")
                    , Fix.insertAt rrange.end " -}"
                    ]
              ]
            , context
            )

        _ ->
            ( [], context )
