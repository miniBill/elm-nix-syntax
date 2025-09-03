module Flake exposing (suite)

import Nix.Syntax.Expression exposing (Attribute(..), Expression(..), Name(..), Pattern(..), RecordFieldPattern(..), StringElement(..))
import Nix.Syntax.Node as Node exposing (Node)
import Test exposing (Test)
import Utils exposing (apply, dot, function, let_, list, parens, record, string, update, var)


input : String
input =
    """{
  description = "Home Manager and NixOS configurations";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";

    # nixpkgs-small.url = "github:NixOS/nixpkgs/nixos-25.05-small";

    # nixpkgs-unstable.url = "nixpkgs/nixos-unstable";

    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    systems.url = "github:nix-systems/default";

    secretdemoclub = {
      url = "github:miniBill/secretdemoclub?dir=server";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };

    pinned-unstable-papermc.url = "github:NixOS/nixpkgs?rev=4cba8b53da471aea2ab2b0c1f30a81e7c451f4b6";

    lix-module = {
      url = "https://git.lix.systems/lix-project/nixos-module/archive/2.93.1.tar.gz";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };

    musnix = {
      url = "github:musnix/musnix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
      inputs.systems.follows = "systems";
    };

    home-manager = {
      url = "github:nix-community/home-manager/release-25.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-index-database = {
      url = "github:nix-community/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # lamdera = {
    #   url = "github:miniBill/lamdera-flake";
    #   inputs.flake-utils.follows = "flake-utils";
    #   inputs.nixpkgs.follows = "nixpkgs";
    # };

    # comma = {
    #   # just run any tool!
    #   url = "github:nix-community/comma";
    #   inputs.nixpkgs.follows = "nixpkgs";
    #   inputs.utils.follows = "flake-utils";
    #   inputs.flake-compat.follows = "flake-compat";
    # };

    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.systems.follows = "systems";
    };

    # flake-compat = {
    #   url = "github:edolstra/flake-compat";
    #   flake = false;
    # };
  };

  outputs =
    inputs:
    let
      allowedUnfree = [
        "code"
        "discord"
        "google-chrome"
        # "lamdera"
        "minecraft-launcher"
        "slack"
        "spotify"
        "vscode"
        "zoom"
      ];

      pkgs =
        system:
        import inputs.nixpkgs {
          system = system;
          config = {
            # overlays = [ inputs.comma.overlay ];
            allowUnfreePredicate = pkg: builtins.elem (inputs.nixpkgs.lib.getName pkg) allowedUnfree;
            permittedInsecurePackages = [
              "zotero-6.0.26"
            ];
          };
        };

      withConfig =
        {
          system,
          username ? "minibill",
          module,
        }:
        inputs.home-manager.lib.homeManagerConfiguration {
          pkgs = pkgs system;
          modules = [ module ];
          extraSpecialArgs = inputs // {
            username = username;
          };
        };
    in
    {
      homeConfigurations = {
        "minibill@gadiriel" = withConfig {
          system = "aarch64-darwin";
          module = ./machines/gadiriel/home-manager.nix;
        };
        "minibill@ithaca" = withConfig {
          system = "aarch64-linux";
          module = ./machines/ithaca/home-manager.nix;
        };
        "minibill@sohu" = withConfig {
          system = "aarch64-linux";
          module = ./machines/sohu/home-manager.nix;
        };
        "minibill@thamiel" = withConfig {
          system = "x86_64-linux";
          module = ./machines/thamiel/home-manager.nix;
        };
        "minibill@tharmas" = withConfig {
          system = "x86_64-linux";
          module = ./machines/tharmas/home-manager.nix;
        };
        "minibill@edge" = withConfig {
          system = "x86_64-linux";
          module = ./machines/edge/home-manager.nix;
        };
        "francesca@edge" = withConfig {
          system = "x86_64-linux";
          username = "francesca";
          module = ./machines/edge/home-manager.nix;
        };
        "minibill@milky" = withConfig {
          system = "x86_64-linux";
          module = ./machines/milky/home-manager.nix;
        };
        "minibill@uriel" = withConfig {
          system = "x86_64-linux";
          module = ./machines/uriel/home-manager.nix;
        };
        "llibinim@uriel" = withConfig {
          system = "x86_64-linux";
          username = "llibinim";
          module = ./machines/uriel/home-manager.nix;
        };
        "minibill@nathanda" = withConfig {
          system = "x86_64-linux";
          module = ./machines/nathanda/home-manager.nix;
        };
      };
      nixosConfigurations = {
        uriel = inputs.nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          specialArgs = inputs;
          modules = [ ./machines/uriel/configuration.nix ];
        };
        sohu = inputs.nixpkgs.lib.nixosSystem {
          system = "aarch64-linux";
          specialArgs = inputs;
          modules = [ ./machines/sohu/configuration.nix ];
        };
        tharmas = inputs.nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          specialArgs = inputs;
          modules = [ ./machines/tharmas/configuration.nix ];
        };
        edge = inputs.nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          specialArgs = inputs;
          modules = [ ./machines/edge/configuration.nix ];
        };
        thamiel = inputs.nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          specialArgs = inputs;
          modules = [ ./machines/thamiel/configuration.nix ];
        };
        ithaca = inputs.nixpkgs.lib.nixosSystem {
          system = "aarch64-linux";
          specialArgs = inputs;
          modules = [ ./machines/ithaca/configuration.nix ];
        };
        milky = inputs.nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          specialArgs = inputs;
          modules = [ ./machines/milky/configuration.nix ];
        };
        nathanda = inputs.nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          specialArgs = inputs;
          modules = [ ./machines/nathanda/configuration.nix ];
        };
      };
    };
}
"""


value : Node Expression
value =
    record
        [ ( [ "description" ]
          , string "Home Manager and NixOS configurations"
          )
        , ( [ "inputs" ], inputs )
        , ( [ "outputs" ], function "inputs" outputs )
        ]


inputs : Node Expression
inputs =
    record
        [ ( [ "nixpkgs", "url" ]
          , string "github:NixOS/nixpkgs/nixos-25.05"
          )
        , ( [ "nixos-hardware", "url" ]
          , string "github:NixOS/nixos-hardware/master"
          )
        , ( [ "systems", "url" ]
          , string "github:nix-systems/default"
          )
        , ( [ "secretdemoclub" ]
          , record
                [ ( [ "url" ]
                  , string "github:miniBill/secretdemoclub?dir=server"
                  )
                , ( [ "inputs", "nixpkgs", "follows" ]
                  , string "nixpkgs"
                  )
                , ( [ "inputs", "flake-utils", "follows" ]
                  , string "flake-utils"
                  )
                ]
          )
        , ( [ "pinned-unstable-papermc", "url" ]
          , string "github:NixOS/nixpkgs?rev=4cba8b53da471aea2ab2b0c1f30a81e7c451f4b6"
          )
        , ( [ "lix-module" ]
          , record
                [ ( [ "url" ]
                  , string "https://git.lix.systems/lix-project/nixos-module/archive/2.93.1.tar.gz"
                  )
                , ( [ "inputs", "nixpkgs", "follows" ]
                  , string "nixpkgs"
                  )
                , ( [ "inputs", "flake-utils", "follows" ]
                  , string "flake-utils"
                  )
                ]
          )
        , ( [ "musnix" ]
          , record
                [ ( [ "url" ]
                  , string "github:musnix/musnix"
                  )
                , ( [ "inputs", "nixpkgs", "follows" ]
                  , string "nixpkgs"
                  )
                ]
          )
        , ( [ "agenix" ]
          , record
                [ ( [ "url" ]
                  , string "github:ryantm/agenix"
                  )
                , ( [ "inputs", "nixpkgs", "follows" ]
                  , string "nixpkgs"
                  )
                , ( [ "inputs", "home-manager", "follows" ]
                  , string "home-manager"
                  )
                , ( [ "inputs", "systems", "follows" ]
                  , string "systems"
                  )
                ]
          )
        , ( [ "home-manager" ]
          , record
                [ ( [ "url" ]
                  , string "github:nix-community/home-manager/release-25.05"
                  )
                , ( [ "inputs", "nixpkgs", "follows" ]
                  , string "nixpkgs"
                  )
                ]
          )
        , ( [ "nix-index-database" ]
          , record
                [ ( [ "url" ]
                  , string "github:nix-community/nix-index-database"
                  )
                , ( [ "inputs", "nixpkgs", "follows" ]
                  , string "nixpkgs"
                  )
                ]
          )
        , ( [ "flake-utils" ]
          , record
                [ ( [ "url" ]
                  , string "github:numtide/flake-utils"
                  )
                , ( [ "inputs", "systems", "follows" ]
                  , string "systems"
                  )
                ]
          )
        ]


outputs : Node Expression
outputs =
    let
        allowedUnfree : Node Expression
        allowedUnfree =
            list
                [ string "code"
                , string "discord"
                , string "google-chrome"
                , string "minecraft-launcher"
                , string "slack"
                , string "spotify"
                , string "vscode"
                , string "zoom"
                ]

        pkgs : Node Expression
        pkgs =
            function "system"
                (apply (var "import")
                    [ dot
                        (var "inputs")
                        [ "nixpkgs" ]
                    , record
                        [ ( [ "system" ]
                          , var "system"
                          )
                        , ( [ "config" ]
                          , record
                                [ ( [ "allowUnfreePredicate" ]
                                  , function "pkg"
                                        (apply
                                            (dot
                                                (var "builtins")
                                                [ "elem" ]
                                            )
                                            [ parens
                                                (apply
                                                    (dot
                                                        (var "inputs")
                                                        [ "nixpkgs"
                                                        , "lib"
                                                        , "getName"
                                                        ]
                                                    )
                                                    [ var "pkg" ]
                                                )
                                            , var "allowedUnfree"
                                            ]
                                        )
                                  )
                                , ( [ "permittedInsecurePackages" ]
                                  , list
                                        [ string "zotero-6.0.26"
                                        ]
                                  )
                                ]
                          )
                        ]
                    ]
                )

        withConfig : Node Expression
        withConfig =
            Node.empty
                (FunctionExpr
                    (Node.empty
                        (RecordPattern
                            [ RecordFieldPattern
                                (Node.empty "system")
                                Nothing
                            , RecordFieldPattern
                                (Node.empty "username")
                                (Just (string "minibill"))
                            , RecordFieldPattern
                                (Node.empty "module")
                                Nothing
                            ]
                            { open = False
                            }
                        )
                    )
                    (apply
                        (dot
                            (var "inputs")
                            [ "home-manager"
                            , "lib"
                            , "homeManagerConfiguration"
                            ]
                        )
                        [ record
                            [ ( [ "pkgs" ]
                              , apply
                                    (var "pkgs")
                                    [ var "system"
                                    ]
                              )
                            , ( [ "modules" ]
                              , list
                                    [ var "module"
                                    ]
                              )
                            , ( [ "extraSpecialArgs" ]
                              , update
                                    (var "inputs")
                                    (record
                                        [ ( [ "username" ]
                                          , var "username"
                                          )
                                        ]
                                    )
                              )
                            ]
                        ]
                    )
                )
    in
    let_
        [ ( "allowedUnfree", allowedUnfree )
        , ( "pkgs", pkgs )
        , ( "withConfig", withConfig )
        ]
        (record
            [ ( [ "homeConfigurations" ]
              , Node.empty (RecordExpr homeConfigurations)
              )
            , ( [ "nixosConfigurations" ]
              , record nixosConfigurations
              )
            ]
        )


homeConfigurations : List (Node Attribute)
homeConfigurations =
    let
        conf :
            { a
                | username : Maybe String
                , arch : String
                , os : String
            }
            -> String
            -> Node Attribute
        conf c name =
            Node.empty
                (Attribute
                    (Node.empty
                        [ Node.empty
                            (StringName
                                [ StringLiteral
                                    (Maybe.withDefault "minibill" c.username ++ "@" ++ name)
                                ]
                            )
                        ]
                    )
                    (apply
                        (var "withConfig")
                        [ record
                            (List.filterMap identity
                                [ Just
                                    ( [ "system" ]
                                    , string (c.arch ++ "-" ++ c.os)
                                    )
                                , Maybe.map
                                    (\u -> ( [ "username" ], string u ))
                                    c.username
                                , Just
                                    ( [ "module" ]
                                    , Node.empty
                                        (PathExpr
                                            [ [ StringLiteral "." ]
                                            , [ StringLiteral "machines" ]
                                            , [ StringLiteral name ]
                                            , [ StringLiteral "home-manager.nix" ]
                                            ]
                                        )
                                    )
                                ]
                            )
                        ]
                    )
                )

        d : { arch : String, os : String, username : Maybe String }
        d =
            { arch = "x86_64"
            , os = "linux"
            , username = Nothing
            }
    in
    [ conf { d | arch = "aarch64", os = "darwin" } "gadiriel"
    , conf { d | arch = "aarch64" } "ithaca"
    , conf { d | arch = "aarch64" } "sohu"
    , conf d "thamiel"
    , conf d "tharmas"
    , conf d "edge"
    , conf { d | username = Just "francesca" } "edge"
    , conf d "milky"
    , conf d "uriel"
    , conf { d | username = Just "llibinim" } "uriel"
    , conf d "nathanda"
    ]


nixosConfigurations : List ( List String, Node Expression )
nixosConfigurations =
    let
        conf :
            String
            -> String
            -> ( List String, Node Expression )
        conf arch name =
            ( [ name
              ]
            , apply
                (dot (var "inputs")
                    [ "nixpkgs"
                    , "lib"
                    , "nixosSystem"
                    ]
                )
                [ record
                    [ ( [ "system" ]
                      , string (arch ++ "-linux")
                      )
                    , ( [ "specialArgs" ]
                      , var "inputs"
                      )
                    , ( [ "modules" ]
                      , list
                            [ Node.empty
                                (PathExpr
                                    [ [ StringLiteral "." ]
                                    , [ StringLiteral "machines" ]
                                    , [ StringLiteral name ]
                                    , [ StringLiteral "configuration.nix" ]
                                    ]
                                )
                            ]
                      )
                    ]
                ]
            )
    in
    [ conf "x86_64" "uriel"
    , conf "aarch64" "sohu"
    , conf "x86_64" "tharmas"
    , conf "x86_64" "edge"
    , conf "x86_64" "thamiel"
    , conf "aarch64" "ithaca"
    , conf "x86_64" "milky"
    , conf "x86_64" "nathanda"
    ]


suite : Test
suite =
    Test.test "Parsing flake.nix" <|
        \_ ->
            Utils.checkParser input value
