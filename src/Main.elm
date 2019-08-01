module Main exposing (main)

import Browser
import Cons
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as JD
import Json.Encode as JE
import List
import Maybe exposing (withDefault)


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \m -> Sub.none
        }



-- MODEL


type alias PlayerName =
    String


type alias SecurityType =
    String


type alias SecurityAmount =
    Int


allPlayers : Cons.Cons PlayerName
allPlayers =
    Cons.appendList (Cons.singleton "Speez") [ "Yam" ]


securities : Cons.Cons SecurityType
securities =
    Cons.appendList (Cons.singleton "Shit's rekt") [ "GOOG", "F" ]


type alias Assets =
    { monies : Int
    , securities : Dict.Dict SecurityType Int
    }


decodeSecurities : JD.Decoder (Dict.Dict SecurityType Int)
decodeSecurities =
    JD.dict JD.int


decodeAssets : JD.Decoder Assets
decodeAssets =
    JD.map2 Assets
        (JD.field "monies" JD.int)
        (JD.field "securities" decodeSecurities)


assetsGetSecurity : Assets -> SecurityType -> Int
assetsGetSecurity assets security =
    case Dict.get security assets.securities of
        Nothing ->
            0

        Just amount ->
            amount


canBuy : Assets -> SecurityType -> Market -> Bool
canBuy assets security market =
    case lowestAsk market of
        Nothing ->
            False

        Just ask ->
            assets.monies >= ask


canSell : Assets -> SecurityType -> Market -> Bool
canSell assets security market =
    case highestBid market of
        Nothing ->
            False

        Just bid ->
            assetsGetSecurity assets security > 0


bookPrice =
    100


assetsBuyBook : Assets -> Maybe Assets
assetsBuyBook assets =
    if assets.monies >= bookPrice then
        Just
            { assets
                | monies = assets.monies - bookPrice
                , securities =
                    Cons.foldl
                        (\security newSecurities ->
                            Dict.update security (Maybe.withDefault 0 >> (+) 1 >> Just) newSecurities
                        )
                        assets.securities
                        securities
            }

    else
        Nothing


assetsSellBook : Assets -> Maybe Assets
assetsSellBook assets =
    let
        hasEveryAsset =
            Cons.all (\security -> assetsGetSecurity assets security > 0) securities
    in
    if hasEveryAsset then
        Just
            { assets
                | monies = assets.monies + bookPrice
                , securities =
                    Cons.foldl
                        (\security newSecurities ->
                            Dict.insert security (assetsGetSecurity assets security - 1) newSecurities
                        )
                        assets.securities
                        securities
            }

    else
        Nothing


type alias GameState =
    { players : Dict.Dict PlayerName Assets
    , markets : Dict.Dict SecurityType Market
    , bankMonies : Int
    , clock : Int
    }


initGameState =
    { players = Dict.fromList <| Cons.toList <| Cons.map (\player -> ( player, { monies = 1000, securities = Dict.empty } )) allPlayers
    , markets = Dict.fromList <| Cons.toList <| Cons.map (\security -> ( security, defaultMarket )) securities
    , bankMonies = 0
    , clock = 0
    }


decodePlayersDict : JD.Decoder (Dict.Dict PlayerName Assets)
decodePlayersDict =
    JD.dict decodeAssets


decodeMarketsDict =
    JD.dict decodeMarket


decodeGameState : JD.Decoder GameState
decodeGameState =
    JD.map4 GameState
        (JD.field "players" decodePlayersDict)
        (JD.field "markets" decodeMarketsDict)
        (JD.field "bankMonies" JD.int)
        (JD.field "clock" JD.int)


type alias Model =
    { selected : PlayerName
    , gameState : GameState
    }


marketMakerPrices : List Int
marketMakerPrices =
    [ 10, 20, 30, 40, 50, 60, 70, 80, 90 ]


type alias Market =
    { -- Descending order
      openBids : List Int
    , -- Ascending order
      openAsks : List Int
    }


decodeMarket =
    JD.map2 Market
        (JD.field "openBids" (JD.list JD.int))
        (JD.field "openAsks" (JD.list JD.int))


lowestAsk : Market -> Maybe Int
lowestAsk market =
    List.head market.openAsks


highestBid : Market -> Maybe Int
highestBid market =
    List.head market.openBids


marketBuy : Market -> Maybe Market
marketBuy market =
    Maybe.map
        (\ask ->
            { market
                | openAsks = List.drop 1 market.openAsks
                , openBids = ask :: market.openBids
            }
        )
        (lowestAsk market)


marketSell : Market -> Maybe Market
marketSell market =
    Maybe.map
        (\bid ->
            { market
                | openBids = List.drop 1 market.openBids
                , openAsks = bid :: market.openAsks
            }
        )
        (highestBid market)


defaultMarket : Market
defaultMarket =
    let
        startCut =
            List.length marketMakerPrices // 2
    in
    { openBids = List.reverse (List.take startCut marketMakerPrices)
    , openAsks = List.drop startCut marketMakerPrices
    }


init : () -> ( Model, Cmd Msg )
init whatever =
    ( { selected = Cons.head allPlayers
      , gameState = initGameState
      }
    , getGameState
    )



-- UPDATE


type GameStateMsg
    = Buy SecurityType
    | Sell SecurityType
    | BuyBook
    | SellBook


type Msg
    = OrderMsg GameStateMsg
    | SwitchTo PlayerName
    | GotUpdate (Result Http.Error GameState)



--postUpdate : (Result Http.Error a -> String) -> a -> a -> Cmd Msg
--postUpdate msg old new =
--    Http.post
--        { url = "/states/omg"
--        , body = Http.jsonBody <| Json.Encode.object [ ( "old", Json.Encode.object old ), ( "new", Json.Encode.object new ) ]
--
--        --        , expect = Http.expectJson msg (Json.Decode.field "current_state" decodeGameState)
--        , expect = Http.expectString
--        }


updateAsset : GameState -> PlayerName -> Int -> SecurityType -> Int -> GameState
updateAsset gameState selectedPlayer deltaMonies security deltaCount =
    { gameState
        | players =
            Dict.update (Debug.log (Debug.toString selectedPlayer) selectedPlayer)
                (Maybe.map
                    (\assets ->
                        { assets
                            | securities = Dict.update security (Maybe.withDefault 0 >> (+) deltaCount >> Just) assets.securities
                            , monies = assets.monies + deltaMonies
                        }
                    )
                )
                gameState.players
    }


updateGameStateInternal : GameState -> PlayerName -> GameStateMsg -> GameState
updateGameStateInternal gameState selectedPlayer msg =
    let
        selectedPlayerAssets =
            must (Dict.get selectedPlayer gameState.players)
    in
    case Debug.log "msg is" msg of
        Buy security ->
            let
                market =
                    must (Dict.get security gameState.markets)
            in
            case lowestAsk market of
                Nothing ->
                    gameState

                Just ask ->
                    if canBuy selectedPlayerAssets security market then
                        let
                            updatedGameState =
                                updateAsset gameState selectedPlayer -ask security 1
                        in
                        { updatedGameState
                            | markets = Dict.update security (must >> marketBuy >> must >> Just) updatedGameState.markets
                            , bankMonies = gameState.bankMonies + ask
                        }

                    else
                        gameState

        Sell security ->
            let
                market =
                    must (Dict.get security gameState.markets)
            in
            case highestBid market of
                Nothing ->
                    gameState

                Just bid ->
                    if canSell selectedPlayerAssets security market then
                        let
                            updatedGameState =
                                updateAsset gameState selectedPlayer bid security -1
                        in
                        { updatedGameState
                            | markets = Dict.update security (must >> marketSell >> must >> Just) updatedGameState.markets
                            , bankMonies = gameState.bankMonies - bid
                        }

                    else
                        gameState

        BuyBook ->
            case assetsBuyBook selectedPlayerAssets of
                Nothing ->
                    Debug.log "can't buy book" gameState

                Just newAssets ->
                    { gameState
                        | players = Dict.insert selectedPlayer newAssets gameState.players
                        , bankMonies = gameState.bankMonies + bookPrice
                    }

        SellBook ->
            case assetsSellBook selectedPlayerAssets of
                Nothing ->
                    Debug.log "can't sell book" gameState

                Just newAssets ->
                    { gameState
                        | players = Dict.insert selectedPlayer newAssets gameState.players
                        , bankMonies = gameState.bankMonies - bookPrice
                    }


updateGameState : GameState -> PlayerName -> GameStateMsg -> GameState
updateGameState gameState selected msg =
    let
        newState =
            updateGameStateInternal gameState selected msg
    in
    { newState
        | clock = newState.clock + 1
    }


getGameState : Cmd Msg
getGameState =
    Http.get
        -- TODO
        { url = ""
        , expect = Http.expectJson GotUpdate decodeGameState
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OrderMsg gsMsg ->
            -- TODO
            -- update gamestate
            -- start http post
            ( { model
                | gameState = updateGameState model.gameState model.selected gsMsg
              }
            , Cmd.none
            )

        SwitchTo player ->
            -- TODO
            -- Don't perturb the HTTP request state, since a long poll should already be running.
            ( { model
                | selected = player
              }
            , Cmd.none
            )

        GotUpdate (Ok newGameState) ->
            let
                mdl =
                    Debug.log ("got update: " ++ Debug.toString s) model
            in
            ( { mdl
                | gameState = newGameState
              }
            , getGameState
            )

        GotUpdate (Err error) ->
            -- TODO: don't busyloop
            Debug.log ("error: " ++ Debug.toString error) ( model, getGameState )



-- VIEW


must : Maybe a -> a
must x =
    case x of
        Just result ->
            result

        Nothing ->
            Debug.todo "Assertion failed"


cellWidth =
    100


halfCellWidth =
    cellWidth // 2


viewMarket : SecurityType -> Market -> Assets -> Html Msg
viewMarket security market assets =
    tr []
        (List.concat
            [ [ text <| security ++ " " ++ String.fromInt (assetsGetSecurity assets security) ]
            , List.map (\bid -> td [ width cellWidth ] [ text (String.fromInt bid) ]) (List.reverse (List.drop 1 market.openBids))
            , case List.head market.openBids of
                Nothing ->
                    []

                Just topBid ->
                    [ td [ width cellWidth ]
                        [ table [ style "border" "1px solid black" ]
                            [ tr []
                                [ td [ width halfCellWidth ] [ text "x" ]
                                , td [ width halfCellWidth ] [ button [ onClick <| OrderMsg (Sell security), disabled (not (canSell assets security market)) ] [ text ("SELL " ++ String.fromInt topBid) ] ]
                                ]
                            ]
                        ]
                    ]
            , case List.head market.openAsks of
                Nothing ->
                    []

                Just bottomAsk ->
                    [ td [ width cellWidth ]
                        [ table [ style "border" "1px solid black" ]
                            [ tr []
                                [ td [ width halfCellWidth ] [ button [ onClick <| OrderMsg (Buy security), disabled (not (canBuy assets security market)) ] [ text ("BUY " ++ String.fromInt bottomAsk) ] ]
                                , td [ width halfCellWidth ] [ text "x" ]
                                ]
                            ]
                        ]
                    ]
            , []
            , List.map (\ask -> td [ width cellWidth ] [ text (String.fromInt ask) ]) (List.drop 1 market.openAsks)
            ]
        )


viewGameState : GameState -> PlayerName -> Html Msg
viewGameState gameState selectedPlayer =
    let
        selectedPlayerAssets =
            must (Dict.get selectedPlayer gameState.players)
    in
    div []
        [ p [] <| Cons.toList <| Cons.map (\player -> button [ onClick (SwitchTo player) ] [ text player ]) allPlayers
        , p [] [ text (selectedPlayer ++ " / Turn: " ++ String.fromInt gameState.clock) ]
        , p []
            [ text
                ("Moneys: "
                    ++ Debug.toString
                        selectedPlayerAssets.monies
                )
            ]
        , p []
            [ text
                ("BankMonies: "
                    ++ Debug.toString gameState.bankMonies
                )
            ]
        , p []
            [ button [ onClick <| OrderMsg BuyBook ] [ text "BUY BOOK" ]
            , button [ onClick <| OrderMsg SellBook ] [ text "SELL BOOK" ]
            ]
        , table [ style "border" "1px solid black" ]
            (List.map (\( security, market ) -> viewMarket security market selectedPlayerAssets) (Dict.toList gameState.markets))
        ]


view : Model -> Html Msg
view model =
    viewGameState model.gameState model.selected
