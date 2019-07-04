module Main exposing (main)

import Browser
import Cons
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List
import Maybe exposing (withDefault)


main =
    Browser.sandbox { init = init, update = update, view = view }



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


type alias Model =
    { selected : PlayerName
    , players : Dict.Dict PlayerName Assets
    , markets : Dict.Dict SecurityType Market
    , bankMonies : Int
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


init : Model
init =
    { selected = Cons.head allPlayers
    , players = Dict.fromList <| Cons.toList <| Cons.map (\player -> ( player, { monies = 1000, securities = Dict.empty } )) allPlayers
    , markets = Dict.fromList <| Cons.toList <| Cons.map (\security -> ( security, defaultMarket )) securities
    , bankMonies = 0
    }



-- UPDATE


type Msg
    = Buy SecurityType
    | Sell SecurityType
    | BuyBook
    | SellBook
    | SwitchTo PlayerName


updateAsset : Model -> Int -> SecurityType -> Int -> Model
updateAsset model deltaMonies security deltaCount =
    { model
        | players =
            Dict.update (Debug.log (Debug.toString model.selected) model.selected)
                (Maybe.map
                    (\assets ->
                        { assets
                            | securities = Dict.update security (Maybe.withDefault 0 >> (+) deltaCount >> Just) assets.securities
                            , monies = assets.monies + deltaMonies
                        }
                    )
                )
                model.players
    }


update : Msg -> Model -> Model
update msg model =
    let
        selectedPlayerAssets =
            must (Dict.get model.selected model.players)
    in
    case Debug.log "msg is" msg of
        Buy security ->
            let
                market =
                    must (Dict.get security model.markets)
            in
            case lowestAsk market of
                Nothing ->
                    model

                Just ask ->
                    if canBuy selectedPlayerAssets security market then
                        let
                            updatedModel =
                                updateAsset model -ask security 1
                        in
                        { updatedModel
                            | markets = Dict.update security (must >> marketBuy >> must >> Just) updatedModel.markets
                            , bankMonies = model.bankMonies + ask
                        }

                    else
                        model

        Sell security ->
            let
                market =
                    must (Dict.get security model.markets)
            in
            case highestBid market of
                Nothing ->
                    model

                Just bid ->
                    if canSell selectedPlayerAssets security market then
                        let
                            updatedModel =
                                updateAsset model bid security -1
                        in
                        { updatedModel
                            | markets = Dict.update security (must >> marketSell >> must >> Just) updatedModel.markets
                            , bankMonies = model.bankMonies - bid
                        }

                    else
                        model

        BuyBook ->
            case assetsBuyBook selectedPlayerAssets of
                Nothing ->
                    Debug.log "can't buy book" model

                Just newAssets ->
                    { model
                        | players = Dict.insert model.selected newAssets model.players
                        , bankMonies = model.bankMonies + bookPrice
                    }

        SellBook ->
            case assetsSellBook selectedPlayerAssets of
                Nothing ->
                    Debug.log "can't sell book" model

                Just newAssets ->
                    { model
                        | players = Dict.insert model.selected newAssets model.players
                        , bankMonies = model.bankMonies - bookPrice
                    }

        SwitchTo player ->
            { model
                | selected = player
            }



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
                                , td [ width halfCellWidth ] [ button [ onClick (Sell security), disabled (not (canSell assets security market)) ] [ text ("SELL " ++ String.fromInt topBid) ] ]
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
                                [ td [ width halfCellWidth ] [ button [ onClick (Buy security), disabled (not (canBuy assets security market)) ] [ text ("BUY " ++ String.fromInt bottomAsk) ] ]
                                , td [ width halfCellWidth ] [ text "x" ]
                                ]
                            ]
                        ]
                    ]
            , []
            , List.map (\ask -> td [ width cellWidth ] [ text (String.fromInt ask) ]) (List.drop 1 market.openAsks)
            ]
        )


view : Model -> Html Msg
view model =
    let
        selectedPlayerAssets =
            must (Dict.get model.selected model.players)
    in
    div []
        [ p [] <| Cons.toList <| Cons.map (\player -> button [ onClick (SwitchTo player) ] [ text player ]) allPlayers
        , p [] [ text model.selected ]
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
                    ++ Debug.toString model.bankMonies
                )
            ]
        , p []
            [ button [ onClick BuyBook ] [ text "BUY BOOK" ]
            , button [ onClick SellBook ] [ text "SELL BOOK" ]
            ]
        , table [ style "border" "1px solid black" ]
            (List.map (\( security, market ) -> viewMarket security market selectedPlayerAssets) (Dict.toList model.markets))
        ]
