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
    Cons.appendList (Cons.singleton "AMZN") [ "GOOG", "F" ]


type alias Assets =
    { monies : Int
    , securities : Dict.Dict SecurityType Int
    }


type alias Model =
    { selected : PlayerName
    , players : Dict.Dict PlayerName Assets
    , markets : Dict.Dict SecurityType Market
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
    , players = Dict.fromList <| Cons.toList <| Cons.map (\player -> ( player, { monies = 100, securities = Dict.empty } )) allPlayers
    , markets = Dict.fromList <| Cons.toList <| Cons.map (\security -> ( security, defaultMarket )) securities
    }



-- UPDATE


type Msg
    = Buy SecurityType
    | Sell SecurityType


updateAsset : Model -> SecurityType -> (Int -> Int) -> Model
updateAsset model security f =
    { model
        | players =
            Dict.update (Debug.log (Debug.toString model.selected) model.selected)
                (Maybe.map
                    (\assets ->
                        { assets
                            | securities = Dict.update security (Maybe.withDefault 0 >> f >> Just) assets.securities
                        }
                    )
                )
                model.players
    }


update : Msg -> Model -> Model
update msg model =
    let
        selectedPlayer =
            must (Dict.get model.selected model.players)
    in
    case Debug.log "msg is" msg of
        Buy security ->
            updateAsset model security (\count -> count + 1)

        Sell security ->
            updateAsset model security (\count -> count - 1)



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


viewMarket : SecurityType -> Market -> Html Msg
viewMarket security market =
    tr []
        (List.concat
            [ [ text security ]
            , List.map (\bid -> td [ width cellWidth ] [ text (String.fromInt bid) ]) (List.reverse (List.drop 1 market.openBids))
            , case List.head market.openBids of
                Nothing ->
                    []

                Just topBid ->
                    [ td [ width halfCellWidth ] [ text "x" ]
                    , td [ width halfCellWidth ] [ button [ onClick <| Sell security ] [ text ("SELL " ++ String.fromInt topBid) ] ]
                    ]
            , case List.head market.openAsks of
                Nothing ->
                    []

                Just bottomAsk ->
                    [ td [ width halfCellWidth ] [ button [ onClick <| Buy security ] [ text ("BUY " ++ String.fromInt bottomAsk) ] ]
                    , td [ width halfCellWidth ] [ text "x" ]
                    ]
            , []
            , List.map (\ask -> td [ width cellWidth ] [ text (String.fromInt ask) ]) (List.drop 1 market.openAsks)
            ]
        )


view : Model -> Html Msg
view model =
    let
        selectedPlayer =
            must (Dict.get model.selected model.players)
    in
    div []
        [ p [] [ text model.selected ]
        , p []
            [ text
                ("Moneys: "
                    ++ Debug.toString
                        selectedPlayer.monies
                )
            ]
        , table []
            (List.map (\( security, market ) -> viewMarket security market) (Dict.toList model.markets))
        ]
