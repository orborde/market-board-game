module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as JD
import Json.Encode as JE
import List
import Maybe exposing (withDefault)
import Url
import Url.Builder
import Url.Parser as UP


main =
    Browser.application
        { init = init
        , onUrlChange = \_ -> Debug.todo "should never get called"
        , onUrlRequest = \_ -> Debug.todo "should never get called"
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias PlayerName =
    String


type alias SecurityType =
    String


type alias SecurityAmount =
    Int


type alias Assets =
    { monies : Int
    , securities : Dict.Dict SecurityType Int
    }


moniesField =
    "monies"


securitiesField =
    "securities"


encodeAssets : Assets -> JE.Value
encodeAssets assets =
    JE.object
        [ ( moniesField, JE.int assets.monies )
        , ( securitiesField, JE.dict identity JE.int assets.securities )
        ]


decodeAssets : JD.Decoder Assets
decodeAssets =
    JD.map2 Assets
        (JD.field moniesField JD.int)
        (JD.field securitiesField (JD.dict JD.int))


assetsGetSecurity : Assets -> SecurityType -> Int
assetsGetSecurity assets security =
    case Dict.get security assets.securities of
        Nothing ->
            0

        Just amount ->
            amount


canBuy : Assets -> SecurityType -> Market -> Bool
canBuy assets _ market =
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

        Just _ ->
            assetsGetSecurity assets security > 0


bookPrice =
    100


assetsBuyBook : Assets -> List SecurityType -> Maybe Assets
assetsBuyBook assets securities =
    if assets.monies >= bookPrice then
        Just
            { assets
                | monies = assets.monies - bookPrice
                , securities =
                    List.foldl
                        (\security newSecurities ->
                            Dict.update security (Maybe.withDefault 0 >> (+) 1 >> Just) newSecurities
                        )
                        assets.securities
                        securities
            }

    else
        Nothing


assetsSellBook : Assets -> List SecurityType -> Maybe Assets
assetsSellBook assets securities =
    let
        hasEveryAsset =
            List.all (\security -> assetsGetSecurity assets security > 0) securities
    in
    if hasEveryAsset then
        Just
            { assets
                | monies = assets.monies + bookPrice
                , securities =
                    List.foldl
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


initGameState players securities =
    { players = Dict.fromList <| List.map (\player -> ( player, { monies = 1000, securities = Dict.empty } )) players
    , markets = Dict.fromList <| List.map (\security -> ( security, defaultMarket )) securities
    , bankMonies = 0
    , clock = 0
    }


decodePlayersDict : JD.Decoder (Dict.Dict PlayerName Assets)
decodePlayersDict =
    JD.dict decodeAssets


decodeMarketsDict =
    JD.dict decodeMarket


playersField =
    "players"


marketsField =
    "markets"


bankMoniesField =
    "bankMonies"


clockField =
    "clock"


decodeGameState : JD.Decoder GameState
decodeGameState =
    JD.map4 GameState
        (JD.field playersField decodePlayersDict)
        (JD.field marketsField decodeMarketsDict)
        (JD.field bankMoniesField JD.int)
        (JD.field clockField JD.int)


encodeGameState : GameState -> JE.Value
encodeGameState state =
    JE.object
        [ ( playersField, JE.dict (\k -> k) encodeAssets state.players )
        , ( marketsField, JE.dict (\k -> k) encodeMarket state.markets )
        , ( bankMoniesField, JE.int state.bankMonies )
        , ( clockField, JE.int state.clock )
        ]


type alias PlayPageModel =
    { gameName : GameName
    , selected : PlayerName
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


openBidsField =
    "openBids"


openAsksField =
    "openAsks"


encodeMarket : Market -> JE.Value
encodeMarket market =
    JE.object
        [ ( openBidsField, JE.list JE.int market.openBids )
        , ( openAsksField, JE.list JE.int market.openAsks )
        ]


decodeMarket =
    JD.map2 Market
        (JD.field openBidsField (JD.list JD.int))
        (JD.field openAsksField (JD.list JD.int))


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


type alias CreatePageModel =
    { gameName : GameName
    , players : List PlayerName
    , securities : List SecurityType
    }


type AppState
    = LoadAppState GameName
    | CreateAppState CreatePageModel
    | PlayAppState PlayPageModel


type alias Model =
    { appState : AppState
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url _ =
    let
        routeWeAreAt =
            Maybe.withDefault UnknownRoute (UP.parse route url)
    in
    case routeWeAreAt of
        UnknownRoute ->
            Debug.todo "rekt"

        GameRoute gameName ->
            ( { appState = LoadAppState gameName
              }
            , getGameState gameName
            )


initLoadAppState : GameName -> ( AppState, Cmd Msg )
initLoadAppState gameName =
    ( LoadAppState gameName
    , pollGameState gameName Nothing
    )


initCreateAppState : GameName -> ( AppState, Cmd Msg )
initCreateAppState gameName =
    ( CreateAppState
        { gameName = gameName
        , players = []
        , securities = []
        }
    , pollGameState gameName Nothing
    )



-- UPDATE


type GameStateMsg
    = Buy SecurityType
    | Sell SecurityType
    | BuyBook
    | SellBook


type CreateMsgType
    = StartGameMsg
    | SetPlayerListMsg String
    | SetSecuritiesListMsg String


type PlayMsgType
    = OrderMsg GameStateMsg
    | SwitchTo PlayerName


type Msg
    = PlayMsg PlayMsgType
    | CreateMsg CreateMsgType
    | LoadMsg
    | GotUpdate (Result Http.Error GameState)
    | FinishedSend (Result Http.Error ())


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
            case assetsBuyBook selectedPlayerAssets (Dict.keys gameState.markets) of
                Nothing ->
                    Debug.log "can't buy book" gameState

                Just newAssets ->
                    { gameState
                        | players = Dict.insert selectedPlayer newAssets gameState.players
                        , bankMonies = gameState.bankMonies + bookPrice
                    }

        SellBook ->
            case assetsSellBook selectedPlayerAssets (Dict.keys gameState.markets) of
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


currentStateField =
    "current_state"


expectGameState =
    Http.expectJson GotUpdate (JD.field currentStateField decodeGameState)


type alias GameName =
    String


type alias GamePart =
    String


gamePartUrl : GameName -> List GamePart -> String
gamePartUrl name path =
    Url.Builder.relative (List.concat [ [ name ], path ]) []


getGameState : GameName -> Cmd Msg
getGameState gameName =
    Http.get
        { url = gamePartUrl gameName [ "state" ]
        , expect = expectGameState
        }


pollGameState : GameName -> Maybe GameState -> Cmd Msg
pollGameState gameName oldState =
    let
        oldJson =
            case oldState of
                Just gs ->
                    encodeGameState gs

                Nothing ->
                    JE.null
    in
    Http.post
        { url = gamePartUrl gameName [ "poll" ]
        , expect = expectGameState
        , body =
            Http.jsonBody
                (JE.object
                    [ ( currentStateField, oldJson )
                    ]
                )
        }


postGameState : GameName -> Maybe GameState -> GameState -> Cmd Msg
postGameState gameName old new =
    let
        oldJson =
            case old of
                Just gs ->
                    encodeGameState gs

                Nothing ->
                    JE.null
    in
    Http.post
        { url = gamePartUrl gameName [ "state" ]
        , expect = Http.expectWhatever FinishedSend
        , body =
            Http.jsonBody
                (JE.object
                    [ ( "old", oldJson )
                    , ( "new", encodeGameState new )
                    ]
                )
        }


updateCreate : CreateMsgType -> CreatePageModel -> ( AppState, Cmd Msg )
updateCreate msg model =
    case msg of
        StartGameMsg ->
            -- Assume that we can only get this if the arguments are valid.
            let
                ( newAppState, pollCmd ) =
                    initLoadAppState model.gameName
            in
            ( newAppState
            , Cmd.batch
                [ postGameState model.gameName Nothing (initGameState model.players model.securities)
                , pollCmd
                ]
            )

        SetPlayerListMsg playersList ->
            ( CreateAppState
                { model
                    | players = String.split "," playersList
                }
            , Cmd.none
            )

        SetSecuritiesListMsg securitiesList ->
            ( CreateAppState
                { model
                    | securities = String.split "," securitiesList
                }
            , Cmd.none
            )


updatePlay : PlayMsgType -> PlayPageModel -> ( PlayPageModel, Cmd Msg )
updatePlay msg model =
    case msg of
        OrderMsg gsMsg ->
            let
                newGameState =
                    updateGameState model.gameState model.selected gsMsg
            in
            ( { model
                | gameState = newGameState
              }
            , postGameState model.gameName (Just model.gameState) newGameState
            )

        SwitchTo player ->
            -- Don't perturb the HTTP request state, since a long poll should already be running.
            ( { model
                | selected = player
              }
            , Cmd.none
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.appState, msg ) of
        -- Locally originated events
        ( PlayAppState playModel, PlayMsg playMsg ) ->
            let
                ( newModel, cmd ) =
                    updatePlay playMsg playModel
            in
            ( { model
                | appState = PlayAppState newModel
              }
            , cmd
            )

        -- TODO: do we even need this? There's no functionality attached; we're really just appeasing the type gods.
        ( LoadAppState _, LoadMsg ) ->
            ( model, Cmd.none )

        ( CreateAppState createModel, CreateMsg createMsg ) ->
            let
                ( newAppState, cmd ) =
                    updateCreate createMsg createModel
            in
            ( { model
                | appState = newAppState
              }
            , cmd
            )

        -- Network events
        ( CreateAppState createModel, GotUpdate (Ok newGameState) ) ->
            let
                _ =
                    Debug.log ("got update: " ++ Debug.toString s)
            in
            ( { model
                | appState =
                    PlayAppState
                        { gameName = createModel.gameName
                        , gameState = newGameState
                        , selected = must (List.head (Dict.keys newGameState.players))
                        }
              }
            , pollGameState createModel.gameName (Just newGameState)
            )

        ( CreateAppState createModel, GotUpdate (Err error) ) ->
            let
                _ =
                    Debug.log ("failed to poll. I hope your phone has lots of battery! : " ++ Debug.toString error) createModel
            in
            ( model, pollGameState createModel.gameName Nothing )

        ( PlayAppState playModel, GotUpdate (Ok newGameState) ) ->
            let
                mdl =
                    Debug.log ("got update: " ++ Debug.toString s) playModel
            in
            ( { model
                | appState =
                    PlayAppState
                        { mdl
                            | gameState = newGameState
                        }
              }
            , pollGameState playModel.gameName (Just newGameState)
            )

        ( PlayAppState playModel, GotUpdate (Err error) ) ->
            let
                _ =
                    Debug.log ("failed to poll. I hope your phone has lots of battery! : " ++ Debug.toString error) playModel
            in
            ( model, pollGameState playModel.gameName (Just playModel.gameState) )

        ( LoadAppState gameName, GotUpdate (Err error) ) ->
            let
                _ =
                    Debug.log ("failed to load, trying to create: " ++ Debug.toString error) model

                ( newAppState, cmd ) =
                    initCreateAppState gameName
            in
            ( { model
                | appState = newAppState
              }
            , cmd
            )

        ( LoadAppState gameName, GotUpdate (Ok newGameState) ) ->
            let
                _ =
                    Debug.log ("got update: " ++ Debug.toString s)
            in
            ( { model
                | appState =
                    PlayAppState
                        { gameName = gameName
                        , gameState = newGameState
                        , selected = must (List.head (Dict.keys newGameState.players))
                        }
              }
            , pollGameState gameName (Just newGameState)
            )

        ( _, FinishedSend (Ok _) ) ->
            Debug.log "successfully sent to server" ( model, Cmd.none )

        ( _, FinishedSend (Err error) ) ->
            Debug.log ("send error: " ++ Debug.toString error) ( model, Cmd.none )

        -- Weird invalid stuff
        ( LoadAppState _, PlayMsg _ ) ->
            Debug.todo "invalid message for state?!"

        ( LoadAppState _, CreateMsg _ ) ->
            Debug.todo "invalid message for state?!"

        ( CreateAppState _, PlayMsg _ ) ->
            Debug.todo "invalid message for state?!"

        ( CreateAppState _, LoadMsg ) ->
            Debug.todo "invalid message for state?!"

        ( PlayAppState _, CreateMsg _ ) ->
            Debug.todo "invalid message for state?!"

        ( PlayAppState _, LoadMsg ) ->
            Debug.todo "invalid message for state?!"



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


viewMarket : SecurityType -> Market -> Assets -> Html PlayMsgType
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


viewGameState : GameState -> PlayerName -> Html PlayMsgType
viewGameState gameState selectedPlayer =
    let
        selectedPlayerAssets =
            must (Dict.get selectedPlayer gameState.players)
    in
    div []
        [ p [] <| List.map (\player -> button [ onClick (SwitchTo player) ] [ text player ]) (Dict.keys gameState.players)
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


viewLoad : String -> Html Msg
viewLoad gameName =
    h1 [] [ text ("LOADING" ++ gameName) ]


viewPlay : PlayPageModel -> Html PlayMsgType
viewPlay model =
    viewGameState model.gameState model.selected


diagnoseCreatePageModel : CreatePageModel -> Maybe String
diagnoseCreatePageModel model =
    if List.length model.players < 2 then
        Just "Need at least 2 players"

    else if List.length model.securities < 2 then
        Just "Need at least 2 securities"

    else
        Nothing


createPageCreateButton : CreatePageModel -> Html Msg
createPageCreateButton model =
    case diagnoseCreatePageModel model of
        Nothing ->
            button [ onClick (CreateMsg StartGameMsg) ] [ text "Start game" ]

        Just problem ->
            button [ disabled True ] [ text problem ]


viewCreate : CreatePageModel -> Html Msg
viewCreate model =
    div []
        [ h1 []
            [ text ("Creating " ++ model.gameName) ]
        , text "Players (comma-separated)"
        , input
            [ placeholder "Alice,Bob,Spencer"
            , value (String.join "," model.players)
            , onInput (SetPlayerListMsg >> CreateMsg)
            ]
            []
        , text "Securities (comma-separated)"
        , input
            [ placeholder "AMZN,MSFT,GOOG"
            , value (String.join "," model.securities)
            , onInput (SetSecuritiesListMsg >> CreateMsg)
            ]
            []
        , createPageCreateButton model
        ]


type Route
    = UnknownRoute
    | GameRoute GameName



-- disordered-bits.club/myCoolGame <- user loads this
-- disordered-bits.club/myCoolGame/state <- thing getGameState hits?
-- disordered-bits.club/myCoolGame/poll  <- polling endpoint


route : UP.Parser (Route -> a) a
route =
    UP.oneOf
        [ UP.map UnknownRoute UP.top
        , UP.map GameRoute UP.string
        ]


view : Model -> Browser.Document Msg
view model =
    let
        html =
            case model.appState of
                LoadAppState gameName ->
                    viewLoad gameName

                CreateAppState createModel ->
                    viewCreate createModel

                PlayAppState playModel ->
                    Html.map PlayMsg (viewPlay playModel)
    in
    { title = "Market Game"
    , body = [ html ]
    }
