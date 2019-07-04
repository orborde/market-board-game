module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Dict
import Cons
import Maybe exposing (withDefault)

main =
  Browser.sandbox { init = init, update = update, view = view }


-- MODEL

type alias PlayerName = String
type alias SecurityType = String
type alias SecurityAmount = Int

allPlayers : Cons.Cons PlayerName
allPlayers = Cons.appendList (Cons.singleton "Speez") ["Yam"]
         
securities : Cons.Cons SecurityType
securities = Cons.appendList (Cons.singleton "AMZN") ["GOOG", "F"]

type alias Assets = {
        monies : Int,
        securities : Dict.Dict SecurityType Int
    }
             
type alias Model = {
        selected : PlayerName,
        players : Dict.Dict PlayerName Assets
    }

init : Model
init =
    {
        selected = Cons.head allPlayers,
        players = Dict.fromList <| Cons.toList <| Cons.map (\player -> (player, { monies = 100, securities = Dict.empty })) allPlayers
    }

-- UPDATE

type Msg = Buy SecurityType | Sell SecurityType

updateAsset : Model -> SecurityType -> (Int -> Int) -> Model
updateAsset model security f =
    { model |
          players = Dict.update (Debug.log (Debug.toString model.selected) model.selected)
          (Maybe.map (\assets ->
                          {assets |
                               securities = Dict.update security (Maybe.withDefault 0 >> f >> Just) assets.securities}))
          model.players
    }
                       
update : Msg -> Model -> Model
update msg model =
    let selectedPlayer = must (Dict.get model.selected model.players)
    in
        case (Debug.log "msg is" msg) of
            Buy security ->
                updateAsset model security (\count -> count+1)
          
            Sell security ->
                updateAsset model security (\count -> count-1)

-- VIEW

must : Maybe a -> a
must x = case x of
             Just result -> result
             Nothing -> Debug.todo "Assertion failed"

view : Model -> Html Msg
view model =
    let selectedPlayer = must (Dict.get model.selected model.players)

    in
        div []
            [ p [] [ text model.selected ]
            , p [] [ text ("Moneys: " ++ Debug.toString
                         selectedPlayer.monies) ]
                  
            , table []
                (Cons.toList (Cons.map (\ security ->
                           tr []
                               [ td [] [text security]
                               , td [] [button [ onClick <| Sell security ] [ text "-" ]]
                               , td [] [text <| String.fromInt (Maybe.withDefault 0 (Dict.get security selectedPlayer.securities))]
                               , td [] [button [ onClick <| Buy security ] [ text "+" ]]
                               ]
                          )
                     securities))
            ]
                          
