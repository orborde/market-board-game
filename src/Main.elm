module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Dict
import Maybe exposing (withDefault)

main =
  Browser.sandbox { init = init, update = update, view = view }


-- MODEL

type alias PlayerName = String
type alias SecurityType = String
type alias SecurityAmount = Int

players : List PlayerName
players = ["Speez", "Yam"]
         
securities : List SecurityType
securities = ["AMZN", "GOOG", "F"]

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
        monies = 100,
        assets = Dict.empty
    }

-- UPDATE

type Msg = Buy SecurityType | Sell SecurityType

update : Msg -> Model -> Model
update msg model =
  case msg of
      Buy security ->
          { model |
                assets = Dict.update security (\count -> Just <| (Maybe.withDefault 0 count) + 1) model.assets
          }
          
      Sell security ->
          { model |
                assets = Dict.update security (\count -> Just <| (Maybe.withDefault 0 count) - 1) model.assets
          }

-- VIEW

view : Model -> Html Msg
view model =
  div []
      [ p []
            [ text ("Moneys: " ++ Debug.toString model.monies) ]
            
      , table []
          (List.map (\ security ->
                          tr []
                          [ td [] [text security]
                          , td [] [button [ onClick <| Sell security ] [ text "-" ]]
                          , td [] [text <| String.fromInt (Maybe.withDefault 0 (Dict.get security model.assets))]
                          , td [] [button [ onClick <| Buy security ] [ text "+" ]]
                          ]
                     )
                securities)
      ]
                          
