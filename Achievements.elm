module Achievements exposing ( .. )

import Html exposing             ( Html, div, input, text, button )
import Html.Attributes exposing  ( placeholder )
import Html.Events exposing      ( onInput, onClick )
import Http exposing             ( Error, send, get, Request, emptyBody )
import Json.Decode exposing      ( at, int, field, list, map2 )

main =
  Html.program
    { init = initialModel
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }

type alias Key = String
type alias User = String
type alias Id = Int
type alias Minutes = Int

type alias Model = 
    {
        key : Key,
        user : User,
        games : List Game,
        numberOfGames : Int,
        listOfGames : List (Id, Minutes),
        errorMsg : String
    }

type alias Game = 
    {
        name : String,
        timePlayed : Int,
        achievements : List Achievement
    }

type Achievement = Achievement { label : String, unlocked : Bool }

type Msg = User User         -- the user whose achievement stats are desired 
         | Key Key           -- the key to used for all queries
         | Fetch             -- the command to actually fetch the data from the Steam servers
         | Games (List Game) -- update the game list
         | FetchCollection (Result Http.Error (Int, List (Id, Minutes)))

initialModel : (Model, Cmd Msg)
initialModel = (Model "" "" [] 0 [] "", Cmd.none)

view : Model -> Html Msg
view model = 
  div [] 
      [
    div [] 
        [
          input [ placeholder "Steam API Key", onInput Key ] []
        ],
    div []
        [
          input [ placeholder "User ID", onInput User] []
        ],
    div []
        [
          button [ onClick Fetch ] [ text "Fetch data" ]
        ],
    div [] [ text (toString model.numberOfGames) ],
    div [] [ text (toString model.listOfGames) ],
    div [] [ text model.errorMsg ]
      ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of 
  User user -> ({ model | user = user }, Cmd.none)
  Key key   -> ({ model | key = key }, Cmd.none)
  Fetch     -> (model, fetchGames model.key model.user)
  Games gs  -> ({ model | games = gs }, Cmd.none)
  FetchCollection (Err err) -> ({ model | errorMsg = toString err }, Cmd.none)
  FetchCollection (Ok (n, gs)) -> ({ model | numberOfGames = n, listOfGames = gs }, Cmd.none) 
    --process model response

process : Model -> Result Http.Error String -> (Model, Cmd Msg)
process model result = case result of
  Err error -> (model, Cmd.none)
  Ok gameString -> (model, Cmd.none)

fetchGames : Key -> User -> Cmd Msg
fetchGames key user = 
  let query = String.concat [
                  ownedQuery, "key=", key, 
                  "&steamID=", user, 
                  "&", includeFree, 
                  "&format=json"
              ]

      appidPlaytime = map2 (\x y -> (x, y)) (field "appid" int) (field "playtime_forever" int)
      gameCountAndGames = map2 (,) (field "game_count" int) (field "games" (list appidPlaytime))
      request = get query (field "response" gameCountAndGames)

  in Http.send FetchCollection request

server : String
server = "http://localhost:80/steam/"

prependServer : String -> String
prependServer str = String.concat[ server, str ]

ownedQuery: String
ownedQuery = prependServer "IPlayerService/GetOwnedGames/v0001/?"

userStatsQuery : String
userStatsQuery = prependServer "ISteamUserStats/GetUserStatsForGame/v0002/?appid="

schemaQuery : String
schemaQuery = prependServer "ISteamUserStats/GetSchemaForGame/v2/?key="

recentQuery : String
recentQuery = prependServer "IPlayerService/GetRecentlyPlayedGames/v0001/?"

includeFree : String
includeFree = "include_played_free_games=1"