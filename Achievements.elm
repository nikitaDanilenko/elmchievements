module Achievements exposing ( .. )

import Html exposing             ( Html, div, input, text, button )
import Html.Attributes exposing  ( placeholder )
import Html.Events exposing      ( onInput, onClick )
import Http exposing             ( Error, send, get, Request, emptyBody )
import Json.Decode exposing      ( at, int, field, string, list, map, map2 )
import List exposing             ( length, foldr )
import Task exposing             ( perform, sequence, Task )

import Auxiliaries exposing      ( catMaybes ) 

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
        achievementsObtained : Int,
        achievementsObtainable : Int
    }

type alias Stats = 
  {
    name : String,
    gettable : Int,
    gotten : Int,
    preciseQuota : Float,
    discreteQuota : Int
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
          input [ placeholder "User ID", onInput User ] []
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
  FetchCollection _ -> (model, Cmd.none)
  --FetchCollection (Err err) -> ({ model | errorMsg = toString err }, Cmd.none)
  --FetchCollection (Ok (n, gs)) -> ({ model | numberOfGames = n, listOfGames = gs }, 
  --                                  processGames model.key model.user n gs) 

fetchGames : Key -> User -> Cmd Msg
fetchGames key user = 
  let query = mkAddress ownedQuery [
                  mkKeyString key, 
                  mkUserString user, 
                  includeFree, 
                  jsonFormat
              ]

      appidPlaytime     = map2 (,) (field "appid" int)      (field "playtime_forever" int)
      gameCountAndGames = map2 (,) (field "game_count" int) (field "games" (list appidPlaytime))
      request = get query (field "response" gameCountAndGames)

  in Http.send FetchCollection request

-- Make a query to find out the general stats of any game.
mkGameStatsQuery : Key -> Id -> String
mkGameStatsQuery key gameId = mkAddress schemaQuery [ mkKeyString key, mkAppString gameId ]

mkUserStatsQuery : Key -> User -> Id -> String
mkUserStatsQuery key user gameId =
  mkAddress userStatsQuery [ mkAppString gameId, mkKeyString key, mkUserString user, jsonFormat ]

mkAppString : Id -> String
mkAppString gameId = String.concat [ appParam, toString gameId ]

mkKeyString : Key -> String
mkKeyString key = String.concat [ keyParam, key ]

mkUserString : User -> String
mkUserString user = String.concat [ userParam, user ]

{- Create a full address by interspersing ampersands and prepending the server label to
   the given list of strings. -}
mkAddress : String -> List String -> String
mkAddress initial strs = prependServer (String.concat (initial :: List.intersperse "&" strs))

{- Flatten a list of results to a result list. All failures are removed, so that
   the result of this function is the list of those results that are "Ok". -}
flattenResults : List (Result e r) -> List r
flattenResults = catMaybes << List.map Result.toMaybe

processGames : Key -> User -> Int -> List (Id, Minutes) -> Cmd (List Game)
processGames key user n gs = 
  let f : Id -> Minutes -> Task Never (Maybe Game)
      f gameId time =
        let (gameName, gameAchieved) = fetchAchievementsPerGame key user gameId 
            cmdGame =  
              Http.toTask (fetchAchievementNumber key gameId)
                |> Task.andThen (\an -> Http.toTask gameName 
                  |> Task.andThen (\name -> Task.onError (always (Task.succeed 0)) 
                                                         (Http.toTask gameAchieved)
                    |> Task.andThen (\got -> Task.succeed (Just (Game name time got an)))))
        in Task.onError (always (Task.succeed Nothing)) cmdGame  
  in Task.perform catMaybes (Task.sequence (List.map (uncurry f) gs))

--createStats : List Game -> List ()

{- Fetch the number of achievements a game has. If the game has achievements, the number of
   achievements is returned as a Just, otherwise Nothing is returned. -}
fetchAchievementNumber : Key -> Id -> Http.Request Int
fetchAchievementNumber key gameId = 
  let query = mkGameStatsQuery key gameId
      numOfAchs = Json.Decode.map length (at [ "game", "availableGameStats", "achievements" ] 
                             (list (field "defaultvalue" int)))
  in get query numOfAchs

fetchAchievementsPerGame : Key -> User -> Id -> (Http.Request String, Http.Request Int)
fetchAchievementsPerGame key user gameId =
  let query = get (mkUserStatsQuery key user gameId)

      --decodeNameLength = field "playerstats" parts
      lengthDecoder = Json.Decode.map length (field "achievements" (list (field "achieved" int)))
      --parts = map2 (,) (field "gameName" string) 
      --                 (Json.Decode.map length (field "achievements" (list (field "achieved" int))))
  in (query (at [ "playerstats", "gameName" ] string), 
      query (field "playerstats" lengthDecoder ))
{- The server to query for the Steam API. -}
server : String
server = "http://localhost:80/steam/"

keyParam : String
keyParam = "key="

userParam : String
userParam = "steamID="

appParam : String
appParam = "appid="

jsonFormat : String
jsonFormat = "format=json"

prependServer : String -> String
prependServer str = String.concat [ server, str ]

ownedQuery: String
ownedQuery = "IPlayerService/GetOwnedGames/v0001/?"

userStatsQuery : String
userStatsQuery = "ISteamUserStats/GetUserStatsForGame/v0002/?"

schemaQuery : String
schemaQuery = "ISteamUserStats/GetSchemaForGame/v2/?"

recentQuery : String
recentQuery = "IPlayerService/GetRecentlyPlayedGames/v0001/?"

includeFree : String
includeFree = "include_played_free_games=1"