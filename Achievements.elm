module Achievements exposing ( .. )

import Html exposing             ( Html, div, input, text, button, h1, table, tr, td, progress )
import Html.Attributes exposing  ( placeholder )
import Html.Events exposing      ( onInput, onClick )
import Http exposing             ( Error, send, get, Request, emptyBody )
import Json.Decode exposing      ( at, int, field, string, list, map, map2 )
import List exposing             ( length, foldr )
import Task exposing             ( perform, sequence, Task )
import Table exposing            ( State )

import Auxiliaries exposing      ( catMaybes, truncateAt, (///), sumMap )
import Duration exposing         ( Duration, toDuration )

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

{- The different types of possible states the page can transition. -}
type DisplayState = Initial -- The state at the beginning of the application.
                  | Loading -- A loading screen while the information is being queried.
                  | Table   -- The table state that shows all relevant information.

type alias Model = 
    {
        key : Key,
        user : User,
        statistics : Statistics,
        gameStats : List Stats,
        timePlayed : Duration,
        gotten : Int,
        gettable : Int,
        preciseQuota : Float,
        roundedQuota : Float,
        average : Float,
        displayState : DisplayState,
        tableState : Table.State,
        nameQuery : String,
        errorMsg : String
    }

type alias Game = 
    {
        name : String,
        timePlayed : Int,
        obtained : Int,
        obtainable : Int
    }

type alias Stats = 
  {
    name : String,         -- name of the game
    obtained : Int,        -- number of obtained achievements
    obtainable : Int,      -- number of obtainable achievements
    preciseQuota : Float,  -- the exact percentage of the gettable achievements
    discreteQuota : Int,   -- the rounded percentage of the gettable achievements
    improvement : Float,   {- how much one more achievement in this game would additively change 
                              the overall completion rate -}
    timePlayed : Int       -- the amount of time spent on this game (so far)
  }

type alias Statistics = {
  listOfStats : List Stats,
  timeWithAchievements: Duration,
  timeOverall : Duration,
  obtained : Int,
  obtainableInStartedGames : Int,
  obtainableOverall : Int,
  averageOfPreciseQuotas : Float,
  averageOfRoundedQuotas : Float,
  averageOverallInStarted : Float,
  averageOverall : Float
}

type Achievement = Achievement { label : String, unlocked : Bool }

type Msg = User User         -- the user whose achievement stats are desired 
         | Key Key           -- the key to used for all queries
         | Fetch             -- the command to actually fetch the data from the Steam servers
         | FetchCollection (Result Http.Error (Int, List (Id, Minutes)))
         | Finished Statistics
         | SetQuery String
         | SetTableState Table.State

initialModel : (Model, Cmd Msg)
initialModel = ({
    key = "",
    user = "",
    gameStats = [],
    timePlayed = Duration.toDuration 0,
    gotten = 0,
    gettable = 0,
    preciseQuota = 0.0,
    roundedQuota = 0.0,
    average = 0.0,
    displayState = Initial,
    tableState = Table.initialSort "Name",
    nameQuery = "",
    errorMsg =  ""
  }
  , Cmd.none)

view : Model -> Html Msg
view model = 
  let initialView : Model -> Html Msg
      initialView model = 
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
          div [] [ text model.errorMsg ]
            ]

      loadingView : Model -> Html Msg
      loadingView _ = 
        div [] [
          div [] [ text "Fetching game information" ],
          div [] [ progress [] [] ]
        ]


      tableView : Model -> Html Msg
      tableView model =
        let lowerQuery = String.toLower model.nameQuery
            filteredGames = List.filter (String.contains lowerQuery << String.toLower << .name) model.gameStats
            
            toCell : String -> Html Msg
            toCell str = td [] [ text str ]

            toTable : List (List String) -> Html Msg
            toTable = table [] << List.map (tr [] << List.map toCell)
        in
          div []
            [ h1 [] [ text "Achievement statistics for all games" ]
            , toTable [
              ["Overall play time:",                       Duration.toString model.timePlayed],
              ["Achievements obtained:",                   toString model.gotten],
              ["Achievements obtainable:",                 toString model.gettable],
              ["Overall quota:",                           toString (truncate model.average)],
              ["Average of the exact completion rates:",   toString (truncate model.preciseQuota)],
              ["Average of the rounded completion rates:", toString (truncate model.roundedQuota)]
            ]
            , input [ placeholder "Search by game title", onInput SetQuery ] []
            , Table.view config model.tableState filteredGames
            ]

      currentView = case model.displayState of 
        Initial -> initialView
        Loading -> loadingView
        Table   -> tableView

    in currentView model

truncate : Float -> Float
truncate = truncateAt precision

config : Table.Config Stats Msg
config = Table.config
      { toId = .name
      , toMsg = SetTableState
      , columns =
          [ Table.stringColumn "Name"          .name
          , Table.intColumn    "Obtained"      .obtained
          , Table.intColumn    "Obtainable"    .obtainable
          , Table.floatColumn  "Exact quota"   (truncate << .preciseQuota) 
          , Table.intColumn    "Rounded quota" .discreteQuota
          , Table.floatColumn  "Improvement"   (truncate << .improvement)
          , Table.stringColumn "Play time"     (Duration.toString << Duration.toDuration << .timePlayed)
          ]
      }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of 
  User user                    -> ({ model | user = user }, Cmd.none)
  Key key                      -> ({ model | key = key }, Cmd.none)
  Fetch                        -> ({ model | displayState = Loading }, fetchGames model.key model.user)
  FetchCollection (Err err)    -> ({ model | errorMsg = toString err }, Cmd.none)
  FetchCollection (Ok (n, gs)) -> (model, processGames model.key model.user (List.take 25 gs))
  Finished statistics          -> ({ model | 
                                      gameStats = statistics.listOfStats,
                                      timePlayed = stati,
                                      gotten = have,
                                      gettable = want,
                                      preciseQuota = ex,
                                      roundedQuota = rd,
                                      average = av,
                                      displayState = Table }, 
                                   Cmd.none)
  SetQuery q                   -> ({ model | nameQuery = q }, Cmd.none)
  SetTableState s              -> (model, Cmd.none)

{- Fetch the games associated with the given user (using the supplied key).
   The resulting request is then passed on for further processing to fetch the individual game statistics. -}
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

{- Given a key, a user, and a list of pairs of ids and durations in minutes,
   this function creates a task which essentially does the following for every game:
    1. try to fetch the achievement statistics for the game in general
      1.1 if these do not exist ignore the game
    2. fetch the individual user statistics for the given game
    3. extract the necessary information from the statistics and wrap these up in a complete Game.

   The resulting list contains only those games, where the achievement query was successful, i.e.
   only games that do have achievements. -}
preprocessGames : Key -> User -> List (Id, Minutes) -> Task Never (List Game)
preprocessGames key user gs = 
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
  in Task.map catMaybes (Task.sequence (List.map (uncurry f) gs))

processGames : Key -> User -> List (Id, Minutes) -> Cmd Msg
processGames key user gs =
  let d2 = sumMap Tuple.first gs
      f (gs, d1, h, w, f1, f2, av) = Finished gs d1 d2 h w f1 f2 av
  in Task.perform (f << createStats) (preprocessGames key user gs)

{- Given a list of games, this function computes the metadata of interest.
   The result is constructed as
    List Stats: the list of statistics per game
    Duration:   The overall play time in all games ever.
                This includes games without achievements and games that have achievements,
                but where not a single one has been unlocked yet.
    Int:        The number of achieved achievements.
    Int:        The number of achievable achievements in those games where at least one
                achievement has been obtained.
    Float:      The average of the precise completion rates taken over all those games,
                where at least one achievement has been obtained.
    Float:      The average of the rounded completion rates taken over all those games,
                where at least one achievement has been obtained.
    Float:      The overall quotient of the obtained achievements and the number of total
                obtainable achievements in those games where at least one achievement has
                been unlocked.
   -}
createStats : List Game -> (List Stats, Duration, Int, Int, Float, Float, Float)
createStats gs =
  let startedGames       = List.filter (((<) 0) << .obtained) gs
      startedGamesLength = List.length startedGames

      {- Compute by how much the overall completion percentage would be changed
         if an additional achievement was achieved. Note that the resulting number
         can be zero (got all achievements already) or even negative (no achievements yet). -}
      improve : Int -> Int -> Float
      improve have want = 
        if (have >= want) then 0
        else let gameNumber = if (have > 0) then startedGamesLength else (1 + startedGamesLength)
             in 100 / (toFloat want * toFloat gameNumber)

      percentage : Int -> Int -> Float
      percentage x y = 100 * x /// y

      {- Collect and compute the information that is available per game already.
         The result is basically a GameStats, but the improvement value in case
         you make a single achievement more is missing.
        -}
      mkStats : List Game -> List Stats
      mkStats = 
        let f game = 
          let exactQuota = percentage game.obtained game.obtainable
          in Stats game.name 
                   game.obtained
                   game.obtainable 
                   exactQuota
                   (Basics.round exactQuota)
                   (improve game.obtained game.obtainable)
                   game.timePlayed
        in List.map f
  
      stats = mkStats startedGames
    
      allAchievements   = sumMap .obtainable stats
      gotAchievements   = sumMap .obtained stats
      exactProgresses   = sumMap .preciseQuota stats
      roundedProgresses = sumMap .discreteQuota stats
      minutes           = sumMap .timePlayed gs

  in (stats, 
      toDuration minutes,
      gotAchievements,
      allAchievements,
      exactProgresses / Basics.toFloat startedGamesLength,
      roundedProgresses /// startedGamesLength,
      percentage gotAchievements allAchievements)

-- The precision with which the achievement quotas are computed.
precision : Int
precision = 3

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

      lengthDecoder = Json.Decode.map (length << List.filter ((/=) 0)) (field "achievements" (list (field "achieved" int)))
  in (query (at [ "playerstats", "gameName" ] string), 
      query (field "playerstats" lengthDecoder ))

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
--userStatsQuery = "ISteamUserStats/GetUserStatsForGame/v0002/?"
userStatsQuery = "ISteamUserStats/GetPlayerAchievements/v0001/?"

schemaQuery : String
schemaQuery = "ISteamUserStats/GetSchemaForGame/v2/?"

recentQuery : String
recentQuery = "IPlayerService/GetRecentlyPlayedGames/v0001/?"

includeFree : String
includeFree = "include_played_free_games=1"