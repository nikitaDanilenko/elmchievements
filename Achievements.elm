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
import Duration exposing         ( Duration, toDuration, zero )

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
    -- Name of the game.
    name : String,   
    -- Number of obtained achievements.
    obtained : Int,
    -- Number of obtainable achievements.             
    obtainable : Int,
    -- The exact percentage of the gettable achievements.
    preciseQuota : Float,
    -- The rounded percentage of the gettable achievements.
    discreteQuota : Int,
     {- How much one more achievement in this game would affect
        the average of the precise completion rates (including only the started games). -}
    changeInStartedCompletionRatesPrecise : Float, 
    {- How much one more achievement in this game would affect
        the average of the rounded completion rates (including only the started games). -}
    changeInStartedCompletionRatesRounded : Float,
    {- How much one more achievement in this game would affect 
       the average of the precise completion rates in total (including unplayed games). -}
    changeInOverallCompletionRatesPrecise : Float,
    {- How much one more achievement in this game would affect 
       the average of the rounded completion rates in total (including unplayed games). -}
    changeInOverallCompletionRatesRounded : Float,
    {- How much one more achievement would affect the quotient of the obtained achievements
       and the overall achievements in started games. -}
    changeInStartedTotalQuota : Float,
    {- How much one more achievement would affect the quotient of the obtained achievements
       and the overall achievement, i.e. all possible achievements taken over all games. -}
    changeInTotalQuota : Float,
    -- The amount of time spent on this game (so far).
    timePlayed : Int                 
  }

type alias Statistics = {
  listOfStats : List Stats,
  numberOfGamesTotal : Int,
  --numberOfGamesWithAchievements : Int,
  --numberOfStartedGamesWithAchievements : Int,
  timeOverall : Duration--,
  --timeWithAchievements: Duration,
  --timeWithStartedAchievements : Duration,
  --obtained : Int,
  --obtainableInStartedGames : Int,
  --obtainableOverall : Int,
  --averageOfPreciseQuotas : Float,
  --averageOfRoundedQuotas : Float,
  --averageOverallInStarted : Float,
  --averageOverall : Float
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
    statistics = {
      listOfStats = [],
      numberOfGamesTotal = 0--,
      --numberOfGamesWithAchievements = 0,
      --numberOfStartedGamesWithAchievements = 0,
      timeOverall = zero,
      --timeWithAchievements = zero,
      --timeWithStartedAchievements = zero,
      --obtained = 0,
      --obtainableInStartedGames = 0,
      --obtainableOverall = 0,
      --averageOfPreciseQuotas = 0.0,
      --averageOfRoundedQuotas = 0.0,
      --averageOverallInStarted = 0.0,
      --averageOverall = 0.0
    },
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
            filteredGames = List.filter (String.contains lowerQuery << String.toLower << .name) model.statistics.listOfStats
            
            toCell : String -> Html Msg
            toCell str = td [] [ text str ]

            toTable : List (List String) -> Html Msg
            toTable = table [] << List.map (tr [] << List.map toCell)

            stats = model.statistics
        in
          div []
            [ h1 [] [ text "Achievement statistics for all games" ]
            , toTable [
              ["Total number of games:",                    toString stats.numberOfGamesTotal],
              ["Number of games with achievements:",        toString stats.numberOfGamesWithAchievements],
              ["Number of games with at least one obtained achievement:",
                                                            toString stats.numberOfStartedGamesWithAchievements],
              ["Overall play time:",                        Duration.toString stats.timeOverall],
              ["Play time in games with achievements:",     Duration.toString stats.timeWithAchievements],
              ["Achievements obtained:",                    toString stats.obtained],
              ["Achievements obtainable in started games:", toString stats.obtainableInStartedGames],
              ["Achievements obtainable overall",           toString stats.obtainableOverall],
              ["Average of the exact completion rates:",    toString (truncate stats.averageOfPreciseQuotas)],
              ["Average of the rounded completion rates:",  toString (truncate stats.averageOfRoundedQuotas)],
              ["Overall average in started games:",         toString (truncate stats.averageOverallInStarted)],
              ["Overall average:",                          toString (truncate stats.averageOverall)]
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
  FetchCollection (Ok (n, gs)) -> (model, processGames model.key model.user n (List.take n gs))
  Finished statistics          -> ({ model | statistics = statistics, displayState = Table }, Cmd.none)
  SetQuery q                   -> ({ model | nameQuery = q }, Cmd.none)
  SetTableState s              -> ({ model | tableState = s }, Cmd.none)

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

processGames : Key -> User -> Int -> List (Id, Minutes) -> Cmd Msg
processGames key user n gs =
  let overallDuration = Duration.toDuration (sumMap Tuple.second gs)
  in Task.perform (Finished << createStats n overallDuration) (preprocessGames key user gs)

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
createStats : Int -> Duration -> List Game -> Statistics
createStats n overallDuration gs =
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
    
      allAchievements     = sumMap .obtainable gs
      startedAchievements = sumMap .obtainable stats
      gotAchievements     = sumMap .obtained stats
      exactProgresses     = sumMap .preciseQuota stats
      roundedProgresses   = sumMap .discreteQuota stats
      minutes             = sumMap .timePlayed gs

  in  { 
      listOfStats = stats,
      numberOfGamesTotal = n,
      numberOfGamesWithAchievements = List.length stats,
      numberOfStartedGamesWithAchievements = startedGamesLength,
      timeOverall = overallDuration,
      timeWithAchievements = Duration.toDuration (sumMap .timePlayed gs),
      timeWithStartedAchievements = Duration.toDuration (sumMap .timePlayed stats),
      obtained = gotAchievements,
      obtainableInStartedGames = startedAchievements,
      obtainableOverall = sumMap .obtainable gs,
      averageOfPreciseQuotas = exactProgresses / Basics.toFloat startedGamesLength,
      averageOfRoundedQuotas = roundedProgresses /// startedGamesLength,
      averageOverallInStarted = percentage gotAchievements startedAchievements,
      averageOverall = percentage gotAchievements allAchievements
      }

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