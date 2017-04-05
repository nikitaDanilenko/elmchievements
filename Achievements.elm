module Achievements exposing ( .. )

import Html exposing              ( Html, div, input, text, button, h1, table, tr, td, progress, 
                                    header, node, label, fieldset, a )
import Html.Attributes exposing   ( placeholder, autocomplete, rel, type_, href, id, class, checked, href )
import Html.Events exposing       ( onInput, onClick )
import Html.Events.Extra exposing ( onEnter )
import Http exposing              ( Error, send, get, Request, emptyBody )
import Json.Decode exposing       ( at, int, field, string, list, map, map2 )
import List exposing              ( length, foldr )
import Task exposing              ( perform, sequence, Task )
import Table exposing             ( State )

import Auxiliaries exposing       ( catMaybes, truncateAt, (///), sumMap, unwords )
import Duration exposing          ( Duration, toDuration, zero )

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
        --noAchievmentState : Table.State,
        showUnplayedGames : Bool,
        showPerfectGames : Bool,
        nameQuery : String,
        errorMsg : String
    }

type alias Game = 
    {
        name : String,
        steamID : Id,
        timePlayed : Int,
        obtained : Int,
        obtainable : Int
    }

type alias Stats = 
  {
    -- Name of the game.
    name : String,
    -- The Steam "number" of the game.
    steamID : Id,
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


{- The type of overall statistics.
   The information in this collection is partly redundant,
   because it is possible to compute most of the information from the given list of individual game statistics.
   However, to compute the list one needs to compute several of the values in this collection as well,
   which is why we rather use a redundant data type than recompute the values later. -}
type alias Statistics = {
  -- The list of individual game statistics.
  listOfStats : List Stats,
  -- The list of all those games that do not have any achievements.
  listOfGamesWithoutAchievements : List Game,
  -- The number of all games owned by the user.
  numberOfGamesTotal : Int,
    -- The number of all those games that have been played for at least one minute.
  numberOfPlayedGames : Int,
  -- The number of all those games that have achievements.
  numberOfGamesWithAchievements : Int,
  -- The number of all those games that have achievements and where at least one achievement has been collected.
  numberOfStartedGamesWithAchievements : Int,
  -- The number of games with achievements, where every achievement has been unlocked.
  numberOfPerfectGames : Int,
  -- The overall play time in all games ever.
  timeOverall : Duration,
  -- The overall time spent in all games with achievements, including games with achievements with no unlocked ones.
  timeWithAchievements: Duration,
  -- The overall time spent in games with achievements, where at least one achievement has been unlocked.
  timeWithStartedAchievements : Duration,
  -- The total number of obtained achievements. 
  obtained : Int,
  -- The number of achievements that can be obtained in those games, where at least one achievement has been unlocked.
  obtainableInStartedGames : Int,
  -- The number of achievements in all games.
  obtainableOverall : Int,
  -- The average of all precise quotas taken over all games where at least one achievement has been obtained.
  averageOfPreciseQuotasStarted : Float,
  -- The average of all rounded quotas taken over all games where at least one achievement has been obtained.
  averageOfRoundedQuotasStarted : Float,
  -- The average of all precise quotas taken over all games.
  averageOfPreciseQuotas : Float,
  -- The average of all rounded quotas taken over all games.
  averageOfRoundedQuotas : Float,
  -- The quotient of all obtained achievements and all achievements obtainable in started games.
  averageOverallInStarted : Float,
  -- The quotient of all obtained achievements and all possible achievements.
  averageOverall : Float
}

emptyStatistics : Statistics
emptyStatistics = {
    listOfStats = [],
    listOfGamesWithoutAchievements = [],
    numberOfGamesTotal = 0,
    numberOfPlayedGames = 0,
    numberOfGamesWithAchievements = 0,
    numberOfStartedGamesWithAchievements = 0,
    numberOfPerfectGames = 0,
    timeOverall = zero,
    timeWithAchievements = zero,
    timeWithStartedAchievements = zero,
    obtained = 0,
    obtainableInStartedGames = 0,
    obtainableOverall = 0,
    averageOfPreciseQuotasStarted = 0.0,
    averageOfRoundedQuotasStarted = 0.0,
    averageOfPreciseQuotas = 0.0,
    averageOfRoundedQuotas = 0.0,
    averageOverallInStarted = 0.0,
    averageOverall = 0.0
  }

type Achievement = Achievement { label : String, unlocked : Bool }

type Msg = User User         -- the user whose achievement stats are desired 
         | Key Key           -- the key to used for all queries
         | Fetch             -- the command to actually fetch the data from the Steam servers
         | FetchCollection (Result Http.Error (Int, List (Id, Minutes)))
         | Finished Statistics
         | SetQuery String
         | SetTableState Table.State
         | ToggleShowPerfect
         | ToggleShowUnplayed
         --| SetNoAchievementState Table.State

initialModel : (Model, Cmd Msg)
initialModel = ({
    key = "",
    user = "",
    statistics = emptyStatistics,
    displayState = Initial,
    tableState = Table.initialSort "Name",
    --noAchievmentState = Table.initialSort "Name",
    showUnplayedGames = True,
    showPerfectGames = True,
    nameQuery = "",
    errorMsg =  ""
  }
  , Cmd.none)

checkbox : List (Html.Attribute msg) -> msg -> String -> Html msg
checkbox attrs msg name =
  label []
    [ input (List.concat [[ type_ "checkbox", onClick msg ], attrs]) []
    , text name
    ]

view : Model -> Html Msg
view model = 
  let initialView : Model -> Html Msg
      initialView model = 
        div [ id "initialMain" ] 
            [
          div [ id "keyField" ] 
              [
                input [ autocomplete True, onInput Key, onEnter Fetch ] []
              ],
          div [ id "userField" ]
              [
                input [ autocomplete True, onInput User, onEnter Fetch ] []
              ],
          div [ id "fetchButton" ]
              [
                button [ class "button", onClick Fetch ] [ text "Fetch data" ]
              ],
          div [ id "errorLabel" ] [ text model.errorMsg ]
            ]

      loadingView : Model -> Html Msg
      loadingView _ = 
        div [ id "loadingMain" ] [
          div [ id "loadingInfoText" ] [ text "Fetching game information" ],
          div [ id "loadingBar"] [ progress [ id "fetchingProgressBar" ] [] ]
        ]


      tableView : Model -> Html Msg
      tableView model =
        let lowerQuery = String.toLower model.nameQuery
            containsPredicate = String.contains lowerQuery << String.toLower << .name
            removePerfect = if not model.showPerfectGames then \g -> g.obtainable /= g.obtained else always True 
            removeUnplayed = if not model.showUnplayedGames then \g -> g.obtained > 0 else always True
            filteredGames = List.filter (\g -> containsPredicate g && removeUnplayed g && removePerfect g) model.statistics.listOfStats
            
            toCell : String -> Html Msg
            toCell str = td [] [ text str ]

            toTable : List (List String) -> Html Msg
            toTable = table [] << List.map (tr [] << List.map toCell)

            stats = model.statistics
        in
          div [ id "statisticsMain" ]
            [ h1 [] [ text "Achievement statistics for all games" ]
            , toTable [
              ["Total number of games:",                    toString stats.numberOfGamesTotal],
              ["Number of played games:",                   toString stats.numberOfPlayedGames],
              ["Number of games with achievements:",        toString stats.numberOfGamesWithAchievements],
              ["Number of games with at least one obtained achievement:",
                                                            toString stats.numberOfStartedGamesWithAchievements],
              ["Number of perfect games:",                  toString stats.numberOfPerfectGames],
              ["Overall play time:",                        unwords [
                                                                Duration.toString stats.timeOverall,
                                                                String.concat ["(", Duration.toFullString stats.timeOverall, ")"]
                                                            ]],
              ["Play time in games with achievements:",     unwords [
                                                                Duration.toString stats.timeWithAchievements,
                                                                String.concat ["(", Duration.toFullString stats.timeWithAchievements, ")"]
                                                            ]],
              ["Play time in games started achievements:",  unwords [
                                                                Duration.toString stats.timeWithStartedAchievements,
                                                                String.concat ["(", Duration.toFullString stats.timeWithStartedAchievements, ")"]
                                                            ]],
              ["Achievements obtained:",                    toString stats.obtained],
              ["Achievements obtainable in started games:", toString stats.obtainableInStartedGames],
              ["Achievements obtainable overall",           toString stats.obtainableOverall],
              ["Average of the exact completion rates in started games (CRSP):",
                                                            toString (truncate stats.averageOfPreciseQuotasStarted)],
              ["Average of the rounded completion rates in started games (CRSR):",  
                                                            toString (truncate stats.averageOfRoundedQuotasStarted)],
              ["Average of the exact completion rates overall (CRTP):",
                                                            toString (truncate stats.averageOfPreciseQuotas)],
              ["Average of the rounded completion rates overall (CRTR):",  
                                                            toString (truncate stats.averageOfRoundedQuotas)],
              ["Overall average in started games (QS):",    toString (truncate stats.averageOverallInStarted)],
              ["Overall average (QT):",                     toString (truncate stats.averageOverall)]
            ]
            , fieldset [] [ checkbox [ checked model.showPerfectGames ] ToggleShowPerfect "Show perfect games"
            , checkbox [ checked model.showUnplayedGames ] ToggleShowUnplayed "Show unstarted games"
            ]
            , input [ placeholder "Search by game title", onInput SetQuery ] []
            , div [ class "achievementTable" ] [ Table.view config model.tableState filteredGames ]
            {- Currently, most games without achievements do not provide stats at all (not even their name!),
               which is why we omit this table for now. -}
            --, Table.view configNoAchievements model.noAchievmentState stats.listOfGamesWithoutAchievements
            ]

      currentView = case model.displayState of 
        Initial -> initialView
        Loading -> loadingView
        Table   -> tableView

    in div [ id "mainPage"]
           [
             node "link" [ rel "stylesheet", type_ "text/css", href "style.css" ] [],
             currentView model 
           ]

truncate : Float -> Float
truncate = truncateAt precision

config : Table.Config Stats Msg
config = Table.config
      { toId = String.toLower << .name
      , toMsg = SetTableState
      , columns =
          [ Table.veryCustomColumn { 
              name     = "Name",
              viewData = displayNamesAsLinks,
              sorter   = Table.increasingOrDecreasingBy .name
            }
          , Table.intColumn    "Obtained"      .obtained
          , Table.intColumn    "Obtainable"    .obtainable
          , Table.floatColumn  "Exact quota"   (truncate << .preciseQuota) 
          , Table.intColumn    "Rounded quota" .discreteQuota
          , Table.floatColumn  "\x0394 CRSP"   (truncate << .changeInStartedCompletionRatesPrecise)
          , Table.floatColumn  "\x0394 CRSR"   (truncate << .changeInStartedCompletionRatesRounded)
          , Table.floatColumn  "\x0394 CRTP"   (truncate << .changeInOverallCompletionRatesPrecise)
          , Table.floatColumn  "\x0394 CRTR"   (truncate << .changeInOverallCompletionRatesRounded)
          , Table.floatColumn  "\x0394 QS"     (truncateAt 5 << .changeInStartedTotalQuota)
          , Table.floatColumn  "\x0394 QT"     (truncateAt 5 << .changeInTotalQuota)
          , Table.stringColumn "Play time"     (Duration.toShortString << Duration.toDuration << .timePlayed)
          ]
      }

displayNamesAsLinks : Stats -> Table.HtmlDetails Msg
displayNamesAsLinks stats = {
    attributes = [ class "achievementTableLink" ],
    children   = [ a [ href (String.concat [ "http://store.steampowered.com/app/", toString (stats.steamID) ]) ] 
                     [ text stats.name ] ]
  }

--configNoAchievements : Table.Config Game Msg
--configNoAchievements = Table.config {
--    toId = String.toLower << .name,
--    toMsg = SetNoAchievementState,
--    columns = [
--      Table.stringColumn "Name" .name,
--      Table.stringColumn "Play time" (Duration.toString << Duration.toDuration << .timePlayed)
--    ]
--  }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of 
  User user                    -> ({ model | user = user }, Cmd.none)
  Key key                      -> ({ model | key = key }, Cmd.none)
  Fetch                        -> ({ model | displayState = Loading }, fetchGames model.key model.user)
  FetchCollection (Err err)    -> ({ model | errorMsg = toString err }, Cmd.none)
  FetchCollection (Ok (n, gs)) -> (model, processGames model.key model.user n gs)
  Finished statistics          -> ({ model | statistics = statistics, displayState = Table }, Cmd.none)
  SetQuery q                   -> ({ model | nameQuery = q }, Cmd.none)
  SetTableState s              -> ({ model | tableState = s }, Cmd.none)
  --SetNoAchievementState s      -> ({ model | noAchievmentState = s}, Cmd.none)
  ToggleShowUnplayed           -> ({ model | showUnplayedGames = not (model.showUnplayedGames) }, Cmd.none)
  ToggleShowPerfect            -> ({ model | showPerfectGames = not (model.showPerfectGames) }, Cmd.none)

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
preprocessGames : Key -> User -> List (Id, Minutes) -> Task Never (List Game, List Game)
preprocessGames key user gs = 
  let f : Id -> Minutes -> Task Never (Maybe Game)
      f gameId time =
        let (gameName, gameAchieved) = fetchAchievementsPerGame key user gameId 
            cmdGame =
              Http.toTask gameName
                |> Task.andThen (\name -> Task.onError (always (Task.succeed 0)) (Http.toTask (fetchAchievementNumber key gameId))
                  |> Task.andThen (\achNumber -> Task.onError (always (Task.succeed 0)) (Http.toTask gameAchieved)
                    |> Task.andThen (\achieved -> Task.succeed (Just { 
                            name       = name,
                            steamID    = gameId,
                            timePlayed = time,
                            obtained   = achieved,
                            obtainable = achNumber 
                          }
                        )
                      )
                    ) 
                  )
        in Task.onError (always (Task.succeed Nothing)) cmdGame
  in Task.map (List.partition (\g -> g.obtainable > 0) << catMaybes) (Task.sequence (List.map (uncurry f) gs))

processGames : Key -> User -> Int -> List (Id, Minutes) -> Cmd Msg
processGames key user n gs =
  let overallDuration = Duration.toDuration (sumMap Tuple.second gs)
      playedGames     = List.length (List.filter (\(_, m) -> m > 0) gs)
  in Task.perform (Finished << uncurry (createStats n playedGames overallDuration)) (preprocessGames key user gs)

{- Given a list of games, this function computes the metadata of interest. -}
createStats : Int -> Int -> Duration -> List Game -> List Game -> Statistics
createStats allGames playedGames overallDuration gs gamesWithoutAchievements =
  let startedGames       = List.filter (((<) 0) << .obtained) gs
      startedGamesLength = List.length startedGames

      preciseInStarted = List.map (\g -> percentage g.obtained g.obtainable) startedGames
      sumOfPreciseInStarted = List.sum preciseInStarted
      sumOfRoundedInStarted = sumMap Basics.round preciseInStarted

      obtainableInStarted : Int
      obtainableInStarted = sumMap .obtainable startedGames

      obtainableOverall : Int
      obtainableOverall = sumMap .obtainable gs

      obtainedOverall : Int
      obtainedOverall = sumMap .obtained startedGames

      sgl : Float
      sgl = Basics.toFloat startedGamesLength

      {- Compute by how much the overall completion percentage would be changed
         if an additional achievement was achieved. Note that the resulting number
         can be zero (got all achievements already) or even negative (no achievements yet). -}
      change : (Int -> Int -> Int) -> Int -> Int -> Float -> Float
      change mkNewNumber have want sumOfPreviousQuotas = 
        if (have >= want) then 0
        else let gameNumber = Basics.toFloat (mkNewNumber have startedGamesLength)
             in (sgl
                    * 
                      (sumOfPreviousQuotas + percentage 1 want) 
                      - gameNumber * sumOfPreviousQuotas) 
                  / (gameNumber * sgl)

      changeTotalQuota : Bool -> Int -> Int -> Int -> Int -> Float
      changeTotalQuota overall have want allHave allWant = 
        let currentQuota = allHave /// allWant
            (newNum, newDen) = if (have >= want) then (allHave, allWant) 
                        else if (have > 0) then (1 + allHave, allWant)
                        else (1 + allHave, (if overall then 0 else want) + allWant) 
        in  100 * ((newNum /// newDen) - currentQuota)

      changeGameSize : Int -> Int -> Int
      changeGameSize have previousSize = if (have > 0) then previousSize else 1 + previousSize

      ignoreProgress : Int -> Int -> Int
      ignoreProgress _ x = x

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
          in { name = game.name,
               steamID = game.steamID,
               obtained = game.obtained,
               obtainable = game.obtainable, 
               preciseQuota = exactQuota,
               discreteQuota = Basics.round exactQuota,
               changeInStartedCompletionRatesPrecise = change changeGameSize game.obtained game.obtainable sumOfPreciseInStarted,
               changeInStartedCompletionRatesRounded = change changeGameSize game.obtained game.obtainable (Basics.toFloat sumOfRoundedInStarted),
               changeInOverallCompletionRatesPrecise = change ignoreProgress game.obtained game.obtainable sumOfPreciseInStarted,
               changeInOverallCompletionRatesRounded = change ignoreProgress game.obtained game.obtainable (Basics.toFloat sumOfRoundedInStarted),
               changeInStartedTotalQuota             = changeTotalQuota False game.obtained game.obtainable obtainedOverall obtainableInStarted,
               changeInTotalQuota                    = changeTotalQuota True  game.obtained game.obtainable obtainedOverall obtainableOverall,
               timePlayed = game.timePlayed
                 }
        in List.map f
  
      allStats     = mkStats gs
      startedStats = List.filter (\s -> s.obtained > 0) allStats
    
      allAchievements     = sumMap .obtainable gs
      startedAchievements = sumMap .obtainable startedStats
      gotAchievements     = sumMap .obtained startedStats
      exactProgresses     = sumMap .preciseQuota startedStats
      roundedProgresses   = sumMap .discreteQuota startedStats
      minutes             = sumMap .timePlayed gs

  in  { 
      listOfStats = allStats,
      listOfGamesWithoutAchievements = gamesWithoutAchievements,
      numberOfGamesTotal = allGames,
      numberOfPlayedGames = playedGames,
      numberOfGamesWithAchievements = List.length allStats,
      numberOfStartedGamesWithAchievements = startedGamesLength,
      numberOfPerfectGames = List.length (List.filter (\s -> s.obtained == s.obtainable) startedStats),
      timeOverall = overallDuration,
      timeWithAchievements = Duration.toDuration (sumMap .timePlayed gs),
      timeWithStartedAchievements = Duration.toDuration (sumMap .timePlayed startedStats),
      obtained = gotAchievements,
      obtainableInStartedGames = startedAchievements,
      obtainableOverall = sumMap .obtainable gs,
      averageOfPreciseQuotasStarted = exactProgresses / Basics.toFloat startedGamesLength,
      averageOfRoundedQuotasStarted = roundedProgresses /// startedGamesLength,
      averageOfPreciseQuotas = sumOfPreciseInStarted / Basics.toFloat allGames,
      averageOfRoundedQuotas = sumOfRoundedInStarted /// allGames,
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