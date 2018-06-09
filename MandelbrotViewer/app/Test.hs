{-# LANGUAGE OverloadedStrings, Arrows #-}
-- Title:    Yampa/SDL Stub
-- Author:   Gerold Meisinger
-- Homepage: http://lambdor.net
-- Date:     June 2010
-- License:  Creative Commons 3.0 BY-NC-SA

module Test where

--import IdentityList

--import Maybe
import Control.Monad

import FRP.Yampa              as Yampa
import FRP.Yampa.Geometry
import SDL hiding (Point, initialize)
import SDL.Primitive
-- import SDL
-- import SDL.Events as SDL.Events
-- import SDL.Keysym as SDL.Keysym


-- types --------

type Position2 = Point2  Double
type Velocity2 = Vector2 Double

type Input = [SDL.Event]    -- non-deterministic events from input devices
type Logic = Yampa.Event () -- deterministic events from object processor

data ObjEvents = ObjEvents
    { oeInput :: Input
    , oeLogic :: Logic
    } deriving (Show)

data State = Rectangle Position2 Rect Color | Debug String
     deriving (Show)

data ObjOutput = ObjOutput
    { ooState         :: State
    , ooKillRequest   :: Yampa.Event ()       -- NoEvent|Event ()
    , ooSpawnRequests :: Yampa.Event [Object]
    }

defaultObjOutput = ObjOutput
    { ooState         = undefined
    , ooKillRequest   = Yampa.NoEvent
    , ooSpawnRequests = Yampa.NoEvent
    }

type Object = SF ObjEvents ObjOutput


-- utility ---------

instance (Show a) => Show (Yampa.Event a) where
    show (Yampa.Event a) = "LogicEvent: " ++ (show a)
    show Yampa.NoEvent   = "NoEvent"

instance Show (SF a b) where
    show sf = "SF"


-- main ---------

main :: IO ()
main = do
    -- Uncomment 'mainSteps' or 'mainLoop'!
  mainLoop  -- Runs the complete reactimate loop.
    --mainSteps -- Tests each reactimate step individually.
  where
    mainLoop :: IO ()
    mainLoop = do
        reactimate initialize input output (process objs)
        SDL.quit
       where
         playerObj   = playerObject (Point2 16 16)
                                    (Rect (-8) (-8) 8 8)
                                    (Color 0x00000000)
         obstacleObj = staticObject (Point2 48 48)
                                    (Rect (-8) (-8) 8 8)
                                    (Color 0x000000FF)
         objs = (listToIL [playerObj, obstacleObj])

    mainSteps :: IO ()
    mainSteps = do
        -- initialize :: IO Input
        -- Poll first 'SDL.Event's (should only be 'LostFocus').
        events <- initialize

        -- input :: IO (DTime, Maybe Input)
        -- Poll 'SDL.Event's at each step (probably []).
        events <- input False

        -- hits :: [(ILKey, State)] -> [ILKey]
        -- Testing player over obstalce => collision event.
        putStrLn $ "hits 1: " ++ (show $ hits $ assocsIL $ fmap ooState oos1)

        -- Testing player over enemy => no event.
        putStrLn $ "hits 2: " ++ (show $ hits $ assocsIL $ fmap ooState oos2)

        -- route :: (Input, IL ObjOutput) -> IL sf -> IL (ObjEvents, sf)
        -- Routes 'key' SDL.Event to all 'Object's and
        -- previous object 'State's, if there are any.

        -- First routing step.
        -- No collision events are checked as there are no 'State's yet.
        putStrLn "first route: "
        --mapM putStrLn $ showILObjEvents $ route ([key], emptyIL) objs
        putStrLn $ show $ assocsIL $ route ([key], emptyIL) objs

        -- Intermediate routing step.
        -- Assuming player over obstacle object => create collision event.
        putStrLn "route step: "
        putStrLn $ show $ assocsIL $ route ([key], oos1) objs

        -- killAndSpawn :: (Input, IL ObjOutput)
        --              -> (Yampa.Event (IL Object -> IL Object))
        -- Kill and spawn new objects corresponding to 'ObjOutput' requests.
        putStr "objs before kill&Spawn: "
        putStrLn $ show $ keysIL objs
        putStr "objs after kill&Spawn: "
        putStrLn $ show $ keysIL $
            case (killAndSpawn (([], emptyIL), oos1)) of
                (Yampa.Event d) -> d objs
                _         -> objs

        -- output :: IL ObjOutput -> IO Bool
        -- Just render the 'State's or quit if there is none.
        o1 <- output False oos1
        putStrLn $ show o1
        o2 <- output False oos2
        putStrLn $ show o2
        o3 <- output False emptyIL
        putStrLn $ show o3

        SDL.quit
      where
        key = KeyDown (Keysym
            { symKey = SDL.SDLK_RIGHT
            , symModifiers = []
            , symUnicode = '\0'
            })
        playerObj   = playerObject (Point2 16 16)
                                   (Rect (-8) (-8) 8 8)
                                   (Color 0x00000000)
        obstacleObj = staticObject (Point2 48 48)
                                   (Rect (-8) (-8) 8 8)
                                   (Color 0x000000FF)
        objs = (listToIL [playerObj, obstacleObj])

        enemyObj = staticObject (Point2 80 80)
                                (Rect (-8) (-8) 8 8)
                                (Color 0x00FF0000)
        ooPlayer = defaultObjOutput
            { ooState = Rectangle (Point2 48 48)
                                  (Rect (-8) (-8) 8 8)
                                  (Color 0x00000000)
            }
        ooObstacle = defaultObjOutput
            { ooState = Rectangle (Point2 48 48)
                                  (Rect (-8) (-8) 8 8)
                                  (Color 0x000000FF)
            , ooKillRequest   = Event ()
            , ooSpawnRequests = Event [enemyObj]
            }
        ooEnemy = defaultObjOutput
            { ooState = Rectangle (Point2 80 80)
                                  (Rect (-8) (-8) 8 8)
                                  (Color 0x00FF0000)
            }
        oos1 = listToIL [ooPlayer, ooObstacle]
        oos2 = listToIL [ooPlayer, ooEnemy]

-- reactimation IO ----------

initialize :: IO Input
initialize = do
    SDL.init [SDL.InitVideo]
    screen <- SDL.setVideoMode windowWidth windowHeight
                               windowDepth [SDL.HWSurface]
    SDL.setCaption windowCaption []

    SDL.fillRect screen Nothing (Color 0x006495ED) -- 0x00RRGGBB
    SDL.flip screen
    events <- unfoldWhileM (/= SDL.NoEvent) SDL.pollEvent

    putStrLn $ "initialize (sense): " ++ show events
    return events
  where
    windowWidth   = 96
    windowHeight  = 96
    windowDepth   = 32
    windowCaption = "Yampa/SDL Stub"

input :: Bool -> IO (DTime, Maybe Input)
input _ = do
    events <- unfoldWhileM (/= SDL.NoEvent) SDL.pollEvent
    putStrLn $ "input (sense): " ++ show events
    return (1.0, Just events)

output :: Bool -> IL ObjOutput -> IO Bool
output _ oos = do
    putStrLn $ "output (actuate) + " ++ (show delayMs) ++ "ms delay: "

    screen <- SDL.getVideoSurface
    SDL.fillRect screen Nothing (Color 0x006495ED) -- Pixel 0x--RRGGBB

    mapM_ (\oo -> render (ooState oo) screen) (elemsIL oos) -- render 'State'!

    SDL.flip screen
    SDL.delay delayMs

    return $ null $ keysIL oos
  where
    delayMs = 500

    render :: State -> SDL.Surface -> IO ()
    render (Rectangle pos rect color) screen = do
        SDL.fillRect screen gRect color
        return ()
      where
        -- center rectangle around position
        x0 = round (point2X pos) + (rectX rect)
        y0 = round (point2Y pos) + (rectY rect)
        x1 = round (point2X pos) + (rectW rect)
        y1 = round (point2Y pos) + (rectH rect)
        gRect = Just (Rect x0 y0 (x1 - x0) (y1 - y0))
    render (Debug s) screen = putStrLn s


-- reactimate process ----------

process :: IL Object -> SF Input (IL ObjOutput)
process objs0 =
  proc input -> do
    rec
        -- 'process' stores the 'State's (note: rec) and
        -- passes them over to core
        oos <- core objs0 -< (input, oos)
    returnA -< oos

core :: IL Object -> SF (Input, IL ObjOutput) (IL ObjOutput)
core objs = dpSwitch route
                     objs
                     (arr killAndSpawn >>> notYet)
                     (\sfs' f -> core (f sfs'))

-- 1. process previous object 'State's (if any) and
--    generate logical events
-- 2. distribute input and logical events to the corresponding objects
route :: (Input, IL ObjOutput) -> IL sf -> IL (ObjEvents, sf)
route (input, oos) objs = mapIL routeAux objs
  where
    hs = hits (assocsIL (fmap ooState oos)) -- process all object 'State's
    routeAux (k, obj) = (ObjEvents
        { oeInput = input
        -- hit events are only routed to the objects they belong to (routing)
        , oeLogic = if k `elem` hs then Event () else Yampa.NoEvent
        }, obj)

hits :: [(ILKey, State)] -> [ILKey]
hits kooss = concat (hitsAux kooss)
  where
    hitsAux [] = []
    -- Check each object 'State' against each other
    hitsAux ((k,oos):kooss) =
        [ [k, k'] | (k', oos') <- kooss, oos `hit` oos' ]
        ++ hitsAux kooss

    hit :: State -> State -> Bool
    (Rectangle p1 _ _) `hit` (Rectangle p2 _ _) = p1 == p2
    _ `hit` _ = False

killAndSpawn :: ((Input, IL ObjOutput), IL ObjOutput)
             -> Yampa.Event (IL Object -> IL Object)
killAndSpawn ((input, _), oos) =
    if any checkEscKey input
        then Event (\_ -> emptyIL) -- kill all 'State' on [Esc] => quit
        else foldl (mergeBy (.)) noEvent events
  where
    events :: [Yampa.Event (IL Object -> IL Object)]
    events = [ mergeBy (.)
                      (ooKillRequest oo `tag` (deleteIL k))
                      (fmap  (foldl (.) id . map insertIL_)
                             (ooSpawnRequests oo))
             | (k, oo) <- assocsIL oos ]
    checkEscKey (SDL.KeyDown (SDL.Keysym SDL.SDLK_ESCAPE  _ _)) = True
    checkEscKey _ = False


-- objects ----------
type Point = Point Double Double
-- type Color = Color Double Double Double
type Color1 = Color
type Rect  = Rect Point Point

playerObject :: Position2 -> Rect -> Color -> Object
playerObject p0 rect color = proc objEvents -> do
    -- .+^ is Point-Vector-addition
    -- ^+^ is Vector-Vector addition
    -- here we sum up all vectors based on the possibly multiple
    -- user inputs, thus allowing diagonal moves
    p <- (p0 .+^) ^<< integral -<
        foldl (^+^) (vector2 0 0) $ mapMaybe checkKey (oeInput objEvents)
    returnA -< defaultObjOutput { ooState = Rectangle p rect color }
    where
        checkKey (SDL.KeyUp (SDL.Keysym SDL.SDLK_UP    _ _)) =
            Just $ vector2    0 (-32)
        checkKey (SDL.KeyUp (SDL.Keysym SDL.SDLK_LEFT  _ _)) =
            Just $ vector2 (-32)   0
        checkKey (SDL.KeyUp (SDL.Keysym SDL.SDLK_DOWN  _ _)) =
            Just $ vector2    0   32
        checkKey (SDL.KeyUp (SDL.Keysym SDL.SDLK_RIGHT _ _)) =
            Just $ vector2   32    0
        checkKey _ = Nothing

staticObject :: Position2 -> Rect -> Color -> Object
staticObject p0 rect color = proc objEvents -> do
    returnA -< defaultObjOutput { ooState         = Rectangle p0 rect color
                                , ooKillRequest   = (oeLogic objEvents)
                                , ooSpawnRequests = (debugIfKilled objEvents)
                                }
  where
    debugIfKilled objEvents =
        case (oeLogic objEvents) of
            Yampa.Event () -> Event [debugObject "hit"]
            _              -> Event []

debugObject :: String -> Object
debugObject s = proc objEvents -> do
    returnA -< defaultObjOutput { ooState       = Debug s
                                , ooKillRequest = Event ()
                                }
