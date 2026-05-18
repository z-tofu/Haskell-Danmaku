module Main (main) where

import Raylib.Core (initWindow, windowShouldClose, closeWindow, setTargetFPS, beginDrawing, endDrawing, clearBackground)
import Raylib.Util.Colors (rayWhite)
import Raylib.Core.Textures (loadTexture)
-- import Raylib.Types (Texture)
import Apecs
import Linear (V2(..))

import Components
import Constants
import Stage (stageSystem)
import Systems.Input (handleInput)
import Systems.Movement (moveSystem)
import Systems.Collision (collisionSystem)
import Systems.Render (drawSystem)
import Systems.EnemyAction (enemyAction)
import Systems.Despawn (despawnSystem)
import Systems.LoadSprite (Assets(..))

gameLoop :: Assets -> World -> IO ()
gameLoop assets world = do
  runWith world $ do
    handleInput
    moveSystem
    collisionSystem
    stageSystem     -- Handles timers and spawning
    enemyAction
    despawnSystem

  beginDrawing
  clearBackground rayWhite
  runWith world (drawSystem assets)
  endDrawing

  shouldClose <- windowShouldClose
  if shouldClose then closeWindow Nothing else gameLoop assets world

main :: IO ()
main = do
  _ <- initWindow screenWidth screenHeight "Haskell Danmaku"
  setTargetFPS 60

  assets <- Assets 
     <$> loadTexture "assets/sprite.png"

  w <- initWorld
  runWith w $ do
    -- Initialize Globals
    set global (GameTicks 0)
    set global Stage1
    -- Spawn Player
    newEntity_ (Player, Position (V2 400 300), Velocity (V2 0 0), PlayerFireRate 0)

  gameLoop assets w
