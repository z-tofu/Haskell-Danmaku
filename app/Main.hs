{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Raylib.Core (initWindow, windowShouldClose, closeWindow, setTargetFPS, beginDrawing, endDrawing, clearBackground, isKeyDown)
import Raylib.Core.Shapes (drawCircle)
import Raylib.Util.Colors (rayWhite, maroon, skyBlue, orange, darkGreen)
import Raylib.Types (KeyboardKey(..))
import Apecs
import Linear (V2(..), normalize, (^*), distance)
import Control.Monad (forM_)
import Control.Monad (when)



data Enemy = Enemy deriving Show
instance Component Enemy where type Storage Enemy = Map Enemy

data EnemyBullet = EnemyBullet deriving Show
instance Component EnemyBullet where type Storage EnemyBullet = Map EnemyBullet

data Player = Player deriving Show
instance Component Player where type Storage Player = Unique Player

data PlayerBullet = PlayerBullet deriving Show
instance Component PlayerBullet where type Storage PlayerBullet = Map PlayerBullet

newtype Position = Position (V2 Float) deriving Show
instance Component Position where type Storage Position = Map Position

newtype Velocity = Velocity (V2 Float) deriving Show
instance Component Velocity where type Storage Velocity= Map Velocity

newtype PlayerFireRate = PlayerFireRate Int deriving Show
instance Component PlayerFireRate where type Storage PlayerFireRate = Map PlayerFireRate

newtype EnemyFireRate = EnemyFireRate Int deriving Show
instance Component EnemyFireRate where type Storage EnemyFireRate = Map EnemyFireRate

newtype EnemyHealth = EnemyHealth Int deriving Show 
instance Component EnemyHealth where type Storage EnemyHealth = Map EnemyHealth


-- makeWorld
makeWorld "World" [''Position, ''Velocity, ''Player, ''EnemyBullet, ''PlayerBullet, ''PlayerFireRate, ''Enemy, ''EnemyHealth, ''EnemyFireRate]


spawnCircle :: Int -> V2 Float -> Float -> SystemT World IO ()
spawnCircle count pos speed = do 
   let step = (2 * pi) / fromIntegral count
   forM_ [0..count-1] $ \i -> do 
      let angle = fromIntegral i * step
          vel = V2 (cos angle) (sin angle) ^* speed
      newEntity_ (EnemyBullet, Position pos, Velocity vel)


enemyAction :: SystemT World IO ()
enemyAction = cmapM $ \(Enemy, Velocity _, Position pos, EnemyFireRate cd) -> do 
   when (cd <= 0) $ do 
      spawnCircle 12 pos 2.8
   let newCd = if cd <= 0 then 60 else max 0 (cd - 1) -- number in then block controls enemy firerate
   return $ EnemyFireRate newCd

collisionSystem :: SystemT World IO ()
collisionSystem = do 
   cmapM_ $ \(Player, Position pPos, playerEnt) -> do 
      cmapM_ $ \(EnemyBullet, Position bPos, bulletEnt) -> do 
         let d = distance pPos bPos
         when (d < 7) $ do 
            liftIO $ putStrLn "Pichuun!!"
            set playerEnt (Position (V2 400 400))
            destroy bulletEnt (Proxy :: Proxy EnemyBullet)
   
   cmapM_ $ \(PlayerBullet, Position bPos, bulletEnt) -> do 
      cmapM_ $ \(Enemy, Position ePos, EnemyHealth health, enemyEnt) -> do 
         let d = distance bPos ePos 
         when (d < 15) $ do 
            destroy bulletEnt (Proxy :: Proxy PlayerBullet)
            let newHealth = health - 1 
            if newHealth <= 0 
               then do 
                  liftIO $ putStrLn "Enemy died"
                  destroy enemyEnt (Proxy :: Proxy (Enemy, EnemyHealth, Position))
               else do 
                  set enemyEnt (EnemyHealth newHealth)


moveSystem :: SystemT World IO ()
moveSystem = cmap $ \(Position (V2 x y), Velocity (V2 vx vy)) ->
  Position (V2 (x + vx) (y + vy))

-- Input
handleInput :: SystemT World IO ()
handleInput = cmapM $ \(Player, Velocity _, Position pos, PlayerFireRate cd) -> do 
    right <- liftIO $ isKeyDown KeyRight
    left  <- liftIO $ isKeyDown KeyLeft
    down  <- liftIO $ isKeyDown KeyDown
    up    <- liftIO $ isKeyDown KeyUp
    shift <- liftIO $ isKeyDown KeyLeftShift
    z     <- liftIO $ isKeyDown KeyZ

-- Player movement
    let speed = (if shift then 3 else 5)
        vx = (if right then 1 else 0) + (if left then -1 else 0)
        vy = (if down then 1 else 0) + (if up then -1 else 0)
        rawDir = V2 vx vy 

        finalVelocity = if rawDir == V2 0 0 then V2 0 0 else normalize rawDir ^* speed

-- Shooting
    when (z && cd <= 0) $
        newEntity_ (PlayerBullet, Position pos, Velocity (V2 0 (-8)))
    let newCd = if z && cd <= 0 then 10 else max 0 (cd - 1) -- Number in the then block controls firerate 

    return $ (Velocity finalVelocity, PlayerFireRate newCd)

drawSystem :: SystemT World IO ()
drawSystem = do 
   cmapM_ $ \(EnemyBullet, Position (V2 x y)) ->
      liftIO $ drawCircle (round x) (round y) 4 maroon
   
   cmapM_ $ \(Player, Position (V2 x y)) ->
      liftIO $ drawCircle (round x) (round y) 6 skyBlue

   cmapM_ $ \(PlayerBullet, Position (V2 x y)) ->
      liftIO $ drawCircle (round x) (round y) 3 orange

   cmapM_ $ \(Enemy, Position (V2 x y)) ->
      liftIO $ drawCircle (round x) (round y) 6 darkGreen

-- Game loop
gameLoop :: World -> Int -> IO ()
gameLoop world frameCount = do 
   runWith world $ do 
      handleInput
      moveSystem
      collisionSystem
      enemyAction

   beginDrawing
   clearBackground rayWhite
   runWith world drawSystem
   endDrawing

   shouldClose <- windowShouldClose
   if shouldClose then closeWindow Nothing else gameLoop world (frameCount + 1)


main :: IO ()
main = do 
   _ <- initWindow 800 450 "Haskell Danmaku"
   setTargetFPS 60

   w <- initWorld
   runWith w $ do 
      newEntity_ (Player, Position (V2 400 300), Velocity (V2 0 0), PlayerFireRate 0)
      newEntity_ (Enemy, Position (V2 400 100), Velocity (V2 0 0), EnemyHealth 10, EnemyFireRate 0)
  
   gameLoop w 0

