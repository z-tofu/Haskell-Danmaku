{-# OPTIONS -Wno-missing-export-lists #-}

module Systems.EnemyAction (enemyAction) where

import Apecs
import Components
import Control.Monad (forM_, when)
import Linear (V2 (..), (^*))

spawnCircle :: Int -> V2 Float -> Float -> SystemT World IO ()
spawnCircle count pos speed = do
  let step = (2 * pi) / fromIntegral count
  forM_ [0 .. count - 1] $ \i -> do
    let angle = fromIntegral i * step
        vel = V2 (cos angle) (sin angle) ^* speed
    newEntity_ (EnemyBullet, Position pos, Velocity vel)


spawnSpinningRose :: Int -> Float -> Float -> V2 Float -> Float -> SystemT World IO ()
spawnSpinningRose count k rotationOffset pos speed = do
   let step = (2 * pi) / fromIntegral count
   forM_ [0..count-1] $ \i -> do
      let angle = (fromIntegral i * step) + rotationOffset
      let r = abs (5 * cos (k * angle))
          speed' = speed * abs r 
          minSpeed = 0.3
          vel = V2 (cos angle) (sin angle) ^* max minSpeed speed'
      newEntity_ (EnemyBullet, Position pos, Velocity vel)



enemyAction :: SystemT World IO ()
enemyAction = cmapM $ \(Enemy, Velocity _, Position pos, EnemyFireRate cd, EnemyCd fr) -> do
   when (cd <= 0) $ do
      -- spawnSpinningRose 120 5.0 5.0 pos 1.7
      spawnCircle 12 pos 2.8
   let newCd = if cd <= 0 then fr else max 0 (cd - 1) -- number in then block controls enemy firerate
   return $ EnemyFireRate newCd 
