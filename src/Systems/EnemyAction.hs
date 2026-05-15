module Systems.EnemyAction (enemyAction) where

import Apecs
import Linear (V2(..), (^*))
import Components
import Control.Monad (forM_, when)

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
