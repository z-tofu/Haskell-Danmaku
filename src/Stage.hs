module Stage (stageSystem) where 

import Apecs
import Linear (V2(..))
import Components


spawnWaveEnemy :: V2 Float -> V2 Float -> Int -> Int -> SystemT World IO ()
spawnWaveEnemy pos vel hp cd = do
   newEntity_ (Enemy, Position pos, Velocity vel, EnemyHealth hp, EnemyCd cd, EnemyFireRate 0)

stageSystem :: SystemT World IO ()
stageSystem = do 
   GameTicks ticks <- get global
   set global (GameTicks (ticks + 1))

   stage <- get global 

   case stage of
      Stage1 -> stage1TimeLine ticks
      Stage2 -> return ()
      BossFight -> return ()

stage1TimeLine :: Int -> SystemT World IO ()
stage1TimeLine ticks = do
   case ticks of 
      60  -> spawnWaveEnemy (V2 200 (-20)) (V2 0 2) 5 60
      120 -> spawnWaveEnemy (V2 600 (-20)) (V2 (-1) 1) 5 120
      -- 300 -> do 
      --    spawnWaveEnemy (V2 300 (-20)) (V2 1 1) 3
      --    spawnWaveEnemy (V2 500 (-20)) (V2 (-1) 1) 3
      1200 -> set global BossFight
      _ -> return ()
