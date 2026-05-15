module Systems.Collision (collisionSystem) where 

import Apecs
import Linear (V2(..), distance)
import Components
import Control.Monad (when)

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


