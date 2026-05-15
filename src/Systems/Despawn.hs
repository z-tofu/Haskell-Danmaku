module Systems.Despawn (despawnSystem) where 

import Apecs
import Linear (V2(..))
import Components
import Control.Monad (when)

isOutOfBounds :: V2 Float -> Bool
isOutOfBounds (V2 x y) = x < -100 || x > 900 || y < -100 || y > 550

despawnSystem :: SystemT World IO ()
despawnSystem = do 
   cmapM_ $ \(Enemy, Position pos, ent) -> do 
      when (isOutOfBounds pos) $ do 
         destroy ent (Proxy :: Proxy (Enemy, Position, Velocity, EnemyHealth, EnemyFireRate))
   
   cmapM_ $ \(EnemyBullet, Position pos, ent) -> do 
      when (isOutOfBounds pos) $ do 
         destroy ent (Proxy :: Proxy (EnemyBullet, Position, Velocity))

   cmapM_ $ \(PlayerBullet, Position pos, ent) -> do 
      when (isOutOfBounds pos) $ do 
         destroy ent (Proxy :: Proxy (PlayerBullet, Position, Velocity))
