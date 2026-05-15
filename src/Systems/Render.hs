module Systems.Render (drawSystem) where 


import Raylib.Core.Shapes (drawCircle)
import Raylib.Util.Colors (maroon, skyBlue, orange, darkGreen)
import Apecs
import Linear (V2(..))
import Components



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


