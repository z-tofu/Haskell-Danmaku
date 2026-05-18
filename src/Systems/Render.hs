module Systems.Render (drawSystem) where 

import Raylib.Core.Shapes (drawCircle)
import Raylib.Util.Colors (maroon, skyBlue, orange, darkGreen, white)
import Raylib.Core.Textures (drawTexture)
import Apecs
import Linear (V2(..))
import Components

import Systems.LoadSprite (Assets(..))



drawSystem :: Assets -> SystemT World IO ()
drawSystem assets = do 
   cmapM_ $ \(EnemyBullet, Position (V2 x y)) -> 
      liftIO $ drawCircle (round x) (round y) 4 maroon
   
   cmapM_ $ \(Player, Position (V2 x y)) -> liftIO $ do 
      let spriteWidth = 32
          spriteHeight = 32
          offsetX = round x - (spriteWidth `div` 2)
          offsetY = round y - (spriteHeight `div` 2)
      drawTexture (playerSprite assets) offsetX offsetY white
      drawCircle (round x) (round y) 6 skyBlue

   cmapM_ $ \(PlayerBullet, Position (V2 x y)) ->
      liftIO $ drawCircle (round x) (round y) 3 orange

   cmapM_ $ \(Enemy, Position (V2 x y)) ->
      liftIO $ drawCircle (round x) (round y) 6 darkGreen


