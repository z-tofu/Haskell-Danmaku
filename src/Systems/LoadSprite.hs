module Systems.LoadSprite (Assets(..)) where

import Raylib.Types (Texture)

data Assets = Assets 
   { playerSprite :: Texture}
