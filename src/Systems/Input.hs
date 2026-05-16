module Systems.Input (handleInput) where 

import Constants (playerSlowSpeed, playerSpeed)

import Apecs
import Linear (V2(..), normalize, (^*))
import Components
import Raylib.Core (isKeyDown)
import Raylib.Types (KeyboardKey(..))
import Control.Monad (when)

handleInput :: SystemT World IO ()
handleInput = cmapM $ \(Player, Velocity _, Position pos, PlayerFireRate cd) -> do 
    right <- liftIO $ isKeyDown KeyRight
    left  <- liftIO $ isKeyDown KeyLeft
    down  <- liftIO $ isKeyDown KeyDown
    up    <- liftIO $ isKeyDown KeyUp
    shift <- liftIO $ isKeyDown KeyLeftShift
    z     <- liftIO $ isKeyDown KeyZ

-- Player movement
    let speed = (if shift then fromIntegral playerSlowSpeed else fromIntegral playerSpeed)
        vx = (if right then 1 else 0) + (if left then -1 else 0)
        vy = (if down then 1 else 0) + (if up then -1 else 0)
        rawDir = V2 vx vy 

        finalVelocity = if rawDir == V2 0 0 then V2 0 0 else normalize rawDir ^* speed

-- Shooting
    when (z && cd <= 0) $
        newEntity_ (PlayerBullet, Position pos, Velocity (V2 0 (-8)))
    let newCd = if z && cd <= 0 then 10 else max 0 (cd - 1) -- Number in the then block controls firerate 

    return $ (Velocity finalVelocity, PlayerFireRate newCd)


