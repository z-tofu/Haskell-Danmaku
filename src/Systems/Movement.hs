module Systems.Movement (moveSystem) where 

import Apecs
import Linear (V2(..))
import Components

moveSystem :: SystemT World IO ()
moveSystem = cmap $ \(Position (V2 x y), Velocity (V2 vx vy)) -> 
   Position (V2 (x + vx) (y + vy))
