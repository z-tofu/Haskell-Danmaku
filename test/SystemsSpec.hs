module SystemsSpec (spec) where

import Test.Hspec
import Apecs
import Linear (V2(..))

import Components
import Systems.Movement (moveSystem)
import Systems.Collision (collisionSystem)
import Systems.Despawn (despawnSystem)

spec :: Spec
spec = do
  describe "Movement System" $ do
    it "updates position based on velocity" $ do
      w <- initWorld
      runWith w $ do
        ent <- newEntity (Position (V2 0 0), Velocity (V2 10 (-5)))
        moveSystem
        Position (V2 x y) <- get ent
        liftIO $ do
          x `shouldBe` 10
          y `shouldBe` (-5)

  describe "Collision System" $ do
    it "damages enemy when hit by player bullet" $ do
      w <- initWorld
      runWith w $ do
        -- Create enemy
        enemyEnt <- newEntity (Enemy, Position (V2 0 0), EnemyHealth 10)
        -- Create bullet hitting the enemy
        bulletEnt <- newEntity (PlayerBullet, Position (V2 0 0))
        
        collisionSystem
        
        EnemyHealth newHp <- get enemyEnt
        liftIO $ newHp `shouldBe` 9
        
        -- Check if bullet is destroyed
        hasBullet <- exists bulletEnt (Proxy :: Proxy PlayerBullet)
        liftIO $ hasBullet `shouldBe` False

    it "kills enemy when health reaches 0" $ do
      w <- initWorld
      runWith w $ do
        enemyEnt <- newEntity (Enemy, Position (V2 0 0), EnemyHealth 1)
        bulletEnt <- newEntity (PlayerBullet, Position (V2 0 0))
        
        collisionSystem
        
        hasEnemy <- exists enemyEnt (Proxy :: Proxy EnemyHealth)
        liftIO $ hasEnemy `shouldBe` False

    it "respawns player when hit by enemy bullet" $ do
      w <- initWorld
      runWith w $ do
        playerEnt <- newEntity (Player, Position (V2 0 0))
        bulletEnt <- newEntity (EnemyBullet, Position (V2 0 0))
        
        collisionSystem
        
        Position (V2 px py) <- get playerEnt
        liftIO $ do
          px `shouldBe` 400
          py `shouldBe` 400
        
        hasBullet <- exists bulletEnt (Proxy :: Proxy EnemyBullet)
        liftIO $ hasBullet `shouldBe` False

  describe "Despawn System" $ do
    it "despawns enemies that go out of bounds" $ do
      w <- initWorld
      runWith w $ do
        enemyEnt <- newEntity (Enemy, Position (V2 1000 1000), Velocity (V2 0 0), EnemyHealth 10, EnemyFireRate 0)
        despawnSystem
        hasEnemy <- exists enemyEnt (Proxy :: Proxy Enemy)
        liftIO $ hasEnemy `shouldBe` False

    it "does not despawn enemies inside bounds" $ do
      w <- initWorld
      runWith w $ do
        enemyEnt <- newEntity (Enemy, Position (V2 400 200), Velocity (V2 0 0), EnemyHealth 10, EnemyFireRate 0)
        despawnSystem
        hasEnemy <- exists enemyEnt (Proxy :: Proxy Enemy)
        liftIO $ hasEnemy `shouldBe` True
    
    it "despawn player out of bounds" $ do 
      w <- initWorld 
      runWith w $ do 
        playerEnt <- newEntity (Player, Position (V2 1000 1000), Velocity (V2 0 0), PlayerFireRate 10)
        despawnSystem
        hasPlayer <- exists playerEnt (Proxy :: Proxy Player) 
        liftIO $ hasPlayer `shouldBe` False
