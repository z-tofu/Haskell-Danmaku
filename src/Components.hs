{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Components where 

import Apecs 
import Linear (V2)


newtype GameTicks = GameTicks Int deriving (Show, Eq, Num)
instance Component GameTicks where type Storage GameTicks = Global GameTicks

instance Semigroup GameTicks where 
   GameTicks a <> GameTicks b = GameTicks (a + b)
instance Monoid GameTicks where 
   mempty = GameTicks 0

data GameStage = Stage1 | Stage2 | BossFight deriving (Show, Eq)
instance Component GameStage where type Storage GameStage = Global GameStage

instance Semigroup GameStage where
  _ <> b = b
instance Monoid GameStage where
  mempty = Stage1

data Enemy = Enemy deriving Show
instance Component Enemy where type Storage Enemy = Map Enemy

data EnemyBullet = EnemyBullet deriving Show
instance Component EnemyBullet where type Storage EnemyBullet = Map EnemyBullet

data Player = Player deriving Show
instance Component Player where type Storage Player = Unique Player

data PlayerBullet = PlayerBullet deriving Show
instance Component PlayerBullet where type Storage PlayerBullet = Map PlayerBullet

newtype Position = Position (V2 Float) deriving Show
instance Component Position where type Storage Position = Map Position

newtype Velocity = Velocity (V2 Float) deriving Show
instance Component Velocity where type Storage Velocity= Map Velocity

newtype PlayerFireRate = PlayerFireRate Int deriving Show
instance Component PlayerFireRate where type Storage PlayerFireRate = Map PlayerFireRate

newtype EnemyFireRate = EnemyFireRate Int deriving Show
instance Component EnemyFireRate where type Storage EnemyFireRate = Map EnemyFireRate

newtype EnemyHealth = EnemyHealth Int deriving Show 
instance Component EnemyHealth where type Storage EnemyHealth = Map EnemyHealth


-- makeWorld
makeWorld "World" [''Position, ''Velocity, ''Player, ''EnemyBullet, ''PlayerBullet, ''PlayerFireRate, ''Enemy, ''EnemyHealth, ''EnemyFireRate, ''GameStage, ''GameTicks]


