{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module User.Actions.Battle where

import System.Random (randomRIO)

data Golem = Golem { gAttack :: Int, gHp :: Int } deriving Show
data Player = Player { pAttack :: Int, pHp :: Int } deriving (Show)
data Battle = Fight | RunAway deriving (Show, Read, Eq)

battle :: (Int,Int) -> (Int,Int) -> IO Bool
battle (youHealth, youAttack) (golHealth, golAttack) = do 
    putStrLn "You have encountered a Golem! Choose your action: Fight or RunAway?"
    putStrLn $ "The Golem has " ++ show golHealth ++ " health and " ++ show golAttack ++ " attack points." 
    putStrLn $ "You have " ++ show youHealth ++ " health and " ++ show youAttack ++ " attack points." 
    putStrLn "Choose your action: Fight or RunAway"
    selectedAction <- getLine
    let selectedAction2 =  (read @Battle selectedAction)
    -- putStrLn $ show selectedAction2
    if selectedAction2 == Fight then fightLoop else return False 

    where
        fightLoop :: IO Bool 
        fightLoop = do
          golemLoose <- randomRIO @Int (-30,0)
          youLoose <- randomRIO @Int (-30,0)
          let youHealth = youHealth + youLoose 
          let golemHealth = golemHealth + golemLoose 
          if youHealth > 0 then fightLoop else return True
    -- return True

-- battle' :: IO Bool

-- battle' = putStrLn "You have encountered a Golem! Choos your action: Fight or RunAway" >>= \_ -> 
--     getLine                         >>= \selectedAction ->
--     putStrLn $ show selectedAction  >>= \_ ->
--     return True

        -- 

    -- selectedAction2 <-  (read selectedAction :: Battle)
                