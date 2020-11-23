{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}

module Lib
    ( someFunc
    ) where

import qualified GitHub as GH
import qualified Servant.Client               as SC
import           Network.HTTP.Client          (defaultManagerSettings,
                                               newManager)
                 
someFunc :: IO ()
someFunc = do
  putStrLn "Let's try a GitHubCall"
  testGitHubCall
  putStrLn "end."


testGitHubCall :: IO ()
testGitHubCall = 
  (SC.runClientM GH.test =<< env) >>= \case

    Left err -> do
      putStrLn $ "heuston, we have a problem: " ++ show err
    Right res -> do
      putStrLn $ "the votes of the github jury are " ++ show res
      

  where env :: IO SC.ClientEnv
        env = do
          manager <- newManager defaultManagerSettings
          return $ SC.mkClientEnv manager (SC.BaseUrl SC.Http "api.github.com" 80 "")
