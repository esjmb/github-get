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
{-# LANGUAGE DuplicateRecordFields     #-}

module Lib
    ( someFunc
    ) where

import qualified GitHub as GH
import qualified Servant.Client               as SC
import           Network.HTTP.Client          (newManager)
import           Network.HTTP.Client.TLS      (tlsManagerSettings)
import           System.Environment           (getArgs)
import Data.Text hiding (map,intercalate, groupBy, concat)
import Data.List (intercalate, groupBy, sortBy)
import Data.Either

someFunc :: IO ()
someFunc = do
  putStrLn "Let's try a GitHubCall"
  (rName:_) <- getArgs
  putStrLn $ "name is " ++ rName
  
  testGitHubCall $ pack rName
  putStrLn "end."


testGitHubCall :: Text -> IO ()
testGitHubCall name = 
  (SC.runClientM (GH.getUser (Just "haskell-app") name) =<< env) >>= \case

    Left err -> do
      putStrLn $ "heuston, we have a problem: " ++ show err
    Right res -> do
      putStrLn $ "the votes of the github jury are " ++ show res
      
      -- now lets get the users repositories
      (SC.runClientM (GH.getUserRepos (Just "haskell-app") name) =<< env) >>= \case
        Left err -> do
          putStrLn $ "heuston, we have a problem (gettign repos): " ++ show err
        Right repos -> do
          putStrLn $ " repositories are:" ++
            intercalate ", " (map (\(GH.GitHubRepo n _ _ ) -> unpack n) repos)

          -- now lets get the full list of collaborators from repositories
          partitionEithers <$> mapM (getContribs name) repos >>= \case

            ([], contribs) ->
              putStrLn $ " contributors are: " ++
              (intercalate "\n\t" .
               map (\(GH.RepoContributor n c) -> "[" ++ show n ++ "," ++ show c ++ "]") .
               groupContributors $ concat contribs)

            (ers, _)-> do
              putStrLn $ "heuston, we have a problem (getting contributors): " ++ show ers
                
           
     
  where env :: IO SC.ClientEnv
        env = do
          manager <- newManager tlsManagerSettings
          return $ SC.mkClientEnv manager (SC.BaseUrl SC.Http "api.github.com" 80 "")

        getContribs :: GH.Username -> GH.GitHubRepo -> IO (Either SC.ClientError [GH.RepoContributor])
        getContribs name (GH.GitHubRepo repo _ _) =
          SC.runClientM (GH.getRepoContribs (Just "haskell-app") name repo) =<< env

        groupContributors :: [GH.RepoContributor] -> [GH.RepoContributor]
        groupContributors  = sortBy (\(GH.RepoContributor _ c1) (GH.RepoContributor _ c2) ->  compare c1 c2) .
                             map mapfn .
                             groupBy (\(GH.RepoContributor l1 _) (GH.RepoContributor l2 _) ->  l1 == l2)
         where mapfn :: [GH.RepoContributor] -> GH.RepoContributor
               mapfn xs@((GH.RepoContributor l _):_) = GH.RepoContributor l . sum $ 
                                                       map (\(GH.RepoContributor _ c) -> c)  xs
               
              
                
          



       
