
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields     #-}

module Lib
    ( someFunc
    ) where

import qualified GitHub as GH
import qualified Servant.Client               as SC
import           System.Environment           (getArgs)
import Data.Text ( Text, pack, unpack )
import Data.List (intercalate, groupBy, sortBy)
import Data.Either ( partitionEithers )
import           Servant.API                (BasicAuthData (..))
import Data.ByteString.UTF8 (fromString)

someFunc :: IO ()
someFunc = do
  putStrLn "Let's try a GitHubCall"
  (rName:user:token:_) <- getArgs
  putStrLn $ "name is " ++ rName
  putStrLn $ "github account for API call is " ++ user
  putStrLn $ "github token for api call is " ++ token

  let auth = BasicAuthData (fromString user) (fromString token)

  testGitHubCall auth $ pack rName
  putStrLn "end."

testGitHubCall :: BasicAuthData -> Text -> IO ()
testGitHubCall auth name = 
  GH.runClientM (GH.getUser (Just "haskell-app") auth name) >>= \case

    Left err -> do
      putStrLn $ "heuston, we have a problem: " ++ show err
    Right res -> do
      putStrLn $ "the votes of the github jury are " ++ show res

      -- now lets get the users repositories. Note this is now running paged cass.
      GH.runClientPagedM (GH.getUserRepos (Just "haskell-app") auth name) >>= \case
        Left err -> do
          putStrLn $ "heuston, we have a problem (gettign repos): " ++ show err
        Right repos -> do
          putStrLn $ " repositories are:" ++
            intercalate ", " (map (\(GH.GitHubRepo n _ _ ) -> unpack n) repos)

          -- now lets get the full list of collaborators from repositories
          (partitionEithers <$> mapM (getContribs auth name) repos) >>= \case

            ([], contribs) ->
              putStrLn $ " contributors are: " ++
              (intercalate "\n\t" .
               map (\(GH.RepoContributor n c) -> "[" ++ show n ++ "," ++ show c ++ "]") .
               groupContributors $ concat contribs)

            (ers, _)-> do
              putStrLn $ "heuston, we have a problem (getting contributors): " ++ show ers

  where getContribs :: BasicAuthData -> GH.Username -> GH.GitHubRepo -> IO (Either SC.ClientError [GH.RepoContributor])
        getContribs auth name (GH.GitHubRepo repo _ _) =
          GH.runClientPagedM (GH.getRepoContribs (Just "haskell-app") auth name repo)

        groupContributors :: [GH.RepoContributor] -> [GH.RepoContributor]
        groupContributors  = sortBy (\(GH.RepoContributor _ c1) (GH.RepoContributor _ c2) -> compare c1 c2) .
                             map mapfn .
                             groupBy (\(GH.RepoContributor l1 _) (GH.RepoContributor l2 _) -> l1 == l2)
         where mapfn :: [GH.RepoContributor] -> GH.RepoContributor

               mapfn xs@((GH.RepoContributor l _):_) = GH.RepoContributor l .sum $
                                                       map (\(GH.RepoContributor _ c) -> c)  xs








