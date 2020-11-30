{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module GitHub where

import Data.Aeson ( FromJSON )
import Data.Proxy ( Proxy(..) )
import Data.Text ( Text )
import GHC.Generics ( Generic )
import qualified Servant.Client               as SC
import Servant.API
    ( type (:<|>)(..),
      BasicAuth,
      BasicAuthData,
      Capture,
      JSON,
      Header,
      type (:>),
      Get )
import Servant.Client ( client, ClientM )
import qualified Data.HashMap as HM
import Data.List.Split  (splitOn)
import Data.ByteString.UTF8 (toString)
import           Network.HTTP.Client          (newManager)
import           Network.HTTP.Client.TLS      (tlsManagerSettings)
import Servant.API.ResponseHeaders
import Servant.API.QueryParam (QueryParam)
import Data.String (IsString)

-- a type i created to wrap up paged requests
type GetPaged a b = QueryParam "page" String :> Get a (GitHubPaged b)

type Username  = Text
type UserAgent = Text
type Reponame  = Text

data GitHubUser =
  GitHubUser { login :: Text
             , name  :: Text
             , email :: Maybe Text
             } deriving (Generic, FromJSON, Show)

data GitHubRepo =
  GitHubRepo { name :: Text
             , full_name :: Maybe Text
             , language :: Maybe Text
             } deriving (Generic, FromJSON, Show)

data RepoContributor =
  RepoContributor { login :: Text
                  , contributions :: Integer
                  } deriving (Generic, FromJSON, Show)

type GitHubAPI = "users" :> Header  "user-agent" UserAgent
                         :> BasicAuth "github" Int
                         :> Capture "username" Username
                         :> Get '[JSON] GitHubUser

            :<|> "users" :> Header  "user-agent" UserAgent
                         :> BasicAuth "github" Int
                         :> Capture "username" Username
                         :> "repos"
                         :> GetPaged '[JSON] [GitHubRepo]

            :<|> "repos" :> Header  "user-agent" UserAgent
                         :> BasicAuth "github" Int
                         :> Capture "username" Username
                         :> Capture "repo"     Reponame
                         :> "contributors"
                         :> GetPaged '[JSON] [RepoContributor]

            -- This call has been implemented to return link Headers so that we can gather multipage responses
            :<|> "repositories" :> Header  "user-agent" UserAgent
                                :> BasicAuth "github" Int
                                :> GetPaged '[JSON] [GitHubRepo]

gitHubAPI :: Proxy GitHubAPI
gitHubAPI = Proxy



type GitHubPaged a = Headers '[Header "link" Text, Header "X-Ratelimit-Remaining" Int] a
type ClientMPaged a = Maybe String -> ClientM (GitHubPaged a)

getUser ::          Maybe UserAgent -> BasicAuthData -> Username            -> ClientM GitHubUser
getUserRepos ::     Maybe UserAgent -> BasicAuthData -> Username            -> ClientMPaged [GitHubRepo]
getRepoContribs ::  Maybe UserAgent -> BasicAuthData -> Username -> Reponame -> ClientMPaged [RepoContributor]
getRepositories ::  Maybe Text      -> BasicAuthData                       -> ClientMPaged [GitHubRepo]

getUser :<|> getUserRepos :<|> getRepoContribs :<|> getRepositories = client gitHubAPI

-- run a GitHub API call where teh result is pagnated
runClientPagedM fn = recursiveCall "1"
 where
   recursiveCall page = do
    putStrLn $ "running runClientPagedM " ++ page
    (SC.runClientM (fn $ Just page)=<< env) >>= \case
     Left e -> return $ Left e -- silently return error if there is one
     Right (Headers rs hs) ->
           -- the following is a bit of a hack but does the job extracting nextPage link, and can be used
           -- to extract other links also, as it gets all links returned by the links header field, before
           -- extracting the "next" link page
       let getLnk xs = let (a:b:_) = splitOn ";" xs
                           clean s = filter (not .( `elem` ['>','"'])) .head .tail .splitOn s
                       in (clean "rel=" b, clean "page=" a)
           nextPage = (HM.lookup "link" .HM.fromList $ getHeaders hs) >>= 
                      foldr ((\(n,v) a -> case a of
                                           Nothing -> if n == "next" then Just v else Nothing
                                           x -> x) .getLnk) Nothing .splitOn "," .toString
       in case nextPage of
         Just x -> recursiveCall x >>= \case
           Right rs' -> return .Right $ rs ++ rs'
           e -> return e
         Nothing -> return $ Right rs
     
-- runa github API call ignoring pagination if any
runClientM :: forall a. ClientM a -> IO (Either SC.ClientError a)
runClientM fn = SC.runClientM fn =<< env

env :: IO SC.ClientEnv
env = do
  manager <- newManager tlsManagerSettings
  return $ SC.mkClientEnv manager (SC.BaseUrl SC.Http "api.github.com" 80 "")
