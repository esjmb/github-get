{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module GitHub where 

import           Control.Monad       (mzero)
import           Data.Aeson
import           Data.Proxy
import           Data.Text
import           GHC.Generics
import           Network.HTTP.Client (defaultManagerSettings, newManager)
import           Servant.API
import           Servant.Client

type Username = Text
type UserAgent = Text

data GitHubUser =
  GitHubUser { login :: Text
             , name  :: Text
             , email :: Text
             } deriving (Generic, FromJSON, Show)

type GitHubAPI = "users" :> Header "user-agent" UserAgent 
                         :> Capture "username" Username  :> Get '[JSON] GitHubUser
            :<|> "test2" :> Get '[JSON] Text

gitHubAPI :: Proxy GitHubAPI
gitHubAPI = Proxy

test :: Maybe UserAgent -> Username -> ClientM GitHubUser
test2 :: ClientM Text

test :<|> test2 = client gitHubAPI
