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

type GitHubAPI = "users" :> Capture "username" Username  :> Get '[JSON] Text
            :<|> "test2" :> Get '[JSON] Text

gitHubAPI :: Proxy GitHubAPI
gitHubAPI = Proxy

test :: Username -> ClientM Text
test2 :: ClientM Text

test :<|> test2 = client gitHubAPI
