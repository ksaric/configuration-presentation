module Lib
    ( someFunc
    ) where

import           Cardano.Prelude hiding (option)

import           Options.Applicative

someFunc :: IO ()
someFunc = putTextLn "someFunc"

-- Level 0, top-level type

data CardanoConfiguration = CardanoConfiguration
    { coreConfiguration :: !CoreConfiguration
    , ntpConfiguration  :: !NTPConfiguration
    } deriving (Eq, Show)

-- Level 1

data CoreConfiguration = CoreConfiguration
    { ccGenesisFile :: !FilePath
    , ccGenesisHash :: !Text
    , ccNodesConfig :: !NodesConfig -- Level 2
    } deriving (Eq, Show)

data NTPConfiguration = NTPConfiguration
    { ntpResponseTimeout :: !Int
    , ntpPollDelay       :: !Int
    , ntpServers         :: ![Text]
    } deriving (Eq, Show)

-- Level 2

data NodesConfig = NodesConfig
    { ncNodeId       :: !Int
    , ncNumCoreNodes :: !Int
    } deriving (Eq, Show)


mainnetPresets :: CardanoConfiguration
mainnetPresets = CardanoConfiguration
    { coreConfiguration =
        CoreConfiguration
            { ccGenesisFile = "mainnet-genesis.json"
            , ccGenesisHash = "5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb"
            , ccNodesConfig =
                NodesConfig
                    { ncNodeId          = 0
                    , ncNumCoreNodes    = 3
                    }
            }
    , ntpConfiguration =
        NTPConfiguration
          { ntpResponseTimeout = 30000000
          , ntpPollDelay       = 1800000000
          , ntpServers         =
                [ "0.pool.ntp.org"
                , "2.pool.ntp.org"
                , "3.pool.ntp.org"
                ]
          }
    }

-- CLI

-- | Nodes config parser.
-- https://hackage.haskell.org/package/optparse-applicative-0.15.1.0/docs/Options-Applicative.html
parseNodesConfig :: Parser NodesConfig
parseNodesConfig =
    NodesConfig
        <$> option auto
            (  long "node-id"
            <> metavar "INTEGER"
            <> help "Number of the node."
            )
        <*> option auto
            (  long "num-core-nodes"
            <> metavar "INTEGER"
            <> help "The number of the core nodes."
            )




