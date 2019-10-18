{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE DeriveGeneric      #-}

module Lib
    ( someFunc
    ) where

import           Cardano.Prelude hiding (option)

import           Text.Show.Pretty (pPrint)
import           Data.Monoid.Generic
import           Options.Applicative

someFunc :: IO ()
someFunc = do

    cliValues <- execParser opts

    putTextLn "==========================================="
    pPrint mainnetPresets
    putTextLn "==========================================="

    putTextLn "==========================================="
    pPrint cliValues
    putTextLn "==========================================="

    let finalValues :: CardanoConfiguration
        finalValues = mainnetPresets <> cliValues

    putTextLn "==========================================="
    pPrint finalValues
    putTextLn "==========================================="

  where
    opts :: ParserInfo CardanoConfiguration
    opts = info (parseCardanoConfiguration <**> helper)
        ( fullDesc
        <> progDesc "Tool for testing configuration"
        <> header "configuration-presentation"
        )

-- Level 0, top-level type

data CardanoConfiguration = CardanoConfiguration
    { coreConfiguration :: !CoreConfiguration
    , ntpConfiguration  :: !NTPConfiguration
    } deriving (Eq, Show, Generic)
    deriving Semigroup via GenericSemigroup CardanoConfiguration
    deriving Monoid    via GenericMonoid CardanoConfiguration

-- Level 1

data CoreConfiguration = CoreConfiguration
    { ccGenesisFile :: !(Last FilePath)
    , ccGenesisHash :: !(Last Text)
    , ccNodesConfig :: !(Last NodesConfig) -- Level 2
    } deriving (Eq, Show, Generic)
    deriving Semigroup via GenericSemigroup CoreConfiguration
    deriving Monoid    via GenericMonoid CoreConfiguration

data NTPConfiguration = NTPConfiguration
    { ntpResponseTimeout :: !(Last Int)
    , ntpPollDelay       :: !(Last Int)
    , ntpServers         :: !(Last [Text]) -- Somewhat specific.
    } deriving (Eq, Show, Generic)
    deriving Semigroup via GenericSemigroup NTPConfiguration
    deriving Monoid    via GenericMonoid NTPConfiguration

-- Level 2

data NodesConfig = NodesConfig
    { ncNodeId       :: !(Last Int)
    , ncNumCoreNodes :: !(Last Int)
    } deriving (Eq, Show, Generic)
    deriving Semigroup via GenericSemigroup NodesConfig
    deriving Monoid    via GenericMonoid NodesConfig

mainnetPresets :: CardanoConfiguration
mainnetPresets = CardanoConfiguration
    { coreConfiguration =
        CoreConfiguration
            { ccGenesisFile = Last . Just $ "mainnet-genesis.json"
            , ccGenesisHash = Last . Just $ "5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb"
            , ccNodesConfig =
                Last . Just $ NodesConfig
                    { ncNodeId          = Last . Just $  0
                    , ncNumCoreNodes    = Last . Just $  3
                    }
            }
    , ntpConfiguration =
        NTPConfiguration
          { ntpResponseTimeout = Last . Just $  30000000
          , ntpPollDelay       = Last . Just $  1800000000
          , ntpServers         = Last . Just $
                [ "0.pool.ntp.org"
                , "2.pool.ntp.org"
                , "3.pool.ntp.org"
                ]
          }
    }

-- CLI

-- | Lift the parser to an optional @Last@ type.
lastOption :: Parser a -> Parser (Last a)
lastOption parser = Last <$> optional parser

-- | General @Last@ auto option from @Read@ instance.
lastAutoOption :: Read a => Mod OptionFields a -> Parser (Last a)
lastAutoOption args = lastOption (option auto args)

-- | Specific @Last@ @Int@ option.
lastIntOption :: Mod OptionFields Int -> Parser (Last Int)
lastIntOption = lastAutoOption

-- | Specific @Last@ @Integer@ option.
lastIntegerOption :: Mod OptionFields Integer -> Parser (Last Integer)
lastIntegerOption = lastAutoOption

-- | Specific @Last@ @Double@ option.
lastDoubleOption :: Mod OptionFields Double -> Parser (Last Double)
lastDoubleOption = lastAutoOption

-- | Specific @Last@ @Bool@ option.
lastBoolOption :: Mod OptionFields Bool -> Parser (Last Bool)
lastBoolOption = lastAutoOption

-- | Specific @Last@ @[Text]@ option.
lastTextListOption :: Mod OptionFields [Text] -> Parser (Last [Text])
lastTextListOption = lastAutoOption

-- | Specific @Last@ @IsString a@ option.
lastStrOption :: IsString a => Mod OptionFields a -> Parser (Last a)
lastStrOption args = Last <$> optional (strOption args)

-- | Nodes config parser.
-- https://hackage.haskell.org/package/optparse-applicative-0.15.1.0/docs/Options-Applicative.html
parseNodesConfig :: Parser NodesConfig
parseNodesConfig =
    NodesConfig
        <$> lastIntOption
            (  long "node-id"
            <> metavar "INTEGER"
            <> help "Number of the node."
            )
        <*> lastIntOption
            (  long "num-core-nodes"
            <> metavar "INTEGER"
            <> help "The number of the core nodes."
            )

-- | NTP configuration parser.
parseNTPConfiguration :: Parser NTPConfiguration
parseNTPConfiguration =
    NTPConfiguration
        <$> lastIntOption
            (  long "response-timeout"
            <> metavar "INTEGER"
            <> help "NTP max response timeout."
            )
        <*> lastIntOption
            (  long "poll-delay"
            <> metavar "INTEGER"
            <> help "Poll delay between requests."
            )
        <*> lastTextListOption
            (  long "servers"
            <> metavar "[TEXT]"
            <> help "List of servers."
            )

-- | Core configuration parser.
parseCoreConfiguration :: Parser CoreConfiguration
parseCoreConfiguration =
    CoreConfiguration
        <$> lastStrOption
            (  long "genesis-file"
            <> metavar "FILEPATH"
            <> help "Filepath to the genesis file."
            )
        <*> lastStrOption
            (  long "genesis-hash"
            <> metavar "TEXT"
            <> help "The hash of the genesis."
            )
        <*> (Last . Just <$> parseNodesConfig)

-- | Parse cardano configuration.
parseCardanoConfiguration :: Parser CardanoConfiguration
parseCardanoConfiguration =
    CardanoConfiguration
        <$> parseCoreConfiguration
        <*> parseNTPConfiguration

