module Options
    ( Options (..)
    , options
    ) where

import           Data.Semigroup      ((<>))
import           Options.Applicative

data Options = Options
    { fullscreenMode :: !Bool
    } deriving Show

options :: IO Options
options = execParser parserInfo

parserInfo :: ParserInfo Options
parserInfo =
    info (parser <**> helper)
        ( fullDesc
        <> progDesc "Terrain Visualization"
        <> header "outdoor-terrain - a simple 3D terrain simulation"
        )

parser :: Parser Options
parser = Options
    <$> switch (
            long "fullscreen"
         <> short 'f'
         <> help "Run in fullscreen mode"
    )
