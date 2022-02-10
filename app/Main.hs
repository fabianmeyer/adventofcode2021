module Main where

import RIO
import RIO.Partial
import qualified RIO.List as L
import qualified RIO.List.Partial as L
import System.IO (print, hGetContents)

data DepthMeasurementChange = Increased | Decreased deriving (Show, Eq)

main :: IO ()
main = withBinaryFile "C:\\Users\\fameyer\\Downloads\\aoc\\day1\\input.txt" ReadMode (\input -> do
        content <- hGetContents input
        let measurements = (\it -> read it :: Integer) <$> lines content
        let zipped = zip measurements (L.tail measurements)
        let differences = uncurry (-) <$> zipped
        let changes = (\diff -> if diff < 0 then Increased else Decreased) <$> differences
        let increases = length $ filter (Increased ==) changes

        let windows = L.zip3 measurements (L.tail measurements) (L.tail . L.tail $ measurements)
        let sums = (\(x, y, z) -> x + y + z) <$> windows
        let zipped' = zip sums (L.tail sums)
        let differences' = uncurry (-) <$> zipped'
        let changes' = (\diff -> if diff < 0 then Increased else Decreased) <$> differences'
        let increases' = length $ filter (Increased ==) changes'

        print increases'
    )

