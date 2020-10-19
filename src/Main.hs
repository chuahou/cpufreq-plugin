-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

import           Data.List          (elemIndex)
import           System.Environment (getArgs)
import qualified System.Process     as P

-- | We store frequencies in terms of MHz.
-- 1 GHz = 1000 MHz
type FreqMhz = Int

-- | Governors are represented by strings of their names.
type Governor = String

-- | CPUs are represented by strings of their IDs.
type Cpu = String

-- | Increments to adjust frequency by in MHz.
freqIncr :: FreqMhz
freqIncr = 100

-- | Gets hardware limit through cpufreq-info.
getHardwareLimit :: IO FreqMhz
getHardwareLimit = process <$> P.readProcess "cpufreq-info" ["--hwlimits"] ""
    where process = (`div` 1000) . read . last . words

-- | Gets available governors through cpufreq-info.
getGovernors :: IO [Governor]
getGovernors = words <$> P.readProcess "cpufreq-info" ["-g"] ""

-- | Get current maximum frequency.
getCurrentFreq :: IO FreqMhz
getCurrentFreq = fst <$> getCurrentPolicy

-- | Get current governor.
getCurrentGovernor :: IO Governor
getCurrentGovernor = snd <$> getCurrentPolicy

-- | Get current policy.
getCurrentPolicy :: IO (FreqMhz, Governor)
getCurrentPolicy = do
    policy <- P.readProcess "cpufreq-info" ["-p"] "" >>= return . words
    if length policy == 3
       then return $ ((`div` 1000) . read $ policy !! 1, policy !! 2)
       else fail "Unknown format policy string!"

-- | Get available CPUs.
getCpus :: IO [Cpu]
getCpus = lines <$>
            P.readCreateProcess
                (P.shell $ "cpufreq-info | sed -n " <> sedString) ""
    where sedString = "'s/^.*analyzing CPU \\([0-9]\\+\\):.*$/\\1/p'"

-- | @setGovernor g c@ sets CPU @c@ to use governor @g@.
setGovernor :: Governor -> Cpu -> IO ()
setGovernor g c = P.callProcess "cpufreq-set" ["-g", g, "-c", c]

-- | @setFreq f c@ sets CPU @c@ to use max frequency @f@ MHz.
setFreq :: FreqMhz -> Cpu -> IO ()
setFreq f c = P.callProcess "cpufreq-set" ["--max", show f <> "MHz", "-c", c]

-- | Change to next available governor.
nextGovernor :: IO ()
nextGovernor = do
    curr <- getCurrentGovernor
    govs <- getGovernors
    case elemIndex curr govs of
      Just idx -> if (idx + 1 >= length govs)
                     then forAllCpus $ setGovernor (head govs)
                     else forAllCpus $ setGovernor (govs !! (idx + 1))
      Nothing -> forAllCpus $ setGovernor (head govs)

-- | @changeFreq f@ adjusts max frequency by @f@ MHz.
changeFreq :: FreqMhz -> IO ()
changeFreq f = do curr <- getCurrentFreq
                  forAllCpus $ setFreq (curr + f)

-- | @forAllCpus op@ runs @op@ on each CPU.
forAllCpus :: (Cpu -> IO ()) -> IO ()
forAllCpus op = getCpus >>= mapM_ op

-- | Entry point that ensures we have at least 2 arguments.
main :: IO ()
main = do args <- getArgs
          case args of
            ["increase"] -> changeFreq (freqIncr)
            ["decrease"] -> changeFreq (- freqIncr)
            ["gov"]      -> nextGovernor
            _            -> format <$> getCurrentPolicy >>= putStrLn
    where format (f, g) = show ((fromIntegral f :: Float) / 1000) <> "GHz " <> g
