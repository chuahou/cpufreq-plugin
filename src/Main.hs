-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

import qualified System.Process as P

-- | We store frequencies in terms of MHz.
-- 1 GHz = 1000 MHz
type FreqMhz = Int

-- | Governors are represented by strings of their names.
type Governor = String

-- | Increments to adjust frequency by in MHz.
freqIncr :: FreqMhz
freqIncr = 200

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

-- | Entry point that ensures we have at least 2 arguments.
main :: IO ()
main = getCurrentPolicy >>= putStrLn . show
