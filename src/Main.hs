-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

import           System.Process (callProcess)

-- | Entry point that ensures we have at least 2 arguments.
main :: IO ()
main = callProcess "echo" ["hi"]
