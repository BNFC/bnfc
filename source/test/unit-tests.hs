{-# OPTIONS_GHC -F -pgmF hspec-discover #-}

-- This file is intentionally left blank.

-- See http://hspec.github.io/hspec-discover.html
--
-- This instructs GHC to invoke hspec-discover as a preprocessor on the
-- source file. The rest of the source file is empty, so there is nothing
-- to preprocess. Rather than preprocessing, hspec-discover scans the
-- file system for all spec files belonging to a project and generates
-- the required boilerplate. hspec-discover does not parse any source
-- files, it instead relies on the following conventions:
--
--   * Spec files have to be placed into the same directory as the test
--     driver, or into a subdirectory.
--
--   * The name of a spec file has to end in Spec.hs; the module name has
--     to match the file name.
--
--   * Each spec file has to export a top-level binding spec of type Spec.
