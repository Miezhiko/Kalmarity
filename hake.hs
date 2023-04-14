{-# LANGUAGE
    MultiWayIf
  , UnicodeSyntax
  #-}

import Hake

main ∷ IO ()
main = hake $ do
  "clean | clean the project" ∫
    cabal ["clean"] >> removeDirIfExists buildPath

  kalmarityExecutable ♯ do
    cabal ["install", "--only-dependencies", "--overwrite-policy=always"]
    cabal ["configure"]
    cabal ["build"]
    getCabalBuildPath appName >>=
      \p -> copyFile p kalmarityExecutable
    cleanCabalLocal

  "install | install to system" ◉ [kalmarityExecutable] ∰
    cabal ["install", "--overwrite-policy=always"]

 where
  appName ∷ String
  appName = "kalmarity"

  buildPath ∷ String
  buildPath = "dist-newstyle"

  kalmarityExecutable ∷ String
  kalmarityExecutable =
    {- HLINT ignore "Redundant multi-way if" -}
    if | os ∈ ["win32", "mingw32", "cygwin32"] -> buildPath </> appName ++ "exe"
       | otherwise                             -> buildPath </> appName
