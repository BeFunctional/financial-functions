module Main
  ( main,
  )
where

import Test.DocTest

main :: IO ()
main =
  doctest
    [ "-XOverloadedStrings",
      "-XScopedTypeVariables",
      "-XTypeFamilies",
      "-XTypeApplications",
      "-isrc",
      "src/Numeric/Financial"
    ]
