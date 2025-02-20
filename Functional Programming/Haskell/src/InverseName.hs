module InverseName where

import System.Directory
import System.FilePath
import Control.Arrow (Arrow(first))


renameDirFiles :: FilePath -> IO ()
renameDirFiles directoryPath =
  getDirectoryContents directoryPath >>= \ files ->
    let pdfFiles = filter (\ file -> takeExtension file == ".pdf") files in
    mapM_(\ file -> renamePdf directoryPath file) pdfFiles

renamePdf :: FilePath -> FilePath -> IO ()
renamePdf directory oldName =
  let (firstPart, rest) = break (== '-') oldName
      secondPart = dropWhile (== '-') (dropExtension rest)
      newName = dropWhile (== ' ') secondPart ++ " - " ++  init firstPart ++ ".pdf"
  in renameFile (directory </> oldName) (directory </> newName)

main :: IO ()
main =
  let directoryPath = "./" in
  renameDirFiles directoryPath >> putStrLn "Rewritten"
