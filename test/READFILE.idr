module READFILE

import Effects
import Effect.StdIO
import Effect.File

read_file : String -> Effects.SimpleEff.Eff () [FILE_IO (), STDIO]
read_file fnm = do
  ei <- Effect.File.Default.readFile ("../files/" ++ fnm)
  case ei of
    Left e => putStrLn $ "Failed: " ++ e
    Right c => putStrLn $ "Contents: " ++ c
    
export test_multiple_files : IO ()
test_multiple_files = do
  run $ read_file "EVEN.fsy"
  run $ read_file "FK.fsy"
  run $ read_file "FK+DK.fsy"
    
