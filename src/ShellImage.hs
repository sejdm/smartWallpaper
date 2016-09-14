{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies, TemplateHaskell #-}


module ShellImage (shellImageIO, ShellCommnandSettings (..), shellcommand) where

import Diagrams.Prelude hiding (pad)
import System.Process
import System.Exit

data ShellCommnandSettings = ShellCommnandSet
                    {
                      _shellcommand :: String
                    }
makeLenses ''ShellCommnandSettings

instance Default ShellCommnandSettings where
   def = ShellCommnandSet {_shellcommand = ""}


shellImageIO s  = do (e,n,er) <- readCreateProcessWithExitCode (shell (s ^. shellcommand)) []
                     if e == ExitSuccess then
                       do res <-  loadImageEmb n
                          return $ case res of
                            Right img -> image img # sized (mkWidth 100)
                            Left _ -> text n # scale 3
                       else do print "Shell command error. Using empty diagram."
                               return mempty
