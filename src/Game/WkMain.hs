module Game.WkMain (runWaka) where

import qualified Control.Monad.State.Strict as S
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import SDL.Input.Keyboard (stopTextInput)
import FRP.Yampa (reactimate, identity)
import Data.ObjectName (genObjectName)
import MyData (textFileName)
import MyFile (fileRead)
import Game.WkData (initWaka)
import Game.WkAction (startText,initInput,wkInput)
import Game.WkVideo (withVideo)
import Game.WkAudio2 (withWkAudio)
import Game.WkOutput (wkOut)
import Game.WkLoad (wkLoad)

type FileNum = Int

runWaka :: (MonadIO m,MonadFail m) => FileNum -> Text -> m () 
runWaka fln sIndex = do 
  stopTextInput
  (fonts,surfs) <- wkLoad
  allText <- fileRead (textFileName++show fln++".txt")
  withVideo $ \(renderer,texture) -> do
    let newWaka = startText sIndex allText initWaka
    withWkAudio $ \muses -> do
      source <- genObjectName
      S.runStateT (reactimate 
                     initInput
                     wkInput 
                     (wkOut renderer fonts surfs texture muses source)
                     identity
                  ) newWaka
