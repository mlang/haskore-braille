module Main where

import qualified Algebra.Ring as Ring (C)
import qualified Haskore.Interface.Braille as Braille
import qualified Haskore.Interface.Signal.InstrumentMap as InstrumentMap
import qualified Haskore.Interface.Signal.Write as MusicSignal
import           Haskore.Melody.Standard as StdMelody
import qualified Haskore.Music as Music
import qualified Haskore.Music.Rhythmic  as RhythmicMusic
import qualified Haskore.Performance.Context as Context
import qualified Haskore.Performance.Fancy as FancyPerformance (map)
import qualified Synthesizer.Plain.Filter.Recursive.Comb as Comb (run)
import           Synthesizer.Plain.Instrument (bell)
import qualified Synthesizer.Plain.Play as Play
import qualified Synthesizer.Plain.Signal as Signal
import           System.Exit (exitFailure)
import           System.Environment (getArgs)

data Instrument = Bell deriving (Eq, Ord)

type Music = RhythmicMusic.T () Instrument

context :: Context.T MusicSignal.NonNegTime MusicSignal.Volume (RhythmicMusic.Note () Instrument)
context = MusicSignal.contextMetro 40 RhythmicMusic.qn


instrMap :: InstrumentMap.InstrumentTable MusicSignal.Time MusicSignal.Volume Instrument
instrMap =
   [(Bell, MusicSignal.amplify (0.2::MusicSignal.Volume) bell      )
   ]

defltSampleRate :: (Num a, Ring.C a) => a
defltSampleRate = 11025

-- Volume type arises from Haskore
songToSignalMono :: MusicSignal.Time -> Music -> Signal.T MusicSignal.Volume
songToSignalMono dif song =
   MusicSignal.fromRhythmicMusic defltSampleRate
      (MusicSignal.detuneInstrs dif instrMap) FancyPerformance.map context song

songToSignalStereo :: MusicSignal.Time -> Music -> Signal.T (MusicSignal.Volume,MusicSignal.Volume)
songToSignalStereo det song =
   zip (songToSignalMono (1-det) song)
       (songToSignalMono (1+det) song)

melodySignal :: StdMelody.T -> Signal.T (MusicSignal.Volume, MusicSignal.Volume)
melodySignal mel =
   let (musr, musl) = unzip (songToSignalStereo 0.001
                               (RhythmicMusic.fromStdMelody Bell mel))
   in  zip (Comb.run (round (0.19*defltSampleRate :: MusicSignal.Time)) (0.4::MusicSignal.Volume) musl)
           (Comb.run (round (0.23*defltSampleRate :: MusicSignal.Time)) (0.5::MusicSignal.Volume) musr)



main = do [braille] <- getArgs
          either (\e -> putStrLn (show e) >> exitFailure)
              (\mel -> Play.stereoToInt16 defltSampleRate (melodySignal $ Music.line [mel, Music.rest 16]))
              (Braille.toStdMelody 1 braille)
