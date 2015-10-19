module Main where

import qualified Algebra.Ring as Ring (C)
import qualified Haskore.Interface.Braille as Braille
import qualified Haskore.Interface.Braille.TextTables as TextTables (Locale(..), braillify)
import qualified Haskore.Interface.Signal.InstrumentMap as InstrumentMap
import qualified Haskore.Interface.Signal.Write as MusicSignal
import qualified Haskore.Melody.Standard as StdMelody
import qualified Haskore.Music as Music
import qualified Haskore.Music.Rhythmic  as RhythmicMusic
import qualified Haskore.Performance.Context as Context
import qualified Haskore.Performance.Fancy as FancyPerformance (map)
import qualified Synthesizer.Plain.Filter.Recursive.Comb as Comb (run)
import           Synthesizer.Plain.Instrument (bell, moogGuitar)
import qualified Synthesizer.Plain.Play as Play
import qualified Synthesizer.Plain.Signal as Signal
import           System.Exit (ExitCode(..), exitFailure)
import           System.Environment (getArgs)

data Instrument = Bell | MoogGuitar deriving (Eq, Ord)

type Music = RhythmicMusic.T () Instrument

context :: Context.T MusicSignal.NonNegTime MusicSignal.Volume (RhythmicMusic.Note () Instrument)
context = MusicSignal.contextMetro 40 RhythmicMusic.qn


instrMap :: InstrumentMap.InstrumentTable MusicSignal.Time MusicSignal.Volume Instrument
instrMap =
   [(Bell,  MusicSignal.amplify (0.2::MusicSignal.Volume) bell      )
   ,(MoogGuitar, MusicSignal.amplify (0.2::MusicSignal.Volume) moogGuitar     )
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

melodySignal :: Instrument -> StdMelody.T -> Signal.T (MusicSignal.Volume, MusicSignal.Volume)
melodySignal instr mel =
   let (musr, musl) = unzip (songToSignalStereo 0.001
                               (RhythmicMusic.fromStdMelody instr mel))
   in  zip (Comb.run (round (0.11*defltSampleRate :: MusicSignal.Time)) (0.4::MusicSignal.Volume) musl)
           (Comb.run (round (0.09*defltSampleRate :: MusicSignal.Time)) (0.5::MusicSignal.Volume) musr)



main :: IO ExitCode
main = do [braille] <- getArgs
          either (\e -> print e >> exitFailure)
              (\mel -> Play.stereoToInt16 defltSampleRate $
                       melodySignal Bell $ Music.transpose (-24) $
                       Music.line [mel, Music.qnr, StdMelody.c 2 RhythmicMusic.qn StdMelody.na])
              (Braille.toStdMelody 1 $ TextTables.braillify TextTables.German braille)
