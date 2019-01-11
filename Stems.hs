{-# LANGUAGE OverloadedStrings #-}
module Stems where

import Sound.Tidal.Context
import Macros as M
import Data.Map as Map (fromList,lookup)

type Stem = (String, ParamPattern)


stem :: Pattern String -> ParamPattern
stem pat = Pattern (\a-> concat $ fmap (\(a1,_,s) -> filter (\((s,_),_,_)-> isIn a1 s) $ (arc $ maybe silence id $ Map.lookup s stems) a)   (arc pat $ a))


drum_dh1 = ("drum_dh1",stut 2 0.15 0.85 $ s "kick:5 [hat:2] [kick:5,snare:2,clap:4/4] hat:4")

drum_dh2 = ("drum_dh2", whenmod 8 6 (# s "drumTop") $ superimpose (#lpf "300") $ s "drumLoop" # legato 1 # n "<1 2 1 2 4>" # orbit "1")

drum_amen = ("drum_amen", whenmod 8 6 ((|+| n "4") . (juxBy 0.25 rev)) $ s "amencutup*8" # iter 8 (n $ run 8)  |*| speed "0.9")

drum_trap = ("drum_trap", whenmod 8 7 (stut 2 0.5 1) $ slow 4 $ s "808bd:1 ~ [clap,clap:2,emphasis:3/4] <[hat:4*8 hat:2] hat:2*4> ~ [808bd:1, <~ [~ hat:4*2]>] clap [~ ~ ~ snare:2/2]" |*| speed "0.9 1 0.9 1" # shape "[0.9 0.9 0.9 0]*2")

snare_roll = ("snare_roll", fast 2 $ every 4 (juxBy 0.125 (slow 1.5)) $ s "dr:1*4" # iter 4 (gain "0.6 0.85 1 0.75") # n "1")

perc_high = ("perc_high", s "[techno]*8" # iter 4 (speed "16 64 32 16") # pan (scale (0.25) 0.75 rand))

perc_high2 = ("perc_high2", whenmod 8 4 (degrade . (# crush "4") . (# accelerate (sometimes (*(-1))$ 0.5))) $ s "[dr <hat:2 tamb>]*4" # iter 4 (gain "0.5 0.7 0.8 0.9"))

perc_conga = ("perc_conga", s "conga(5,7)" # (n "0 1 2 3") |+| n "<0 1 2 3>/8" # up "-2")

fill_conga = ("fill_conga", slow 2 $ s "[conga*8,tom*3?,~ [<conga conga:1>] ~ ~]" # jux rev (iter 3 (n "0 1 2 3")))

fill_bass = ("fill_bass", s "808bd:1" # coarse "4" # crush "5" # gain "0.99" |*| accelerate "-1")

ch1 = ("ch1", off 0.85 (|+| n "<~ ~ 19>") $ stut 6 0.7 1.7 $ s "<[[~ chord2] ~ ~ ~] ~>" |+| n "<0 0 -7 -5>/2" # legato "1" # gain "1.2"  # orbit "1"  # room "0.2" # size "0.4")

ch2 = ("ch2", s "pad3/4" # n "<-12, 0 ~ 3>" # legato "1.4" # controlOrbit 1 # cRatio 10  # cAttack 0.3 # cThreshold (-30) # cRelease 0.3 # gain "1.1")

ch3 = ("ch3", every 16 (|*| accelerate 0.1) $ stut 12 0.5 3.2 $ s "<[~ jamiexxss] ~ ~ ~>" # begin "0.6" # n "8" # legato "1" # up "[-4]"# room "0.3" # orbit "1" # size 0.3)

ch4 = ("ch4", every 16 (|*| accelerate 0.1) $ stut 6 0.5 3.2 $ s "<[~ jamiexxss] ~ ~ ~>" # begin "0.6" # n "8" # legato "1" # up "[-4,3]" # room "0.3" # orbit "1" # size 0.3)

amb1 = ("amb1",s "lzr/8" # n "<24 36>" |+| n "[0,12,7]" # gain "0.9" # legato "1.1" # phaserrate "6" # phaserdepth "0.5")

amb2 = ("amb2", whenmod 16 14 (|*| (slow 2 $ accelerate $ scale 0 (-1) saw)) $ sometimes (off 0.8 (|*| up "[12, <~ 24 7>]")) $ off 0.7 (|*| up "7") $ stut 5 0.5 2 $ s "<synth ~ ~ [<kurt:0 kurt:1 kurt:2>,sine:-12 sine:7/2 sine:12 ~ <sine:19 sine:-5>]>" # orbit "1"  # gain "1.1" # sustain "1" # room "0.3" # size "0.3")

b1 = ("b1", every 5 (off 0.2 (|*| up "12") . (# coarse "10")) $ whenmod 4 3 (degrade . (|*| speed "[-1,0.5,-0.5]") . (superimpose (#pan rand))) $ superimpose (# up ((*) "[0,7]" "[0 0 4 4]/4")) $ degrade $ s "<[bass:1*8,~ synth/2] bass:1*6 bass*4>" # legato "1" |*| speed "[1,2,3,5,6,7,8,4 0.5]" # shape "0.2" # room "0.2" # gain "0.4")

m1 = ("m1",jux rev $ s "synth*8" # attack 0.01 # release 0.2 # n "[<0 1 9 6 0>,0]*2" |*| up "[<0 2 -4 9>]/4"  |*| up "[0 14 7 19]*2" |*| iter 4 (speed "1 2 1 4"))

m2 = ("m2", stut 2 0.5 0.7 $ slow 2 $ s "mallet*3 [mallet] mallet [mallet] mallet/2" # sustain 0.75 |+| n "[-24]" |+| every 1 (jux rev) (n $ "0 3 <7 19> 10 8" + "<0 0 5 5>") |+| n "[12,19]" # orbit "0" |+| n "[0]")

m3 = ("m3",whenmod 16 14 (stut 2 0.5 1.5) $ every 3 (off 0.75 $ (|+| n "<-12 24 19 24>")) $ every 1 (|+| n "[0,<19 19 24>,-12]") $ n ((sometimes rev "[0 3 7? 12,10 [24 12/2] 14,19 <22 24?>]") + "<-5 -2>/4") # s "[sine]/8" # reverb 0.4 0.7 # orbit "0" # gain "0.85" # sustain (slow 3 $ scale 0.125 1.5 sine))



kick_1 = ("kick_1", s "kick:5 [kick:5]" # speed "1 [1,<-0.5 ~>]")

kick_2 = ("kick_2",s "[kick:5/2 [kick:5]] ~ " # speed "[1.2 [1]] ~" # gain "[0.8 [1]] ~")

kick_2b = ("kick_2b",s "[kick:5/2 [kick:5]] ~ " # speed "[1.2 [1,-0.5]] ~" # gain "[0.8 [1]] ~")

kick_3 = ("kick_3", s "kick:5(2,5)" # speed "1 1 <[0.25] 1> ~ ~" # gain "1 1 <1.1 1> 1 1")

hh_1 = ("hh_1", stut 2 0.15 1 $ s "~ <hat:4*2 hat:4> ~ hat:4" # speed "1.7" # iter 4 (gain "[1 0.8 0.8 0.7]*4"))

hh_2  = ("hh_2", stut 2 0.15 0.25 $ s "[~ dr tamb:2 ~] [hat:4/2 tamb:2]")

hh_2b = ("hh_2b",stut 4 0.55 0.5 $ s "[~ dr tamb:2 ~] [hat:4/2 tamb:2]")

hh_3 = ("hh_3", s "~ [~ tamb:1] tamb:4/2 [tamb:1]" |*| speed "1 1.2 1 <1.2 1.19>")

sn_1 = ("sn_1", whenmod 4 3 (stut 8 0.15 1) $ s "~ [<clap:1 clap:2>,snare:4]" # speed "1.2")

sn_2 = ("sn_2", s "~ ~ [[clap:1/2,clap:6/2],~ ~ [snare] ~] ~ ~" # speed "~ ~ [1.6 0.5] ~ ~")

sn_3 = ("sn_3", s "~ ~ [emphasis:3/16,clap:1,[~ clap:6 ~ ~ ~ ~ ~ ~], [~ clap] ~ ~ ~ ~ ~ ~ ~] ~ " |*| speed "1.5" |*| speed (scale 0.98 1.02 rand))

sn_4 = ("sn_4", (# pan (scale 0.45 0.55 rand)) $ s "~ [~ clap:6/2] [clap:1,[~ clap:6 snare ~ ~ ~ ~ ~], [~ clap] ~ ~ ~ ~ ~ ~ ~] ~ " |*| speed "1.5" |*| speed (scale 0.98 1.02 rand))

mallet_1 = ("mallet_1", (|+| n "[-12]") $ stut 3 0.7 1 $ s "[~ mallet ~ ~ ~ ~]" # sustain 1)

mallet_2 = ("mallet_2", slow 2 $ n "0 [~ 7] [~ 14] [~ 19]" # s "[mallet]" # sustain 1 |+| n "-12")

bass_1 = ("bass_1", (|+| n "[-24]") $ superimpose ((# s "[~ <~ sawBass>] ~") . (|+| n "12") . (# sustain 0.25) . (|+| lpfMul "[0.125,1,0.25]")) $ s "[~ [fm9]] ~" # sustain 1 # M.at "0.01" # n "0")

bass_2 = ("bass_2", superimpose ((#lpf "400") . (|+| gain "0.1")) $ s "sawBass(3,5)" |+| n "[0]" # lpfMul "[0.25, 0.125]" # gain "1.05")

bass_3 = ("bass_3", s "[808bd:1 808bd:1/4] ~ [~ 808bd:1/8] ~" |*| speed "[1 1.1] 1 2 1" # cut "88")


pitch_1 = ("pitch_1", slow 2 $ s "saw1:24" # legato 1.2)

pitch_2 = ("pitch_2", s "mallet*8" # legato 2 |+| n "36" # pan (scale 0.25 0.5 rand) )

chord_1 = ("chord_1", rev $ s "[synth*2 [~ synth synth]]/2" # legato 0.5 |*| up "[12]" |*| accelerate "<0 0 0 [0 -0.05]>")



new = [kick_1,kick_2, kick_2b, kick_3, hh_1, hh_2, hh_2b,hh_3, sn_1, sn_2, sn_3,sn_4, mallet_1, mallet_2, bass_1,bass_2,bass_3, pitch_1, pitch_2,chord_1]

--

stems = Map.fromList $ [drum_dh1, snare_roll,drum_dh2,drum_amen, drum_trap,perc_high, perc_high2,perc_conga,fill_conga,fill_bass,ch1,ch2,ch3,ch4,amb1,amb2,b1,m1, m2, m3] ++ new
