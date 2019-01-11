{-# LANGUAGE OverloadedStrings #-}
module Test where

import Sound.Tidal.Context hiding (at)
import qualified Data.Map as M

import Stems as S

data Category = Bassline | Drums | Melody | Ambient | Chords | Other   deriving (Show, Eq)


type Phrase = [(Category, ParamPattern)]

data Snippet = Snippet {handle::String, phrases::[Phrase], tempo::Double} deriving (Eq)

instance Show Snippet where
  show = handle



-- tempos:
gorillazstylo = 100/60/2

dremessage = 82/60/2

nasml = 90/60/2

jamiexxss = 118.6/60/2

stilldre = 93/60/2

drebn = 82.5/60/2

drefu = 92/60/2

dreegos = 89/60/2

drexxplosive = 84.5/60/2

drels = 86.4/60/2

asgmayl = 105/60/2

illaj = 84.3/60/2

amb_1::Snippet
amb_1 = Snippet "amb_1" [
  [ -- 1
    (Ambient, midinote "<[0,3,7] [-4,3,7] [3,7,14] [7,10,14]>" # s "[sine6]" # legato "8" # phaserrate (slow 16 $ scale 1 8 saw) # phaserdepth 2 # orbit "0" |+| midinote "<60 60 60 60 56 56 56 56 58 58>/2" # gain "0.9")
  ],[ --2
    (Ambient, midinote "<[0,3,7] [-4,3,7] [3,7,14] [7,10,14]>" # s "[sine6]" # legato "8" # phaserrate (slow 16 $ scale 1 8 saw) # phaserdepth 2 # orbit "0" |+| midinote "<60 60 60 60 56 56 56 56 58 58>/2" # gain "0.9"),
    (Drums, superimpose ((# lpf "200") . (# gain "1.1")) $ slow 4 $ stut 2 0.75 "<2 3 1.5>" $ (off 0.05 (# pan rand)) $ every 4 (# crush "5") $ brak $ s "[kick:5 sn:2,dr(3,5),snare(2,3)]" # orbit "1" # room "0.1" # size "0.3" |*| iter 3 (speed "6 2 4") |*| speed (slow 4 $ scale 0.125 8 sine))
  ]
 ] 1


chill_1::Snippet
chill_1 = Snippet "chill_1" [
  [ --1
  (Other,  slow 2 $ stut 10 0.6 (slow 16 $ "4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20" * "0.5") $ s "synth/4 [metal/9]" #n "[0,~ ~ ~ 14]/16" # gain "0.8" # speed "[0.6 0.5,1.2 1]/32" |*| speed "[1,2,3,4]" # slow 21 (coarse $ fmap round $ scale 0 4 sine) # slow 15 (lpf $ scale 300 3000 sine) # resonance "0.1" # iter 3 (slow 6 $ vowel "o t a")),
  (Drums, stut 3 0.5 1.15 $ s "kick:3 emphasis:2/32"),
  (Drums, slow 2 $ stut 2 0.05 0.85 $ s "[shaker/2 ~ shaker] [snare,clap shaker ~]" # gain "0.9")
  ]] 1

dh_1::Snippet
dh_1  = Snippet "dh_1" [
  [
    (Drums, stut 2 0.15 0.85 $ s "kick:5 [hat:3] [kick:5,snare:3/4,snare,clap:5/2] [hat:2]"),
    (Chords, stut 6 0.7 1.7 $ s "<[[~ chord2] ~ ~ ~] ~>" |+| n "<0 0 -7 -5>/2" # legato "1" # gain "1.2"  # orbit "1"  # room "0.2" # size "0.4")
  ],
  [
    (Drums, stut 2 0.15 0.85 $ s "kick:5 [hat:3] [kick:5,snare:3/4,snare,clap:5/2] [hat:2]"),
    (Chords, off 0.85 (|+| n "<~ ~ 19>") $ stut 6 0.7 1.7 $ s "<[[~ chord2] ~ ~ ~] ~>" |+| n "<0 0 -7 -5>/2" # legato "1" # gain "1.2"  # orbit "1"  # room "0.2" # size "0.4")
  ],[
    (Drums, whenmod 16 14 ((# speed (slow 2 $ scale 1 0.25 saw))) $ stut 2 0.15 0.85 $ s "kick:5 [hat:3] [kick:5,snare:3/4,snare,clap:5/2] [hat:2]"),
    (Chords, off 0.85 ((|+| n "<24 19 [-5] 19>") . (#pan rand)) $ stut "<6 6 3 6>" 0.7 1.7 $ s "<[[~ [chord2,sine:12/2]] ~ ~ ~] ~>" |+| n "<0 0 -7 -5>/2" # sustain "0.125" # gain "1.2"  # orbit "1"  # room "0.2" # size "0.4" )
  ],[
    (Drums, stut 2 0.16 0.85 $ s "kick:5 [hat:3] [kick:5,snare] [hat:2]"),
    (Chords, off 0.85 ((|+| n "<24 19 [-5] 19>") . (#pan rand)) $ stut "<6 6 3 6>" 0.7 1.7 $ s "<[[~ [sine:12]] ~ ~ ~] ~>" |+| n "<0 0 -7 -5>/2" # sustain "0.5" # gain "1.2"  # orbit "1"  # room "0.2" # size "0.4")
    ],[
    (Drums, stut 2 0.16 0.85 $ s "kick:5 [hat:3] [kick:5,snare] [hat:2]")
    ]
  ] 1

dh_2::Snippet
dh_2 = Snippet "dh_2" [
  [ -- 1
  (Chords, s "pad3/4" # n "<-12, 0 ~ 3>" # legato "1.4" # controlOrbit 1 # cRatio 10  # cAttack 0.3 # cThreshold (-30) # cRelease 0.3 # gain "1.1"),
  (Ambient, s "lzr/8" # n "<24 36>" |+| n "[0]" # gain "0.9" # legato "1.1" # phaserrate "6" # phaserdepth "0.5")
  ],[ -- 2
  (Chords, s "pad3/4" # n "<-12, 0 ~ 3>" # legato "1.4" # controlOrbit 1 # cRatio 10  # cAttack 0.3 # cThreshold (-30) # cRelease 0.3 # gain "1.1"),
  (Ambient, s "lzr/8" # n "<24 36>" |+| n "[0]" # gain "0.9" # legato "1.1" # phaserrate "6" # phaserdepth "0.5"),
  (Drums, stut 2 0.1 0.9 $ s "[kick:5 techno/4] hat [kick:5,<snare snare snare snare snare snare snare snare:2>, clap/2 techno/8] hat" # orbit "1")
  ],[ -- 3
  (Chords, s "pad3/4" # n "<-12, 0 ~ 3>" # legato "1.4" # controlOrbit 1 # cRatio 10  # cAttack 0.3 # cThreshold (-30) # cRelease 0.3 # gain "1.1"),
  (Ambient, s "lzr/8" # n "<24 36>" |+| n "[0,12,7]" # gain "0.9" # legato "1.1" # phaserrate "6" # phaserdepth "0.5"),
  (Drums, stut 2 0.1 0.9 $ whenmod 16 14 (# speed (sometimes rev $ slow 2 $ scale 1 4 saw)) $ every 8 ((# crush "4") . (# accelerate "0 -1") . ((sometimes (stut 3 0.5 1)) )) $ s "[kick:5 techno/4] hat [kick:5,<snare snare snare snare snare snare snare snare:2>, clap/2 techno/8] hat" # orbit "1")
  ]
 ] 1

dh_3::Snippet
dh_3 = Snippet "dh_3" [
 [ -- 1
  (Chords, every 16 (|*| accelerate 0.1) $ stut 12 0.5 3.2 $ s "<[~ jamiexxss] ~ ~ ~>" # begin "0.6" # n "8" # legato "1" # up "[-4]"# room "0.3" # orbit "1" # size 0.5)
 ],[ -- 2
   (Chords, every 16 (|*| accelerate 0.1) $ stut 12 0.5 3.2 $ s "<[~ jamiexxss] ~ ~ ~>" # begin "0.6" # n "8" # legato "1" # up "[-4]"# room "0.3" # orbit "1" # size 0.5),
   (Drums, stut 2 0.5 0.80 $ s "kick:5 hat:3 [kick:5,clap:3/4,snare] hat:3" # orbit "0" # room 0 # size 0)
 ],[ -- 3
   (Chords, every 16 (|*| accelerate 0.1) $ stut 12 0.5 3.2 $ s "<[~ jamiexxss] ~ ~ ~>" # begin "0.6" # n "8" # legato "1" # up "[-4]"# room "0.3" # orbit "1" # size 0.3),
   (Drums, juxBy 0.5 rev $ stut 2 0.5 0.80 $ s "kick:5 hat:3 [kick:5,clap:3/4,snare] hat:3" # orbit "0"  # room 0 # size 0)
 ],[ -- 4
   (Chords, every 16 (|*| accelerate 0.1) $ stut 12 0.5 3.2 $ s "<[~ jamiexxss] ~ ~ ~>" # begin "0.6" # n "8" # legato "1" # up "[-4,3]" # room "0.3" # orbit "1" # size 0.3),
   (Drums, every 4 (superimpose (# s" ~ sn:2")) $ stut 2 0.5 0.80 $ s "kick:5 hat:3 [kick:5,clap:3/4,snare] hat:2" # orbit "0" # room 0 # size 0)
 ],[
   (Chords, every 16 (|*| accelerate 0.1) $ stut 6 0.5 3.2 $ s "<[~ jamiexxss] ~ ~ ~>" # begin "0.6" # n "8" # legato "1" # up "[-4,3]" # room "0.3" # orbit "1" # size 0.3),
   (Drums, stut 1 0.5 0.80 $ s "kick:5 hat:3 [kick:5,clap:3/4,snare] hat:2" # orbit "0" # room 0 # size 0)
 ],[(Drums, stut 1 0.5 0.80 $ s "kick:5 hat:3 [kick:5,clap:3/4,snare] hat:2" # orbit "0" # room 0 # size 0)]] 1

dh_4::Snippet
dh_4 = Snippet "dh_4" [
  [
  (Melody,every 5 (off 0.2 (|*| up "12") . (# coarse "10")) $ whenmod 4 3 (degrade . (|*| speed "[-1,0.5,-0.5]") . (superimpose (#pan rand))) $ superimpose (# up ((*) "[0,7]" "[0 0 4 4]/4")) $ degrade $ s "<[bass:1*8,~ synth/2] bass:1*6 bass*4>" # legato "1" |*| speed "[1,2,3,5,6,7,8,4 0.5]" # shape "0.2" # room "0.2" # gain "0.4")
  ],[
  (Melody,every 5 (off 0.2 (|*| up "12") . (# coarse "10")) $ whenmod 4 3 (degrade . (|*| speed "[-1,0.5,-0.5]") . (superimpose (#pan rand))) $ superimpose (# up ((*) "[0,7]" "[0 0 4 4]/4")) $ degrade $ s "<[bass:1*8,~ synth/2] bass:1*6 bass*4>" # legato "1" |*| speed "[1,2,3,5,6,7,8,4 0.5]" # shape "0.2" # room "0.2" # gain "0.4"),
  (Drums, (superimpose ((# slow 8 (s "drumTop:1*4 drumTop:4*4")) . (# gain (slow 20 $ scale 1 1.3 sine)) . (# legato "2") . (# hpf "1500"))) $ whenmod 8 7 (slow 1.5 . (|*| accelerate "-1")) $ s "kick:5 dr [dr,kick,sn:2] [dr dr:1/4]" # gain "1")
  ]] 1

dh_5 = Snippet "dh_5" [
  [
  (Drums, stut 2 0.15 0.85 $ s "kick:5 [hat:2] [kick:5,snare:2,clap:4/4] hat:4")
  ],[
  (Drums, stut 2 0.15 0.85 $ s "kick:5 [hat:2] [kick:5,snare:2,clap:4/4] hat:4"),
  (Melody, whenmod 16 14 (|*| (slow 2 $ accelerate $ scale 0 (-1) saw)) $ sometimes (off 0.8 (|*| up "[12, <~ 24 7>]")) $ off 0.7 (|*| up "7") $ stut 5 0.5 2 $ s "<synth ~ ~ [<kurt:0 kurt:1 kurt:2>,sine:-12 sine:7/2 sine:12 ~ <sine:19 sine:-5>]>" # orbit "1"  # gain "1.1" # sustain "1" # room "0.3" # size "0.3")
  ],[
  (Drums, stut 2 0.15 0.85 $ whenmod 16 14 ((# s "[conga*8,tom*3?]") . (# jux rev (iter 3 (n "0 1 2 3")))) $ s "kick:5 [<conga conga:1>,hat] [kick:5,snare:2,clap:4/4] hat:3"),
  (Melody, whenmod 16 14 (|*| (slow 2 $ accelerate $ scale 0 (-1) saw)) $ sometimes (off 0.8 (|*| up "[12, <~ 24 7>]")) $ off 0.7 (|*| up "7") $ stut 5 0.5 2 $ s "<synth ~ ~ [<kurt:0 kurt:1 kurt:2>,sine:-12 sine:7/2 sine:12 ~ <sine:19 sine:-5>]>" # orbit "1"  # gain "1.1" # sustain "1" # room "0.3" # size "0.3")
  ]] 1



altdh_1::Snippet
altdh_1 = Snippet "altdh_1" [
  [ -- 1
  (Melody, fast 2 $ stut 2 0.15 0.85  $ every 4 (superimpose (|*| speed "-1")) $ fast 2 $ ("<0.1 0.15 0.2 0.25 0.3 0.35 0.4 0.45 0.5>" <~) $ s "~ [~ bass:8/2]"  # hpf (slow 24 $ scale 2000 6000 saw) |*| jux rev (slow 2 (speed "[-1,1,2,5,3 4]")) |*| up "[0 4]/64" # resonance "0.1")
  ],[ --2
  (Melody, fast 2 $ stut 2 0.15 0.85  $ every 4 (superimpose (|*| speed "-1")) $ fast 2 $ ("<0.1 0.15 0.2 0.25 0.3 0.35 0.4 0.45 0.5>" <~) $ s "~ [~ bass:8/2]"  # lpf (slow 24 $ scale 100 20000 saw) |*| jux rev (slow 2 (speed "[-1,1,2,5,3 4]")) |*| up "[0 4]/64" # resonance "0.1")
  ],[
    (Melody, fast 2 $ stut 2 0.15 0.85  $ every 4 (superimpose (|*| speed "-1")) $ fast 2 $ ("<0.1 0.15 0.2 0.25 0.3 0.35 0.4 0.45 0.5>" <~) $ s "~ [~ bass:8/2]"  # lpf (slow 24 $ scale 100 20000 saw) |*| jux rev (slow 2 (speed "[-1,1,2,5,3 4]")) |*| up "[0 4]/64" # resonance "0.1"),
    (Drums, stut 2 0.15 0.85 $ s "[snare:4,kick:5] dr kick:5 dr")
  ],[
    (Melody, fast 2 $ whenmod 16 14 (slow 2 . (|*| accelerate "-0.1") . (off 0.15 (|*| up 19) . off 0.15 (|*| up 7))) $ stut 2 0.15 0.85  $ every 4 (superimpose (|*| speed "-1")) $ fast 2 $ ("<0.1 0.15 0.2 0.25 0.3 0.35 0.4 0.45 0.5>" <~) $ s "~ [~ bass:8/2]"  # lpf (slow 24 $ scale 100 20000 saw) |*| jux rev (slow 2 (speed "[-1,1,2,5,3 4]")) |*| up "[0 4]/64" # resonance "0.1"),
    (Drums, stut 2 0.15 0.85 $ whenmod 16 14 (slow 2 . (|*| accelerate "-0.1") . (off 0.15 (|*| up 19) . off 0.15 (|*| up 7))) $ s "[snare:4,kick:5] dr kick:5 dr")
  ]] 1


dh_6::Snippet
dh_6 = Snippet "dh_6" [
  [
    (Drums, whenmod 8 6 (# s "drumTop") $ superimpose (#lpf "300") $ s "drumLoop" # legato 1 # n "1" # orbit "1")
  ],[
    (Drums, whenmod 8 6 (# s "drumTop") $ superimpose (#lpf "300") $ s "drumLoop" # legato 1 # n "<1 2 1 2 4>" # orbit "1"),
    (Melody, stut 3 0.5 1.3 $ every 4 (off 0.5 (|*| up 12)) $ every 2 (off 0.7 (# up "7")) $ sometimes (superimpose $ (|*| speed "[-1,-2,-3,-4,-8]") . (# pan rand)) $ stut 3 0.5 2 $ s "synth/4" |*| up "[0,12]" # begin "0.2" # cRatio "5" # controlOrbit "1" # cThreshold "-35" # gain "1.5" # cAttack "0.1" # cRelease "0.1" # legato 1)
  ],[
    (Drums, whenmod 8 6 (# s "drumTop") $ superimpose (#lpf "300") $ s "drumLoop" # legato 1 # n "<1 2 1 2 4>" # orbit "1"),
    (Melody, stut 3 0.5 1.3 $ every 4 (off 0.5 (|*| up 12)) $ every 2 (off 0.7 (# up "7")) $ sometimes (superimpose $ (|*| speed "[-1,-2,-3,-4,-8]") . (# pan rand)) $ stut 3 0.5 2 $ s "synth/4" |*| juxBy 0.5 rev (up "<0 12>") # begin "0.2" # cRatio "5" # controlOrbit "1" # cThreshold "-35" # gain "1.5" # cAttack "0.1" # cRelease "0.1" # legato (1/24))],[(Drums, whenmod 8 6 (# s "drumTop") $ superimpose (#lpf "300") $ s "drumLoop" # legato 1 # n "<1 2 1 2 4>" # orbit "1")
  ],[
    (Melody, stut 5 0.5 2.6 $ every 4 (off 0.5 (|*| up 12)) $ every 2 (off 0.7 (# up "7")) $ sometimes (superimpose $ (|*| speed "[-1,-2,-3,-4,-8]") . (# pan rand)) $ stut 3 0.5 4 $ s "synth/8" |*| juxBy 0.5 rev (up "<0 12>") # begin "0.2" # cRatio "5" # controlOrbit "1" # cThreshold "-35" # gain "1.5" # cAttack "0.1" # cRelease "0.1" # legato (1/48)),
    (Drums, whenmod 8 6 (# s "drumTop") $ superimpose (#lpf "300") $ s "drumLoop" # legato 1 # n "<1 2 1 2 4>" # orbit "1")
  ],[
    (Drums, whenmod 8 6 (# s "drumTop") $ superimpose (#lpf "300") $ s "drumLoop" # legato 1 # n "<1>" # orbit "1")
  ]] 1


mel_1::Snippet
mel_1 = Snippet "mel_1" [
  [
  (Melody, s "<[~ synth:2] [[~ synth:2] ~ ~ synth:2] ~ [synth:2/2]>" # up "[0 -3,4 0,11 4]/4" |*| speed "[1,3,4,5,0.5,~ 2]" # orbit "1" # legato "1 2" # room "0.3" # slow 21 (lpf $ scale 500 15000 sine1) # resonance "0.2" |*| iter 4 (accelerate $ sometimes (*(-1)) "0 0 0.05 0") |+| gain "1" |-| gain "0.3")
  ],[
    (Melody, s "<[~ synth:2] [[~ synth:2] ~ ~ synth:2] ~ [synth:2/2]>" # up "[0 -3,4 0,11 4]/4" |*| speed "[1,3,4,5,0.5,~ 2]" # orbit "1" # legato "1 2" # room "0.3" # slow 21 (lpf $ scale 500 15000 sine1) # resonance "0.2" |*| iter 4 (accelerate $ sometimes (*(-1)) "0 0 0.05 0") |+| gain "1" |-| gain "0.3"),
    (Drums, whenmod 16 15 (fast 8) $ s "dr*8" |+| iter 4 (gain "0.9 1.1 1 1") # fast 2 (speed "2 0.5 1") # iter 2 (fast 4 $ n "0 2") |+| slow 16 (gain $ scale 0 0.6 saw) |-| gain " 0.25")
  ],[
    (Melody, s "<[~ synth:2] [[~ synth:2] ~ ~ synth:2] ~ [synth:2/2]>" # up "[0 -3,4 0,11 4]/4" |*| speed "[1,3,4,5,0.5,~ 2]" # orbit "1" # legato "1 2" # room "0.3" # slow 21 (lpf $ scale 500 15000 sine1) # resonance "0.2" |*| iter 4 (accelerate $ sometimes (*(-1)) "0 0 0.05 0") |+| gain "1" |-| gain "0.3"),
    (Drums, whenmod 16 15 (fast 8) $ s "dr*8" |+| iter 4 (gain "0.9 1.1 1 1") # fast 2 (speed "2 0.5 1") # iter 2 (fast 4 $ n "0 2") |+| slow 16 (gain $ scale 0 0.6 saw) |-| gain " 0.25"),
    (Drums, superimpose (# lpf 500) $ whenmod 8 7 (degrade . (#crush "6") . (|*| accelerate "-0.5")) $ every 3 (superimpose (fast 1.5 . rev)) $ fast 0.5 $ s "[kick:5,808bd:1] hat shaker/2 [kick:5,snare:2/3,sn:2] [clap clap/4 clap/4 clap/4] [conga:2/2,emphasis:2/32]" |+| n "1" |+| gain "1" # iter 3 (speed "[0.5 1 -1,1]") # shape 0.8 |-| gain "0.45" # shape "0.9")
  ]] 1





onehundred::Snippet
onehundred = Snippet "onehundred" [
  [ -- 1
  (Other, slow 2 $ stack [slow 1.01 $ n "0 <7 7 7 7 -4 -4>" # s "saw6",slow 1 $ n "<20 20 19 19> <24 24 24 24 22 22 22 [22 21?]> 12" # s "saw6"] # legato "1.6" |+| n "-12"  # lpf (slow 24 $ scale 200 1500 sine) # room "0.5" # size "0.8" # orbit "0" )
  ],[ -- 2
  (Other, slow 2 $ stack [slow 1.01 $ n "0 <7 7 7 7 -4 -4>" # s "saw6",slow 1 $ n "<20 20 19 19> <24 24 24 24 22 22 22 [22 21?]> 12" # s "saw6"] # legato "1.6" |+| n "-12"  # lpf (slow 24 $ scale 200 1500 sine) # room "0.5" # size "0.8" # orbit "0" ),
  (Other, whenmod 4 1 (const silence) $ (0.5 ~>) $ s "bev*30" # begin "<0.3 0.3 0.8 0.8>/2" # attack 0.3 # release 0.75 # up "[0 -4 7]" # gain "0.7" # slow 21 (coarse $ fmap round $ scale 0 14 sine1) # room "0.2" # size "0.3" |*| (slow 3 $ speed "1 4 8 1 4") |*| pan "0 0.5 -0.5 -1 1" |*| pan (slow 2 sine) |*| iter 3 (pan "1 -1 0.5 -0.5") # orbit "1" # hpf "10000" |*| speed "2")
  ],[ -- 3
  (Other, slow 2 $ stack [slow 1.01 $ n "0 <7 7 7 7 -4 -4>" # s "saw6",slow 1 $ n "<20 20 19 19> <24 24 24 24 22 22 22 [22 21?]> 12" # s "saw6"] # legato "1.6" |+| n "-12"  # lpf (slow 24 $ scale 200 1500 sine) # room "0.5" # size "0.8" # orbit "0" ),
  (Other, whenmod 4 1 (const silence) $ (0.5 ~>) $ s "bev*30" # begin "<0.3 0.3 0.8 0.8>/2" # attack 0.3 # release 0.75 # up "[0 -4 7]" # gain "0.7" # slow 21 (coarse $ fmap round $ scale 0 14 sine1) # room "0.2" # size "0.3" |*| (slow 3 $ speed "1 4 8 1 4") |*| pan "0 0.5 -0.5 -1 1" |*| pan (slow 2 sine) |*| iter 3 (pan "1 -1 0.5 -0.5") # orbit "1" # hpf "10000" |*| speed "2"),
  (Other, whenmod 4 3 (# vowel "o a") $ (0.7 ~>) $ whenmod 10 2 (const silence) $ s "bev*20 [~ bev*8?]" # begin "<0.3 0.3 0.8 0.8>/2" # attack 0.2 # release 1 # up "[0,-4 7]" # gain "0.7" # slow 21 (coarse $ fmap round $ scale 0 14 sine1) |*| accelerate "-1" # room "0.5" # size "0.9" |*| speed "1 4 8 1 4 0.5" # pan (slow 2 sine) |*| iter 3 (pan "1 -1 0.5 -0.5") # orbit "1")
  ]] 0.8333333333


nan_1::Snippet
nan_1 = Snippet "nan_1" [
  [
    (Chords, sound "pad2/8" # slow 16 (n "[4,7,11] [5,9,12]") # end "1.5" # gain "1.3")
  ],[
    (Chords, sound "pad2/8" # slow 16 (n "[4,7,11] [5,9,12]") # end "1.5" # gain "1.3"),
    (Melody, whenmod 4 2 (jux rev) $ whenmod 8 7 (slow 2) $ striate 2 $ sound "casio arpy arpy casio? arpy casio/2" # density 2 (accelerate "0.2 -0.5") # slow 21 (coarse (fmap round (scale 0 8 sine1))) # slow 11 (cutoff (scale 1000 10000 sine1)) # resonance "0.3" |*| up "[0,4,7,11,14]")
  ],[
    (Chords, sound "pad2/8" # slow 16 (n "[4,7,11] [5,9,12]") # end "1.5" # gain "1.3"),
    (Melody, whenmod 4 2 (jux rev) $ whenmod 8 7 (slow 2) $ striate 2 $ sound "casio arpy arpy casio? arpy casio/2" # density 2 (accelerate "0.2 -0.5") # slow 21 (coarse (fmap round (scale 0 8 sine1))) # slow 11 (cutoff (scale 1000 10000 sine1)) # resonance "0.3" |*| up "[0,4,7,11,14]"),
    (Drums, juxBy 0.75 rev $ every 4 (fast 2) $ s "amencutup*8" # orbit "1" # room "0" # size "0" # legato 1 # n (palindrome $ run 8) # begin "0" # resonance "0.2"  # hpf "4000")
  ],[
    (Chords, sound "pad2/8" # slow 16 (n "[4,7,11] [5,9,12]") # end "1.5" # gain "1.3"),
    (Melody, whenmod 4 2 (jux rev) $ whenmod 8 7 (slow 2) $ striate 2 $ sound "casio arpy arpy casio? arpy casio/2" # density 2 (accelerate "0.2 -0.5") # slow 21 (coarse (fmap round (scale 0 8 sine1))) # slow 11 (cutoff (scale 1000 10000 sine1)) # resonance "0.3" |*| up "[0,4,7,11,14]"),
    (Drums, jux rev $ every 4 (fast 2) $ s "amencutup*8" # orbit "1" # room "0" # size "0" # legato 1 # n (palindrome $ run 8) # begin "0" # lpf (slow 8 $ scale 800 8000 sine) # resonance "0.2" )
  ]] 1

nan_2::Snippet
nan_2 = Snippet "nan_2" [
  [
    (Drums, sometimes ((density 2) . (rev)) $ sound "bd? [dr? dr? ~ ~,cp/4?]" # slow 21 (cutoff (scale 1000 10000 saw1)) # coarse "10")
  ],[
   (Drums, sometimes ((density 2) . (rev)) $ sound "bd? [dr? dr? ~ ~,cp/4?]" # slow 21 (cutoff (scale 1000 10000 saw1)) # coarse "10"),
   (Drums, whenmod 4 3 (superimpose (const $ sometimes ((# crush "5") . (|*| accelerate "-1.2 1")) $ sound "808bd:1 [808bd:1,hat*4?] snare*3? [<timp? tamb>]" # shape "0.7" # speed "0.75 1 1.6 1" # gain "0.9")) $ s "")
  ],[
    (Drums, sometimes ((density 2) . (rev)) $ sound "bd? [dr? dr? ~ ~,cp/4?]" # slow 21 (cutoff (scale 1000 10000 saw1)) # coarse "10"),
    (Drums, whenmod 4 3 (superimpose (const $ sometimes ((# crush "5") . (|*| accelerate "-1.2 1")) $ sound "808bd:1 [808bd:1,hat*4?] snare*3? [<timp? tamb>]" # shape "0.7" # speed "0.75 1 1.6 1" # gain "0.9")) $ s ""),
    (Chords, superimpose (# pan (scale (-1) 1 rand)) $ sometimes (|+| n "[12,0]") $ sound "[lzr/3 lzr/4?]"  # (slow 15 (n "[0 7 4 -1 -3]")) # iter 3 (slow 2 (speed "1 -1 1")) # slow 8 (coarse (fmap round (scale 0 10 saw1))) # gain "1.1" # end "3.5")
  ],[
    (Chords, superimpose (# pan (scale (-1) 1 rand)) $ sometimes (|+| n "[12,0]") $ sound "[lzr/3 lzr/4?]"  # (slow 15 (n "[0 7 4 -1 -3]")) # iter 3 (slow 2 (speed "1 -1 1")) # slow 8 (coarse (fmap round (scale 0 10 saw1))) # gain "1.1" # end "3.5")
  ]] 1


dre_1::Snippet
dre_1 = Snippet "dre_1"[
  [
    (Chords,whenmod 16 14 (|*| accelerate (slow 2 $ scale 0 (-0.1) saw)) $ (|*| (up "<[0] [-5]>/4")) $ stut "<2>" 0.5 0.75 $ begin "0 0.25 <0.5 0.5 0.5 [0.5 0.75]> 0.75" |*| begin "0.125" # s "<drebn:0>"  # legato 0.5 # gain "0.9"  |*| iter 4 (jux rev $ speed "1 [4,1] [-1,1] [1, <8 ~ 3 ~>]") # orbit "0")
  ],[
    (Chords,whenmod 16 14 (|*| accelerate (slow 2 $ scale 0 (-0.1) saw)) $ (|*| (up "<[0] [-5]>/4")) $ stut "<2>" 0.5 0.75 $ begin "0 0.25 <0.5 0.5 0.5 [0.5 0.75]> 0.75" |*| begin "0.125" # s "<drebn:0>"  # legato 0.5 # gain "0.9"  |*| iter 4 (jux rev $ speed "1 [4,1] [-1,1] [1, <8 ~ 3 ~>]") # orbit "0"),
    (Drums, stut 2 0.3 0.85 $ s "kick:5 hat:4 [kick:2,clap:2,snare:2,clap:0, ~ clap:5 ~ ~ snare:3/4 ~,conga/8] [dr:2,conga:1/4]")
  ],[
    (Chords, (# up "[0 0 [7] 0]") $ s "drebn*32" # attack 0.05 # release 0.1 # begin (slow 8 $ scale 0 0.5 saw) |+| begin (scale 0 0.05 rand) # room "0.4" # size "0.4" # orbit "1" # gain "1.06"),
    (Drums, stut 2 0.3 0.85 $ s "kick:5 hat:4 [kick:2,clap:2,snare:2,clap:0, ~ clap:5 ~ ~ snare:3/4 ~,conga/8] [dr:2,conga:1/4]")
  ],[
    (Chords, (# up "[0 0 [7] 0]") $ s "drebn*32" # attack 0.05 # release 0.1 # begin (slow 8 $ scale 0 0.5 saw) |+| begin (scale 0 0.05 rand) # room "0.4" # size "0.4" # orbit "1" # gain "1.06")
  ],[
    (Other,chop 2 $ (# pan rand) $ stut "4" 0.5 0.75  $ s "drebn"  # legato "1" # begin "<0 0 0.5 0.5 0.75 0.75 0.25 0.25>"  # n "[10,0]" # up "[0]")
    ],[
    (Other,slow 4 $ chop 64 $ s "drebn" # n "3" # legato 1 |*| speed "[1,<~ 0.5>]")
    ]
  ] drebn


snippets::[Snippet]
snippets = [dh_1, dh_2, dh_3, dh_4, dh_5, dh_6, amb_1, mel_1, onehundred, chill_1, altdh_1,nan_1, nan_2, dre_1, test,test2]

snippetMap:: M.Map String Snippet
snippetMap = M.fromList $ fmap (\k -> (handle k, k)) snippets

numPhrases:: Snippet -> Int
numPhrases = length . phrases

getNumPhrases:: String -> Int
getNumPhrases = numPhrases . ((M.!)) snippetMap

getPhrase:: String -> Int -> Phrase
getPhrase h i = maybe (last ps) id $ (atIndex i) ps
  where ps = phrases $ (M.!) snippetMap h

phraseToPattern::Phrase -> ParamPattern
phraseToPattern = stack . fmap snd

toPattern:: String -> Int -> ParamPattern
toPattern s i = phraseToPattern $ getPhrase s i

getStem::String -> Int -> Category -> ParamPattern
getStem s i c = maybe silence snd $ atIndex 0 $ filter (\x -> fst x == c) $ getPhrase s i

getStems::String -> Int ->[Category] -> ParamPattern
getStems s i c = maybe silence snd $ atIndex 0 $ filter (\x-> elem (fst x) c) $ getPhrase s i

toStems = getStems

toStem = getStem


-- cue':: String -> IO ()
-- cue' i = do
--   let snip = M.lookup i snippetMap
--   maybe (putStrLn $ "No snippet for: "++i) f snip
--   where
--     f s =
--       putStrLn $ (++) "snippet: " $ handle snip
--       putStrLn $ "phrases: "++phrases


cue:: String -> IO ()
cue i = do
  let snip = M.lookup i snippetMap
  putStrLn "\n\n"
  putStrLn "\n\n"
  putStrLn (maybe ("No snippet for: "++i) ((++) "Snippet: " . handle) snip)
  putStrLn "\n"
  putStrLn (maybe ("") ( ((++) "# phrases: ") . show .length .phrases ) snip)
  putStrLn "\n"
  putStrLn (maybe "" ((++) "cps: " . show . tempo) snip)
  putStrLn "\n"




  -- playPhrase:: Phrase -> IO ()
  -- playPhrase = do
  --   let pat = phraseToPattern


--
-- xfadeIn :: Time -> Time -> [ParamPattern] -> ParamPattern
-- xfadeIn _ _ [] = silence
-- xfadeIn _ _ (p:[]) = p
-- xfadeIn t now (p:p':_) = overlay (p |*| gain (now `rotR` (_slow t envEqR))) (p' |*| gain (now `rotR` (_slow t (envEq))))

--
-- filterXFadeIn:: Time -> Time -> [ParamPattern] -> ParamPattern
-- filterXFadeIn _ _ [] = silence
-- filterXFadeIn _ _ (p:[]) = p
-- filterXFadeIn t now (p:p':_) =


filterXFadeIn _ _ [] = silence
filterXFadeIn _ _ (p:[]) = p
filterXFadeIn t now (p:p':_) = overlay (p |*| lpf "18000" |*| lpf (now `rotR` (_slow t envL))) (p' |*| hpf "18000" |*| hpf (now `rotR` (_slow t envL)))

-- filterXFadeIn' _ _ [] = silence
-- filterXFadeIn' _ _ (p:[]) = p
-- filterXFadeIn' t now (p:p':_) = overlay (p |*| hpf "18000" |*| hpf (now `rotR` (_slow t envL))) (p' |*| lpf "18000" |*| lpf (now `rotR` (_slow t envL)))


-- Basic stuff..

scParam a = fst $ pF a (Just 0)

(lpfMul, lpfMul_p) = pF "lpfMul" (Just 0)

(at,at_p) = pF "at" (Just 0.01)

(dt,dt_p) = pF "dt" (Just 0.01)

reverb a b = room a # size b

delay' depth time feedback = (delay depth # delaytime time # delayfeedback feedback)
(portion,portion_p) = pF "portion" (Just 1)

(controlOrbit, controlOrbit_p) = pI "controlOrbit" (Just 0)
(cRatio,cRatio_p) = pF "cRatio" (Just 1)
(cThreshold, cThreshold_p) = pF "cThreshold" (Just 0)
(cAttack, cAttack_p) = pF "cAttack" (Just 0.01)
(cRelease, cRelease_p) = pF "cRelease" (Just 0.2)

compressor control ratio thresh atk rel = controlOrbit control # cRatio ratio # cThreshold thresh # cAttack atk # cRelease rel

(backgroundR, backroundR_p) = pI "backgroundR" (Just 0)
(backgroundG, backroundG_p) = pI "backgroundG" (Just 0)
(backgroundB, backroundB_p) = pI "backgroundB" (Just 0)

backgroundRGB r g b = (backgroundR r) # backgroundG g # backgroundB b
(midinote,midinote_p) = pF "midinote" (Just 69)


-- stem :: String -> ParamPattern
-- (stem, stem_p) = pStem (Nothing)


test::Snippet
test = Snippet "test" [
  [
    (Drums, s "bd dr cp dr")
  ]
  ] 1

test2::Snippet
test2 = Snippet "test2" [
  [
    (Drums, s "kurt kurt kurt [kurt casio/3]")
  ]
  ] 1


hmm = stem "test test2"

-- ParamPattern:: (arc->[Event (Map Param Value)])
stem :: Pattern String -> ParamPattern
stem pat = Pattern (\a-> concat $ fmap (\(a1,_,s) -> filter (\((s,_),_,_)-> isIn a1 s) $ (arc $ maybe silence id $ M.lookup s S.stems) a)   (arc pat $ a))
-- stem pat = Pattern (\a-> concat $ fmap (\(a1,_,s) -> filter (\((s,_),_,_)-> isIn a1 s) $ (arc $ toPattern s 1) a)   (arc pat $ a))

-- (arc $ maybe silence id $ M.lookup s S.stems)

-- stem pat = Pattern (\a-> concat $ fmap (\(a1,_,s) -> filter (\((s,_),_,_)-> isIn a1 s) $ (arc $ maybe silence id $ M.lookup S.stems s) a)  (arc pat $ a))
-- stem pat = stack . fmap f . (arc pat) -- (arc pat) a::[Event String]
-- stem pat = Pattern (\a -> (arc $ stack $ fmap f $ (arc pat) a) a)
-- stem pat = Pattern (\a -> (arc $ stack $ fmap f $ (arc pat) a) a)
-- $ (arc pat) a

-- stem pat = Pattern (\a-> concat $ fmap (\((s1,e1),_,s) -> filter (\((s1',e1'),_,_)-> s1<=s1' && s1 >=e1') $ (arc $ toPattern s 1) a)   (arc pat $ a))
-- stem pat = Pattern (\a-> concat $ fmap (\((s1,e1),_,s) -> filter (\((s1',e1'),_,_)-> s1<=s1' && s1' <=e1) $ (arc $ toPattern s 1) a)   (arc pat $ a))

-- [(Arc,Arc,String)]

-- ParamPattern -> Arc

-- Pattern String -> ... Pattern {arc:: Arc->[Event (Map Param Value)]}

f :: Event String -> ParamPattern
f ((s1,e1),(s2,e2),val) = sliceArc (s1,e1) $  toPattern val 1 -- silence if 'val' not found
-- f ((s1,e1),(s2,e2),val) = compress (s1,e1) $ zoom (s1,e1) $ toPattern val 1 -- silence if 'val' not found






-- --    OSC str   dflt val
-- pS :: String -> Maybe String -> (Pattern String -> ParamPattern, Param)
-- pS name defaultV = (make' VS param, param)
--   where param = S name defaultV
--
--
-- make' :: ParamType a => (a -> Value) -> Param -> Pattern a -> ParamPattern
-- make' toValue par p = fmap (\x -> Map.singleton par (toValue x)) p
--
--
-- data Param = S {name :: String, sDefault :: Maybe String}
--            | F {name :: String, fDefault :: Maybe Double}
--            | I {name :: String, iDefault :: Maybe Int}
--   deriving Typeable
--
-- type ParamMap = Map.Map Param Value
--
-- type ParamPattern = Pattern ParamMap
--
-- -- | The pattern datatype, a function from a time @Arc@ to @Event@ values
-- data Pattern a = Pattern {arc :: Arc -> [Event a]}
--   deriving Typeable
--
-- type Event a = (Arc, Arc, a)
--
-- -- (Start, End)
-- type Arc = (Time, Time)
--


-- Util...
atIndex index list = if length list < index+1 || index < 0 then Nothing else Just $ list!!index
