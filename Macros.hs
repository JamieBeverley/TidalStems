{-# LANGUAGE OverloadedStrings #-}

module Macros where

import Sound.Tidal.Context

-- Tempos
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

-- Transitions
filterXFadeIn _ _ [] = silence
filterXFadeIn _ _ (p:[]) = p
filterXFadeIn t now (p:p':_) = overlay (p |*| lpf "18000" |*| lpf (now `rotR` (_slow t envL))) (p' |*| hpf "18000" |*| hpf (now `rotR` (_slow t envL)))

-- filterXFadeIn' _ _ [] = silence
-- filterXFadeIn' _ _ (p:[]) = p
-- filterXFadeIn' t now (p:p':_) = overlay (p |*| hpf "18000" |*| hpf (now `rotR` (_slow t envL))) (p' |*| lpf "18000" |*| lpf (now `rotR` (_slow t envL)))


-- ParamPatterns
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


-- Visuals
(rectX,rectX_p) = pF "rectX" Nothing
(rectY,rectY_p) = pF "rectY" Nothing
(rectW,rectW_p) = pF "rectW" Nothing
(rectH,rectH_p) = pF "rectH" Nothing

rect x y w h = rectX x # rectY y # rectW w # rectH h

(ellipseX,ellipseX_p) = pF "ellipseX" Nothing
(ellipseY,ellipseY_p) = pF "ellipseY" Nothing
(ellipseW,ellipseW_p) = pF "ellipseW" Nothing
(ellipseH,ellipseH_p) = pF "ellipseH" Nothing

ellipse x y w h = ellipseX x # ellipseY y # ellipseW w # ellipseH h

setOrbitColorR orbit = fst $ pF ("colorR" ++ show orbit) Nothing
setOrbitColorG orbit = fst $ pF ("colorG" ++ show orbit) Nothing
setOrbitColorB orbit = fst $ pF ("colorB" ++ show orbit) Nothing

setOrbitColor orbit r g b = setOrbitColorR orbit r # setOrbitColorG orbit g # setOrbitColorB orbit b

(drawOrbit, drawOrbit_p) = pI "drawOrbit" Nothing

(backgroundR, backroundR_p) = pI "backgroundR" (Just 0)
(backgroundG, backroundG_p) = pI "backgroundG" (Just 0)
(backgroundB, backroundB_p) = pI "backgroundB" (Just 0)

backgroundRGB r g b = (backgroundR r) # backgroundG g # backgroundB b
(midinote,midinote_p) = pF "midinote" (Just 69)
