{-# LANGUAGE CPP #-}
#ifndef INCLUDE_MODEL
module Text.TinySegmenter.Score ( bias, b1, b2, b3, e1, e2, e3
                                , Marker(..), CType(..)
                                , h, i, k, o, a, n, m, mo, mb, mu
                                , getCTypes
                                , up1, up2, up3
                                , bp1, bp2, uw1, uw2, uw3, uw4, uw5, uw6
                                , bw1, bw2, bw3, tw1, tw2, tw3, tw4
                                , uc1, uc2, uc3, uc4, uc5, uc6
                                , bc1, bc2, bc3, tc1, tc2, tc3, tc4
                                , uq1, uq2, uq3, bq1, bq2, bq3, bq4
                                , tq1, tq2, tq3, tq4
                                ) where

import           Data.Char
#endif
-- bias
bias :: Int
bias = -332

-- markers, out side of the range of unicode codepoint
b1, b2, b3, e1, e2, e3 :: Int
b1 = 0x110001
b2 = 0x110002
b3 = 0x110003
e1 = 0x110004
e2 = 0x110005
e3 = 0x110006

getCTypes :: Int -> CType
getCTypes c
  | c `elem` ml
  = m
  | (ord '一' <= c && c <= ord '龠') || c `elem` hl
  = h
  | ord 'ぁ' <= c && c <= ord 'ん'
  = i
  | (ord 'ァ' <= c && c <= ord 'ヴ')
    || (ord 'ｱ' <= c && c <= ord 'ﾝ') || c `elem` ksubl
  = k
  | (ord 'a' <= c && c <= ord 'z')
    || (ord 'A' <= c && c <= ord 'Z')
    || (ord 'ａ' <= c && c <= ord 'ｚ')
    || (ord 'Ａ' <= c && c <= ord 'Ｚ')
  = a
  | (ord '0' <= c && c <= ord '9') || (ord '０' <= c && c <= ord '９')
  = n
  | otherwise
  = o
  where
    ml    = fmap ord ("一二三四五六七八九十百千万億兆" :: String)
    hl    = fmap ord ("々〆ヵヶ" :: String)
    ksubl = fmap ord ("ーｰ\xff9e" :: String)
{-# INLINE getCTypes #-}

data CType = CTH | CTI | CTK |CTO | CTA | CTN | CTM

h, i, k, o, a, n, m :: CType
h = CTH
i = CTI
k = CTK
o = CTO
a = CTA
n = CTN
m = CTM
{-# INLINE h #-}
{-# INLINE i #-}
{-# INLINE k #-}
{-# INLINE o #-}
{-# INLINE a #-}
{-# INLINE n #-}
{-# INLINE m #-}

data Marker = MO | MB | MU

mo, mb, mu :: Marker
mo = MO
mb = MB
mu = MU
{-# INLINE mo #-}
{-# INLINE mb #-}
{-# INLINE mu #-}

bc1 :: (CType, CType) -> Int
bc1 (CTH, CTH) = 6
bc1 (CTI, CTI) = 2461
bc1 (CTK, CTH) = 406
bc1 (CTO, CTH) = -1378
bc1 _ = 0
{-# INLINE bc1 #-}

bc2 :: (CType, CType) -> Int
bc2 (CTA, CTA) = -3267
bc2 (CTA, CTI) = 2744
bc2 (CTA, CTN) = -878
bc2 (CTH, CTH) = -4070
bc2 (CTH, CTM) = -1711
bc2 (CTH, CTN) = 4012
bc2 (CTH, CTO) = 3761
bc2 (CTI, CTA) = 1327
bc2 (CTI, CTH) = -1184
bc2 (CTI, CTI) = -1332
bc2 (CTI, CTK) = 1721
bc2 (CTI, CTO) = 5492
bc2 (CTK, CTI) = 3831
bc2 (CTK, CTK) = -8741
bc2 (CTM, CTH) = -3132
bc2 (CTM, CTK) = 3334
bc2 (CTO, CTO) = -2920
bc2 _ = 0
{-# INLINE bc2 #-}

bc3 :: (CType, CType) -> Int
bc3 (CTH, CTH) = 996
bc3 (CTH, CTI) = 626
bc3 (CTH, CTK) = -721
bc3 (CTH, CTN) = -1307
bc3 (CTH, CTO) = -836
bc3 (CTI, CTH) = -301
bc3 (CTK, CTK) = 2762
bc3 (CTM, CTK) = 1079
bc3 (CTM, CTM) = 4034
bc3 (CTO, CTA) = -1652
bc3 (CTO, CTH) = 266
bc3 _ = 0
{-# INLINE bc3 #-}

bp1 :: (Marker, Marker) -> Int
bp1 (MB, MB) = 295
bp1 (MO, MB) = 304
bp1 (MO, MO) = -125
bp1 (MU, MB) = 352
bp1 _ = 0
{-# INLINE bp1 #-}

bp2 :: (Marker, Marker) -> Int
bp2 (MB, MO) = 60
bp2 (MO, MO) = -1762
bp2 _ = 0
{-# INLINE bp2 #-}

bq1 :: (Marker, CType, CType) -> Int
bq1 (MB, CTH, CTH) = 1150
bq1 (MB, CTH, CTM) = 1521
bq1 (MB, CTI, CTI) = -1158
bq1 (MB, CTI, CTM) = 886
bq1 (MB, CTM, CTH) = 1208
bq1 (MB, CTN, CTH) = 449
bq1 (MB, CTO, CTH) = -91
bq1 (MB, CTO, CTO) = -2597
bq1 (MO, CTH, CTI) = 451
bq1 (MO, CTI, CTH) = -296
bq1 (MO, CTK, CTA) = 1851
bq1 (MO, CTK, CTH) = -1020
bq1 (MO, CTK, CTK) = 904
bq1 (MO, CTO, CTO) = 2965
bq1 _ = 0
{-# INLINE bq1 #-}

bq2 :: (Marker, CType, CType) -> Int
bq2 (MB, CTH, CTH) = 118
bq2 (MB, CTH, CTI) = -1159
bq2 (MB, CTH, CTM) = 466
bq2 (MB, CTI, CTH) = -919
bq2 (MB, CTK, CTK) = -1720
bq2 (MB, CTK, CTO) = 864
bq2 (MO, CTH, CTH) = -1139
bq2 (MO, CTH, CTM) = -181
bq2 (MO, CTI, CTH) = 153
bq2 (MU, CTH, CTI) = -1146
bq2 _ = 0
{-# INLINE bq2 #-}

bq3 :: (Marker, CType, CType) -> Int
bq3 (MB, CTH, CTH) = -792
bq3 (MB, CTH, CTI) = 2664
bq3 (MB, CTI, CTI) = -299
bq3 (MB, CTK, CTI) = 419
bq3 (MB, CTM, CTH) = 937
bq3 (MB, CTM, CTM) = 8335
bq3 (MB, CTN, CTN) = 998
bq3 (MB, CTO, CTH) = 775
bq3 (MO, CTH, CTH) = 2174
bq3 (MO, CTH, CTM) = 439
bq3 (MO, CTI, CTI) = 280
bq3 (MO, CTK, CTH) = 1798
bq3 (MO, CTK, CTI) = -793
bq3 (MO, CTK, CTO) = -2242
bq3 (MO, CTM, CTH) = -2402
bq3 (MO, CTO, CTO) = 11699
bq3 _ = 0
{-# INLINE bq3 #-}

bq4 :: (Marker, CType, CType) -> Int
bq4 (MB, CTH, CTH) = -3895
bq4 (MB, CTI, CTH) = 3761
bq4 (MB, CTI, CTI) = -4654
bq4 (MB, CTI, CTK) = 1348
bq4 (MB, CTK, CTK) = -1806
bq4 (MB, CTM, CTM) = -3385
bq4 (MB, CTO, CTO) = -12396
bq4 (MO, CTA, CTA) = 926
bq4 (MO, CTH, CTH) = 266
bq4 (MO, CTH, CTK) = -2036
bq4 (MO, CTN, CTN) = -973
bq4 _ = 0
{-# INLINE bq4 #-}

bw1 :: (Int, Int) -> Int
bw1 t | t == (ord ',', ord 'と') = 660
      | t == (ord ',', ord '同') = 727
      | t == (b1, ord 'あ') = 1404
      | t == (b1, ord '同') = 542
      | t == (ord '、', ord 'と') = 660
      | t == (ord '、', ord '同') = 727
      | t == (ord '」', ord 'と') = 1682
      | t == (ord 'あ', ord 'っ') = 1505
      | t == (ord 'い', ord 'う') = 1743
      | t == (ord 'い', ord 'っ') = -2055
      | t == (ord 'い', ord 'る') = 672
      | t == (ord 'う', ord 'し') = -4817
      | t == (ord 'う', ord 'ん') = 665
      | t == (ord 'か', ord 'ら') = 3472
      | t == (ord 'が', ord 'ら') = 600
      | t == (ord 'こ', ord 'う') = -790
      | t == (ord 'こ', ord 'と') = 2083
      | t == (ord 'こ', ord 'ん') = -1262
      | t == (ord 'さ', ord 'ら') = -4143
      | t == (ord 'さ', ord 'ん') = 4573
      | t == (ord 'し', ord 'た') = 2641
      | t == (ord 'し', ord 'て') = 1104
      | t == (ord 'す', ord 'で') = -3399
      | t == (ord 'そ', ord 'こ') = 1977
      | t == (ord 'そ', ord 'れ') = -871
      | t == (ord 'た', ord 'ち') = 1122
      | t == (ord 'た', ord 'め') = 601
      | t == (ord 'っ', ord 'た') = 3463
      | t == (ord 'つ', ord 'い') = -802
      | t == (ord 'て', ord 'い') = 805
      | t == (ord 'て', ord 'き') = 1249
      | t == (ord 'で', ord 'き') = 1127
      | t == (ord 'で', ord 'す') = 3445
      | t == (ord 'で', ord 'は') = 844
      | t == (ord 'と', ord 'い') = -4915
      | t == (ord 'と', ord 'み') = 1922
      | t == (ord 'ど', ord 'こ') = 3887
      | t == (ord 'な', ord 'い') = 5713
      | t == (ord 'な', ord 'っ') = 3015
      | t == (ord 'な', ord 'ど') = 7379
      | t == (ord 'な', ord 'ん') = -1113
      | t == (ord 'に', ord 'し') = 2468
      | t == (ord 'に', ord 'は') = 1498
      | t == (ord 'に', ord 'も') = 1671
      | t == (ord 'に', ord '対') = -912
      | t == (ord 'の', ord '一') = -501
      | t == (ord 'の', ord '中') = 741
      | t == (ord 'ま', ord 'せ') = 2448
      | t == (ord 'ま', ord 'で') = 1711
      | t == (ord 'ま', ord 'ま') = 2600
      | t == (ord 'ま', ord 'る') = -2155
      | t == (ord 'や', ord 'む') = -1947
      | t == (ord 'よ', ord 'っ') = -2565
      | t == (ord 'れ', ord 'た') = 2369
      | t == (ord 'れ', ord 'で') = -913
      | t == (ord 'を', ord 'し') = 1860
      | t == (ord 'を', ord '見') = 731
      | t == (ord '亡', ord 'く') = -1886
      | t == (ord '京', ord '都') = 2558
      | t == (ord '取', ord 'り') = -2784
      | t == (ord '大', ord 'き') = -2604
      | t == (ord '大', ord '阪') = 1497
      | t == (ord '平', ord '方') = -2314
      | t == (ord '引', ord 'き') = -1336
      | t == (ord '日', ord '本') = -195
      | t == (ord '本', ord '当') = -2423
      | t == (ord '毎', ord '日') = -2113
      | t == (ord '目', ord '指') = -724
      | t == (b1, ord 'あ') = 1404
      | t == (b1, ord '同') = 542
      | t == (ord '｣', ord 'と') = 1682
      | otherwise = 0
{-# INLINE bw1 #-}

bw2 :: (Int, Int) -> Int
bw2 t | t == (ord '.', ord '.') = -11822
      | t == (ord '1', ord '1') = -669
      | t == (ord '―', ord '―') = -5730
      | t == (ord '−', ord '−') = -13175
      | t == (ord 'い', ord 'う') = -1609
      | t == (ord 'う', ord 'か') = 2490
      | t == (ord 'か', ord 'し') = -1350
      | t == (ord 'か', ord 'も') = -602
      | t == (ord 'か', ord 'ら') = -7194
      | t == (ord 'か', ord 'れ') = 4612
      | t == (ord 'が', ord 'い') = 853
      | t == (ord 'が', ord 'ら') = -3198
      | t == (ord 'き', ord 'た') = 1941
      | t == (ord 'く', ord 'な') = -1597
      | t == (ord 'こ', ord 'と') = -8392
      | t == (ord 'こ', ord 'の') = -4193
      | t == (ord 'さ', ord 'せ') = 4533
      | t == (ord 'さ', ord 'れ') = 13168
      | t == (ord 'さ', ord 'ん') = -3977
      | t == (ord 'し', ord 'い') = -1819
      | t == (ord 'し', ord 'か') = -545
      | t == (ord 'し', ord 'た') = 5078
      | t == (ord 'し', ord 'て') = 972
      | t == (ord 'し', ord 'な') = 939
      | t == (ord 'そ', ord 'の') = -3744
      | t == (ord 'た', ord 'い') = -1253
      | t == (ord 'た', ord 'た') = -662
      | t == (ord 'た', ord 'だ') = -3857
      | t == (ord 'た', ord 'ち') = -786
      | t == (ord 'た', ord 'と') = 1224
      | t == (ord 'た', ord 'は') = -939
      | t == (ord 'っ', ord 'た') = 4589
      | t == (ord 'っ', ord 'て') = 1647
      | t == (ord 'っ', ord 'と') = -2094
      | t == (ord 'て', ord 'い') = 6144
      | t == (ord 'て', ord 'き') = 3640
      | t == (ord 'て', ord 'く') = 2551
      | t == (ord 'て', ord 'は') = -3110
      | t == (ord 'て', ord 'も') = -3065
      | t == (ord 'で', ord 'い') = 2666
      | t == (ord 'で', ord 'き') = -1528
      | t == (ord 'で', ord 'し') = -3828
      | t == (ord 'で', ord 'す') = -4761
      | t == (ord 'で', ord 'も') = -4203
      | t == (ord 'と', ord 'い') = 1890
      | t == (ord 'と', ord 'こ') = -1746
      | t == (ord 'と', ord 'と') = -2279
      | t == (ord 'と', ord 'の') = 720
      | t == (ord 'と', ord 'み') = 5168
      | t == (ord 'と', ord 'も') = -3941
      | t == (ord 'な', ord 'い') = -2488
      | t == (ord 'な', ord 'が') = -1313
      | t == (ord 'な', ord 'ど') = -6509
      | t == (ord 'な', ord 'の') = 2614
      | t == (ord 'な', ord 'ん') = 3099
      | t == (ord 'に', ord 'お') = -1615
      | t == (ord 'に', ord 'し') = 2748
      | t == (ord 'に', ord 'な') = 2454
      | t == (ord 'に', ord 'よ') = -7236
      | t == (ord 'に', ord '対') = -14943
      | t == (ord 'に', ord '従') = -4688
      | t == (ord 'に', ord '関') = -11388
      | t == (ord 'の', ord 'か') = 2093
      | t == (ord 'の', ord 'で') = -7059
      | t == (ord 'の', ord 'に') = -6041
      | t == (ord 'の', ord 'の') = -6125
      | t == (ord 'は', ord 'い') = 1073
      | t == (ord 'は', ord 'が') = -1033
      | t == (ord 'は', ord 'ず') = -2532
      | t == (ord 'ば', ord 'れ') = 1813
      | t == (ord 'ま', ord 'し') = -1316
      | t == (ord 'ま', ord 'で') = -6621
      | t == (ord 'ま', ord 'れ') = 5409
      | t == (ord 'め', ord 'て') = -3153
      | t == (ord 'も', ord 'い') = 2230
      | t == (ord 'も', ord 'の') = -10713
      | t == (ord 'ら', ord 'か') = -944
      | t == (ord 'ら', ord 'し') = -1611
      | t == (ord 'ら', ord 'に') = -1897
      | t == (ord 'り', ord 'し') = 651
      | t == (ord 'り', ord 'ま') = 1620
      | t == (ord 'れ', ord 'た') = 4270
      | t == (ord 'れ', ord 'て') = 849
      | t == (ord 'れ', ord 'ば') = 4114
      | t == (ord 'ろ', ord 'う') = 6067
      | t == (ord 'わ', ord 'れ') = 7901
      | t == (ord 'を', ord '通') = -11877
      | t == (ord 'ん', ord 'だ') = 728
      | t == (ord 'ん', ord 'な') = -4115
      | t == (ord '一', ord '人') = 602
      | t == (ord '一', ord '方') = -1375
      | t == (ord '一', ord '日') = 970
      | t == (ord '一', ord '部') = -1051
      | t == (ord '上', ord 'が') = -4479
      | t == (ord '会', ord '社') = -1116
      | t == (ord '出', ord 'て') = 2163
      | t == (ord '分', ord 'の') = -7758
      | t == (ord '同', ord '党') = 970
      | t == (ord '同', ord '日') = -913
      | t == (ord '大', ord '阪') = -2471
      | t == (ord '委', ord '員') = -1250
      | t == (ord '少', ord 'な') = -1050
      | t == (ord '年', ord '度') = -8669
      | t == (ord '年', ord '間') = -1626
      | t == (ord '府', ord '県') = -2363
      | t == (ord '手', ord '権') = -1982
      | t == (ord '新', ord '聞') = -4066
      | t == (ord '日', ord '新') = -722
      | t == (ord '日', ord '本') = -7068
      | t == (ord '日', ord '米') = 3372
      | t == (ord '曜', ord '日') = -601
      | t == (ord '朝', ord '鮮') = -2355
      | t == (ord '本', ord '人') = -2697
      | t == (ord '東', ord '京') = -1543
      | t == (ord '然', ord 'と') = -1384
      | t == (ord '社', ord '会') = -1276
      | t == (ord '立', ord 'て') = -990
      | t == (ord '第', ord 'に') = -1612
      | t == (ord '米', ord '国') = -4268
      | t == (ord '１', ord '１') = -669
      | t == (ord 'ｸ', ord 'ﾞ') = 1319
      | otherwise = 0
{-# INLINE bw2 #-}

bw3 :: (Int, Int) -> Int
bw3 t | t == (ord 'あ', ord 'た') = -2194
      | t == (ord 'あ', ord 'り') = 719
      | t == (ord 'あ', ord 'る') = 3846
      | t == (ord 'い', ord '.') = -1185
      | t == (ord 'い', ord '。') = -1185
      | t == (ord 'い', ord 'い') = 5308
      | t == (ord 'い', ord 'え') = 2079
      | t == (ord 'い', ord 'く') = 3029
      | t == (ord 'い', ord 'た') = 2056
      | t == (ord 'い', ord 'っ') = 1883
      | t == (ord 'い', ord 'る') = 5600
      | t == (ord 'い', ord 'わ') = 1527
      | t == (ord 'う', ord 'ち') = 1117
      | t == (ord 'う', ord 'と') = 4798
      | t == (ord 'え', ord 'と') = 1454
      | t == (ord 'か', ord '.') = 2857
      | t == (ord 'か', ord '。') = 2857
      | t == (ord 'か', ord 'け') = -743
      | t == (ord 'か', ord 'っ') = -4098
      | t == (ord 'か', ord 'に') = -669
      | t == (ord 'か', ord 'ら') = 6520
      | t == (ord 'か', ord 'り') = -2670
      | t == (ord 'が', ord ',') = 1816
      | t == (ord 'が', ord '、') = 1816
      | t == (ord 'が', ord 'き') = -4855
      | t == (ord 'が', ord 'け') = -1127
      | t == (ord 'が', ord 'っ') = -913
      | t == (ord 'が', ord 'ら') = -4977
      | t == (ord 'が', ord 'り') = -2064
      | t == (ord 'き', ord 'た') = 1645
      | t == (ord 'け', ord 'ど') = 1374
      | t == (ord 'こ', ord 'と') = 7397
      | t == (ord 'こ', ord 'の') = 1542
      | t == (ord 'こ', ord 'ろ') = -2757
      | t == (ord 'さ', ord 'い') = -714
      | t == (ord 'さ', ord 'を') = 976
      | t == (ord 'し', ord ',') = 1557
      | t == (ord 'し', ord '、') = 1557
      | t == (ord 'し', ord 'い') = -3714
      | t == (ord 'し', ord 'た') = 3562
      | t == (ord 'し', ord 'て') = 1449
      | t == (ord 'し', ord 'な') = 2608
      | t == (ord 'し', ord 'ま') = 1200
      | t == (ord 'す', ord '.') = -1310
      | t == (ord 'す', ord '。') = -1310
      | t == (ord 'す', ord 'る') = 6521
      | t == (ord 'ず', ord ',') = 3426
      | t == (ord 'ず', ord '、') = 3426
      | t == (ord 'ず', ord 'に') = 841
      | t == (ord 'そ', ord 'う') = 428
      | t == (ord 'た', ord '.') = 8875
      | t == (ord 'た', ord '。') = 8875
      | t == (ord 'た', ord 'い') = -594
      | t == (ord 'た', ord 'の') = 812
      | t == (ord 'た', ord 'り') = -1183
      | t == (ord 'た', ord 'る') = -853
      | t == (ord 'だ', ord '.') = 4098
      | t == (ord 'だ', ord '。') = 4098
      | t == (ord 'だ', ord 'っ') = 1004
      | t == (ord 'っ', ord 'た') = -4748
      | t == (ord 'っ', ord 'て') = 300
      | t == (ord 'て', ord 'い') = 6240
      | t == (ord 'て', ord 'お') = 855
      | t == (ord 'て', ord 'も') = 302
      | t == (ord 'で', ord 'す') = 1437
      | t == (ord 'で', ord 'に') = -1482
      | t == (ord 'で', ord 'は') = 2295
      | t == (ord 'と', ord 'う') = -1387
      | t == (ord 'と', ord 'し') = 2266
      | t == (ord 'と', ord 'の') = 541
      | t == (ord 'と', ord 'も') = -3543
      | t == (ord 'ど', ord 'う') = 4664
      | t == (ord 'な', ord 'い') = 1796
      | t == (ord 'な', ord 'く') = -903
      | t == (ord 'な', ord 'ど') = 2135
      | t == (ord 'に', ord ',') = -1021
      | t == (ord 'に', ord '、') = -1021
      | t == (ord 'に', ord 'し') = 1771
      | t == (ord 'に', ord 'な') = 1906
      | t == (ord 'に', ord 'は') = 2644
      | t == (ord 'の', ord ',') = -724
      | t == (ord 'の', ord '、') = -724
      | t == (ord 'の', ord '子') = -1000
      | t == (ord 'は', ord ',') = 1337
      | t == (ord 'は', ord '、') = 1337
      | t == (ord 'べ', ord 'き') = 2181
      | t == (ord 'ま', ord 'し') = 1113
      | t == (ord 'ま', ord 'す') = 6943
      | t == (ord 'ま', ord 'っ') = -1549
      | t == (ord 'ま', ord 'で') = 6154
      | t == (ord 'ま', ord 'れ') = -793
      | t == (ord 'ら', ord 'し') = 1479
      | t == (ord 'ら', ord 'れ') = 6820
      | t == (ord 'る', ord 'る') = 3818
      | t == (ord 'れ', ord ',') = 854
      | t == (ord 'れ', ord '、') = 854
      | t == (ord 'れ', ord 'た') = 1850
      | t == (ord 'れ', ord 'て') = 1375
      | t == (ord 'れ', ord 'ば') = -3246
      | t == (ord 'れ', ord 'る') = 1091
      | t == (ord 'わ', ord 'れ') = -605
      | t == (ord 'ん', ord 'だ') = 606
      | t == (ord 'ん', ord 'で') = 798
      | t == (ord 'カ', ord '月') = 990
      | t == (ord '会', ord '議') = 860
      | t == (ord '入', ord 'り') = 1232
      | t == (ord '大', ord '会') = 2217
      | t == (ord '始', ord 'め') = 1681
      | t == (ord '市', ord ' ') = 965
      | t == (ord '新', ord '聞') = -5055
      | t == (ord '日', ord ',') = 974
      | t == (ord '日', ord '、') = 974
      | t == (ord '社', ord '会') = 2024
      | t == (ord 'ｶ', ord '月') = 990
      | otherwise = 0
{-# INLINE bw3 #-}

tc1 :: (CType, CType, CType) -> Int
tc1 (CTA, CTA, CTA) = 1093
tc1 (CTH, CTH, CTH) = 1029
tc1 (CTH, CTH, CTM) = 580
tc1 (CTH, CTI, CTI) = 998
tc1 (CTH, CTO, CTH) = -390
tc1 (CTH, CTO, CTM) = -331
tc1 (CTI, CTH, CTI) = 1169
tc1 (CTI, CTO, CTH) = -142
tc1 (CTI, CTO, CTI) = -1015
tc1 (CTI, CTO, CTM) = 467
tc1 (CTM, CTM, CTH) = 187
tc1 (CTO, CTO, CTI) = -1832
tc1 _ = 0
{-# INLINE tc1 #-}

tc2 :: (CType, CType, CType) -> Int
tc2 (CTH, CTH, CTO) = 2088
tc2 (CTH, CTI, CTI) = -1023
tc2 (CTH, CTM, CTM) = -1154
tc2 (CTI, CTH, CTI) = -1965
tc2 (CTK, CTK, CTH) = 703
tc2 (CTO, CTI, CTI) = -2649
tc2 _ = 0
{-# INLINE tc2 #-}

tc3 :: (CType, CType, CType) -> Int
tc3 (CTA, CTA, CTA) = -294
tc3 (CTH, CTH, CTH) = 346
tc3 (CTH, CTH, CTI) = -341
tc3 (CTH, CTI, CTI) = -1088
tc3 (CTH, CTI, CTK) = 731
tc3 (CTH, CTO, CTH) = -1486
tc3 (CTI, CTH, CTH) = 128
tc3 (CTI, CTH, CTI) = -3041
tc3 (CTI, CTH, CTO) = -1935
tc3 (CTI, CTI, CTH) = -825
tc3 (CTI, CTI, CTM) = -1035
tc3 (CTI, CTO, CTI) = -542
tc3 (CTK, CTH, CTH) = -1216
tc3 (CTK, CTK, CTA) = 491
tc3 (CTK, CTK, CTH) = -1217
tc3 (CTK, CTO, CTK) = -1009
tc3 (CTM, CTH, CTH) = -2694
tc3 (CTM, CTH, CTM) = -457
tc3 (CTM, CTH, CTO) = 123
tc3 (CTM, CTM, CTH) = -471
tc3 (CTN, CTN, CTH) = -1689
tc3 (CTN, CTN, CTO) = 662
tc3 (CTO, CTH, CTO) = -3393
tc3 _ = 0
{-# INLINE tc3 #-}

tc4 :: (CType, CType, CType) -> Int
tc4 (CTH, CTH, CTH) = -203
tc4 (CTH, CTH, CTI) = 1344
tc4 (CTH, CTH, CTK) = 365
tc4 (CTH, CTH, CTM) = -122
tc4 (CTH, CTH, CTN) = 182
tc4 (CTH, CTH, CTO) = 669
tc4 (CTH, CTI, CTH) = 804
tc4 (CTH, CTI, CTI) = 679
tc4 (CTH, CTO, CTH) = 446
tc4 (CTI, CTH, CTH) = 695
tc4 (CTI, CTH, CTO) = -2324
tc4 (CTI, CTI, CTH) = 321
tc4 (CTI, CTI, CTI) = 1497
tc4 (CTI, CTI, CTO) = 656
tc4 (CTI, CTO, CTO) = 54
tc4 (CTK, CTA, CTK) = 4845
tc4 (CTK, CTK, CTA) = 3386
tc4 (CTK, CTK, CTK) = 3065
tc4 (CTM, CTH, CTH) = -405
tc4 (CTM, CTH, CTI) = 201
tc4 (CTM, CTM, CTH) = -241
tc4 (CTM, CTM, CTM) = 661
tc4 (CTM, CTO, CTM) = 841
tc4 _ = 0
{-# INLINE tc4 #-}

tq1 :: (Marker, CType, CType, CType) -> Int
tq1 (MB, CTH, CTH, CTH) = -227
tq1 (MB, CTH, CTH, CTI) = 316
tq1 (MB, CTH, CTI, CTH) = -132
tq1 (MB, CTI, CTH, CTH) = 60
tq1 (MB, CTI, CTI, CTI) = 1595
tq1 (MB, CTN, CTH, CTH) = -744
tq1 (MB, CTO, CTH, CTH) = 225
tq1 (MB, CTO, CTO, CTO) = -908
tq1 (MO, CTA, CTK, CTK) = 482
tq1 (MO, CTH, CTH, CTH) = 281
tq1 (MO, CTH, CTI, CTH) = 249
tq1 (MO, CTI, CTH, CTI) = 200
tq1 (MO, CTI, CTI, CTH) = -68
tq1 _ = 0
{-# INLINE tq1 #-}

tq2 :: (Marker, CType, CType, CType) -> Int
tq2 (MB, CTI, CTH, CTH) = -1401
tq2 (MB, CTI, CTI, CTI) = -1033
tq2 (MB, CTK, CTA, CTK) = -543
tq2 (MB, CTO, CTO, CTO) = -5591
tq2 _ = 0
{-# INLINE tq2 #-}

tq3 :: (Marker, CType, CType, CType) -> Int
tq3 (MB, CTH, CTH, CTH) = 478
tq3 (MB, CTH, CTH, CTM) = -1073
tq3 (MB, CTH, CTI, CTH) = 222
tq3 (MB, CTH, CTI, CTI) = -504
tq3 (MB, CTI, CTI, CTH) = -116
tq3 (MB, CTI, CTI, CTI) = -105
tq3 (MB, CTM, CTH, CTI) = -863
tq3 (MB, CTM, CTH, CTM) = -464
tq3 (MB, CTO, CTM, CTH) = 620
tq3 (MO, CTH, CTH, CTH) = 346
tq3 (MO, CTH, CTH, CTI) = 1729
tq3 (MO, CTH, CTI, CTI) = 997
tq3 (MO, CTH, CTM, CTH) = 481
tq3 (MO, CTI, CTH, CTH) = 623
tq3 (MO, CTI, CTI, CTH) = 1344
tq3 (MO, CTK, CTA, CTK) = 2792
tq3 (MO, CTK, CTH, CTH) = 587
tq3 (MO, CTK, CTK, CTA) = 679
tq3 (MO, CTO, CTH, CTH) = 110
tq3 (MO, CTO, CTI, CTI) = -685
tq3 _ = 0
{-# INLINE tq3 #-}

tq4 :: (Marker, CType, CType, CType) -> Int
tq4 (MB, CTH, CTH, CTH) = -721
tq4 (MB, CTH, CTH, CTM) = -3604
tq4 (MB, CTH, CTI, CTI) = -966
tq4 (MB, CTI, CTI, CTH) = -607
tq4 (MB, CTI, CTI, CTI) = -2181
tq4 (MO, CTA, CTA, CTA) = -2763
tq4 (MO, CTA, CTK, CTK) = 180
tq4 (MO, CTH, CTH, CTH) = -294
tq4 (MO, CTH, CTH, CTI) = 2446
tq4 (MO, CTH, CTH, CTO) = 480
tq4 (MO, CTH, CTI, CTH) = -1573
tq4 (MO, CTI, CTH, CTH) = 1935
tq4 (MO, CTI, CTH, CTI) = -493
tq4 (MO, CTI, CTI, CTH) = 626
tq4 (MO, CTI, CTI, CTI) = -4007
tq4 (MO, CTK, CTA, CTK) = -8156
tq4 _ = 0
{-# INLINE tq4 #-}

tw1 :: (Int, Int, Int) -> Int
tw1 t | t == (ord 'に', ord 'つ', ord 'い') = -4681
      | t == (ord '東', ord '京', ord '都') = 2026
      | otherwise = 0
{-# INLINE tw1 #-}

tw2 :: (Int, Int, Int) -> Int
tw2 t | t == (ord 'あ', ord 'る', ord '程') = -2049
      | t == (ord 'い', ord 'っ', ord 'た') = -1256
      | t == (ord 'こ', ord 'ろ', ord 'が') = -2434
      | t == (ord 'し', ord 'ょ', ord 'う') = 3873
      | t == (ord 'そ', ord 'の', ord '後') = -4430
      | t == (ord 'だ', ord 'っ', ord 'て') = -1049
      | t == (ord 'て', ord 'い', ord 'た') = 1833
      | t == (ord 'と', ord 'し', ord 'て') = -4657
      | t == (ord 'と', ord 'も', ord 'に') = -4517
      | t == (ord 'も', ord 'の', ord 'で') = 1882
      | t == (ord '一', ord '気', ord 'に') = -792
      | t == (ord '初', ord 'め', ord 'て') = -1512
      | t == (ord '同', ord '時', ord 'に') = -8097
      | t == (ord '大', ord 'き', ord 'な') = -1255
      | t == (ord '対', ord 'し', ord 'て') = -2721
      | t == (ord '社', ord '会', ord '党') = -3216
      | otherwise = 0
{-# INLINE tw2 #-}

tw3 :: (Int, Int, Int) -> Int
tw3 t | t == (ord 'い', ord 'た', ord 'だ') = -1734
      | t == (ord 'し', ord 'て', ord 'い') = 1314
      | t == (ord 'と', ord 'し', ord 'て') = -4314
      | t == (ord 'に', ord 'つ', ord 'い') = -5483
      | t == (ord 'に', ord 'と', ord 'っ') = -5989
      | t == (ord 'に', ord '当', ord 'た') = -6247
      | t == (ord 'の', ord 'で', ord ',') = -727
      | t == (ord 'の', ord 'で', ord '、') = -727
      | t == (ord 'の', ord 'も', ord 'の') = -600
      | t == (ord 'れ', ord 'か', ord 'ら') = -3752
      | t == (ord '十', ord '二', ord '月') = -2287
      | otherwise = 0
{-# INLINE tw3 #-}

tw4 :: (Int, Int, Int) -> Int
tw4 t | t == (ord 'い', ord 'う', ord '.') = 8576
      | t == (ord 'い', ord 'う', ord '。') = 8576
      | t == (ord 'か', ord 'ら', ord 'な') = -2348
      | t == (ord 'し', ord 'て', ord 'い') = 2958
      | t == (ord 'た', ord 'が', ord ',') = 1516
      | t == (ord 'た', ord 'が', ord '、') = 1516
      | t == (ord 'て', ord 'い', ord 'る') = 1538
      | t == (ord 'と', ord 'い', ord 'う') = 1349
      | t == (ord 'ま', ord 'し', ord 'た') = 5543
      | t == (ord 'ま', ord 'せ', ord 'ん') = 1097
      | t == (ord 'よ', ord 'う', ord 'と') = -4258
      | t == (ord 'よ', ord 'る', ord 'と') = 5865
      | otherwise = 0
{-# INLINE tw4 #-}

uc1 :: CType -> Int
uc1 CTA = 484
uc1 CTK = 93
uc1 CTM = 645
uc1 CTO = -505
uc1 _ = 0
{-# INLINE uc1 #-}

uc2 :: CType -> Int
uc2 CTA = 819
uc2 CTH = 1059
uc2 CTI = 409
uc2 CTM = 3987
uc2 CTN = 5775
uc2 CTO = 646
uc2 _ = 0
{-# INLINE uc2 #-}

uc3 :: CType -> Int
uc3 CTA = -1370
uc3 CTI = 2311
uc3 _ = 0
{-# INLINE uc3 #-}

uc4 :: CType -> Int
uc4 CTA = -2643
uc4 CTH = 1809
uc4 CTI = -1032
uc4 CTK = -3450
uc4 CTM = 3565
uc4 CTN = 3876
uc4 CTO = 6646
{-# INLINE uc4 #-}

uc5 :: CType -> Int
uc5 CTH = 313
uc5 CTI = -1238
uc5 CTK = -799
uc5 CTM = 539
uc5 CTO = -831
uc5 _ = 0
{-# INLINE uc5 #-}

uc6 :: CType -> Int
uc6 CTH = -506
uc6 CTI  = -253
uc6 CTK  = 87
uc6 CTM  = 247
uc6 CTO  = -387
uc6 _ = 0
{-# INLINE uc6 #-}

up1 :: Marker -> Int
up1 MO = -214
up1 _ = 0
{-# INLINE up1 #-}

up2 :: Marker -> Int
up2 MB = 69
up2 MO = 935
up2 _ = 0
{-# INLINE up2 #-}

up3 :: Marker -> Int
up3 MB = 189
up3 _ = 0
{-# INLINE up3 #-}

uq1 :: (Marker, CType) -> Int
uq1 (MB, CTH) = 21
uq1 (MB, CTI) = -12
uq1 (MB, CTK) = -99
uq1 (MB, CTN) = 142
uq1 (MB, CTO) = -56
uq1 (MO, CTH) = -95
uq1 (MO, CTI) = 477
uq1 (MO, CTK) = 410
uq1 (MO, CTO) = -2422
uq1 _ = 0
{-# INLINE uq1 #-}

uq2 :: (Marker, CType) -> Int
uq2 (MB, CTH) = 216
uq2 (MB, CTI) = 113
uq2 (MO, CTK) = 1759
uq2 _ = 0
{-# INLINE uq2 #-}

uq3 :: (Marker, CType) -> Int
uq3 (MB, CTA) = -479
uq3 (MB, CTH) = 42
uq3 (MB, CTI) = 1913
uq3 (MB, CTK) = -7198
uq3 (MB, CTM) = 3160
uq3 (MB, CTN) = 6427
uq3 (MB, CTO) = 14761
uq3 (MO, CTI) = -827
uq3 (MO, CTN) = -3212
uq3 _ = 0
{-# INLINE uq3 #-}

uw1 :: Int -> Int
uw1 t | t == ord ',' = 156
      | t == ord '、' = 156
      | t == ord '「' = -463
      | t == ord 'あ' = -941
      | t == ord 'う' = -127
      | t == ord 'が' = -553
      | t == ord 'き' = 121
      | t == ord 'こ' = 505
      | t == ord 'で' = -201
      | t == ord 'と' = -547
      | t == ord 'ど' = -123
      | t == ord 'に' = -789
      | t == ord 'の' = -185
      | t == ord 'は' = -847
      | t == ord 'も' = -466
      | t == ord 'や' = -470
      | t == ord 'よ' = 182
      | t == ord 'ら' = -292
      | t == ord 'り' = 208
      | t == ord 'れ' = 169
      | t == ord 'を' = -446
      | t == ord 'ん' = -137
      | t == ord '・' = -135
      | t == ord '主' = -402
      | t == ord '京' = -268
      | t == ord '区' = -912
      | t == ord '午' = 871
      | t == ord '国' = -460
      | t == ord '大' = 561
      | t == ord '委' = 729
      | t == ord '市' = -411
      | t == ord '日' = -141
      | t == ord '理' = 361
      | t == ord '生' = -408
      | t == ord '県' = -386
      | t == ord '都' = -718
      | t == ord '｢' = -463
      | t == ord '･' = -135
      | otherwise = 0
{-# INLINE uw1 #-}

uw2 :: Int -> Int
uw2 t | t == ord ',' = -829
      | t == ord '、' = -829
      | t == ord '〇' = 892
      | t == ord '「' = -645
      | t == ord '」' = 3145
      | t == ord 'あ' = -538
      | t == ord 'い' = 505
      | t == ord 'う' = 134
      | t == ord 'お' = -502
      | t == ord 'か' = 1454
      | t == ord 'が' = -856
      | t == ord 'く' = -412
      | t == ord 'こ' = 1141
      | t == ord 'さ' = 878
      | t == ord 'ざ' = 540
      | t == ord 'し' = 1529
      | t == ord 'す' = -675
      | t == ord 'せ' = 300
      | t == ord 'そ' = -1011
      | t == ord 'た' = 188
      | t == ord 'だ' = 1837
      | t == ord 'つ' = -949
      | t == ord 'て' = -291
      | t == ord 'で' = -268
      | t == ord 'と' = -981
      | t == ord 'ど' = 1273
      | t == ord 'な' = 1063
      | t == ord 'に' = -1764
      | t == ord 'の' = 130
      | t == ord 'は' = -409
      | t == ord 'ひ' = -1273
      | t == ord 'べ' = 1261
      | t == ord 'ま' = 600
      | t == ord 'も' = -1263
      | t == ord 'や' = -402
      | t == ord 'よ' = 1639
      | t == ord 'り' = -579
      | t == ord 'る' = -694
      | t == ord 'れ' = 571
      | t == ord 'を' = -2516
      | t == ord 'ん' = 2095
      | t == ord 'ア' = -587
      | t == ord 'カ' = 306
      | t == ord 'キ' = 568
      | t == ord 'ッ' = 831
      | t == ord '三' = -758
      | t == ord '不' = -2150
      | t == ord '世' = -302
      | t == ord '中' = -968
      | t == ord '主' = -861
      | t == ord '事' = 492
      | t == ord '人' = -123
      | t == ord '会' = 978
      | t == ord '保' = 362
      | t == ord '入' = 548
      | t == ord '初' = -3025
      | t == ord '副' = -1566
      | t == ord '北' = -3414
      | t == ord '区' = -422
      | t == ord '大' = -1769
      | t == ord '天' = -865
      | t == ord '太' = -483
      | t == ord '子' = -1519
      | t == ord '学' = 760
      | t == ord '実' = 1023
      | t == ord '小' = -2009
      | t == ord '市' = -813
      | t == ord '年' = -1060
      | t == ord '強' = 1067
      | t == ord '手' = -1519
      | t == ord '揺' = -1033
      | t == ord '政' = 1522
      | t == ord '文' = -1355
      | t == ord '新' = -1682
      | t == ord '日' = -1815
      | t == ord '明' = -1462
      | t == ord '最' = -630
      | t == ord '朝' = -1843
      | t == ord '本' = -1650
      | t == ord '東' = -931
      | t == ord '果' = -665
      | t == ord '次' = -2378
      | t == ord '民' = -180
      | t == ord '気' = -1740
      | t == ord '理' = 752
      | t == ord '発' = 529
      | t == ord '目' = -1584
      | t == ord '相' = -242
      | t == ord '県' = -1165
      | t == ord '立' = -763
      | t == ord '第' = 810
      | t == ord '米' = 509
      | t == ord '自' = -1353
      | t == ord '行' = 838
      | t == ord '西' = -744
      | t == ord '見' = -3874
      | t == ord '調' = 1010
      | t == ord '議' = 1198
      | t == ord '込' = 3041
      | t == ord '開' = 1758
      | t == ord '間' = -1257
      | t == ord '｢' = -645
      | t == ord '｣' = 3145
      | t == ord 'ｯ' = 831
      | t == ord 'ｱ' = -587
      | t == ord 'ｶ' = 306
      | t == ord 'ｷ' = 568
      | otherwise = 0
{-# INLINE uw2 #-}

uw3 :: Int -> Int
uw3 t | t == ord ',' = 4889
      | t == ord '1' = -800
      | t == ord '−' = -1723
      | t == ord '、' = 4889
      | t == ord '々' = -2311
      | t == ord '〇' = 5827
      | t == ord '」' = 2670
      | t == ord '〓' = -3573
      | t == ord 'あ' = -2696
      | t == ord 'い' = 1006
      | t == ord 'う' = 2342
      | t == ord 'え' = 1983
      | t == ord 'お' = -4864
      | t == ord 'か' = -1163
      | t == ord 'が' = 3271
      | t == ord 'く' = 1004
      | t == ord 'け' = 388
      | t == ord 'げ' = 401
      | t == ord 'こ' = -3552
      | t == ord 'ご' = -3116
      | t == ord 'さ' = -1058
      | t == ord 'し' = -395
      | t == ord 'す' = 584
      | t == ord 'せ' = 3685
      | t == ord 'そ' = -5228
      | t == ord 'た' = 842
      | t == ord 'ち' = -521
      | t == ord 'っ' = -1444
      | t == ord 'つ' = -1081
      | t == ord 'て' = 6167
      | t == ord 'で' = 2318
      | t == ord 'と' = 1691
      | t == ord 'ど' = -899
      | t == ord 'な' = -2788
      | t == ord 'に' = 2745
      | t == ord 'の' = 4056
      | t == ord 'は' = 4555
      | t == ord 'ひ' = -2171
      | t == ord 'ふ' = -1798
      | t == ord 'へ' = 1199
      | t == ord 'ほ' = -5516
      | t == ord 'ま' = -4384
      | t == ord 'み' = -120
      | t == ord 'め' = 1205
      | t == ord 'も' = 2323
      | t == ord 'や' = -788
      | t == ord 'よ' = -202
      | t == ord 'ら' = 727
      | t == ord 'り' = 649
      | t == ord 'る' = 5905
      | t == ord 'れ' = 2773
      | t == ord 'わ' = -1207
      | t == ord 'を' = 6620
      | t == ord 'ん' = -518
      | t == ord 'ア' = 551
      | t == ord 'グ' = 1319
      | t == ord 'ス' = 874
      | t == ord 'ッ' = -1350
      | t == ord 'ト' = 521
      | t == ord 'ム' = 1109
      | t == ord 'ル' = 1591
      | t == ord 'ロ' = 2201
      | t == ord 'ン' = 278
      | t == ord '・' = -3794
      | t == ord '一' = -1619
      | t == ord '下' = -1759
      | t == ord '世' = -2087
      | t == ord '両' = 3815
      | t == ord '中' = 653
      | t == ord '主' = -758
      | t == ord '予' = -1193
      | t == ord '二' = 974
      | t == ord '人' = 2742
      | t == ord '今' = 792
      | t == ord '他' = 1889
      | t == ord '以' = -1368
      | t == ord '低' = 811
      | t == ord '何' = 4265
      | t == ord '作' = -361
      | t == ord '保' = -2439
      | t == ord '元' = 4858
      | t == ord '党' = 3593
      | t == ord '全' = 1574
      | t == ord '公' = -3030
      | t == ord '六' = 755
      | t == ord '共' = -1880
      | t == ord '円' = 5807
      | t == ord '再' = 3095
      | t == ord '分' = 457
      | t == ord '初' = 2475
      | t == ord '別' = 1129
      | t == ord '前' = 2286
      | t == ord '副' = 4437
      | t == ord '力' = 365
      | t == ord '動' = -949
      | t == ord '務' = -1872
      | t == ord '化' = 1327
      | t == ord '北' = -1038
      | t == ord '区' = 4646
      | t == ord '千' = -2309
      | t == ord '午' = -783
      | t == ord '協' = -1006
      | t == ord '口' = 483
      | t == ord '右' = 1233
      | t == ord '各' = 3588
      | t == ord '合' = -241
      | t == ord '同' = 3906
      | t == ord '和' = -837
      | t == ord '員' = 4513
      | t == ord '国' = 642
      | t == ord '型' = 1389
      | t == ord '場' = 1219
      | t == ord '外' = -241
      | t == ord '妻' = 2016
      | t == ord '学' = -1356
      | t == ord '安' = -423
      | t == ord '実' = -1008
      | t == ord '家' = 1078
      | t == ord '小' = -513
      | t == ord '少' = -3102
      | t == ord '州' = 1155
      | t == ord '市' = 3197
      | t == ord '平' = -1804
      | t == ord '年' = 2416
      | t == ord '広' = -1030
      | t == ord '府' = 1605
      | t == ord '度' = 1452
      | t == ord '建' = -2352
      | t == ord '当' = -3885
      | t == ord '得' = 1905
      | t == ord '思' = -1291
      | t == ord '性' = 1822
      | t == ord '戸' = -488
      | t == ord '指' = -3973
      | t == ord '政' = -2013
      | t == ord '教' = -1479
      | t == ord '数' = 3222
      | t == ord '文' = -1489
      | t == ord '新' = 1764
      | t == ord '日' = 2099
      | t == ord '旧' = 5792
      | t == ord '昨' = -661
      | t == ord '時' = -1248
      | t == ord '曜' = -951
      | t == ord '最' = -937
      | t == ord '月' = 4125
      | t == ord '期' = 360
      | t == ord '李' = 3094
      | t == ord '村' = 364
      | t == ord '東' = -805
      | t == ord '核' = 5156
      | t == ord '森' = 2438
      | t == ord '業' = 484
      | t == ord '氏' = 2613
      | t == ord '民' = -1694
      | t == ord '決' = -1073
      | t == ord '法' = 1868
      | t == ord '海' = -495
      | t == ord '無' = 979
      | t == ord '物' = 461
      | t == ord '特' = -3850
      | t == ord '生' = -273
      | t == ord '用' = 914
      | t == ord '町' = 1215
      | t == ord '的' = 7313
      | t == ord '直' = -1835
      | t == ord '省' = 792
      | t == ord '県' = 6293
      | t == ord '知' = -1528
      | t == ord '私' = 4231
      | t == ord '税' = 401
      | t == ord '立' = -960
      | t == ord '第' = 1201
      | t == ord '米' = 7767
      | t == ord '系' = 3066
      | t == ord '約' = 3663
      | t == ord '級' = 1384
      | t == ord '統' = -4229
      | t == ord '総' = 1163
      | t == ord '線' = 1255
      | t == ord '者' = 6457
      | t == ord '能' = 725
      | t == ord '自' = -2869
      | t == ord '英' = 785
      | t == ord '見' = 1044
      | t == ord '調' = -562
      | t == ord '財' = -733
      | t == ord '費' = 1777
      | t == ord '車' = 1835
      | t == ord '軍' = 1375
      | t == ord '込' = -1504
      | t == ord '通' = -1136
      | t == ord '選' = -681
      | t == ord '郎' = 1026
      | t == ord '郡' = 4404
      | t == ord '部' = 1200
      | t == ord '金' = 2163
      | t == ord '長' = 421
      | t == ord '開' = -1432
      | t == ord '間' = 1302
      | t == ord '関' = -1282
      | t == ord '雨' = 2009
      | t == ord '電' = -1045
      | t == ord '非' = 2066
      | t == ord '駅' = 1620
      | t == ord '１' = -800
      | t == ord '｣' = 2670
      | t == ord '･' = -3794
      | t == ord 'ｯ' = -1350
      | t == ord 'ｱ' = 551
      | t == ord 'ｽ' = 874
      | t == ord 'ﾄ' = 521
      | t == ord 'ﾑ' = 1109
      | t == ord 'ﾙ' = 1591
      | t == ord 'ﾛ' = 2201
      | t == ord 'ﾝ' = 278
      | otherwise = 0
{-# INLINE uw3 #-}

uw4 :: Int -> Int
uw4 t | t == ord ',' = 3930
      | t == ord '.' = 3508
      | t == ord '―' = -4841
      | t == ord '、' = 3930
      | t == ord '。' = 3508
      | t == ord '〇' = 4999
      | t == ord '「' = 1895
      | t == ord '」' = 3798
      | t == ord '〓' = -5156
      | t == ord 'あ' = 4752
      | t == ord 'い' = -3435
      | t == ord 'う' = -640
      | t == ord 'え' = -2514
      | t == ord 'お' = 2405
      | t == ord 'か' = 530
      | t == ord 'が' = 6006
      | t == ord 'き' = -4482
      | t == ord 'ぎ' = -3821
      | t == ord 'く' = -3788
      | t == ord 'け' = -4376
      | t == ord 'げ' = -4734
      | t == ord 'こ' = 2255
      | t == ord 'ご' = 1979
      | t == ord 'さ' = 2864
      | t == ord 'し' = -843
      | t == ord 'じ' = -2506
      | t == ord 'す' = -731
      | t == ord 'ず' = 1251
      | t == ord 'せ' = 181
      | t == ord 'そ' = 4091
      | t == ord 'た' = 5034
      | t == ord 'だ' = 5408
      | t == ord 'ち' = -3654
      | t == ord 'っ' = -5882
      | t == ord 'つ' = -1659
      | t == ord 'て' = 3994
      | t == ord 'で' = 7410
      | t == ord 'と' = 4547
      | t == ord 'な' = 5433
      | t == ord 'に' = 6499
      | t == ord 'ぬ' = 1853
      | t == ord 'ね' = 1413
      | t == ord 'の' = 7396
      | t == ord 'は' = 8578
      | t == ord 'ば' = 1940
      | t == ord 'ひ' = 4249
      | t == ord 'び' = -4134
      | t == ord 'ふ' = 1345
      | t == ord 'へ' = 6665
      | t == ord 'べ' = -744
      | t == ord 'ほ' = 1464
      | t == ord 'ま' = 1051
      | t == ord 'み' = -2082
      | t == ord 'む' = -882
      | t == ord 'め' = -5046
      | t == ord 'も' = 4169
      | t == ord 'ゃ' = -2666
      | t == ord 'や' = 2795
      | t == ord 'ょ' = -1544
      | t == ord 'よ' = 3351
      | t == ord 'ら' = -2922
      | t == ord 'り' = -9726
      | t == ord 'る' = -14896
      | t == ord 'れ' = -2613
      | t == ord 'ろ' = -4570
      | t == ord 'わ' = -1783
      | t == ord 'を' = 13150
      | t == ord 'ん' = -2352
      | t == ord 'カ' = 2145
      | t == ord 'コ' = 1789
      | t == ord 'セ' = 1287
      | t == ord 'ッ' = -724
      | t == ord 'ト' = -403
      | t == ord 'メ' = -1635
      | t == ord 'ラ' = -881
      | t == ord 'リ' = -541
      | t == ord 'ル' = -856
      | t == ord 'ン' = -3637
      | t == ord '・' = -4371
      | t == ord 'ー' = -11870
      | t == ord '一' = -2069
      | t == ord '中' = 2210
      | t == ord '予' = 782
      | t == ord '事' = -190
      | t == ord '井' = -1768
      | t == ord '人' = 1036
      | t == ord '以' = 544
      | t == ord '会' = 950
      | t == ord '体' = -1286
      | t == ord '作' = 530
      | t == ord '側' = 4292
      | t == ord '先' = 601
      | t == ord '党' = -2006
      | t == ord '共' = -1212
      | t == ord '内' = 584
      | t == ord '円' = 788
      | t == ord '初' = 1347
      | t == ord '前' = 1623
      | t == ord '副' = 3879
      | t == ord '力' = -302
      | t == ord '動' = -740
      | t == ord '務' = -2715
      | t == ord '化' = 776
      | t == ord '区' = 4517
      | t == ord '協' = 1013
      | t == ord '参' = 1555
      | t == ord '合' = -1834
      | t == ord '和' = -681
      | t == ord '員' = -910
      | t == ord '器' = -851
      | t == ord '回' = 1500
      | t == ord '国' = -619
      | t == ord '園' = -1200
      | t == ord '地' = 866
      | t == ord '場' = -1410
      | t == ord '塁' = -2094
      | t == ord '士' = -1413
      | t == ord '多' = 1067
      | t == ord '大' = 571
      | t == ord '子' = -4802
      | t == ord '学' = -1397
      | t == ord '定' = -1057
      | t == ord '寺' = -809
      | t == ord '小' = 1910
      | t == ord '屋' = -1328
      | t == ord '山' = -1500
      | t == ord '島' = -2056
      | t == ord '川' = -2667
      | t == ord '市' = 2771
      | t == ord '年' = 374
      | t == ord '庁' = -4556
      | t == ord '後' = 456
      | t == ord '性' = 553
      | t == ord '感' = 916
      | t == ord '所' = -1566
      | t == ord '支' = 856
      | t == ord '改' = 787
      | t == ord '政' = 2182
      | t == ord '教' = 704
      | t == ord '文' = 522
      | t == ord '方' = -856
      | t == ord '日' = 1798
      | t == ord '時' = 1829
      | t == ord '最' = 845
      | t == ord '月' = -9066
      | t == ord '木' = -485
      | t == ord '来' = -442
      | t == ord '校' = -360
      | t == ord '業' = -1043
      | t == ord '氏' = 5388
      | t == ord '民' = -2716
      | t == ord '気' = -910
      | t == ord '沢' = -939
      | t == ord '済' = -543
      | t == ord '物' = -735
      | t == ord '率' = 672
      | t == ord '球' = -1267
      | t == ord '生' = -1286
      | t == ord '産' = -1101
      | t == ord '田' = -2900
      | t == ord '町' = 1826
      | t == ord '的' = 2586
      | t == ord '目' = 922
      | t == ord '省' = -3485
      | t == ord '県' = 2997
      | t == ord '空' = -867
      | t == ord '立' = -2112
      | t == ord '第' = 788
      | t == ord '米' = 2937
      | t == ord '系' = 786
      | t == ord '約' = 2171
      | t == ord '経' = 1146
      | t == ord '統' = -1169
      | t == ord '総' = 940
      | t == ord '線' = -994
      | t == ord '署' = 749
      | t == ord '者' = 2145
      | t == ord '能' = -730
      | t == ord '般' = -852
      | t == ord '行' = -792
      | t == ord '規' = 792
      | t == ord '警' = -1184
      | t == ord '議' = -244
      | t == ord '谷' = -1000
      | t == ord '賞' = 730
      | t == ord '車' = -1481
      | t == ord '軍' = 1158
      | t == ord '輪' = -1433
      | t == ord '込' = -3370
      | t == ord '近' = 929
      | t == ord '道' = -1291
      | t == ord '選' = 2596
      | t == ord '郎' = -4866
      | t == ord '都' = 1192
      | t == ord '野' = -1100
      | t == ord '銀' = -2213
      | t == ord '長' = 357
      | t == ord '間' = -2344
      | t == ord '院' = -2297
      | t == ord '際' = -2604
      | t == ord '電' = -878
      | t == ord '領' = -1659
      | t == ord '題' = -792
      | t == ord '館' = -1984
      | t == ord '首' = 1749
      | t == ord '高' = 2120
      | t == ord '｢' = 1895
      | t == ord '｣' = 3798
      | t == ord '･' = -4371
      | t == ord 'ｯ' = -724
      | t == ord 'ｰ' = -11870
      | t == ord 'ｶ' = 2145
      | t == ord 'ｺ' = 1789
      | t == ord 'ｾ' = 1287
      | t == ord 'ﾄ' = -403
      | t == ord 'ﾒ' = -1635
      | t == ord 'ﾗ' = -881
      | t == ord 'ﾘ' = -541
      | t == ord 'ﾙ' = -856
      | t == ord 'ﾝ' = -3637
      | otherwise = 0
{-# INLINE uw4 #-}

uw5 :: Int -> Int
uw5 t | t == ord ',' = 465
      | t == ord '.' = -299
      | t == ord '1' = -514
      | t == e2 = -32768
      | t == ord ']' = -2762
      | t == ord '、' = 465
      | t == ord '。' = -299
      | t == ord '「' = 363
      | t == ord 'あ' = 1655
      | t == ord 'い' = 331
      | t == ord 'う' = -503
      | t == ord 'え' = 1199
      | t == ord 'お' = 527
      | t == ord 'か' = 647
      | t == ord 'が' = -421
      | t == ord 'き' = 1624
      | t == ord 'ぎ' = 1971
      | t == ord 'く' = 312
      | t == ord 'げ' = -983
      | t == ord 'さ' = -1537
      | t == ord 'し' = -1371
      | t == ord 'す' = -852
      | t == ord 'だ' = -1186
      | t == ord 'ち' = 1093
      | t == ord 'っ' = 52
      | t == ord 'つ' = 921
      | t == ord 'て' = -18
      | t == ord 'で' = -850
      | t == ord 'と' = -127
      | t == ord 'ど' = 1682
      | t == ord 'な' = -787
      | t == ord 'に' = -1224
      | t == ord 'の' = -635
      | t == ord 'は' = -578
      | t == ord 'べ' = 1001
      | t == ord 'み' = 502
      | t == ord 'め' = 865
      | t == ord 'ゃ' = 3350
      | t == ord 'ょ' = 854
      | t == ord 'り' = -208
      | t == ord 'る' = 429
      | t == ord 'れ' = 504
      | t == ord 'わ' = 419
      | t == ord 'を' = -1264
      | t == ord 'ん' = 327
      | t == ord 'イ' = 241
      | t == ord 'ル' = 451
      | t == ord 'ン' = -343
      | t == ord '中' = -871
      | t == ord '京' = 722
      | t == ord '会' = -1153
      | t == ord '党' = -654
      | t == ord '務' = 3519
      | t == ord '区' = -901
      | t == ord '告' = 848
      | t == ord '員' = 2104
      | t == ord '大' = -1296
      | t == ord '学' = -548
      | t == ord '定' = 1785
      | t == ord '嵐' = -1304
      | t == ord '市' = -2991
      | t == ord '席' = 921
      | t == ord '年' = 1763
      | t == ord '思' = 872
      | t == ord '所' = -814
      | t == ord '挙' = 1618
      | t == ord '新' = -1682
      | t == ord '日' = 218
      | t == ord '月' = -4353
      | t == ord '査' = 932
      | t == ord '格' = 1356
      | t == ord '機' = -1508
      | t == ord '氏' = -1347
      | t == ord '田' = 240
      | t == ord '町' = -3912
      | t == ord '的' = -3149
      | t == ord '相' = 1319
      | t == ord '省' = -1052
      | t == ord '県' = -4003
      | t == ord '研' = -997
      | t == ord '社' = -278
      | t == ord '空' = -813
      | t == ord '統' = 1955
      | t == ord '者' = -2233
      | t == ord '表' = 663
      | t == ord '語' = -1073
      | t == ord '議' = 1219
      | t == ord '選' = -1018
      | t == ord '郎' = -368
      | t == ord '長' = 786
      | t == ord '間' = 1191
      | t == ord '題' = 2368
      | t == ord '館' = -689
      | t == ord '１' = -514
      | t == e2 = -32768
      | t == ord '｢' = 363
      | t == ord 'ｲ' = 241
      | t == ord 'ﾙ' = 451
      | t == ord 'ﾝ' = -343
      | otherwise = 0
{-# INLINE uw5 #-}

uw6 :: Int -> Int
uw6 t | t == ord ',' = 227
      | t == ord '.' = 808
      | t == ord '1' = -270
      | t == e1 = 306
      | t == ord '、' = 227
      | t == ord '。' = 808
      | t == ord 'あ' = -307
      | t == ord 'う' = 189
      | t == ord 'か' = 241
      | t == ord 'が' = -73
      | t == ord 'く' = -121
      | t == ord 'こ' = -200
      | t == ord 'じ' = 1782
      | t == ord 'す' = 383
      | t == ord 'た' = -428
      | t == ord 'っ' = 573
      | t == ord 'て' = -1014
      | t == ord 'で' = 101
      | t == ord 'と' = -105
      | t == ord 'な' = -253
      | t == ord 'に' = -149
      | t == ord 'の' = -417
      | t == ord 'は' = -236
      | t == ord 'も' = -206
      | t == ord 'り' = 187
      | t == ord 'る' = -135
      | t == ord 'を' = 195
      | t == ord 'ル' = -673
      | t == ord 'ン' = -496
      | t == ord '一' = -277
      | t == ord '中' = 201
      | t == ord '件' = -800
      | t == ord '会' = 624
      | t == ord '前' = 302
      | t == ord '区' = 1792
      | t == ord '員' = -1212
      | t == ord '委' = 798
      | t == ord '学' = -960
      | t == ord '市' = 887
      | t == ord '広' = -695
      | t == ord '後' = 535
      | t == ord '業' = -697
      | t == ord '相' = 753
      | t == ord '社' = -507
      | t == ord '福' = 974
      | t == ord '空' = -822
      | t == ord '者' = 1811
      | t == ord '連' = 463
      | t == ord '郎' = 1082
      | t == ord '１' = -270
      | t == e1 = 306
      | t == ord 'ﾙ' = -673
      | t == ord 'ﾝ' = -496
      | otherwise = 0
{-# INLINE uw6 #-}
