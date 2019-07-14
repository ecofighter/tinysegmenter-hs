{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
module Text.TinySegmenter.ByteString.Lazy
  ( tokenize
  , tokenizeToVec
  )
where

import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import           Data.Char
import           Data.Functor.Identity
import qualified Data.ByteString.Builder       as BSB
import qualified Data.ByteString.Lazy          as BSL
import qualified Data.ByteString.Lazy.UTF8     as BSU
import qualified Data.List                     as L
import qualified Data.Vector                   as V
import qualified Data.Vector.Mutable           as MV

#define INCLUDE_MODEL
#include "../Model.hs"

takeThree :: BSL.ByteString -> (Int, Int, Int, BSL.ByteString)
takeThree text = case BSU.uncons text of
  Nothing       -> (e1, e2, e3, text)
  Just (ca, ra) -> case BSU.uncons ra of
    Nothing       -> (ord ca, e1, e2, ra)
    Just (cb, rb) -> case BSU.uncons rb of
      Nothing       -> (ord ca, ord cb, e1, rb)
      Just (cc, rc) -> (ord ca, ord cb, ord cc, rc)
{-# INLINE takeThree #-}

tokenToText :: BSB.Builder -> BSL.ByteString
tokenToText = BSB.toLazyByteString
{-# INLINE tokenToText #-}

data TokenizeState = TS { remain :: !BSL.ByteString
                        , token :: !BSB.Builder
                        , score :: {-# UNPACK #-} !Int
                        , p1 :: !Marker
                        , p2 :: !Marker
                        , p3 :: !Marker
                        , w1 :: {-# UNPACK #-} !Int
                        , w2 :: {-# UNPACK #-} !Int
                        , w3 :: {-# UNPACK #-} !Int
                        , w4 :: {-# UNPACK #-} !Int
                        , w5 :: {-# UNPACK #-} !Int
                        , w6 :: {-# UNPACK #-} !Int
                        , c1 :: !CType
                        , c2 :: !CType
                        , c3 :: !CType
                        , c4 :: !CType
                        , c5 :: !CType
                        , c6 :: !CType
                        }

initialState :: BSL.ByteString -> TokenizeState
initialState text =
  let (one, two, three, rmn) = takeThree text
  in  TS { remain      = rmn
         , token       = BSB.lazyByteString BSL.empty
         , score       = bias
         , p1          = MU
         , p2          = MU
         , p3          = MU
         , w1          = b1
         , w2          = b2
         , w3          = b3
         , w4          = one
         , w5          = two
         , w6          = three
         , c1          = CTO
         , c2          = CTO
         , c3          = CTO
         , c4          = getCTypes one
         , c5          = getCTypes two
         , c6          = getCTypes three
         }
{-# INLINE initialState #-}

moveNext :: Monad m => StateT TokenizeState m ()
moveNext = do
  s@TS {..} <- get
  when (w4 < e1) $ do
    let (newW6, newRemain) = case BSU.uncons remain of
              Just (c, r) -> (ord c, r)
              Nothing -> if | w6 == e1  -> (e2, BSL.empty)
                            | w6 == e2  -> (e3, BSL.empty)
                            | otherwise -> (e1, BSL.empty)
    put $ s { remain = newRemain
            , score  = bias
            , w1     = w2
            , w2     = w3
            , w3     = w4
            , w4     = w5
            , w5     = w6
            , w6     = newW6
            , c1     = c2
            , c2     = c3
            , c3     = c4
            , c4     = c5
            , c5     = c6
            , c6     = getCTypes newW6
            }
{-# INLINE moveNext #-}

updateScore :: Monad m => StateT TokenizeState m ()
updateScore = do
  modify $ \s -> update s $ up1 (p1 s)
  modify $ \s -> update s $ up2 (p2 s)
  modify $ \s -> update s $ up3 (p3 s)
  modify $ \s -> update s $ bp1 (p1 s, p2 s)
  modify $ \s -> update s $ bp2 (p2 s, p3 s)
  modify $ \s -> update s $ uw1 (w1 s)
  modify $ \s -> update s $ uw2 (w2 s)
  modify $ \s -> update s $ uw3 (w3 s)
  modify $ \s -> update s $ uw4 (w4 s)
  modify $ \s -> update s $ uw5 (w5 s)
  modify $ \s -> update s $ uw6 (w6 s)
  modify $ \s -> update s $ bw1 (w2 s, w3 s)
  modify $ \s -> update s $ bw2 (w3 s, w4 s)
  modify $ \s -> update s $ bw3 (w4 s, w5 s)
  modify $ \s -> update s $ tw1 (w1 s, w2 s, w3 s)
  modify $ \s -> update s $ tw2 (w2 s, w3 s, w4 s)
  modify $ \s -> update s $ tw3 (w3 s, w4 s, w5 s)
  modify $ \s -> update s $ tw4 (w4 s, w5 s, w6 s)
  modify $ \s -> update s $ uc1 (c1 s)
  modify $ \s -> update s $ uc2 (c2 s)
  modify $ \s -> update s $ uc3 (c3 s)
  modify $ \s -> update s $ uc4 (c4 s)
  modify $ \s -> update s $ uc5 (c5 s)
  modify $ \s -> update s $ uc6 (c6 s)
  modify $ \s -> update s $ bc1 (c2 s, c3 s)
  modify $ \s -> update s $ bc2 (c3 s, c4 s)
  modify $ \s -> update s $ bc3 (c4 s, c5 s)
  modify $ \s -> update s $ tc1 (c1 s, c2 s, c3 s)
  modify $ \s -> update s $ tc2 (c2 s, c3 s, c4 s)
  modify $ \s -> update s $ tc3 (c3 s, c4 s, c5 s)
  modify $ \s -> update s $ tc4 (c4 s, c5 s, c6 s)
  modify $ \s -> update s $ uq1 (p1 s, c1 s)
  modify $ \s -> update s $ uq2 (p2 s, c2 s)
  modify $ \s -> update s $ uq3 (p3 s, c3 s)
  modify $ \s -> update s $ bq1 (p2 s, c2 s, c3 s)
  modify $ \s -> update s $ bq2 (p2 s, c3 s, c4 s)
  modify $ \s -> update s $ bq3 (p3 s, c2 s, c3 s)
  modify $ \s -> update s $ bq4 (p3 s, c3 s, c4 s)
  modify $ \s -> update s $ tq1 (p2 s, c1 s, c2 s, c3 s)
  modify $ \s -> update s $ tq2 (p2 s, c2 s, c3 s, c4 s)
  modify $ \s -> update s $ tq3 (p3 s, c1 s, c2 s, c3 s)
  modify $ \s -> update s $ tq4 (p3 s, c2 s, c3 s, c4 s)
 where
  update :: TokenizeState -> Int -> TokenizeState
  update s num = s { score = score s + num }
  {-# INLINE update #-}
{-# INLINE updateScore #-}

pushToToken :: Monad m => StateT TokenizeState m ()
pushToToken = do
  s@TS {..} <- get
  when (w3 < e1) $ do
    let !newToken = token <> BSB.charUtf8 (chr w3)
    put $ s { token = newToken }
{-# INLINE pushToToken #-}

isFinished :: Monad m => StateT TokenizeState m Bool
isFinished = (e1 ==) <$> gets w4
{-# INLINE isFinished #-}

tailToResult :: Monad m => StateT TokenizeState m (Maybe BSL.ByteString)
tailToResult = do
  s@TS {..} <- get
  let word = tokenToText token
  if BSL.length word > 0
    then do
      put $ s { remain = BSL.empty, token = BSB.lazyByteString BSL.empty }
      return $ Just word
  else return Nothing
{-# INLINE tailToResult #-}

evalScore :: Monad m => StateT TokenizeState m (Maybe BSL.ByteString)
evalScore = do
  s@TS {..} <- get
  if score > 0
    then do
      let word = tokenToText token
      put $ s { token = BSB.lazyByteString BSL.empty , p1 = p2, p2 = p3, p3 = MB }
      return $ Just word
    else do
      put $ s { p1 = p2, p2 = p3, p3 = MO }
      return Nothing
{-# INLINE evalScore #-}

evalOneStep :: Monad m => StateT TokenizeState m (Maybe BSL.ByteString)
evalOneStep = do
  moveNext
  pushToToken
  updateScore
  evalScore
{-# INLINE evalOneStep #-}

tokenizeM :: StateT TokenizeState Identity (Maybe BSL.ByteString, Bool)
tokenizeM = do
  flag <- isFinished
  if flag
    then do
      word <- tailToResult
      return (word, True)
    else do
      word <- evalOneStep
      return (word, False)
{-# INLINE tokenizeM #-}

tokenize :: BSL.ByteString -> [BSL.ByteString]
tokenize text =
  let f = runState tokenizeM
      g x = case f x of
        ((Just t , _   ), s) -> Just (t, s)
        ((Nothing, flag), s) -> if flag then Nothing else g s
  in  L.unfoldr g $ initialState text
{-# INLINE tokenize #-}

tokenizeToVecM :: StateT TokenizeState (ST s) (MV.MVector s BSL.ByteString, Int)
tokenizeToVecM = do
  len <- BSL.length <$> gets remain
  vec <- lift $ MV.unsafeNew $ fromIntegral len
  body (vec, 0)
  where
    body (!vec, !idx) = do
      flag <- isFinished
      if flag then do
        word <- tailToResult
        case word of
          Just w -> do
            lift $ MV.unsafeWrite vec idx w
            return (vec, idx+1)
          Nothing ->
            return (vec, idx)
      else do
        word <- evalOneStep
        case word of
          Just w -> do
            lift $ MV.unsafeWrite vec idx w
            body (vec, idx+1)
          Nothing ->
            body (vec, idx)
{-# INLINE tokenizeToVecM #-}

tokenizeToVec :: BSL.ByteString -> V.Vector BSL.ByteString
tokenizeToVec text = v
  where
    v = runST $ do
          (vec, len) <- evalStateT tokenizeToVecM $ initialState text
          V.freeze $ MV.unsafeSlice 0 len vec
{-# INLINE tokenizeToVec #-}
