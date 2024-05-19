--------------------------------------------------------------------------------
-- |
-- Module      :  RedViz
-- Copyright   :  (c) Vladimir Lopatin 2022
-- License     :  BSD-3-Clause
--
-- Maintainer  :  Vladimir Lopatin <madjestic13@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- Reading and parsing pgeo file format
--
--------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Graphics.RedViz.PGeo
  ( PGeo(..)
  , VGeo(..)
  , SVGeo(..)
  , sis 
  , sst 
  , svs 
  , smp 
  , sms 
  , svl
  , savl
  , sxf 
  , Vec3
  , readPGeo
--  , readVGeo
  , readBGeo
  , fromPGeo
  , fromVGeo
  , fromSVGeo
  , VAO
  , toVAO
  ) where

import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import Data.Maybe                             (fromMaybe)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString      as BS
import Graphics.Rendering.OpenGL      as GL   (Vertex4(..))
import Data.Store                     as DS
import Data.Store.TH

import Graphics.RedViz.FromVector
import Graphics.RedViz.Utils
import Graphics.RedViz.VAO

--import Debug.Trace   as DT

-- | TODO : replace Vec3 -> Vec4
type Vec3 = (Double, Double, Double)

type Vec4 = (Double, Double, Double, Double)

instance FromVector Vec3 where
  toVertex4 :: Vec3 -> Vertex4 Double
  toVertex4 (k, l, m) = Vertex4 k l m 1.0  

data PGeo
  =  PGeo
     {
       ids   :: [[Int]]  
     , as    :: [Float] -- 1
     , cs    :: [Vec3]  -- 3
     , ns    :: [Vec3]  -- 3
     , uvws  :: [Vec3]  -- 3
     , ps    :: [Vec3]  -- 3 -> 13+1 -> 14 stride -- TODO: add v (vel), m (mass)
     , mats  :: [String]
     , m     :: [Float]
     , v     :: [Vec3] -- [[x,y,x]] ~= [Vec3]
     , av    :: [Vec3]
     , xform :: [[Float]] -- [xform M44]
     }
  deriving Show
deriveJSON defaultOptions ''PGeo

data SVGeo -- VGeo Singleton
  =  SVGeo
     {
       _sis  :: [Int]    -- indices
     , _sst  :: Int      -- stride
     , _svs  :: [Float]  -- all attrs as a flat list
     , _smp  :: FilePath -- material
     , _sms  :: Float    -- mass
     , _svl  :: [Float]  -- velocity
     , _savl :: [Float]  -- angular velocity
     , _sxf  :: [Float]  -- preTransform
     } deriving Show
$(makeLenses ''SVGeo)

fromVGeo :: VGeo -> [SVGeo]
fromVGeo (VGeo is' st' vs' mts' ms' vls' avls' xf' _) = svgeo :: [SVGeo]
  where
    svgeo = SVGeo <$.> is' <*.> st' <*.> vs' <*.> mts' <*.> ms' <*.> vls' <*.> avls' <*.> xf'

data VGeo
  =  VGeo
     {
       is  :: [[Int]]    -- indices
     , st  :: [Int]      -- strides``
     , vs  :: [[Float]]  -- all attrs as a flat list
     , mts :: [FilePath] -- materials
     , ms  :: [Float]    -- masses
     , vls :: [[Float]]  -- velocities
     , avls:: [[Float]]  -- angular velocities
     , xf  :: [[Float]]  -- preTransforms
     , nm  :: FilePath   -- name for record keeping, maybe remove when done?
     } deriving Show
makeStore ''VGeo 

emptyVGeo :: VGeo
emptyVGeo =  VGeo [[]] [] [[]] [] [] [[]] [[]] [[]] "empty vgeo"

fromSVGeo :: SVGeo -> SVAO'
fromSVGeo (SVGeo is' st' vs' _ _ _ _ _) = toVAO'' is' st' vs'

readBGeo :: FilePath -> IO VGeo
readBGeo file = 
  do
    -- _ <- DT.trace "trace" $ return ()
    bs <- BS.readFile file
    return $ case DS.decode bs of
               Right vgeo -> vgeo { nm = file }
               Left _ -> error ("Error reading file : " ++ show file) emptyVGeo

-- readVGeo :: FilePath -> IO VGeo
-- readVGeo file = 
--   do
--     d <- decodeFileStrict file :: IO (Maybe ([[Int]],[Int],[[Float]],[String], [Float], [[Float]], [[Float]]))
--     return $ case d of
--                Just (idxs, st, vaos, mats, mass, vels, xform) -> VGeo idxs st vaos mats mass vels xform
--                Nothing  -> VGeo [[]] [] [[]] [] [] [] [[]]

readPGeo :: FilePath -> IO PGeo
readPGeo jsonFile =
  do
    d <- (eitherDecode <$> BL.readFile jsonFile) :: IO (Either String PGeo)
    --print $ "readPGeo.d :" ++ show d
    -- case d of
    _ <-
      case (fromEither d) of
        Nothing -> print $ "readPGeo.d :" ++ show d
        _ -> putStrLn $ "parsing the " ++ (show jsonFile) ++ ": OK"
    
    let ids'  = (ids   . fromEitherDecode) d
        as'   = (as    . fromEitherDecode) d
        cs'   = (cs    . fromEitherDecode) d
        ns'   = (ns    . fromEitherDecode) d
        uvws' = (uvws  . fromEitherDecode) d
        ps'   = (ps    . fromEitherDecode) d
        mts'  = (mats  . fromEitherDecode) d
        mass' = (m     . fromEitherDecode) d
        vels' = (v     . fromEitherDecode) d
        avels'= (av    . fromEitherDecode) d
        xf'   = (xform . fromEitherDecode) d
    return $ PGeo ids' as' cs' ns' uvws' ps' mts' mass' vels' avels' xf'

      where
        emptyPGeo = PGeo [[]] [] [] [] [] [] [] [] [] [] [[]]
        fromEitherDecode = fromMaybe emptyPGeo . fromEither
        fromEither d =
          case d of
            Right pt -> Just pt
            _ -> Nothing

fromPGeo :: PGeo -> VGeo
fromPGeo (PGeo idx' as' cs' ns' uvw' ps' mts' mass' vels' avels' xf') = VGeo idxs st vaos mts' mass' vels avels xf' "converted from pgeo"
  where
    stride = 13 -- TODO: make it more elegant, right now VBO's are hard-coded to be have stride = 13...
    vao = toVAO idx' as' cs' ns' uvw' ps'
    (idxs, vaos) = unzip $ fmap toIdxVAO vao -- that already outputs [[]], but vao, I think,is still a single element list?
    st           = replicate (length vaos) stride
    vels         = fmap (\(x,y,z)   -> fmap realToFrac [x,y,z]) vels'
    avels        = fmap (\(x,y,z)   -> fmap realToFrac [x,y,z]) avels'
