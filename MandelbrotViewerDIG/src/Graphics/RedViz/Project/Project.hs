{-# LANGUAGE TemplateHaskell #-}

module Graphics.RedViz.Project.Project 
  ( Project  (..)
  , ProjectCamera (..)
  , defaultPCam
  , pTransform
  , pApt
  , pFoc
  , pMouseS
  , pKeyboardRS
  , pKeyboardTS
  , Model    (..)
  , name
  , resx
  , resy
  , camMode
  , models
  , objects
  , background
  , PreObject (..)
  , pname
  , pidx
  , modelIDXs
  , uuid
  , presolvers
  , presolverAttrs
  , solvers
  , solverAttrs
  , options
  , fonts
  , icons
  , defaultFonts
  , defaultIcons
  , cameras
  , Graphics.RedViz.Project.Project.read
  , write
  , defaultProject
  , PreGUI (..)
  , gui
  ) where

import Control.Lens hiding (Empty)
import Data.Aeson
import Data.Aeson.TH as TH
import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy as B hiding (drop, pack)
import Data.Maybe                       (fromMaybe)
import Data.Sort                        (sortOn)                              
import Data.Text ( Text, pack )
import Data.UUID

import Graphics.RedViz.Project.Model
import Graphics.RedViz.Backend

  --import Debug.Trace as DT

data PreObject
  =  PreObject
     {
       _pname          :: String
     , _ptype          :: String
     , _pidx           :: Integer
     , _uuid           :: UUID
     , _modelIDXs      :: [Int]
     , _presolvers     :: [String]
     , _presolverAttrs :: [[Double]]
     , _solvers        :: [String]
     , _solverAttrs    :: [[Double]]
     , _options        :: BackendOptions     
     } deriving Show

$(makeLenses ''PreObject)
deriveJSON TH.defaultOptions {fieldLabelModifier = drop 1} ''PreObject

data ProjectCamera
  =  ProjectCamera
     { _pcname      :: String
     , _pApt        :: Double
     , _pFoc        :: Double
     , _pTransform  :: [Float]
     , _pMouseS     :: Double -- | mouse    "sensitivity"
     , _pKeyboardRS :: Double -- | keyboard "sensitivity"
     , _pKeyboardTS :: Double
     } deriving Show
$(makeLenses ''ProjectCamera)
deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''ProjectCamera

defaultPCam :: ProjectCamera
defaultPCam =
  (ProjectCamera
   {
     _pcname      = "PlayerCamera"
   , _pApt        = 50.0
   , _pFoc        = 100.0
   , _pTransform  =
     [1, 0, 0, 0,
      0, 1, 0, 0,
      0, 0, 1,-11,
      0, 0, 0, 1]
   , _pMouseS     = 0.01
   , _pKeyboardRS = 0.001
   , _pKeyboardTS = 0.1
   })

data PreGUI
  =  PreGUI
     {
       _fonts   :: [Model]
     , _icons   :: [Model]
--     , _gui     :: FilePath
     } deriving Show
$(makeLenses ''PreGUI)
deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''PreGUI

data Project
  =  Project
     {
       _name       :: String
     , _resx       :: Int
     , _resy       :: Int
     , _camMode    :: String
     , _models     :: [Model]
     , _objects    :: [PreObject]
     , _background :: [PreObject]
     , _gui        :: PreGUI
     , _cameras    :: [ProjectCamera]
     } deriving Show
$(makeLenses ''Project)
deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Project

emptyPreGUI :: PreGUI
emptyPreGUI = PreGUI [] []

emptyProject :: Project
emptyProject =
  Project "foobar" (-1) (-1) "AbsoluteLocation" [] [] [] emptyPreGUI []

defaultFonts :: [Model]
defaultFonts =
  [
    (Model   "models/fnt_0.bgeo")
  , (Model   "models/fnt_1.bgeo")
  , (Model   "models/fnt_2.bgeo")
  , (Model   "models/fnt_3.bgeo")
  , (Model   "models/fnt_4.bgeo")
  , (Model   "models/fnt_5.bgeo")
  , (Model   "models/fnt_6.bgeo")
  , (Model   "models/fnt_7.bgeo")
  , (Model   "models/fnt_8.bgeo")
  , (Model   "models/fnt_9.bgeo")
  , (Model   "models/fnt_a.bgeo")
  , (Model   "models/fnt_b.bgeo")
  , (Model   "models/fnt_c.bgeo")
  , (Model   "models/fnt_d.bgeo")
  , (Model   "models/fnt_e.bgeo")
  , (Model   "models/fnt_f.bgeo")
  , (Model   "models/fnt_g.bgeo")
  , (Model   "models/fnt_h.bgeo")
  , (Model   "models/fnt_i.bgeo")
  , (Model   "models/fnt_j.bgeo")
  , (Model   "models/fnt_k.bgeo")
  , (Model   "models/fnt_l.bgeo")
  , (Model   "models/fnt_m.bgeo")
  , (Model   "models/fnt_n.bgeo")
  , (Model   "models/fnt_o.bgeo")
  , (Model   "models/fnt_p.bgeo")
  , (Model   "models/fnt_q.bgeo")
  , (Model   "models/fnt_r.bgeo")
  , (Model   "models/fnt_s.bgeo")
  , (Model   "models/fnt_t.bgeo")
  , (Model   "models/fnt_u.bgeo")
  , (Model   "models/fnt_v.bgeo")
  , (Model   "models/fnt_w.bgeo")
  , (Model   "models/fnt_x.bgeo")
  , (Model   "models/fnt_y.bgeo")
  , (Model   "models/fnt_z.bgeo")
  , (Model   "models/fnt_plus.bgeo")
  , (Model   "models/fnt_minus.bgeo")
  , (Model   "models/fnt_equal.bgeo")
  , (Model   "models/fnt_GT.bgeo")
  , (Model   "models/fnt_comma.bgeo")
  , (Model   "models/fnt_dot.bgeo")
  , (Model   "models/fnt_question.bgeo")
  , (Model   "models/fnt_exclam.bgeo")
  , (Model   "models/fnt_space.bgeo")
  , (Model   "models/fnt_asterics.bgeo")
  , (Model   "models/fnt_slash.bgeo")
  , (Model   "models/fnt_semicolon.bgeo")
  , (Model   "models/fnt_quote.bgeo")
  , (Model   "models/fnt_A.bgeo")
  , (Model   "models/fnt_B.bgeo")
  , (Model   "models/fnt_C.bgeo")
  , (Model   "models/fnt_D.bgeo")
  , (Model   "models/fnt_E.bgeo")
  , (Model   "models/fnt_F.bgeo")
  , (Model   "models/fnt_G.bgeo")
  , (Model   "models/fnt_H.bgeo")
  , (Model   "models/fnt_I.bgeo")
  , (Model   "models/fnt_J.bgeo")
  , (Model   "models/fnt_K.bgeo")
  , (Model   "models/fnt_L.bgeo")
  , (Model   "models/fnt_M.bgeo")
  , (Model   "models/fnt_N.bgeo")
  , (Model   "models/fnt_O.bgeo")
  , (Model   "models/fnt_P.bgeo")
  , (Model   "models/fnt_Q.bgeo")
  , (Model   "models/fnt_R.bgeo")
  , (Model   "models/fnt_S.bgeo")
  , (Model   "models/fnt_T.bgeo")
  , (Model   "models/fnt_U.bgeo")
  , (Model   "models/fnt_V.bgeo")
  , (Model   "models/fnt_W.bgeo")
  , (Model   "models/fnt_X.bgeo")
  , (Model   "models/fnt_Y.bgeo")
  , (Model   "models/fnt_Z.bgeo")
  , (Model   "models/fnt_LT.bgeo")
  ]

defaultIcons :: [Model]
defaultIcons =
  [
    (Model   "models/fnt_crosshair.bgeo")
  , (Model   "models/gizmo.bgeo")
  ]

defaultPreGUI :: PreGUI
defaultPreGUI =
  PreGUI
  defaultFonts
  []

defaultProject :: Project
defaultProject =
  Project
  "Test Project"
  800
  600
  "AbsoluteLocation"
  [ (Model   "models/box.bgeo")]
  [ PreObject
    {
      _pname          = "Box"                    
    , _ptype          = ""                       
    , _pidx           = 0                        
    , _uuid           = nil                      
    , _modelIDXs      = [0]                      
    , _presolvers     = []                       
    , _presolverAttrs = []                       
    , _solvers        = ["rotate", "translate"]  
    , _solverAttrs    = [[0,0,0,0,0,1000],
                         [1000,0,0]]
    , _options        =  defaultBackendOptions
    }       
  ]
  []
  defaultPreGUI
  [(ProjectCamera
    "PlayerCamera"
    50.0
    100.0
    [1, 0, 0, 0,
     0, 1, 0, 0,
     0, 0, 1,-10,
     0, 0, 0, 1])
    1.0
    1.0
    1.0
  ]

write :: Project -> FilePath -> IO ()
write prj fileOut =
  B.writeFile fileOut $ encodePretty' config prj
  where
    config = defConfig { confCompare = comp }

comp :: Text -> Text -> Ordering
comp = keyOrder . (fmap pack) $ ["name", "resx", "resy", "camMode", "models", "objects", "background", "pname", "uuid ", "modelIDXs", "solvers", "solverAttrs", "presolvers", "presolverAttrs", "fonts", "icons", "cameras", "pApt", "pFoc", "pTransform", "pMouseS", "pKeyboardRS", "pKeyboardTS", "options"]

read :: FilePath -> IO Project
read filePath = do
  Prelude.putStrLn $ "filePath : " ++ show filePath
  d <- (eitherDecode <$> B.readFile filePath) :: IO (Either String Project)
  let name'     = (_name     . fromEitherDecode) d
      resx'     = (_resx     . fromEitherDecode) d
      resy'     = (_resy     . fromEitherDecode) d
      camMode'  = (_camMode  . fromEitherDecode) d
      models'   = (_models   . fromEitherDecode) d
      preObjs'  = (_objects  . fromEitherDecode) d
      bgrObjs'  = (_background . fromEitherDecode) d
      gui'      = (_gui      . fromEitherDecode) d
      cameras'  = (_cameras  . fromEitherDecode) d
  return $
    Project
    {
      _name       = name'
    , _resx       = resx'
    , _resy       = resy'
    , _camMode    = camMode'
    , _models     = models'
    , _objects    = sortOn (view uuid) preObjs'
    , _background = sortOn (view uuid) bgrObjs'
    , _gui        = gui'
    , _cameras    = cameras'
    }
    
    where
      fromEitherDecode = fromMaybe emptyProject . fromEither
      fromEither d =
        case d of
          Right pt -> Just pt            
          _ -> Nothing
