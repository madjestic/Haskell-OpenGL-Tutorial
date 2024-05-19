module Graphics.RedViz.Project.Utils
  ( fromProjectCamera
  ) where

import Linear.V3

import Graphics.RedViz.Project.Project
import Graphics.RedViz.Camera
import Graphics.RedViz.Controllable
import Graphics.RedViz.Utils


-- TODO: pass prj resolution to camera
fromProjectCamera :: Project -> ProjectCamera -> Camera
fromProjectCamera prj0 pcam =
  defaultCam
  {
    _apt        = _pApt pcam
  , _foc        = _pFoc pcam
  , _controller =
      defaultCamController
      { _transform = fromList ( Just $ _pTransform pcam) }
  , _mouseS     = pure $ _pMouseS pcam     :: V3 Double
  , _keyboardRS = pure $ _pKeyboardRS pcam :: V3 Double
  , _keyboardTS = pure $ _pKeyboardTS pcam :: V3 Double
  , _res        = (_resx prj0, _resy prj0)
  , _scale      = 1.0                      :: Double
  }
