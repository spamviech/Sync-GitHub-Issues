module Token.TH (loadToken) where

import Control.Monad ((>=>))
import qualified Data.ByteString as ByteString
import Instances.TH.Lift ()
import Language.Haskell.TH (Exp, Q, runIO)
import Language.Haskell.TH.Syntax (Lift(lift))

loadToken :: FilePath -> Q Exp
loadToken = runIO . ByteString.readFile >=> lift
