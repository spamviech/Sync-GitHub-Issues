{-# LANGUAGE TemplateHaskell #-}

module Token (token) where

import Data.ByteString (ByteString())
import Token.TH (loadToken)

token :: ByteString
token = $(loadToken ".githubtoken")
