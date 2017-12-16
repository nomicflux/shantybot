{-# LANGUAGE OverloadedStrings #-}

module Twitter.Internal.AccessInfo where

import Data.ByteString (ByteString)

consumerKey :: IO ByteString
consumerKey = return "zLc0WewcETlhKE9Eq5LweGZtb"

consumerSecret :: IO ByteString
consumerSecret = return "IKLJLogU0R4zHwVR50d79wL5oEGvWRyytSJgvW0Z1BV8rSAJGh"

owner :: IO ByteString
owner = return "shantybot"

ownerId :: IO Integer
ownerId = return 896489823476822016

accessToken :: IO ByteString
accessToken = return "896489823476822016-bpd5FmuR1igEeYqWfMVSGkctIBDPSRs"

accessSecret :: IO ByteString
accessSecret = return "0JmzpmoXZ3YfpnITds7KiucoXaYScV2UmyI4F9qhRI8fx"
