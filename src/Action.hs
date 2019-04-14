data Prefetch =
  | Url
  | Git

newtype Sha256 = String

data Source =
  | 

data Package =
  { source :: Source
  , name :: String
  , attrs :: Aeson.Object
  }

data PackageUpdate =
  { prefetch :: Maybe (Prefech)
  , json :: Aeson.Object
  }

data Action =
  | 
