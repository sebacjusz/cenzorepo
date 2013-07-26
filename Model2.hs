module Model2 where
import Prelude
import Data.ByteString as B
import Database.Persist.Sql
newtype Hash = Hash B.ByteString deriving (Read, Show, Eq, PersistField, PersistFieldSql)
