{-| This is a specialistion of the Data.Map module to the case where
  the keys are dates. The aim is to supply functions in terms of
  the wrapper objects TimePoint and TimeSeries so that users don't
  depend on the internal Data.Map structure 
  
  Only TimePoint(..) and TimeSeries should be exposed, along with the
  accessor/manipulation functions (fetch, map, ...)

  Like Data.Map, many function names clash with those within Prelude
  so this module is best imported @qualified@
  -}
module TimeSeries where

import Control.Applicative
import qualified Data.Map.Strict as M
import Data.Time
import DateUtils

data TimePoint a = TimePoint {
    date    :: Day,
    value   :: a
} deriving (Show, Eq, Ord)

newtype TimeSeries a = TimeSeries {
    get :: M.Map Day a
} deriving Show

-- TODO 
-- instance (Show a) => Show (TimeSeries a) where
--     show = 

instance Functor TimeSeries where
    fmap = TimeSeries.map
    
new :: TimeSeries a
new = TimeSeries M.empty

create :: [TimePoint a] -> TimeSeries a
create = foldr insert new

insert :: TimePoint a -> TimeSeries a -> TimeSeries a
insert tp ts = TimeSeries $ M.insert (date tp) (value tp) (get ts)

length :: TimeSeries a -> Int
length = M.size . get

merge :: (a -> b -> c) -> TimeSeries a -> TimeSeries b -> TimeSeries c
merge f t1 t2 = TimeSeries $ M.intersectionWith f (get t1) (get t2)

map :: (a -> b) -> TimeSeries a -> TimeSeries b
map f t = TimeSeries $ M.map f (get t)

foldl :: (a -> b -> a) -> a -> TimeSeries b -> a
foldl f i = M.foldl f i . get

dates :: TimeSeries a -> [Day]
dates = M.keys . get

fetch :: Day -> TimeSeries a -> Maybe (TimePoint a)
fetch d t = liftA (TimePoint d) $ M.lookup d (get t)

filter :: (TimePoint a -> Bool) -> TimeSeries a -> TimeSeries a
filter p ts = TimeSeries $ M.filterWithKey pred (get ts)
                where pred k v = p $ TimePoint k v

--------------------------------------------------------------------------------
-- Test code...
--
testTs = do 
    today <- getDate
    return $ 
        insert (TimePoint today 3.226) .
        insert (TimePoint (addDays (-1) today) 4.26421) .
        insert (TimePoint (addDays (-3) today) 7.21145) .
        insert (TimePoint (addDays (-2) today) 9.82610) $ new
