module CriterionScraper.Prelude
  ( andM,
    error,
    fail,
    guardM,
    leftToMaybe,
    mapLeft,
    mapRight,
    maybeToLeft,
    maybeToRight,
    note,
    onJust,
    onJustM,
    onLeft,
    onLeftM,
    onNothing,
    onNothingM,
    onRight,
    onRightM,
    orM,
    print,
    product,
    putStr,
    putStrLn,
    rightToMaybe,
    show,
    sum,
    trace,
    traceEvent,
    traceEventIO,
    traceIO,
    traceM,
    traceMarker,
    traceMarkerIO,
    traceShow,
    traceShowId,
    traceShowM,
    unlessM,
    unsafeFoldr1,
    unsafeFromJust,
    unsafeHead,
    unsafeInit,
    unsafeLast,
    unsafeMaximum,
    unsafeMaximumBy,
    unsafeMinimum,
    unsafeMinimumBy,
    unsafeRead,
    unsafeTail,
    whenJust,
    whenJustM,
    whenLeft,
    whenLeftM,
    whenM,
    whenNothing,
    whenNothingM,
    whenRight,
    whenRightM,
    module X,
  )
where

import Control.Applicative as X
  ( Alternative (..),
    Applicative (..),
    Const (..),
    ZipList (..),
    liftA2,
    liftA3,
    optional,
    (<**>),
  )
import qualified Control.Applicative
import Control.Arrow as X ((&&&))
import Control.Category as X ((<<<), (>>>))
import Control.Concurrent.MVar as X (MVar)
import Control.Concurrent.STM as X (STM, TMVar, TVar)
import Control.DeepSeq as X (NFData (..), deepseq, force, ($!!))
import Control.Exception as X (Exception (..), SomeException)
import qualified Control.Exception
import Control.Monad as X hiding (fail, forM, forM_, liftM)
import qualified Control.Monad
import Control.Monad.Except as X
  ( Except,
    ExceptT (..),
    MonadError (..),
    liftEither,
    runExcept,
    runExceptT,
  )
import qualified Control.Monad.Fail
import Control.Monad.Fix as X (MonadFix (..))
import Control.Monad.IO.Class as X (MonadIO (liftIO))
import Control.Monad.Reader as X
  ( MonadReader (..),
    Reader,
    ReaderT (..),
    asks,
    runReader,
  )
import Control.Monad.ST as X (ST, fixST, runST)
import Control.Monad.State.Strict as X
  ( MonadState (..),
    State,
    StateT (..),
    evalState,
    evalStateT,
    execState,
    execStateT,
    get,
    gets,
    modify,
    runState,
    withState,
  )
import Control.Monad.Trans.Class as X (MonadTrans (..))
import Control.Monad.Trans.Maybe as X (MaybeT (..))
import Data.Aeson as X (FromJSON (..), ToJSON (..))
import Data.Bifunctor as X (Bifunctor (..))
import Data.Bool as X hiding (bool)
import Data.ByteString as X (ByteString)
import qualified Data.ByteString.Lazy
import Data.Char as X (Char)
import qualified Data.Char
import Data.Coerce as X (Coercible, coerce)
import Data.Containers.ListUtils as X (nubOrd, nubOrdOn)
import Data.Data as X (Data)
import Data.Either as X
  ( Either (..),
    either,
    isLeft,
    isRight,
    lefts,
    partitionEithers,
    rights,
  )
import Data.Eq as X (Eq (..))
import Data.Foldable as X
  ( Foldable (elem, fold, foldMap, foldl', foldr, foldr', length, null, toList),
    all,
    and,
    any,
    asum,
    concat,
    concatMap,
    find,
    for_,
    notElem,
    or,
    sequenceA_,
    traverse_,
  )
import qualified Data.Foldable
import Data.Function as X (const, fix, flip, id, ($), (&), (.))
import Data.Functor as X
import Data.Functor.Contravariant as X (Contravariant (..))
import Data.Functor.Identity as X (Identity (..))
import Data.Int as X (Int, Int16, Int32, Int64, Int8)
import Data.List as X
  ( break,
    cycle,
    drop,
    dropWhile,
    filter,
    group,
    inits,
    intercalate,
    intersperse,
    isPrefixOf,
    iterate,
    map,
    nub,
    partition,
    permutations,
    repeat,
    replicate,
    reverse,
    scanl,
    scanl',
    scanr,
    sort,
    sortBy,
    sortOn,
    splitAt,
    subsequences,
    tails,
    take,
    takeWhile,
    transpose,
    unfoldr,
    unzip,
    zip,
    zipWith,
    (++),
  )
import Data.List.NonEmpty as X (NonEmpty (..), nonEmpty)
import Data.Map.Strict as X (Map)
import Data.Maybe as X hiding (fromJust)
import qualified Data.Maybe
import Data.Monoid as X hiding (First (..), Last (..))
import Data.Ord as X (Ord (..), Ordering (..), comparing)
import Data.Proxy as X (Proxy (..))
import qualified Data.Proxy
import Data.Semigroup as X (Semigroup (..))
import Data.Set as X (Set)
import Data.String as X (IsString, String)
import Data.Text as X (Text, lines, toLower, toUpper, unlines, unwords, words)
import qualified Data.Text
import Data.Text.Encoding as X (decodeUtf8', encodeUtf8)
import qualified Data.Text.IO
import Data.Traversable as X hiding (forM)
import qualified Data.Traversable
import Data.Tuple as X (curry, fst, snd, swap, uncurry)
import Data.Void as X (Void, absurd, vacuous)
import qualified Debug.Trace
import GHC.Enum as X (Bounded (..), Enum (..))
import GHC.Float as X (Double (..), Float (..), Floating (..), RealFloat (..))
import GHC.Generics as X (Generic, Generic1)
import GHC.Num as X (Integer, Num (..), subtract)
import GHC.Show as X (Show)
import Numeric.Natural as X (Natural)
import System.IO as X (FilePath, IO)
import Text.Read as X (Read, readMaybe)
import qualified Prelude

andM :: (Foldable t, Monad m) => t (m Bool) -> m Bool
andM =
  foldr
    ( \mx acc -> do
        x <- mx
        if x then acc else pure False
    )
    (pure True)

error :: String -> a
error = Prelude.error

fail :: MonadFail m => Text -> m a
fail = Control.Monad.Fail.fail . Data.Text.unpack

guardM :: MonadPlus m => m Bool -> m ()
guardM f = guard =<< f

leftToMaybe :: Either l r -> Maybe l
leftToMaybe = either Just (const Nothing)

mapLeft :: (a -> b) -> Either a x -> Either b x
mapLeft f = either (Left . f) Right

mapRight :: (a -> b) -> Either x a -> Either x b
mapRight = fmap

maybeToLeft :: r -> Maybe l -> Either l r
maybeToLeft r = maybe (Right r) Left

maybeToRight :: l -> Maybe r -> Either l r
maybeToRight l = maybe (Left l) Right

note :: a -> Maybe b -> Either a b
note _ (Just x) = Right x
note e Nothing = Left e

onJust :: Applicative m => (a -> m ()) -> Maybe a -> m ()
onJust = flip whenJust

onJustM :: Monad m => (a -> m ()) -> m (Maybe a) -> m ()
onJustM = flip whenJustM

onLeft :: Applicative m => (a -> m b) -> Either a b -> m b
onLeft f = either f pure

onLeftM :: Monad m => (a -> m b) -> m (Either a b) -> m b
onLeftM f x = x >>= onLeft f

onNothing :: Applicative m => m a -> Maybe a -> m a
onNothing = flip maybe pure

onNothingM :: Monad m => m a -> m (Maybe a) -> m a
onNothingM = flip whenNothingM

onRight :: Applicative m => (b -> m a) -> Either a b -> m a
onRight = either pure

onRightM :: Monad m => (b -> m a) -> m (Either a b) -> m a
onRightM f x = x >>= onRight f

orM :: (Foldable t, Monad m) => t (m Bool) -> m Bool
orM =
  foldr
    ( \mx acc -> do
        x <- mx
        if not x then acc else pure True
    )
    (pure False)

print :: (MonadIO m, Show a) => a -> m ()
print = liftIO . Prelude.print

putStr :: MonadIO m => Text -> m ()
putStr = liftIO . Data.Text.IO.putStr

putStrLn :: MonadIO m => Text -> m ()
putStrLn = liftIO . Data.Text.IO.putStrLn

{-# INLINE product #-}
product :: (Foldable f, Num a) => f a -> a
product = foldl' (*) 1

rightToMaybe :: Either l r -> Maybe r
rightToMaybe = either (const Nothing) Just

show :: Show a => a -> Text
show = Data.Text.pack . Prelude.show

{-# INLINE sum #-}
sum :: (Foldable f, Num a) => f a -> a
sum = foldl' (+) 0

{-# WARNING trace "trace" #-}
trace :: Text -> a -> a
trace = Debug.Trace.trace . Data.Text.unpack

{-# WARNING traceEvent "traceEvent" #-}
traceEvent :: Text -> a -> a
traceEvent = Debug.Trace.traceEvent . Data.Text.unpack

{-# WARNING traceEventIO "traceEventIO" #-}
traceEventIO :: MonadIO m => Text -> m ()
traceEventIO = liftIO . Debug.Trace.traceEventIO . Data.Text.unpack

{-# WARNING traceIO "traceIO" #-}
traceIO :: MonadIO m => Text -> m ()
traceIO = liftIO . Debug.Trace.traceIO . Data.Text.unpack

{-# WARNING traceM "traceM" #-}
traceM :: Applicative f => Text -> f ()
traceM = Debug.Trace.traceM . Data.Text.unpack

{-# WARNING traceMarker "traceMarker" #-}
traceMarker :: Text -> a -> a
traceMarker = Debug.Trace.traceMarker . Data.Text.unpack

{-# WARNING traceMarkerIO "traceMarkerIO" #-}
traceMarkerIO :: MonadIO m => Text -> m ()
traceMarkerIO = liftIO . Debug.Trace.traceMarkerIO . Data.Text.unpack

{-# WARNING traceShow "traceShow" #-}
traceShow :: Show a => a -> b -> b
traceShow = Debug.Trace.traceShow

{-# WARNING traceShowId "traceShowId" #-}
traceShowId :: Show a => a -> a
traceShowId = Debug.Trace.traceShowId

{-# WARNING traceShowM "traceShowM" #-}
traceShowM :: (Show a, Applicative f) => a -> f ()
traceShowM = Debug.Trace.traceShowM

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM p m = p >>= flip unless m

unsafeFoldr1 :: Foldable t => (a -> a -> a) -> t a -> a
unsafeFoldr1 = Data.Foldable.foldr1

unsafeFromJust :: Maybe a -> a
unsafeFromJust = Data.Maybe.fromJust

unsafeHead :: [a] -> a
unsafeHead = Prelude.head

unsafeInit :: [a] -> [a]
unsafeInit = Prelude.init

unsafeLast :: [a] -> a
unsafeLast = Prelude.last

unsafeMaximum :: (Foldable t, Ord a) => t a -> a
unsafeMaximum = Data.Foldable.maximum

unsafeMaximumBy :: Foldable t => (a -> a -> Ordering) -> t a -> a
unsafeMaximumBy = Data.Foldable.maximumBy

unsafeMinimum :: (Foldable t, Ord a) => t a -> a
unsafeMinimum = Data.Foldable.minimum

unsafeMinimumBy :: Foldable t => (a -> a -> Ordering) -> t a -> a
unsafeMinimumBy = Data.Foldable.minimumBy

unsafeRead :: Read a => String -> a
unsafeRead = Prelude.read

unsafeTail :: [a] -> [a]
unsafeTail = Prelude.tail

whenLeft :: Applicative m => Either a b -> (a -> m b) -> m b
whenLeft = flip onLeft

whenLeftM :: Monad m => m (Either a b) -> (a -> m b) -> m b
whenLeftM = flip onLeftM

whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust = for_

whenJustM :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
whenJustM x f = x >>= onJust f

whenM :: Monad m => m Bool -> m () -> m ()
whenM p m = p >>= flip when m

whenNothing :: Monad m => Maybe a -> m a -> m a
whenNothing = flip onNothing

whenNothingM :: Monad m => m (Maybe a) -> m a -> m a
whenNothingM x f = x >>= onNothing f

whenRight :: Applicative m => Either a b -> (b -> m a) -> m a
whenRight = flip onRight

whenRightM :: Monad m => m (Either a b) -> (b -> m a) -> m a
whenRightM = flip onRightM
