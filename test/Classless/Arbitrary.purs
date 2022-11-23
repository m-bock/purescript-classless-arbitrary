module Test.Classless.Arbitrary where

import Prelude

import Classless (class Init, class InitIt, initIt, initRecord, initSum, sequenceProduct, sequenceRecord, (~))
import Classless.Arbitrary (Arbitrary)
import Classless.Arbitrary as Arb
import Data.Function as F
import Data.Generic.Rep (class Generic, Argument(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Heterogeneous.Mapping (class Mapping, class MappingWithIndex, hmap, hmapWithIndex, mappingWithIndex)
import Record (union)
import Record as R
import Test.QuickCheck.Gen (Gen)
import Unsafe.Coerce (unsafeCoerce)

type Foo =
  { foo :: Int
  , bar :: Maybe String
  --  , baz :: Array Boolean
  --  , points :: Array { x :: Int, y :: Int }
  }

sample :: Arbitrary Foo
sample = Arb.record $ identity
  { foo: arbitrary -- Arb.int
  , bar: Arb.maybe Arb.string
  -- , baz: Arb.array Arb.boolean
  -- , points: Arb.array $ Arb.record $ identity { x: arbitrary, y: Arb.int }
  }

sample22 :: Arbitrary Foo
sample22 = Arb.record $ myInit

class Default a where
  def :: a

instance Default (Maybe Int) where
  def = Just 1

instance Default (Int) where
  def = 1

-- xzz :: Maybe  { a :: Int}
-- xzz = RE.sequenceRecord $ identity { a : def }

data Fooo = Fooo

instance Mapping Fooo Int String where
  mapping _ _ = ""

-- sss :: String /\  String
-- sss = hmap Fooo (def unit /\ 2 )

data Baz
  = Foo Int (Maybe String)
  | Bar Boolean
  | Baz (Array { x :: Int, y :: Int })

derive instance Generic Baz _

sample3 :: Arbitrary Baz
sample3 = Arb.sum
  { "Foo": arbitrary ~ Arb.maybe Arb.string
  , "Bar": Arb.boolean
  , "Baz": Arb.array $ Arb.record $ identity { x: arbitrary, y: Arb.int }
  }

-- sample33 :: Arbitrary Baz
-- sample33 = Arb.sum
--   { "Foo": arbitrary ~ Arb.maybe Arb.string
--   , "Bar": Arb.boolean
--   , "Baz": Arb.array $ Arb.record $ identity { x: arbitrary, y: Arb.int }
--   }

xx :: Gen Int
xx = arbitrary

data Abc = Abc String | Xyz Int

derive instance Generic Abc _

sample33 :: Arbitrary Abc
sample33 = Arb.sum $ myInit

sample2 :: Arbitrary { a :: Int, b :: Int, c :: String }
sample2 = Arb.record $ union { c: Arb.string } myInit

x :: { foo :: Gen Int }
x = initRecord I

data I = I

instance (UserArbitrary a) => Init I (Gen a) where
  init _ = arbitrary

class UserArbitrary a where
  arbitrary :: Gen a

instance (UserArbitrary a) => UserArbitrary (Maybe a) where
  arbitrary = Arb.maybe arbitrary

instance UserArbitrary String where
  arbitrary = Arb.string

instance UserArbitrary Int where
  arbitrary = Arb.int

class MyInit a where
  myInit :: a

instance (InitIt I a) => MyInit a where
  myInit = initIt I