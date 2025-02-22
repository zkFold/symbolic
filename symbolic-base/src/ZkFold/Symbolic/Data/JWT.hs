{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module ZkFold.Symbolic.Data.JWT
    ( Certificate (..)
    , SigningKey (..)
    , TokenHeader (..)
    , TokenPayload (..)
    , Signature
    , ClientSecret (..)
    , IsSymbolicJSON (..)
    , SecretBits
    , secretBits
    , toAsciiBits
    , signPayload
    , verifySignature
    ) where

import           Control.DeepSeq                    (NFData, force)
import           Data.Aeson                         (FromJSON (..), genericParseJSON)
import qualified Data.Aeson                         as JSON
import           Data.Aeson.Casing                  (aesonPrefix, snakeCase)
import           Data.Constraint                    (Dict (..), withDict, (:-) (..))
import           Data.Constraint.Nat                (Max, divNat, minusNat, plusNat, timesNat)
import           Data.Constraint.Unsafe             (unsafeAxiom, unsafeSNat)
import           Data.Maybe                         (fromMaybe)
import           Data.Scientific                    (toBoundedInteger)
import qualified Data.Text                          as T
import           Generic.Random                     (genericArbitrary, uniform)
import           GHC.Generics                       (Generic, Par1 (..))
import           GHC.TypeLits                       (withKnownNat)
import           Prelude                            (fmap, pure, type (~), ($), (.), (<$>))
import qualified Prelude                            as P
import           Test.QuickCheck                    (Arbitrary (..))

import           ZkFold.Base.Algebra.Basic.Class
import           ZkFold.Base.Algebra.Basic.Number
import           ZkFold.Base.Data.HFunctor          (hmap)
import qualified ZkFold.Base.Data.Vector            as V
import           ZkFold.Base.Data.Vector            ((!!))
import           ZkFold.Symbolic.Algorithms.RSA
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Data.Bool
import           ZkFold.Symbolic.Data.ByteString    (ByteString (..), concat, toWords)
import           ZkFold.Symbolic.Data.Class
import           ZkFold.Symbolic.Data.Combinators
import           ZkFold.Symbolic.Data.Eq
import           ZkFold.Symbolic.Data.FieldElement
import           ZkFold.Symbolic.Data.Input         (SymbolicInput)
import           ZkFold.Symbolic.Data.Ord
import           ZkFold.Symbolic.Data.UInt
import qualified ZkFold.Symbolic.Data.VarByteString as VB
import           ZkFold.Symbolic.Data.VarByteString (VarByteString (..), wipeUnassigned, (@+))
import           ZkFold.Symbolic.MonadCircuit       (newAssigned)


class IsSymbolicJSON a where
    type MaxLength a :: Natural

    toJsonBits :: a -> VarByteString (MaxLength a) (Context a)


-- | RSA Public key with Key ID
--
data Certificate ctx
    = Certificate
        { pubKid :: VarByteString 320 ctx
        , pubKey :: PublicKey 2048 ctx
        }
    deriving Generic

deriving instance
    ( P.Eq (PublicKey 2048 ctx)
    , P.Eq (VarByteString 320 ctx)
    ) => P.Eq (Certificate ctx)
deriving instance
    ( P.Show (PublicKey 2048 ctx)
    , P.Show (VarByteString 320 ctx)
    ) => P.Show (Certificate ctx)
deriving instance
    ( NFData (PublicKey 2048 ctx)
    , NFData (VarByteString 320 ctx)
    ) => NFData (Certificate ctx)
instance
    ( SymbolicData (PublicKey 2048 ctx)
    , Symbolic ctx
    ) => SymbolicData (Certificate ctx)
instance
    ( SymbolicInput (PublicKey 2048 ctx)
    , Symbolic ctx
    ) => SymbolicInput (Certificate ctx)


-- | RSA Private key with Key ID
--
data SigningKey ctx
    = SigningKey
        { prvKid :: VarByteString 320 ctx
        , prvKey :: PrivateKey 2048 ctx
        }
    deriving Generic

deriving instance
    ( P.Eq (PrivateKey 2048 ctx)
    , P.Eq (VarByteString 320 ctx)
    ) => P.Eq (SigningKey ctx)
deriving instance
    ( P.Show (PrivateKey 2048 ctx)
    , P.Show (VarByteString 320 ctx)
    ) => P.Show (SigningKey ctx)
deriving instance
    ( NFData (PrivateKey 2048 ctx)
    , NFData (VarByteString 320 ctx)
    ) => NFData (SigningKey ctx)
instance
    ( SymbolicData (PrivateKey 2048 ctx)
    , Symbolic ctx
    ) => SymbolicData (SigningKey ctx)
instance
    ( SymbolicInput (PrivateKey 2048 ctx)
    , Symbolic ctx
    ) => SymbolicInput (SigningKey ctx)

-- | Json Web Token header with information about encryption algorithm and signature
--
data TokenHeader ctx
    = TokenHeader
        { hdAlg :: VarByteString 72 ctx
        -- ^ Signature or encryption algorithm
        , hdKid :: VarByteString 320 ctx
        -- ^ Key ID
        , hdTyp :: VarByteString 32 ctx
        -- ^ Type of token
        }
    deriving Generic

deriving instance
    ( P.Eq (ctx (V.Vector 72))
    , P.Eq (ctx (V.Vector 320))
    , P.Eq (ctx (V.Vector 32))
    , P.Eq (ctx Par1)
    ) => P.Eq (TokenHeader ctx)
deriving instance
    ( P.Show (ctx (V.Vector 72))
    , P.Show (ctx (V.Vector 320))
    , P.Show (ctx (V.Vector 32))
    , P.Show (ctx Par1)
    ) => P.Show (TokenHeader ctx)
deriving instance
    ( NFData (ctx (V.Vector 72))
    , NFData (ctx (V.Vector 320))
    , NFData (ctx (V.Vector 32))
    , NFData (ctx Par1)
    ) => NFData (TokenHeader ctx)

deriving instance Symbolic ctx => SymbolicData (TokenHeader ctx)
deriving instance Symbolic ctx => SymbolicInput (TokenHeader ctx)

instance Symbolic ctx => FromJSON (TokenHeader ctx) where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance
    ( Symbolic ctx
    , Context (TokenHeader ctx) ~ ctx
    , NFData (VarByteString (MaxLength (TokenHeader ctx)) ctx)
    ) => IsSymbolicJSON (TokenHeader ctx) where

    type MaxLength (TokenHeader ctx) = 648
    toJsonBits TokenHeader{..} = force $
                    (fromType @"{\"alg\":\"")   @+ hdAlg
        `VB.append` (fromType @"\",\"kid\":\"") @+ hdKid
        `VB.append` (fromType @"\",\"typ\":\"") @+ hdTyp
        `VB.append` (fromType @"\"}")


-- | Json Web Token payload with information about the issuer, bearer and TTL
--
data TokenPayload ctx
    = TokenPayload
        { plIss           :: VarByteString 256 ctx
        -- ^ Issuer (who created and signed the token)
        , plAzp           :: VarByteString 1024 ctx
        -- ^ Authorized party (the party to which the token was issued)
        , plAud           :: VarByteString 1024 ctx
        -- ^ Audience (who or what the token is intended for)
        , plSub           :: VarByteString 256 ctx
        -- ^ Subject (whom the token refers to)
        , plHd            :: VarByteString 256 ctx
        -- ^ Hosted domain
        , plEmail         :: VarByteString 512 ctx
        -- ^ User email limited to 64 characters
        , plEmailVerified :: VarByteString 40 ctx
        -- ^ "true" or "false", max 5 bytes
        , plAtHash        :: VarByteString 256 ctx
        -- ^ Access token hash value
        , plName          :: VarByteString 512 ctx
        -- ^ Full name limited to 64 characters
        , plPicture       :: VarByteString 1024 ctx
        -- ^ URL to the profile picture limited to 128 characters
        , plGivenName     :: VarByteString 256 ctx
        -- ^ Given name limited to 32 characters
        , plFamilyName    :: VarByteString 256 ctx
        -- ^ Family name limited to 32 characters
        , plIat           :: VarByteString 80 ctx
        -- ^ Issued at (seconds since Unix epoch), a decimal number
        , plExp           :: VarByteString 80 ctx
        -- ^ Expiration time (seconds since Unix epoch), a decimal number
        }
    deriving Generic

deriving instance
    ( P.Eq (ctx (V.Vector 40))
    , P.Eq (ctx (V.Vector 80))
    , P.Eq (ctx (V.Vector 256))
    , P.Eq (ctx (V.Vector 512))
    , P.Eq (ctx (V.Vector 1024))
    , P.Eq (ctx Par1)
    ) => P.Eq (TokenPayload ctx)
deriving instance
    ( P.Show (ctx (V.Vector 40))
    , P.Show (ctx (V.Vector 80))
    , P.Show (ctx (V.Vector 256))
    , P.Show (ctx (V.Vector 512))
    , P.Show (ctx (V.Vector 1024))
    , P.Show (ctx Par1)
    ) => P.Show (TokenPayload ctx)
deriving instance Symbolic ctx => SymbolicData (TokenPayload ctx)
deriving instance Symbolic ctx => SymbolicInput (TokenPayload ctx)
instance Symbolic ctx => Arbitrary (TokenPayload ctx) where
    arbitrary = genericArbitrary uniform

instance Symbolic ctx => FromJSON (TokenPayload ctx) where
    parseJSON = genericParseJSON (aesonPrefix snakeCase) . stringify
        where
            -- We store everything as ByteStrings for simplicity.
            -- We need to convert ints and bools to strings to avoid conversion errors
            --
            stringify :: JSON.Value -> JSON.Value
            stringify (JSON.Number s) =
                JSON.String (T.pack . P.show . fromMaybe (P.error "instance FromJSON JWT :: Invalid integer") . toBoundedInteger @P.Int $ s)
            stringify (JSON.Bool b)   = JSON.String (T.pack $ P.show b)
            stringify (JSON.Object o) = JSON.Object $ fmap stringify o
            stringify rest            = rest

instance (Symbolic ctx, Context (TokenPayload ctx) ~ ctx) => IsSymbolicJSON (TokenPayload ctx) where
    type MaxLength (TokenPayload ctx) = 7088
    toJsonBits TokenPayload{..} =
                    (fromType @"{\"iss\":\"")   @+ plIss
        `VB.append` (fromType @"\",\"azp\":\"") @+ plAzp
        `VB.append` (fromType @"\",\"aud\":\"") @+ plAud
        `VB.append` (fromType @"\",\"sub\":\"") @+ plSub
        `VB.append` (fromType @"\",\"hd\":\"")  @+ plHd
        `VB.append` (fromType @"\",\"email\":\"") @+ plEmail
        `VB.append` (fromType @"\",\"email_verified\":") @+ plEmailVerified
        `VB.append` (fromType @",\"at_hash\":\"") @+ plAtHash
        `VB.append` (fromType @"\",\"name\":\"")  @+ plName
        `VB.append` (fromType @"\",\"picture\":\"") @+ plPicture
        `VB.append` (fromType @"\",\"given_name\":\"")  @+ plGivenName
        `VB.append` (fromType @"\",\"family_name\":\"") @+ plFamilyName
        `VB.append` (fromType @"\",\"iat\":") @+ plIat
        `VB.append` (fromType @",\"exp\":")   @+ plExp
        `VB.append` (fromType @"}")

data ClientSecret ctx
    = ClientSecret
        { csHeader    :: TokenHeader ctx
        , csPayload   :: TokenPayload ctx
        , csSignature :: Signature 2048 ctx
        }
    deriving Generic

deriving instance
    ( NFData (TokenHeader ctx)
    , NFData (TokenPayload ctx)
    , NFData (Signature 2048 ctx)
    ) => NFData (ClientSecret ctx)
deriving instance Symbolic ctx => SymbolicData (ClientSecret ctx)
deriving instance Symbolic ctx => SymbolicInput (ClientSecret ctx)

-- | The lowest number of bits to store the padded length of a bytestring of @n@ bits
--
type BufLen n = Max (Log2 n + 1) 3

-- | The smallest multiple of 6 not less than @n@
--
type Next6 (n :: Natural) = (Div (n + 5) 6) * 6

-- | The number of bits required to store a base64 representation as an ASCII string
-- Each symbol in base64 requires 6 bits, but in ASCII it takes 8 bits, hence the ratio of 8/6
--
type ASCII (n :: Natural) = (Div n 6) * 8

---------------------------------------------------------------------------------------------------
    -- Helper axioms
---------------------------------------------------------------------------------------------------

knownBufLen' :: forall n . KnownNat n :- KnownNat (BufLen n)
knownBufLen' = Sub $ withKnownNat @(BufLen n) (unsafeSNat $ P.max (ilog2 (value @n) + 1) 3) Dict

knownBufLen :: forall n {r} . KnownNat n => (KnownNat (BufLen n) => r) -> r
knownBufLen = withDict (knownBufLen' @n)

monoAdd :: forall (a :: Natural) (b :: Natural) (c :: Natural) . (a <= b) :- (a <= (c + b))
monoAdd = Sub unsafeAxiom

oneReg :: forall n c . Dict (NumberOfRegisters (BaseField c) (BufLen n) ('Fixed (BufLen n)) ~ 1)
oneReg = unsafeAxiom -- @BufLen n@ is always greater than 2

knownOneReg' :: forall n c . Dict (KnownNat (NumberOfRegisters (BaseField c) (BufLen n) ('Fixed (BufLen n))))
knownOneReg' = withKnownNat @(NumberOfRegisters (BaseField c) (BufLen n) ('Fixed (BufLen n))) (unsafeSNat 1) Dict

knownOneReg :: forall n c {r} . (KnownNat (NumberOfRegisters (BaseField c) (BufLen n) ('Fixed (BufLen n))) => r) -> r
knownOneReg = withDict (knownOneReg' @n @c)

knownNumWords' :: forall n c . KnownNat n :- KnownNat (Div (GetRegisterSize (BaseField c) (BufLen n) ('Fixed (BufLen n)) + OrdWord - 1) OrdWord)
knownNumWords' = Sub $
    withKnownNat @(Div (GetRegisterSize (BaseField c) (BufLen n) ('Fixed (BufLen n)) + OrdWord - 1) OrdWord)
        (unsafeSNat $ knownBufLen @n $ wordSize (value @(BufLen n)))
        Dict
    where
        wordSize :: Natural -> Natural
        wordSize n = (n + (value @OrdWord) -! 1) `div` (value @OrdWord)

knownNumWords :: forall n c {r} . KnownNat n => (KnownNat (Div (GetRegisterSize (BaseField c) (BufLen n) ('Fixed (BufLen n)) + OrdWord - 1) OrdWord) => r) -> r
knownNumWords = withDict (knownNumWords' @n @c)

withDiv :: forall n {r}. KnownNat n => (KnownNat (Div ((n + OrdWord) - 1) OrdWord) => r) -> r
withDiv =
    withDict (plusNat @n @OrdWord) $
        withDict (monoAdd @1 @OrdWord @n) $
            withDict (minusNat @(n + OrdWord) @1) $
                withDict (divNat @(n + OrdWord - 1) @OrdWord)

withNext6 :: forall n {r}. KnownNat n => (KnownNat (Next6 n) => r) -> r
withNext6 =
    withDict (plusNat @n @5) $
        withDict (divNat @(n + 5) @6) $
            withDict (timesNat @(Div (n + 5) 6) @6)

withAscii :: forall n {r}. KnownNat n => (KnownNat (ASCII n) => r) -> r
withAscii =
    withDict (divNat @n @6) $
        withDict (timesNat @(Div n 6) @8)

divMul :: forall a b . (Mod a b ~ 0) :- ((Div a b) * b ~ a)
divMul = Sub unsafeAxiom

mulMod :: forall n . Dict (Mod (Next6 n) 6 ~ 0)
mulMod = unsafeAxiom

withDivMul :: forall a b {r}. (Mod a b ~ 0) => ((Div a b) * b ~ a => r) -> r
withDivMul = withDict (divMul @a @b)

---------------------------------------------------------------------------------------------------

feToUInt :: forall n ctx . Symbolic ctx => FieldElement ctx -> UInt (BufLen n) ('Fixed (BufLen n)) ctx
feToUInt (FieldElement c) = UInt $ withDict (oneReg @n @ctx) $ hmap (V.singleton . unPar1) c

uintToFe :: forall n ctx . Symbolic ctx => UInt (BufLen n) ('Fixed (BufLen n)) ctx -> FieldElement ctx
uintToFe (UInt v) = FieldElement $ withDict (oneReg @n @ctx) $ hmap (Par1 . V.item) v

-- | The smallest multiple of 6 not less than the given UInt
--
padTo6
    :: forall n ctx
    .  Symbolic ctx
    => KnownNat n
    => UInt (BufLen n) ('Fixed (BufLen n)) ctx
    -> FieldElement ctx
padTo6 ui = FieldElement $ fromCircuitF v $ \bits ->
    do
        val <- horner $ V.fromVector bits

        toPad <- newAssigned $ \p -> fromConstant @Natural 6 - p val
        valBits <- V.unsafeToVector @3 <$> expansion 3 toPad

        f <- newAssigned $ \p -> one - p (valBits !! 1) * p (valBits !! 2)
        hi1 <- newAssigned $ \p -> p f * p (valBits !! 1)
        hi2 <- newAssigned $ \p -> p f * p (valBits !! 2)
        res <- horner [valBits !! 0, hi1, hi2]

        pure $ Par1 res
    where
        UInt v =
            knownBufLen @n $
                knownNumWords @n @ctx $
                    knownOneReg @n @ctx $
                        withDiv @(BufLen n) $
                            ui `mod` (fromConstant @Natural 6)


-- | Increase capacity of a VarByteString and increase its length to the nearest multiple of 6
-- Required for base64 encoding.
--
padBytestring6
    :: forall n ctx
    .  Symbolic ctx
    => KnownNat n
    => VarByteString n ctx -> VarByteString (Next6 n) ctx
padBytestring6 VarByteString{..} = VarByteString (bsLength + mod6) (withNext6 @n $ VB.shiftL newBuf mod6)
    where
        mod6 = padTo6 @n $ feToUInt @n bsLength
        newBuf = withNext6 @n $ resize bsBuffer


-- | Convert a base64-encoded string into an ASCII-encoded string
-- It is expected that both capacity and length of the input bytestring are divisible by 6
-- If either of them is not, apply @padBytestring6@ first.
--
base64ToAscii
    :: forall n ctx
    .  Symbolic ctx
    => KnownNat n
    => Mod n 6 ~ 0
    => NFData (ctx (V.Vector 8))
    => NFData (ctx (V.Vector (ASCII n)))
    => VarByteString n ctx -> VarByteString (ASCII n) ctx
base64ToAscii VarByteString{..} = withAscii @n $ wipeUnassigned $ VarByteString newLen result
    where
        words6 = withDivMul @n @6 $ toWords @(Div n 6) @6 bsBuffer
        ascii  = word6ToAscii <$> words6
        result = force $ concat ascii

        newLen =
            knownBufLen @n $
                withDiv @(BufLen n) $
                    knownOneReg @n @ctx $
                        knownNumWords @n @ctx $
                            scale (4 :: Natural) . (uintToFe @n) . (`div` (fromConstant @Natural 3)) . (feToUInt @n) $ bsLength


{-
    Symbols  |  Base64url  |  ASCII
    A..Z         0..25       65..90
    a..z         26..51      97..122
    0..9         52..61      48..57
    -            62          45
    _            63          95
-}
word6ToAscii :: forall ctx . (Symbolic ctx, NFData (ctx (V.Vector 8))) => ByteString 6 ctx -> ByteString 8 ctx
word6ToAscii (ByteString bs) = force $ ByteString $ fromCircuitF bs $ \bits ->
    do
        let bitsSym = V.fromVector bits

        fe <- horner (P.reverse bitsSym)

        z <- newAssigned (P.const zero)
        o <- newAssigned (P.const one)

        let bits25 = [z,o,o,z,z,o]
            bits51 = [o,o,z,z,o,o]
            bits61 = [o,o,o,o,z,o]
            bits62 = [o,o,o,o,o,z]

        isAZ   <- blueprintGE @6 bits25 bitsSym
        leaz   <- blueprintGE @6 bits51 bitsSym
        le09   <- blueprintGE @6 bits61 bitsSym
        ledash <- blueprintGE @6 bits62 bitsSym

        isaz   <- newAssigned $ \p -> p leaz * (one - p isAZ)
        is09   <- newAssigned $ \p -> p le09 * (one - p leaz)
        isdash <- newAssigned $ \p -> p ledash * (one - p le09)
        isus   <- newAssigned $ \p -> one - p ledash

        asciiAZ   <- newAssigned $ \p -> p isAZ   * (p fe + (fromConstant @Natural 65))
        asciiaz   <- newAssigned $ \p -> p isaz   * (p fe + (fromConstant @Natural 71))
        ascii09   <- newAssigned $ \p -> p is09   * (p fe - (fromConstant @Natural 4 ))
        asciidash <- newAssigned $ \p -> p isdash * (p fe - (fromConstant @Natural 17))
        asciius   <- newAssigned $ \p -> p isus   * (p fe + (fromConstant @Natural 32))

        s1 <- newAssigned $ \p -> p asciiAZ   + p asciiaz
        s2 <- newAssigned $ \p -> p ascii09   + p s1
        s3 <- newAssigned $ \p -> p asciidash + p s2
        s4 <- newAssigned $ \p -> p asciius   + p s3

        V.unsafeToVector . P.reverse <$> expansion 8 s4


toAsciiBits
    :: forall a ctx
    .  IsSymbolicJSON a
    => Context a ~ ctx
    => KnownNat (MaxLength a)
    => Symbolic ctx
    => NFData (ctx (V.Vector 8))
    => NFData (ctx (V.Vector (ASCII (Next6 (MaxLength a)))))
    => a -> VarByteString (ASCII (Next6 (MaxLength a))) ctx
toAsciiBits = withNext6 @(MaxLength a) $ withDict (mulMod @(MaxLength a)) $ base64ToAscii . padBytestring6 . toJsonBits


type SecretBits ctx =
    ( NFData (ctx (V.Vector 8))
    , NFData (ctx (V.Vector 648))
    , NFData (ctx (V.Vector 864))
    , NFData (ctx (V.Vector 9456))
    , NFData (ctx (V.Vector 10328))
    , NFData (ctx Par1)
    )


secretBits'
    :: forall ctx
    .  Symbolic ctx
    => SecretBits ctx
    => TokenHeader ctx
    -> TokenPayload ctx
    -> VarByteString 10328 ctx
secretBits' h p =  force $
       toAsciiBits h
    @+ (fromType @".")
    @+ toAsciiBits p


-- | Client secret as a ByteString: @ASCII(base64UrlEncode(header) + "." + base64UrlEncode(payload))@
--
secretBits
    :: forall ctx
    .  Symbolic ctx
    => SecretBits ctx
    => ClientSecret ctx -> VarByteString 10328 ctx
secretBits ClientSecret {..} = secretBits' csHeader csPayload

-- | Sign token payload and form a ClientSecret
--
signPayload :: (SecretBits ctx, RSA 2048 10328 ctx) => SigningKey ctx -> TokenPayload ctx -> ClientSecret ctx
signPayload SigningKey{..} csPayload = ClientSecret{..}
    where
        csHeader = TokenHeader "RS256" prvKid "JWT"
        csSignature = signVar (secretBits' csHeader csPayload) prvKey

-- | Verify that the given JWT was correctly signed with a matching key (i.e. Key IDs match and the signature is correct).
--
verifySignature :: (SecretBits ctx, RSA 2048 10328 ctx) => Certificate ctx -> ClientSecret ctx -> (Bool ctx, ByteString 256 ctx)
verifySignature Certificate{..} cs@ClientSecret{..} =
    let (sigVerified, secretHash) = verifyVar (secretBits cs) csSignature pubKey
     in (pubKid == hdKid csHeader && sigVerified, secretHash)

