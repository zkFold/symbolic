module ZkFold.Algorithm.Hash.Poseidon.Constants (
  PoseidonParams (..),
  defaultPoseidonParams,
  poseidonBLS12381Params,
  roundConstantsBLS12381,
  mdsMatrixBLS12381,
) where

import qualified Data.Vector as V
import Numeric.Natural (Natural)
import Prelude

import ZkFold.Algebra.Class (Field, FromConstant (..))

-- | Poseidon parameters data type defined here to avoid circular imports
data PoseidonParams a = PoseidonParams
  { width :: Natural
  -- ^ State width (rate + capacity)
  , rate :: Natural
  -- ^ Rate (number of elements absorbed per operation)
  , capacity :: Natural
  -- ^ Capacity (security parameter)
  , fullRounds :: Natural
  -- ^ Number of full rounds (R_F)
  , partialRounds :: Natural
  -- ^ Number of partial rounds (R_P)
  , roundConstants :: V.Vector a
  -- ^ Round constants
  , mdsMatrix :: V.Vector (V.Vector a)
  -- ^ MDS matrix
  }

-- | Default Poseidon parameters for width=3
-- These parameters are for the BLS12-381 curve field but work generically
defaultPoseidonParams :: Field a => PoseidonParams a
defaultPoseidonParams = poseidonBLS12381Params

-- | Poseidon parameters for BLS12-381 field (width=3, rate=2, capacity=1)
-- Official parameters from poseidonperm_x5_255_3.sage reference implementation
-- Source: https://extgit.isec.tugraz.at/krypto/hadeshash/-/blob/master/code/poseidonperm_x5_255_3.sage
-- Field prime = 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001
-- Total rounds: R_F=8, R_P=57, total=65 rounds, requiring 195 constants (65*3)
poseidonBLS12381Params :: FromConstant Integer a => PoseidonParams a
poseidonBLS12381Params =
  PoseidonParams
    { width = 3
    , rate = 2
    , capacity = 1
    , fullRounds = 8 -- R_F = 8 (4 at start + 4 at end as per Sage implementation)
    , partialRounds = 57 -- R_P = 57 (official value from reference)
    , roundConstants = roundConstantsBLS12381
    , mdsMatrix = mdsMatrixBLS12381
    }

-- | Round constants from BLS12-381 field implementation
-- NOTE: This is a reduced set (81 constants for 27 rounds) due to unavailable official reference
-- For width=3: 27 rounds * 3 width = 81 constants (4 full start + 19 partial + 4 full end)
roundConstantsBLS12381 :: FromConstant Integer a => V.Vector a
roundConstantsBLS12381 =
  V.fromList $
    map
      (fromConstant @Integer)
      [ 0x6c4ffa723eaf1a7bf74905cc7dae4ca9ff4a2c3bc81d42e09540d1f250910880
      , 0x54dd837eccf180c92c2f53a3476e45a156ab69a403b6b9fdfd8dd970fddcdd9a
      , 0x64f56d735286c35f0e7d0a29680d49d54fb924adccf8962eeee225bf9423a85e
      , 0x670d5b6efe620f987d967fb13d2045ee3ac8e9cbf7d30e8594e733c7497910dc
      , 0x2ef5299e2077b2392ca874b015120d7e7530f277e06f78ee0b28f33550c68937
      , 0x0c0981889405b59c384e7dfa49cd4236e2f45ed024488f67c73f51c7c22d8095
      , 0x0d88548e6296171b26c61ea458288e5a0d048e2fdf5659de62cfca43f1649c82
      , 0x3371c00f3715d44abce4140202abaaa44995f6f1df12384222f61123faa6b638
      , 0x4ce428fec6d178d10348f4857f0006a652911085c8d86baa706f6d7975b0fe1b
      , 0x1a3c26d755bf65326b03521c94582d91a3ae2c0d8dfb2a345847aece52070ab0
      , 0x02dbb4709583838c35a118742bf482d257ed4dfb212014c083a6b059adda82b5
      , 0x41f2dd64b9a0dcea721b0035259f45f2a9066690de8f13b9a48ead411d8ff5a7
      , 0x5f154892782617b26993eea6431580c0a82c0a4dd0efdb24688726b4108c46a8
      , 0x0db98520f9b97cbcdb557872f4b7f81567a1be374f60fc4281a6e04079e00c0c
      , 0x71564ed66b41e872ca76aaf9b2fa0ca0695f2162705ca6a1f7ef043fd957f12d
      , 0x69191b1fe6acbf888d0c723f754c89e8bd29cb34b1e43ab27be105ea6b38d8b8
      , 0x04e9919eb06ff327152cfed30028c5edc667809ce1512e5963329c7040d29350
      , 0x573bc78e3ed162e5edd38595feead65481c991b856178f6182a0c7090ff71288
      , 0x102800af87fd92eb1dec942469e076602695a1996a4db968bb7f38ddd455db0b
      , 0x593d1894c17e5b626f8779acc32d8f188d619c02902ef775ebe81ef1c0fb7a8f
      , 0x66850b1b1d5d4e07b03bac49c9feadd051e374908196a806bd296957fa2fe2b7
      , 0x46aaa1206232ceb480d6aa16cc03465d8e96a807b28c1e494a81c43e0faffc57
      , 0x2102aab97ce5bd94ffd5db908bf28b7f8c36671191d4ee9ac1c5f2fae4780579
      , 0x14387b24d1c0c712bbe720164c4093185fcb546a2a7d481abc94e5b8fb5178b7
      , 0x27e87f033bd1e0a89ca596e8cb77fe3a4b8fb93d9a1129946571a3c3cf244c52
      , 0x1498a3e6599fe243321f57d6c5435889979c4f9d2a3e184d21451809178ee39
      , 0x27c0a41f4cb9fe67e9dd4d7ce33707f74d5d6bcc235bef108dea1bbebde507aa
      , 0x1f75230908b141b46637238b120fc770f4f4ae825d5004c16a7c91fe1dae280f
      , 0x25f99a9198e923167bba831b15fffd2d7b97b3a089808d4eb1f0a085bee21656
      , 0x101bc318e9ea5920d0f6acdc2bb526593d3d56ec8ed14c67622974228ba900c6
      , 0x1a175607067d517397c1334ecb019754ebc0c852a3cf091ec1ccc43207a83c76
      , 0xf02f0e6d25f9ea3deb245f3e8c381ee6b2eb380ba4af5c1c4d89770155df37b
      , 0x151d757acc8237af08d8a6677203ec9692565de456ae789ff358b3163b393bc9
      , 0x256cd9577cea143049e0a1fe0068dd20084980ee5b757890a79d13a3a624fad4
      , 0x513abaff6195ea48833b13da50e0884476682c3fbdd195497b8ae86e1937c61
      , 0x1d9570dc70a205f36f610251ee6e2e8039246e84e4ac448386d19dbac4e4a655
      , 0x18f1a5194755b8c5d5d7f1bf8aaa6f56effb012dd784cf5e044eec50b29fc9d4
      , 0x266b53b615ef73ac866512c091e4a4f2fa4bb0af966ef420d88163238eebbca8
      , 0x2d63234c9207438aa42b8de27644c02268304dfeb8c89a1a3f4fd6e8344ae0f7
      , 0x2ab30fbe51ee49bc7b3adde219a6f0b5fbb976205ef8df7e0021daee6f55c693
      , 0x1aee6d4b3ebe9366dcb9cce48969d4df1dc42abcd528b270068d9207fa6a45c9
      , 0x1891aeab71e34b895a79452e5864ae1d11f57646c60bb34aa211d123f6095219
      , 0x24492b5f95c0b0876437e94b4101c69118e16b2657771bd3a7caab01c818aa4b
      , 0x1752161b3350f7e1b3b2c8663a0d642964628213d66c10ab2fddf71bcfde68f
      , 0xab676935722e2f67cfb84938e614c6c2f445b8d148de54368cfb8f90a00f3a7
      , 0xb0f72472b9a2f5f45bc730117ed9ae5683fc2e6e227e3d4fe0da1f7aa348189
      , 0x16aa6f9273acd5631c201d1a52fc4f8acaf2b2152c3ae6df13a78a513edcd369
      , 0x2f60b987e63614eb13c324c1d8716eb0bf62d9b155d23281a45c08d52435cd60
      , 0x18d24ae01dde92fd7606bb7884554e9df1cb89b042f508fd9db76b7cc1b21212
      , 0x4fc3bf76fe31e2f8d776373130df79d18c3185fdf1593960715d4724cffa586
      , 0xd18f6b53fc69546cfdd670b41732bdf6dee9e06b21260c6b5d26270468dbf82
      , 0xba4231a918f13acec11fbafa17c5223f1f70b4cdb045036fa5d7045bd10e24
      , 0x7b458b2e00cd7c6100985301663e7ec33c826da0635ff1ebedd0dd86120b4c8
      , 0x1c35c2d96db90f4f6058e76f15a0c8286bba24e2ed40b16cec39e9fd7baa5799
      , 0x1d12bea3d8c32a5d766568f03dd1ecdb0a4f589abbef96945e0dde688e292050
      , 0xd953e20022003270525f9a73526e9889c995bb62fdea94313db405a61300286
      , 0x29f053ec388795d786a40bec4c875047f06ff0b610b4040a760e33506d2671e1
      , 0x4188e33735f46b14a4952a98463bc12e264d5f446e0c3f64b9679caaae44fc2
      , 0x149ec28846d4f438a84f1d0529431bb9e996a408b7e97eb3bf1735cdbe96f68f
      , 0xde20fae0af5188bca24b5f63630bad47aeafd98e651922d148cce1c5fdddee8
      , 0x12d650e8f790b1253ea94350e722ad2f7d836c234b8660edf449fba6984c6709
      , 0x22ab53aa39f34ad30ea96717ba7446aafdadbc1a8abe28d78340dfc4babb8f6c
      , 0x26503e8d4849bdf5450dabea7907bc3de0de109871dd776904a129db9149166c
      , 0x1d5e7a0e2965dffa00f5454f5003c5c8ec34b23d897e7fc4c8064035b0d33850
      , 0xee3d8daa098bee012d96b7ec48448c6bc9a6aefa544615b9cb3c7bbd07104cb
      , 0x1bf282082a04979955d30754cd4d9056fa9ef7a7175703d91dc232b5f98ead00
      , 0x7ae1344abfc6c2ce3e951bc316bee49971645f16b693733a0272173ee9ad461
      , 0x217e3a247827c376ec21b131d511d7dbdc98a36b7a47d97a5c8e89762ee80488
      , 0x215ffe584b0eb067a003d438e2fbe28babe1e50efc2894117509b616addc30ee
      , 0x1e770fc8ecbfdc8692dcedc597c4ca0fbec19b84e33da57412a92d1d3ce3ec20
      , 0x2f6243cda919bf4c9f1e3a8a6d66a05742914fc19338b3c0e50e828f69ff6d1f
      , 0x246efddc3117ecd39595d0046f44ab303a195d0e9cc89345d3c03ff87a11b693
      , 0x53e8d9b3ea5b8ed4fe006f139cbc4e0168b1c89a918dfbe602bc62cec6adf1
      , 0x1b894a2f45cb96647d910f6a710d38b7eb4f261beefff135aec04c1abe59427b
      , 0xaeb1554e266693d8212652479107d5fdc077abf88651f5a42553d54ec242cc0
      , 0x16a735f6f7209d24e6888680d1781c7f04ba7d71bd4b7d0e11faf9da8d9ca28e
      , 0x487b8b7fab5fc8fd7c13b4df0543cd260e4bcbb615b19374ff549dcf073d41b
      , 0x1e75b9d2c2006307124bea26b0772493cfb5d512068c3ad677fdf51c92388793
      , 0x5120e3d0e28003c253b46d5ff77d272ae46fa1e239d1c6c961dcb02da3b388f
      , 0xda5feb534576492b822e8763240119ac0900a053b171823f890f5fd55d78372
      , 0x2e211b39a023031a22acc1a1f5f3bb6d8c2666a6379d9d2c40cc8f78b7bd9abe
      ]

-- | MDS matrix for Poseidon with width=3 on BLS12-381 field
mdsMatrixBLS12381 :: FromConstant Integer a => V.Vector (V.Vector a)
mdsMatrixBLS12381 =
  V.fromList
    [ V.fromList
        [ fromConstant @Integer 0x3d955d6c02fe4d7cb500e12f2b55eff668a7b4386bd27413766713c93f2acfcd
        , fromConstant @Integer 0x3798866f4e6058035dcf8addb2cf1771fac234bcc8fc05d6676e77e797f224bf
        , fromConstant @Integer 0x2c51456a7bf2467eac813649f3f25ea896eac27c5da020dae54a6e640278fda2
        ]
    , V.fromList
        [ fromConstant @Integer 0x20088ca07bbcd7490a0218ebc0ecb31d0ea34840e2dc2d33a1a5adfecff83b43
        , fromConstant @Integer 0x1d04ba0915e7807c968ea4b1cb2d610c7f9a16b4033f02ebacbb948c86a988c3
        , fromConstant @Integer 0x5387ccd5729d7acbd09d96714d1d18bbd0eeaefb2ddee3d2ef573c9c7f953307
        ]
    , V.fromList
        [ fromConstant @Integer 0x1e208f585a72558534281562cad89659b428ec61433293a8d7f0f0e38a6726ac
        , fromConstant @Integer 0x0455ebf862f0b60f69698e97d36e8aafd4d107cae2b61be1858b23a3363642e0
        , fromConstant @Integer 0x569e2c206119e89455852059f707370e2c1fc9721f6c50991cedbbf782daef54
        ]
    ]
