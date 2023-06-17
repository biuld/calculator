module Language.Calculator.WASM.Printer where

import Language.Calculator.WASM.Types (Func (..), Inst (..), Module (..), Val (..), ValType (..))

class Show a => Wat a where
    wat :: a -> String

wrap :: String -> String
wrap s = "(" <> s <> ")"

join :: (Foldable t, Semigroup a) => a -> t a -> a
join delimiter = foldl1 (\acc a -> acc <> delimiter <> a)

wrapBlank :: Foldable t => t String -> String
wrapBlank s = wrap $ join " " s

instance Wat ValType where
    wat I32t = "i32"
    wat I64t = "i64"
    wat F32t = "f32"
    wat F64t = "f64"

instance Wat Val where
    wat (I32v i32) = show i32
    wat (I64v i64) = show i64
    wat (F32v f32) = show f32
    wat (F64v f64) = show f64

instance Wat Inst where
    wat (Const ty v) = wat ty <> ".const " <> wat v

instance Wat Func where
    wat f =
        let
            i = "$" <> f.id
            (param, result) = f.ty
            p = wrapBlank $ "param" : fmap wat param
            r = wrapBlank $ "result" : fmap wat result
            b = wrapBlank $ fmap wat f.body
         in
            wrapBlank ["func", i, p, r, b]

instance Wat Module where
    wat m =
        let
            i = case m.id of
                Just id -> "$" <> id
                Nothing -> []
            start = case m.start of
                Just id -> wrapBlank ["start", "$" <> id]
                Nothing -> []
            fs = join "" $ fmap wat m.func
         in
            wrapBlank [x | x <- ["module", i, fs, start], not $ null x]
