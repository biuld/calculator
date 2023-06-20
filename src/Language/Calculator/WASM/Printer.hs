module Language.Calculator.WASM.Printer where

import Language.Calculator.WASM.Types (Func (..), Inst (..), Module (..), Signage (..), Val (..), ValType (..))

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

instance Wat Signage where
    wat Sign = "s"
    wat UnSign = "u"

instance Wat Inst where
    wat (Const ty v) = wat ty <> ".const " <> wat v
    wat (Equal ty a b) = join " " [wat (Const ty a), wat (Const ty b), wat ty <> ".eq"]
    wat (NotEqual ty a b) = join " " [wat (Const ty a), wat (Const ty b), wat ty <> ".ne"]
    wat (Gt ty si a b) = case ty of
        I32t -> join " " [wat (Const ty a), wat (Const ty b), "i32.gt_" <> wat si]
        I64t -> join " " [wat (Const ty a), wat (Const ty b), "i64.gt_" <> wat si]
        F32t -> join " " [wat (Const ty a), wat (Const ty b), "f32.gt"]
        F64t -> join " " [wat (Const ty a), wat (Const ty b), "f64.gt"]
    wat (Lt ty si a b) = case ty of
        I32t -> join " " [wat (Const ty a), wat (Const ty b), "i32.lt_" <> wat si]
        I64t -> join " " [wat (Const ty a), wat (Const ty b), "i64.lt_" <> wat si]
        F32t -> join " " [wat (Const ty a), wat (Const ty b), "f32.lt"]
        F64t -> join " " [wat (Const ty a), wat (Const ty b), "f64.lt"]
    wat (Ge ty si a b) = case ty of
        I32t -> join " " [wat (Const ty a), wat (Const ty b), "i32.ge_" <> wat si]
        I64t -> join " " [wat (Const ty a), wat (Const ty b), "i64.ge_" <> wat si]
        F32t -> join " " [wat (Const ty a), wat (Const ty b), "f32.ge"]
        F64t -> join " " [wat (Const ty a), wat (Const ty b), "f64.ge"]
    wat (Le ty si a b) = case ty of
        I32t -> join " " [wat (Const ty a), wat (Const ty b), "i32.le_" <> wat si]
        I64t -> join " " [wat (Const ty a), wat (Const ty b), "i64.le_" <> wat si]
        F32t -> join " " [wat (Const ty a), wat (Const ty b), "f32.le"]
        F64t -> join " " [wat (Const ty a), wat (Const ty b), "f64.le"]
    wat (Add ty a b) = join " " [wat (Const ty a), wat (Const ty b), wat ty <> ".add"]
    wat (Sub ty a b) = join " " [wat (Const ty a), wat (Const ty b), wat ty <> ".sub"]
    wat (Mul ty a b) = join " " [wat (Const ty a), wat (Const ty b), wat ty <> ".mul"]
    wat (Div ty si a b) = case ty of
        I32t -> join " " [wat (Const ty a), wat (Const ty b), "i32.div_" <> wat si]
        I64t -> join " " [wat (Const ty a), wat (Const ty b), "i64.div_" <> wat si]
        F32t -> join " " [wat (Const ty a), wat (Const ty b), "f32.div"]
        F64t -> join " " [wat (Const ty a), wat (Const ty b), "f64.div"]
    wat (Rem ty si a b) = case ty of
        I32t -> join " " [wat (Const ty a), wat (Const ty b), "i32.rem_" <> wat si]
        I64t -> join " " [wat (Const ty a), wat (Const ty b), "i64.rem_" <> wat si]
        F32t -> join " " [wat (Const ty a), wat (Const ty b), "f32.rem"]
        F64t -> join " " [wat (Const ty a), wat (Const ty b), "f64.rem"]
    wat (IfThenElse b l r) =
        let
            lhs = fmap wat l
            rhs = fmap wat r
         in
            join " " $ [wat (Const I32t b), "if", "then"] ++ lhs ++ ["else"] ++ rhs
    wat (Loop is) = join " " $ fmap wat is
    wat _ = undefined

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
            fs = join " " $ fmap wat m.func
         in
            wrapBlank [x | x <- ["module", i, fs, start], not $ null x]
