module Language.Calculator.Wasm.Printer
  (
    printWatModule,
  )
where

import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Language.Calculator.Wasm.Types (WatExpr (..), WatFunction (..), WatLit (..), WatModule (..), WatOp (..), WatType (..), WatTypeSignature(..))

-- | Prints a WatType to its WAT string representation
printWatType :: WatType -> Text
printWatType I32 = "i32"
printWatType I64 = "i64"
printWatType F32 = "f32"
printWatType F64 = "f64"
printWatType Void = "(void)"
printWatType (WatTypeRef name) = "(ref " <> name <> ")"

-- | Prints a WatOp to its WAT string representation
printWatOp :: WatOp -> Text
printWatOp I32Add = "i32.add"
printWatOp I32Sub = "i32.sub"
printWatOp I32Mul = "i32.mul"
printWatOp I32DivS = "i32.div_s"
printWatOp I32Eq = "i32.eq"
printWatOp I32Ne = "i32.ne"
printWatOp I32LtS = "i32.lt_s"
printWatOp I32GtS = "i32.gt_s"
printWatOp I32And = "i32.and"
printWatOp I32Or = "i32.or"
printWatOp I32Xor = "i32.xor"
printWatOp F32Add = "f32.add"
printWatOp F32Sub = "f32.sub"
printWatOp F32Mul = "f32.mul"
printWatOp F32Div = "f32.div"
printWatOp F32Eq = "f32.eq"
printWatOp F32Ne = "f32.ne"
printWatOp F32Lt = "f32.lt"
printWatOp F32Gt = "f32.gt"
printWatOp LocalGet = "local.get"
printWatOp LocalSet = "local.set"
printWatOp LocalTee = "local.tee"
printWatOp If = "if"
printWatOp Else = "else"
printWatOp End = "end"
printWatOp Loop = "loop"
printWatOp Block = "block"
printWatOp Br = "br"
printWatOp BrIf = "br_if"
printWatOp Call = "call"

-- | Prints a WatExpr to its WAT string representation
printWatExpr :: Int -> WatExpr -> Text
printWatExpr indent (WatConst lit) =
  let prefix = T.pack (replicate indent ' ')
   in prefix <> case lit of
        LitI32 val -> "i32.const " <> T.pack (show val)
        LitI64 val -> "i64.const " <> T.pack (show val)
        LitF32 val -> "f32.const " <> T.pack (show val)
        LitF64 val -> "f64.const " <> T.pack (show val)
printWatExpr indent (WatOp op args) =
  let argLines = map (printWatExpr (indent + 2)) args
      argStr = if null argLines then "" else T.intercalate "\n" argLines
   in T.pack (replicate indent ' ') <> printWatOp op <> (if T.null argStr then "" else "\n" <> argStr)
printWatExpr indent (WatIf cond thenBranch elseBranch) =
  let condStr = printWatExpr (indent + 2) cond
      thenLines = map (printWatExpr (indent + 2)) thenBranch
      thenStr = if null thenLines then "" else T.intercalate "\n" thenLines
      elseContent = case elseBranch of
        Just es ->
          let esLines = map (printWatExpr (indent + 4)) es
              esStr = if null esLines then "" else T.intercalate "\n" esLines
           in "\n" <> T.pack (replicate (indent + 2) ' ') <> "else\n" <> (if T.null esStr then "" else esStr <> "\n")
        Nothing -> ""
   in T.pack (replicate indent ' ')
        <> "(if (result i32)\n"
        <> condStr
        <> "\n"
        <> T.pack (replicate (indent + 2) ' ')
        <> "(then\n"
        <> (if T.null thenStr then "" else thenStr <> "\n")
        <> T.pack (replicate (indent + 2) ' ')
        <> ")"
        <> elseContent
        <> T.pack (replicate indent ' ')
        <> ")"
printWatExpr indent (WatBlock label exprs) =
  let labelStr = maybe "" (("$" <>) . (<> " ")) label
      exprLines = map (printWatExpr (indent + 2)) exprs
      exprsStr = if null exprLines then "" else T.intercalate "\n" exprLines
   in T.pack (replicate indent ' ')
        <> "(block "
        <> labelStr
        <> "\n"
        <> (if T.null exprsStr then "" else exprsStr <> "\n")
        <> T.pack (replicate indent ' ')
        <> ")"
printWatExpr indent (WatLoop label exprs) =
  let labelStr = maybe "" (("$" <>) . (<> " ")) label
      exprLines = map (printWatExpr (indent + 2)) exprs
      exprsStr = if null exprLines then "" else T.intercalate "\n" exprLines
   in T.pack (replicate indent ' ')
        <> "(loop "
        <> labelStr
        <> "\n"
        <> (if T.null exprsStr then "" else exprsStr <> "\n")
        <> T.pack (replicate indent ' ')
        <> ")"
printWatExpr indent (WatCall funcName args) =
  let argLines = map (printWatExpr (indent + 2)) args
      argStr = if null argLines then "" else T.intercalate "\n" argLines
   in T.pack (replicate indent ' ')
        <> "(call $"
        <> funcName
        <> "\n"
        <> (if T.null argStr then "" else argStr <> "\n")
        <> T.pack (replicate indent ' ')
        <> ")"
printWatExpr indent (WatRef funcName) = T.pack (replicate indent ' ') <> "(ref.func $" <> funcName <> ")"
printWatExpr indent (WatLocalGet idx _) = T.pack (replicate indent ' ') <> "(local.get $" <> idx <> ")"
printWatExpr indent (WatLocalSet idx _) = T.pack (replicate indent ' ') <> "(local.set $" <> idx <> ")"
printWatExpr indent (WatLocalTee idx _) = T.pack (replicate indent ' ') <> "(local.tee $" <> idx <> ")"

-- | Prints a WatFunction to its WAT string representation
printWatFunction :: Int -> Map.Map Text WatTypeSignature -> WatFunction -> Text
printWatFunction indent _ func =
  let nameStr = funcName func
      typeAlias = funcTypeAlias func

      -- Format parameters
      paramsStr = T.intercalate " " $ map (("(param $" <>) . (<> ")") . (\(n, t) -> n <> " " <> printWatType t)) (funcParams func)

      localsStr = T.intercalate " " $ map (("(local $" <>) . (<> " ") . (\(n, t) -> n <> " " <> printWatType t)) (funcLocals func)
      bodyLines = map (printWatExpr (indent + 2)) (funcBody func)
      bodyContent = if null bodyLines then "" else T.intercalate "\n" bodyLines
   in T.pack (replicate indent ' ')
        <> "(func $"
        <> nameStr
        <> " (type $"
        <> typeAlias
        <> ")"
        <> (if T.null paramsStr then "" else " " <> paramsStr)
        <> "\n"
        <> (if T.null localsStr then "" else T.pack (replicate (indent + 2) ' ') <> localsStr <> "\n")
        <> (if T.null bodyContent then "" else bodyContent <> "\n")
        <> T.pack (replicate indent ' ')
        <> ")"

-- | Prints a WatTypeSignature to its WAT string representation
printWatTypeSignature :: Int -> Text -> WatTypeSignature -> Text
printWatTypeSignature indent alias sig =
  let paramsStr = T.intercalate " " $ map printWatType sig.sigParams
      resultsStr = T.intercalate " " $ map printWatType sig.sigResults
   in T.pack (replicate indent ' ')
        <> "(type $"
        <> alias
        <> " (func"
        <> (if T.null paramsStr then "" else " (param " <> paramsStr <> ")")
        <> (if T.null resultsStr then "" else " (result " <> resultsStr <> ")")
        <> "))"

-- | Prints a WatModule to its WAT string representation
printWatModule :: WatModule -> Text
printWatModule m =
  let typeSignaturesList = Map.toList (moduleTypes m)
      typesStr = T.intercalate "\n" $ map (\(alias, sig) -> printWatTypeSignature 2 alias sig) typeSignaturesList
      funcs = map (printWatFunction 2 (moduleTypes m)) (moduleFunctions m)
      funcsStr = if null funcs then "" else T.intercalate "\n" funcs
   in "(module\n"
        <> (if T.null typesStr then "" else typesStr <> "\n")
        <> (if T.null funcsStr then "" else funcsStr <> "\n")
        <> ")"