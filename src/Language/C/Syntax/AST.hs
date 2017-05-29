module Language.C.Syntax.AST
  ( Preprocessor(..), Ident(..), CAST(..), CExtDecl(..), CFunDef(..)

  -- declaration constructors
  , CDecl(..), CDeclr(..), CDeclSpec(..), CStorageSpec(..), CTypeQual(..)
  , CDirectDeclr(..), CTypeSpec(..), CTypeName(..), CSUSpec(..), CSUTag(..)
  , CEnum(..), CInit(..), CPartDesig(..), CFunSpec(..), CPtrDeclr(..)

  -- statements and expression constructors
  , CStat(..), CCompoundBlockItem(..), CExpr(..), CConst(..), CUnaryOp(..)
  , CBinaryOp(..), CAssignOp(..)

  -- infix and smart constructors
  , (.>.),(.<.),(.==.),(.!=.),(.||.),(.&&.),(.*.),(./.),(.-.),(.+.),(.=.),(.+=.)
  , (.*=.),(.>=.),(.<=.),(...),(.->.)
  , seqCStat
  , indirect, address, index, intE, charE, floatE, stringE, mkCallE, mkUnaryE
  , nullE

  -- util
  , cNameStream
  ) where

import Control.Monad (mplus)


--------------------------------------------------------------------------------
--                               Top Level                                    --
--------------------------------------------------------------------------------

data CAST
  = CAST [CExtDecl]
  deriving (Show, Eq, Ord)


data CExtDecl
  = CDeclExt    CDecl
  | CFunDefExt  CFunDef
  | CCommentExt String
  | CPPExt      Preprocessor
  deriving (Show, Eq, Ord)

data CFunDef
  = CFunDef [CDeclSpec] CDeclr [CDecl] CStat
  deriving (Show, Eq, Ord)

{-
  This is currently a very rough AST for preprocessor. Preprocessor macros
  can be inserted at the top level and at the statement level
-}
data Preprocessor
  = PPDefine  String String
  | PPInclude String
  | PPUndef   String
  | PPIf      String
  | PPIfDef   String
  | PPIfNDef  String
  | PPElse    String
  | PPElif    String
  | PPEndif   String
  | PPError   String
  | PPPragma  [String]
  deriving (Show, Eq, Ord)

data Ident
 = Ident String
 deriving (Show, Eq, Ord)


--------------------------------------------------------------------------------
--                               C Declarations                               --
--------------------------------------------------------------------------------
{-
  C Declarations provide tools for laying out memory objections.
-}

data CDecl
  = CDecl [CDeclSpec] [(CDeclr, Maybe CInit)]
  deriving (Show, Eq, Ord)

----------------
-- Specifiers --
----------------

-- top level specifier
data CDeclSpec
  = CStorageSpec CStorageSpec
  | CTypeSpec    CTypeSpec
  | CTypeQual    CTypeQual
  | CFunSpec     CFunSpec
  deriving (Show, Eq, Ord)

data CStorageSpec
  = CTypeDef
  | CExtern
  | CStatic
  | CAuto
  | CRegister
  deriving (Show, Eq, Ord)

data CTypeQual
  = CConstQual
  | CVolatQual
  deriving (Show, Eq, Ord)

data CFunSpec = Inline
  deriving (Show, Eq, Ord)

data CTypeSpec
  = CVoid
  | CChar
  | CShort
  | CInt
  | CLong
  | CFloat
  | CDouble
  | CSigned
  | CUnsigned
  | CSUType      CSUSpec
  | CTypeDefType Ident
  | CEnumType    CEnum
  deriving (Show, Eq, Ord)

-- CTypeName is necessary for cast operations, see C99 pp81 and pp122
-- For now, we only need to use these casts for malloc, so this is
-- incomplete with respect to C99
data CTypeName
  = CTypeName [CTypeSpec] Bool
  deriving (Show, Eq, Ord)

data CSUSpec
  = CSUSpec CSUTag (Maybe Ident) [CDecl]
  deriving (Show, Eq, Ord)

data CSUTag
  = CStructTag
  | CUnionTag
  deriving (Show, Eq, Ord)

data CEnum
  = CEnum (Maybe Ident) [(Ident, Maybe CExpr)]
  deriving (Show, Eq, Ord)

-----------------
-- Declarators --
-----------------
{-
  Declarators give us labels to point at and describe the level of indirection.
  between a label and the underlieing memory

  this is incomplete, see c99 reference p115
-}

data CDeclr
  = CDeclr (Maybe CPtrDeclr) CDirectDeclr
  deriving (Show, Eq, Ord)

data CPtrDeclr = CPtrDeclr [CTypeQual]
  deriving (Show, Eq, Ord)

data CDirectDeclr
  = CDDeclrIdent Ident
  | CDDeclrArr   CDirectDeclr (Maybe CExpr)
  | CDDeclrFun   CDirectDeclr [CTypeSpec]
  | CDDeclrRec   CDeclr
  deriving (Show, Eq, Ord)

------------------
-- Initializers --
------------------
{-
  Initializers allow us to fill our objects with values right as they are
  declared rather than as a side-effect later in the program.
-}

data CInit
  = CInitExpr CExpr
  | CInitList [([CPartDesig], CInit)]
  deriving (Show, Eq, Ord)

data CPartDesig
  = CArrDesig    CExpr
  | CMemberDesig CExpr
  deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------
--                                C Statments                                 --
--------------------------------------------------------------------------------
{-
  The separation between C Statements and C Expressions is fuzzy. Here we take
  statements as side-effecting operations sequenced by the ";" in pedantic C
  concrete syntax. Though operators like "++" that are represented as C
  Expressions in this AST also perform side-effects.
-}

data CStat
  = CLabel    Ident CStat
  | CGoto     Ident
  | CSwitch   CExpr CStat
  | CCase     CExpr CStat
  | CDefault  CStat
  | CExpr     (Maybe CExpr)
  | CCompound [CCompoundBlockItem]
  | CIf       CExpr CStat (Maybe CStat)
  | CWhile    CExpr CStat Bool
  | CFor      (Maybe CExpr) (Maybe CExpr) (Maybe CExpr) CStat
  | CCont
  | CBreak
  | CReturn   (Maybe CExpr)
  | CComment  String
  | CPPStat   Preprocessor
  deriving (Show, Eq, Ord)

data CCompoundBlockItem
  = CBlockStat CStat
  | CBlockDecl CDecl
  deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------
--                                C Expressions                               --
--------------------------------------------------------------------------------
{-
  See C Statments...
-}

data CExpr
  = CComma       [CExpr]
  | CAssign      CAssignOp CExpr CExpr
  | CCond        CExpr CExpr CExpr
  | CBinary      CBinaryOp CExpr CExpr
  | CCast        CTypeName CExpr
  | CUnary       CUnaryOp CExpr
  | CSizeOfExpr  CExpr
  | CSizeOfType  CTypeName
  | CIndex       CExpr CExpr
  | CCall        CExpr [CExpr]
  | CMember      CExpr Ident Bool
  | CVar         Ident
  | CConstant    CConst
  | CCompoundLit CDecl CInit
  deriving (Show, Eq, Ord)


data CAssignOp
  = CAssignOp
  | CMulAssOp
  | CDivAssOp
  | CRmdAssOp
  | CAddAssOp
  | CSubAssOp
  | CShlAssOp
  | CShrAssOp
  | CAndAssOp
  | CXorAssOp
  | COrAssOp
  deriving (Show, Eq, Ord)


data CBinaryOp
  = CMulOp
  | CDivOp
  | CRmdOp
  | CAddOp
  | CSubOp
  | CShlOp
  | CShrOp
  | CLeOp
  | CGrOp
  | CLeqOp
  | CGeqOp
  | CEqOp
  | CNeqOp
  | CAndOp
  | CXorOp
  | COrOp
  | CLndOp
  | CLorOp
  deriving (Show, Eq, Ord)


data CUnaryOp
  = CPreIncOp
  | CPreDecOp
  | CPostIncOp
  | CPostDecOp
  | CAdrOp
  | CIndOp
  | CPlusOp
  | CMinOp
  | CCompOp
  | CNegOp
  deriving (Show, Eq, Ord)


data CConst
  = CIntConst    Integer
  | CCharConst   Char
  | CFloatConst  Float
  | CStringConst String
  deriving (Show, Eq, Ord)


--------------------------------------------------------------------------------
--                      Infix and Smart Constructors                          --
--------------------------------------------------------------------------------
{-
  These are helpful when building up ASTs in Haskell code. They correspond to
  the concrete syntax of C. This is an incomplete set...
-}

seqCStat :: [CStat] -> CStat
seqCStat = CCompound . fmap CBlockStat

(.<.),(.>.),(.==.),(.!=.),(.||.),(.&&.),(.*.),(./.),(.-.),(.+.),(.=.),(.+=.),(.*=.),(.<=.),(.>=.)
  :: CExpr -> CExpr -> CExpr
a .<. b  = CBinary CLeOp a b
a .>. b  = CBinary CGrOp a b
a .==. b = CBinary CEqOp a b
a .!=. b = CBinary CNeqOp a b
a .||. b = CBinary CLorOp a b
a .&&. b = CBinary CLndOp a b
a .*. b  = CBinary CMulOp a b
a ./. b  = CBinary CDivOp a b
a .-. b  = CBinary CSubOp a b
a .+. b  = CBinary CAddOp a b
a .<=. b = CBinary CLeqOp a b
a .>=. b = CBinary CGeqOp a b
a .=. b  = CAssign CAssignOp a b
a .+=. b = CAssign CAddAssOp a b
a .*=. b = CAssign CMulAssOp a b


indirect, address :: CExpr -> CExpr
indirect = CUnary CIndOp
address  = CUnary CAdrOp

index :: CExpr -> CExpr -> CExpr
index = CIndex

(...),(.->.) :: CExpr -> String -> CExpr
i ... n  = CMember i (Ident n) True
i .->. n = CMember i (Ident n) False

intE :: Integer -> CExpr
intE = CConstant . CIntConst

floatE :: Float -> CExpr
floatE = CConstant . CFloatConst

charE :: Char -> CExpr
charE = CConstant . CCharConst

stringE :: String -> CExpr
stringE = CConstant . CStringConst

mkCallE :: String -> [CExpr] -> CExpr
mkCallE s = CCall (CVar . Ident $ s)

mkUnaryE :: String -> CExpr -> CExpr
mkUnaryE s a = mkCallE s [a]

nullE :: CExpr
nullE = CVar . Ident $ "NULL"

--------------------------------------------------------------------------------

cNameStream :: [String]
cNameStream = filter (\n -> not $ elem (head n) ['0'..'9']) names
  where base :: [Char]
        base = ['0'..'9'] ++ ['a'..'z']
        names = [[x] | x <- base] `mplus` (do n <- names
                                              [n++[x] | x <- base])
