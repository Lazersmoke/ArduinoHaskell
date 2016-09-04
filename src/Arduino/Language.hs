{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Arduino.Language where

import Data.List

-- word add to string
(<+>) :: String -> String -> String
(<+>) a "" = a
(<+>) a b = a ++ " " ++ b

parens :: String -> String
parens s = "(" ++ s ++ ")"

tuple :: [String] -> String
tuple = parens . intercalate ", "

class Type haskellType where
  type Inhabited haskellType :: Flag
  typeName :: haskellType -> String -> String

instance Type Bool where
  type Inhabited Bool = 'Yes
  typeName _ var = "boolean" <+> var
instance Type Int where
  type Inhabited Int = 'Yes
  typeName _ var = "int" <+> var
instance Type Float where
  type Inhabited Float = 'Yes
  typeName _ var = "float" <+> var
instance Type Double where
  type Inhabited Double = 'Yes
  typeName _ var = "double" <+> var
instance Type Char where
  type Inhabited Char = 'Yes
  typeName _ var = "char" <+> var
instance Type () where
  type Inhabited () = 'No
  typeName _ var = "void" <+> var
instance Type t => Type (Ptr t) where
  type Inhabited (Ptr t) = 'Yes
  typeName _ var = typeName (undefined :: t) var ++ "*" <+> var
instance Type t => Type (Value a t) where
  type Inhabited (Value a t) = 'No
  typeName _ = typeName (undefined :: t) 
-- bool (*int)(bool,int,a)
instance CFunction f => Type (Function f) where
  type Inhabited (Function f) = 'Yes
  typeName _ v = last ty "" <+> parens ("*" ++ v) ++ tuple (map ($ "") (init ty))
    where ty = funTypeName (undefined :: f)

class Type (LiteralType h) => Literalable h where
  type LiteralType h :: *
  literal :: h -> Value 'No (LiteralType h)
instance Literalable Int where
  type LiteralType Int = Int
  literal = Value . show
literalInt :: Int -> Value 'No Int
literalInt = literal
instance Literalable Float where
  type LiteralType Float = Float
  literal = Value . (++"f") . show
instance Literalable Double where
  type LiteralType Double = Double
  literal = Value . show
instance Literalable Char where
  type LiteralType Char = Char
  literal = Value . show
instance Literalable Bool where
  type LiteralType Bool = Bool
  literal True = Value "true"
  literal False = Value "false"
instance Literalable String where
  type LiteralType String = Ptr Char
  literal x = Value $ "\"" ++ x ++ "\""

class Type t => Mathable t where
instance Mathable Float where
instance Mathable Double where
instance Mathable Char where
instance Mathable Int where

class CFunction a where
  funTypeName :: a -> [String -> String]
instance (Type t, Inhabited t ~ 'Yes) => CFunction (IO t) where
  funTypeName _ = [typeName (undefined :: t)]
instance (Type t, CFunction a) => CFunction (t -> a) where
  funTypeName _ = typeName (undefined :: t) : funTypeName (undefined :: a)

data Ptr a

pointTo :: Type t => Value a t -> Value 'No (Ptr t)
pointTo = Value . parens . ("&"++) . unValue

dereference :: Type t => Value a (Ptr t) -> Value 'Yes t
dereference = Value . parens . ("*"++) . unValue

sizeof :: Type t => t -> Value 'No Int
sizeof = Value . ("sizeof"++) . parens . flip typeName ""

ternary :: Type t => Value a Bool -> Value b t -> Value c t -> Value 'No t
ternary p t f = Value . parens $ unValue p <+> "?" <+> unValue t <+> ":" <+> unValue f

unsafeCast :: forall a b t. Type t => Value a b -> Value a t
unsafeCast x = Value . parens $ parens (typeName (undefined :: t) "") ++ unValue x

binOp :: Type t' => String -> Value a t -> Value b t -> Value 'No t'
binOp op x y = Value . parens $ unValue x <+> op <+> unValue y

-- Equality
(==.),(/=.) :: Value a t -> Value b t -> Value 'No Bool
(==.) = binOp "=="
(/=.) = binOp "!="
-- Comparision
(<.),(>.),(<=.),(>=.) :: Mathable t => Value a t -> Value b t -> Value 'No Bool
(<.)  = binOp "<"
(>.)  = binOp ">"
(<=.) = binOp "<="
(>=.) = binOp ">="
-- Bin Combinator
(&&.),(||.) :: Value a Bool -> Value b Bool -> Value 'No Bool
(&&.) = binOp "&&"
(||.) = binOp "||"
not :: Value a Bool -> Value 'No Bool
not x = Value . parens $ "!" ++ unValue x
-- Numeric Operators
(+.),(-.),(*.),(/.) :: Mathable t => Value a t -> Value b t -> Value 'No t
(+.) = binOp "+"
(-.) = binOp "-"
(*.) = binOp "*"
(/.) = binOp "/"
-- Variables
initialize :: forall t a. (Type t, Inhabited t ~ 'Yes) => String -> Value a t -> CMonad (Value 'Yes t)
initialize name val = do
  useName <- getNewName name
  trustMeLn $ typeName (undefined :: t) useName <+> "=" <+> unValue val ++ ";"
  return $ Value useName

emptyVar :: forall t. (Type t, Inhabited t ~ 'Yes) => String -> CMonad (Value 'Yes t)
emptyVar name = do
  useName <- getNewName name
  trustMeLn $ typeName (undefined :: t) useName ++ ";"
  return $ Value useName

(=:) :: Value 'Yes t -> Value a t -> CMonad ()
(=:) var val = trustMeLn $ unValue var <+> "=" <+> unValue val ++ ";"

class FunctionArgs funtype restype where
  -- get a result type from a function and a list of args
  funArgs :: Function a -> [String] -> funtype -> restype
-- IO Actions are at the end for some reason
instance Type a => FunctionArgs (IO a) (Value 'No a) where
  funArgs (Function a) args _ = Value $ a ++ tuple (reverse args)
instance FunctionArgs b b' => FunctionArgs (a -> b) (Value n a -> b') where
  funArgs f args _ val = funArgs f (unValue val : args) (undefined :: b) :: b'

newtype Function t = Function {unFunction :: String}

data Flag = Yes | No

data Value (assingable :: Flag) t where
  Value :: Type t => String -> Value a t

instance Show (Value a t) where
  show = unValue

unValue :: Value a t -> String
unValue (Value s) = s

--testTree :: Value ()
--testTree = CMethod "main" $ CSemicolon (delay (CIntLit 5)) (CReturn (CIntLit 5)) 

--delay :: CExpr 'CInt -> CExpr 'CAction
--delay (CIntLit i) = CFunctionCall $ "delay(" ++ show i ++ ")"

newtype Identifier = Identifier String deriving (Eq,Show)

data Generation = Generate [Identifier] String

data CMonad a = CMonad (Generation -> (a,Generation))

-- AST
-- -- [Declaration]
--
-- Declaration
-- -- Include String
-- -- Method 

runCMonad :: 
  CMonad a -> -- CMonad to convert
  Generation -> -- Generation So Far
  (a,Generation) -- Return Value, used names, new Source Code
runCMonad (CMonad run) = run

instance Show (CMonad a) where
  show c = let (_,Generate _ s) = runCMonad c (Generate [] "") in s

testTree :: CMonad ()
testTree = do
  inMethod "main" $ do
    meowVar <- initialize "meow" (literal (5 :: Int))
    meowVar =: literalInt 5
    return ()
  inMethod "loop" $ do
    digitalWrite (literalInt 13) (literal True)
    delay (literalInt 500)
    digitalWrite (literalInt 13) (literal False)

digitalWrite :: Value a Int -> Value b Bool -> CMonad ()
digitalWrite pin onOff = trustMeLn $ "digitalWrite(" ++ unValue pin ++ "," <+> unValue onOff ++ ");"

delay :: Value a Int -> CMonad ()
delay time = trustMeLn $ "delay(" ++ unValue time ++ ");"

inMethod :: String -> CMonad a -> CMonad a
inMethod s cma = do
  trustMeLn $ "void " ++ s ++ "(){"
  ca <- indent cma
  trustMeLn "}"
  return ca

indent :: CMonad a -> CMonad a
indent cma = CMonad $ \g -> let (a,Generate u s) = runCMonad cma g in
  (a,Generate u (unlines . map ("  "++) . lines $ s))

instance Functor CMonad where
  fmap f c = CMonad $ \g -> let (a,g') = runCMonad c g in (f a,g')

instance Applicative CMonad where
  pure a = CMonad $ \g -> (a,g)
  pf <*> pa = CMonad $ \g -> 
    let
      (f,g') = runCMonad pf g
      (x,g'') = runCMonad pa g'
    in (f x,g'')
    

instance Monad CMonad where
  --return a = CMonad $ \s -> (a,s)
  (>>=) x f = CMonad $ \g -> let (a,g') = runCMonad x g in runCMonad (f a) g'

trustMe :: String -> CMonad ()
trustMe str = CMonad $ \(Generate u s) -> ((),Generate u (s ++ str))

trustMeLn :: String -> CMonad ()
trustMeLn = trustMe . (++"\n")

getNewName :: String -> CMonad String
getNewName chosenName = CMonad $ \(Generate u s) ->
  if Identifier chosenName `elem` u
    then runCMonad (getNewName (chosenName ++ "0")) (Generate u s)
    else (chosenName,Generate (Identifier chosenName : u) s)

--reference :: CRef a -> CMonad ()
--reference = sourceCode . getRef
{-
express :: CExpr a -> CMonad ()
express = sourceCode . unCExpr

declare :: TypeName a => String -> CExpr a -> CMonad (CExpr a)
declare r e = do
  sourceCode $ typeName e ++ " " ++ unCExpr (CRefVal r) ++ " = " ++ unCExpr e ++ ";"
  return (CRefVal r)

semicolon :: CMonad ()
semicolon = sourceCode "; "

backtrack :: CMonad ()
backtrack = CMonad $ \s -> ((),init s)

--returnRef :: CRef a -> CMonad ()
--returnRef r = do 
  --sourceCode "return "
  --reference r

returnExp :: CExpr a -> CMonad ()
returnExp e = do 
  sourceCode "return "
  express e
  semicolon

{-
goal :: CMonad ()
goal = 
  inMethod "loop" $ do
    intRead <- readInt :: CMonad (CRef 'CInt)
    digitalWrite 13 True :: CMonad ()
    (printf :: CRef 'CInt -> CMonad ()) (intRead :: CRef 'CInt)
    delay 1000 :: CMonad ()
    digitalWrite 13 False :: CMonad ()
    delay 1000 :: CMonad ()
goalv :: CMonad ()
goalv = 
  inMethod "loop" (digitalWrite 13 True >> delay 1000 >> digitalWrite 13 False >> delay 1000)


newgoal :: CMonad ()
newgoal = do
  

-}

blink :: CMonad ()
blink = do
  inMethod "setup" $
    pinMode 13 False
  sourceCode "\n"
  inMethod "loop" $ do
    digitalWrite 13 True
    delay 1000
    digitalWrite 13 False
    delay 1000

testProgram :: CMonad ()
testProgram = 
  inMethod "main" $ do
    testVar <- declare "testVar" (CIntVal 5)
    _ <- declare "testVar2" (CBoolVal True)
    sourceCode "return "
    express testVar

inMethod :: String -> CMonad a -> CMonad a
inMethod s cma = do
  sourceCode $ "void " ++ s ++ "(){"
  ca <- cma
  sourceCode "}"
  return ca

digitalWrite :: CExpr 'CInt -> CExpr 'CBool -> CMonad ()
digitalWrite t on = cFunction "digitalWrite" [t,on]

delay :: CExpr 'CInt -> CMonad ()
delay t = cFunction "delay" [unCExpr t]

pinMode :: CExpr 'CInt -> CExpr 'CBool -> CMonad ()
pinMode i True = cFunction "pinMode" [unCExpr i, "INPUT"]
pinMode i False = cFunction "pinMode" [unCExpr i, "OUTPUT"]

cFunction :: String -> [String] -> CMonad ()
cFunction fName xs = do
  sourceCode fName
  sourceCode "("
  sourceCode . intercalate "," $ xs
  sourceCode ")"
  semicolon
-}
