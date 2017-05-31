-- Wymagamy, by moduł zawierał tylko bezpieczne funkcje
{-# LANGUAGE Safe #-}
-- Definiujemy moduł zawierający rozwiązanie.
-- Należy zmienić nazwę modułu na {Imie}{Nazwisko} gdzie za {Imie}
-- i {Nazwisko} należy podstawić odpowiednio swoje imię i nazwisko
-- zaczynające się wielką literą oraz bez znaków diakrytycznych.
module KacperJedrzejewski (typecheck, eval) where
--
-- Importujemy moduły z definicją języka oraz typami potrzebnymi w zadaniu
import AST
import DataTypes
import qualified Data.Map.Strict as Map
import Prelude hiding (Map, lookup)
 --[("x",Num),("y",Bool1),("q",Num)]
   -- [()]
-- Funkcja sprawdzająca typy
-- Dla wywołania typecheck vars e zakładamy, że zmienne występujące
-- w vars są już zdefiniowane i mają typ int, i oczekujemy by wyrażenia e
-- miało typ int
-- UWAGA: to nie jest jeszcze rozwiązanie; należy zmienić jej definicję.
data Exp = 
      INT -- int
      | BOOL -- bool 
     deriving (Eq, Show)
data Error p =
      E p String --zwracam blad na pozycji p  
      deriving (Eq, Show)



data Environment a = 
      Environment (Map.Map Var a) 
      deriving (Eq)


typecheck :: [Var] -> Expr p -> TypeCheckResult p
typecheck xs expr = case infer_type (Environment (Map.fromList (zip xs (cycle [INT])))) expr of 
                    Right INT -> Ok 
                    Right BOOL -> Error  (getData expr) "bledy typ wyniku"
                    Left (E p x)  -> Error p x

infer_type :: Environment  Exp -> Expr p -> Either (Error p) Exp

infer_type gamma (ENum p x ) = Right INT 

infer_type gamma (EBool p x ) = Right BOOL


infer_type (Environment gamma) (EVar p e) = case Map.lookup e gamma of 
                                            Nothing -> Left (E p "Undefined variable")
                                            Just a -> Right a 
infer_type  gamma (EUnary  p UNeg w ) = case (infer_type  gamma w) of 
                                        Right INT -> Right INT
                                        Right BOOL -> Left (E p "- przed wartoscia bool'owska")
                                        Left error -> Left error 

infer_type gamma (EUnary  p UNot w ) = case (infer_type gamma w) of 
                                        Right BOOL -> Right BOOL
                                        Right INT -> Left (E p "- przed wartoscia bool'owska")
                                        Left error -> Left error 


infer_type gamma (EIf  p wyr wyr1 wyr2 ) = case (infer_type gamma wyr) of 
                                           Right INT -> Left (E p "Wartosc w if jest typu int")
                                           Left error -> Left error 
                                           Right BOOL -> case (infer_type gamma wyr1) of
                                                         Right BOOL -> case (infer_type gamma wyr2) of
                                                                      Right INT -> Left (E p "Wartosc w else jest typu bool")
                                                                      Left error -> Left error
                                                                      Right BOOL -> Right BOOL

                                                         Left error -> Left error
                                                         Right INT -> case (infer_type gamma wyr2) of
                                                                      Right BOOL -> Left (E p "Wartosc w else jest typu bool")
                                                                      Left error -> Left error
                                                                      Right INT -> Right INT

infer_type (Environment gamma) (ELet  p var wyr1 wyr2 ) = case (infer_type (Environment gamma) wyr1) of
                                                          Left error -> Left error
                                                          Right BOOL -> case (infer_type (Environment (Map.insert var BOOL gamma)) wyr2) of
                                                                        Right BOOL -> Right BOOL
                                                                        Right INT -> Right INT
                                                                        Left error -> Left error
                                                          Right INT -> case (infer_type (Environment (Map.insert var INT gamma)) wyr2) of
                                                                        Right BOOL -> Right BOOL
                                                                        Right INT -> Right INT
                                                                        Left error -> Left error

infer_type gamma (EBinary p bop wyr1 wyr2 ) = case bop of 
                                            BAnd -> case (infer_type gamma wyr1) of 
                                                    Right INT -> Left (E p "Pierwsza wartosc jest typu int")
                                                    Left error -> Left error 
                                                    Right BOOL -> case (infer_type gamma wyr2) of
                                                                  Right INT -> Left (E p "Druga wartosc jest typu int")
                                                                  Left error -> Left error 
                                                                  Right BOOL -> Right BOOL
                                       
                                            BOr -> case (infer_type gamma wyr1) of 
                                                   Right INT -> Left (E p "Pierwsza wartosc jest typu int")
                                                   Left error -> Left error 
                                                   Right BOOL -> case (infer_type gamma wyr2) of
                                                                 Right INT -> Left (E p "Druga wartosc jest typu int")
                                                                 Left error -> Left error 
                                                                 Right BOOL -> Right BOOL
                                                                                      
                                        
                                       
                                       
                                            BEq -> case (infer_type gamma wyr1) of 
                                                   Right BOOL -> Left (E p "Pierwsza wartosc jest typu bool")
                                                   Left error -> Left error 
                                                   Right INT -> case (infer_type gamma wyr2) of
                                                                Right BOOL -> Left (E p "Druga wartosc jest typu bool")
                                                                Left error -> Left error 
                                                                Right INT -> Right BOOL
                                       
                                       
                                            BNeq -> case (infer_type gamma wyr1) of 
                                                    Right BOOL -> Left (E p "Pierwsza wartosc jest typu bool")
                                                    Left error -> Left error 
                                                    Right INT -> case (infer_type gamma wyr2) of
                                                                 Right BOOL -> Left (E p "Druga wartosc jest typu bool")
                                                                 Left error -> Left error 
                                                                 Right INT -> Right BOOL
                                       
                                       
                                            BLt -> case (infer_type gamma wyr1) of 
                                                   Right BOOL -> Left (E p "Pierwsza wartosc jest typu bool")
                                                   Left error -> Left error 
                                                   Right INT -> case (infer_type gamma wyr2) of
                                                                Right BOOL -> Left (E p "Druga wartosc jest typu bool")
                                                                Left error -> Left error 
                                                                Right INT -> Right BOOL
                                        
                                            BGt -> case (infer_type gamma wyr1) of 
                                                   Right BOOL -> Left (E p "Pierwsza wartosc jest typu bool")
                                                   Left error -> Left error 
                                                   Right INT -> case (infer_type gamma wyr2) of
                                                                Right BOOL -> Left (E p "Druga wartosc jest typu bool")
                                                                Left error -> Left error 
                                                                Right INT -> Right BOOL                          
                                            
                                            BLe -> case (infer_type gamma wyr1) of 
                                                   Right BOOL -> Left (E p "Pierwsza wartosc jest typu bool")
                                                   Left error -> Left error 
                                                   Right INT -> case (infer_type gamma wyr2) of
                                                                Right BOOL -> Left (E p "Druga wartosc jest typu bool")
                                                                Left error -> Left error 
                                                                Right INT -> Right BOOL                          
                                                                   
                                            BGe -> case (infer_type gamma wyr1) of 
                                                   Right BOOL -> Left (E p "Pierwsza wartosc jest typu bool")
                                                   Left error -> Left error 
                                                   Right INT -> case (infer_type gamma wyr2) of
                                                                Right BOOL -> Left (E p "Druga wartosc jest typu bool")
                                                                Left error -> Left error 
                                                                Right INT -> Right BOOL                          
                                        
                                       
                                            BAdd -> case (infer_type gamma wyr1) of 
                                                    Right BOOL -> Left (E p "Pierwsza wartosc jest typu bool")                                                   
                                                    Left error -> Left error 
                                                    Right INT -> case (infer_type gamma wyr2) of
                                                                 Right BOOL -> Left (E p "Druga wartosc jest typu bool")
                                                                 Left error -> Left error 
                                                                 Right INT -> Right INT                          
                                        
                                            BSub -> case (infer_type gamma wyr1) of 
                                                    Right BOOL -> Left (E p "Pierwsza wartosc jest typu bool")
                                                    Left error -> Left error 
                                                    Right INT -> case (infer_type gamma wyr2) of
                                                                 Right BOOL -> Left (E p "Druga wartosc jest typu bool")
                                                                 Left error -> Left error 
                                                                 Right INT -> Right INT
                                       
                                       
                                            BMul -> case (infer_type gamma wyr1) of 
                                                    Right BOOL -> Left (E p "Pierwsza wartosc jest typu bool")
                                                    Left error -> Left error 
                                                    Right INT -> case (infer_type gamma wyr2) of
                                                                 Right BOOL -> Left (E p "Druga wartosc jest typu bool")
                                                                 Left error -> Left error 
                                                                 Right INT -> Right INT  
                                       
                                            BDiv -> case (infer_type gamma wyr1) of 
                                                    Right BOOL -> Left (E p "Pierwsza wartosc jest typu bool")
                                                    Left error -> Left error 
                                                    Right INT -> case (infer_type gamma wyr2) of
                                                                 Right BOOL -> Left (E p "Druga wartosc jest typu bool")
                                                                 Left error -> Left error 
                                                                 Right INT -> Right INT
                                        
                                            BMod -> case (infer_type gamma wyr1) of 
                                                    Right BOOL -> Left (E p "Pierwsza wartosc jest typu bool")
                                                    Left error -> Left error 
                                                    Right INT -> case (infer_type gamma wyr2) of
                                                                 Right BOOL -> Left (E p "Druga wartosc jest typu bool")
                                                                 Left error -> Left error 
                                                                 Right INT -> Right INT                                      
                                       
                                            
                                       
                                       
                                            


 


-- Funkcja obliczająca wyrażenia
-- Dla wywołania eval input e przyjmujemy, że dla każdej pary (x, v)
-- znajdującej się w input, wartość zmiennej x wynosi v.
-- Możemy założyć, że wyrażenie e jest dobrze typowane, tzn.
-- typecheck (map fst input) e = Ok
-- UWAGA: to nie jest jeszcze rozwiązanie; należy zmienić jej definicję.
data Value = 
        VInt Integer 
        | VBool Bool 
        deriving (Show, Eq, Ord)


eval :: [(Var,Integer)] -> Expr p -> EvalResult
eval xs expr = case infer_eval (Map.fromList ( map (\(v, i) -> (v, VInt i)) xs)) expr of
                 Left error -> RuntimeError
                 Right x  -> case x of
                                  VInt value -> Value value
                                  VBool _ -> undefined

infer_eval :: Map.Map Var Value -> Expr p -> Either (Error p) Value

infer_eval gamma (ENum p x ) =  Right (VInt x)
infer_eval gamma (EBool p x ) = Right (VBool x)

infer_eval gamma (EVar p e) = case Map.lookup e gamma of
                              Just x -> Right x
                              Nothing -> undefined

infer_eval  gamma (EUnary  p UNeg w ) = case infer_eval gamma w of
                                        Left error -> Left error 
                                        Right x -> Right (let VInt v = x in VInt ( negate v))

infer_eval gamma (EUnary  p UNot w ) = case infer_eval gamma w of
                                    Left error -> Left error 
                                    Right x ->  Right (let VBool v = x in VBool ( not v))
                                         
infer_eval gamma (EIf  p wyr wyr1 wyr2 ) =  case infer_eval gamma wyr of
                                            Left error -> Left error 
                                            Right x -> case x of
                                                        VBool True -> infer_eval gamma wyr1
                                                        VBool False -> infer_eval gamma wyr2
                                           
infer_eval  gamma (ELet  p var wyr1 wyr2 ) = case (infer_eval gamma wyr1) of
                                                          Left error -> Left error
                                                          Right x -> infer_eval (Map.insert var x gamma) wyr2


infer_eval gamma (EBinary p bop wyr1 wyr2 ) = case bop of 
                                            BAnd ->Right (VBool (case (infer_eval gamma wyr1) of 
                                                               
                                                                Right (VBool x) -> case (infer_eval gamma wyr2) of
                                                                          
                                                                             Right (VBool y) -> x && y) )
                                            
                                            BOr -> Right (VBool(case (infer_eval gamma wyr1) of 
                                                                
                                                                Right (VBool x) -> case (infer_eval gamma wyr2) of
                                                                             
                                                                             Right (VBool y) -> x || y))
                                       
                                            BEq -> Right (VBool(case (infer_eval gamma wyr1) of 
                                                              
                                                                Right (VInt x) -> case (infer_eval gamma wyr2) of
                                                                            
                                                                             Right (VInt y) -> x == y))
                                            BNeq -> Right (VBool(case (infer_eval gamma wyr1) of 
                                                           
                                                                Right (VInt x) -> case (infer_eval gamma wyr2) of
                                                                             
                                                                             Right (VInt y) -> x /= y))
                                            BLt -> Right (VBool(case (infer_eval gamma wyr1) of 
                                                             
                                                                Right (VInt x) -> case (infer_eval gamma wyr2) of
                                                                           
                                                                             Right (VInt y) -> x < y))
                                            BGt -> Right (VBool(case (infer_eval gamma wyr1) of 
                                                                
                                                                Right (VInt x) -> case (infer_eval gamma wyr2) of
                                                                             
                                                                             Right (VInt y) -> x > y))
                                            BLe -> Right (VBool(case (infer_eval gamma wyr1) of 
                                                        
                                                                Right (VInt x) -> case (infer_eval gamma wyr2) of
                                                                             
                                                                             Right (VInt y) -> x <= y))
                                            BGe -> Right (VBool(case (infer_eval gamma wyr1) of 
                                                         
                                                                Right (VInt x) -> case (infer_eval gamma wyr2) of
                                                                    
                                                                             Right (VInt y) -> x >= y))
                                            BAdd -> Right (VInt(case (infer_eval gamma wyr1) of 
                                                     
                                                                Right (VInt x) -> case (infer_eval gamma wyr2) of
                                                                    
                                                                             Right (VInt y) -> x + y))
                                            BSub -> Right (VInt(case (infer_eval gamma wyr1) of 
                                               
                                                                Right (VInt x) -> case (infer_eval gamma wyr2) of
                                                                         
                                                                             Right (VInt y) -> x - y))
                                            BMul -> Right (VInt(case (infer_eval gamma wyr1) of 
                                                      
                                                                Right (VInt x) -> case (infer_eval gamma wyr2) of
                                                                          
                                                                             Right (VInt y) -> x * y))
                                            
                                            BDiv -> case (infer_eval gamma wyr2) of 
                                                     Right (VInt 0) -> Left (E p "dzielenie przez zero")
                                                     Right (VInt _) -> Right (VInt( case (infer_eval gamma wyr1) of 
                                                                                   Right (VInt x) -> case (infer_eval gamma wyr2) of
                                                                                                      Right (VInt y) -> x `div` y))
                                            
                                            BMod -> case (infer_eval gamma wyr2) of 
                                                     Right (VInt 0) -> Left (E p "dzielenie przez zero")
                                                     Right (VInt _) -> Right (VInt( case (infer_eval gamma wyr1) of 
                                                                                   Right (VInt x) -> case (infer_eval gamma wyr2) of
                                                                                                      Right (VInt y) -> x `mod` y))