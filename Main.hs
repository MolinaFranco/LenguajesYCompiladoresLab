import BoolExp
import Comm
import IntExp
import State

-- Estado inicial (todas las variables empiezan en 0)
initialState :: State
initialState _ = 0

-- Programa cuenta regresiva desde x hasta 1, luego imprime 999
progOK :: Comm
progOK =
  Seq (Read "x") $
    Seq
      ( While
          (Rel Gt (Var "x") (Const 0))
          ( Seq
              (Print (Var "x"))
              (Assign "x" (Operation Sub (Var "x") (Const 1)))
          )
      )
      (Print (Const 999))

-- Programa que falla: intenta leer sin entrada
progFail :: Comm
progFail = Seq (Read "x") (Print (Var "x"))

testok = run progOK initialState [3]

testfail = run progFail initialState []

-- Punto de entrada (opcional)
main :: IO ()
main = do
  putStrLn "Ejecutando progOK con entrada [3]:"
  testok
  putStrLn "\nEjecutando progFail sin entrada:"
  testfail
