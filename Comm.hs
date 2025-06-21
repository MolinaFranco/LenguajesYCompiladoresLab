-- {-# LANGUAGE LambdaCase #-}

module Comm
  ( Comm (..),
    interpComm,
    run,
  )
where

import BoolExp
import IntExp
import State

{-
Gramaticas Comandos:
⟨comm⟩ ::= skip
\| ⟨var⟩ := ⟨intexp⟩
\| ⟨comm⟩ ; ⟨comm⟩
\| if ⟨boolexp⟩ then ⟨comm⟩ else ⟨comm⟩
\| newvar ⟨var⟩ := ⟨intexp⟩ in ⟨comm⟩
\| while ⟨boolexp⟩ do ⟨comm⟩
\| fail
-}

type Var = String

type Output = [String]

type Input = [Int]

data Comm
  = Skip
  | Assign Var IntExp
  | Seq Comm Comm
  | Condicional BoolExp Comm Comm
  | NewVar Var IntExp Comm
  | While BoolExp Comm
  | Fail
  | Print IntExp
  | Read Var
  deriving (Eq, Show)

interpComm :: Comm -> State -> Input -> Maybe (State, Output, Input)
interpComm comm state input = case comm of
  Skip -> Just (state, [], input)
  Fail -> Nothing
  Assign v ie ->
    let newVal = interpIntExp ie state
     in Just (updateState v newVal state, [], input)
  Seq c1 c2 ->
    case interpComm c1 state input of
      Nothing -> Nothing
      Just (s1, out1, input1) ->
        case interpComm c2 s1 input1 of
          Nothing -> Nothing
          Just (s2, out2, input2) ->
            Just (s2, out1 ++ out2, input2)
  Condicional b ct cf ->
    if interpBoolExp b state
      then interpComm ct state input
      else interpComm cf state input
  NewVar v ie c ->
    let initial = interpIntExp ie state
        oldVal = state v
        updatedState = updateState v initial state
     in case interpComm c updatedState input of
          Nothing -> Nothing
          Just (s', out, input') ->
            Just (updateState v oldVal s', out, input')
  While be body ->
    let evalWhile s inp =
          if interpBoolExp be s
            then case interpComm body s inp of
              Nothing -> Nothing
              Just (s', out1, inp') ->
                case evalWhile s' inp' of
                  Nothing -> Nothing
                  Just (s'', out2, inp'') ->
                    Just (s'', out1 ++ out2, inp'')
            else Just (s, [], inp)
     in evalWhile state input
  Print e ->
    let v = interpIntExp e state
     in Just (state, [show v], input)
  Read v -> case input of
    [] -> Nothing
    (i : is) -> Just (updateState v i state, [], is)

-- Utilidad para ejecutar un programa con estado e input dados
run :: Comm -> State -> Input -> IO ()
run c st inp = case interpComm c st inp of
  Nothing -> putStrLn "Fallo en la ejecución."
  Just (_, out, _) -> mapM_ putStrLn out