module Example.Util.Prog

import public Control.Monad.Elin
import public Data.List.Quantifiers.Extra
import System.Posix.File
import Data.C.Ptr

%default total

||| An example program that can fail with one of the listed errors and
||| produces a result of the given type.
public export
0 Prog : List Type -> Type -> Type
Prog = Elin World

--------------------------------------------------------------------------------
-- Error handling
--------------------------------------------------------------------------------

||| An error handler for errors of type `a`
public export
0 Handler : Type -> Type
Handler a = a -> Prog [] ()

||| Generalized continuation: Bind and error handling
export
bracketCase : (Result es a -> Prog fs b) -> Prog es a -> Prog fs b
bracketCase = flip bindResult

||| A prog that cannot fail can always be converted to a prog that
||| could potentially fail.
export
anyErr : Prog [] a -> Prog es a
anyErr = bracketCase $ \(Right v) => pure v

||| Forget all errors and the result and run a prog only for its effects
export
clear : Prog es a -> Prog [] ()
clear = bracketCase (const $ pure ())

||| Guaranteed to run a cleanup function based on a program's outcome
export
guaranteeCase : (Result es a -> Prog [] ()) -> Prog es a -> Prog es a
guaranteeCase f = bracketCase $ \x => anyErr (f x) >> fromResult x

||| Guaranteed to run a cleanup function independent of a program's outcome
export
finally : Prog [] () -> Prog es a -> Prog es a
finally = guaranteeCase . const

||| Handles all errors of a program.
export
handleErrors : (hs : All Handler es) -> Prog es () -> Prog fs ()
handleErrors hs =
  bracketCase $ \case
    Left x  => anyErr $ collapse' $ hzipWith id hs x
    Right _ => pure ()

export
onErrno : Has Errno es => Errno -> Prog es a -> Prog es a -> Prog es a
onErrno err h =
  bracketCase $ \case
    Right a => pure a
    Left  x => case project Errno x of
      Just e  => if err == e then h else throw e
      Nothing => fail x

||| Specialized version of `handleErrors` for better type inference
export %inline
handleError : Handler e -> Prog [e] () -> Prog fs ()
handleError h = handleErrors [h]

export
dropErr : Has e es => (e -> a) -> Prog es a -> Prog es a
dropErr f =
  bracketCase $ \case
    Right v => pure v
    Left  x => maybe (fail x) (pure . f) (project e x)

export
logAndDropErr : Has e es => (e -> Prog [] a) -> Prog es a -> Prog es a
logAndDropErr h =
  bracketCase $ \case
    Right v => pure v
    Left  x => maybe (fail x) (anyErr . h) (project e x)

--------------------------------------------------------------------------------
-- Running programs
--------------------------------------------------------------------------------

export %inline
stdoutLn' : String -> Prog [] ()
stdoutLn' = clear {es = [Errno]} . stdoutLn

export %inline
stderrLn' : String -> Prog [] ()
stderrLn' = clear {es = [Errno]} . stderrLn

||| Runs a program that has all its errors handled.
export
runProg : Prog [] a -> IO a
runProg p = runElinIO p >>= \(Right v) => pure v

export %inline
runProgWith : All Handler es -> Prog es () -> IO ()
runProgWith hs = runProg . handleErrors hs

export %inline
prettyOut : Interpolation a => a -> Prog [] ()
prettyOut = stdoutLn' . interpolate

export %inline
prettyErr : Interpolation a => a -> Prog [] ()
prettyErr = stderrLn' . interpolate

export %inline
prettyErrno : Errno -> Prog [] ()
prettyErrno = prettyErr
