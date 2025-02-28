{-# LANGUAGE CApiFFI #-}

module Jiten.Yomichan.Core where

import Data.ByteString (ByteString, packCString)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Foreign (Ptr)
import Foreign.C (CString, newCString, peekCString, withCString)
import Foreign.C.Types (CSize (..))
import Foreign.Ptr (FunPtr, nullPtr)

data JSRuntime

data JSContext

foreign import ccall "JS_NewRuntime"
  jsNewRuntime :: IO (Ptr JSRuntime)

foreign import ccall "JS_FreeRuntime"
  jsFreeRuntime :: Ptr JSRuntime -> IO ()

foreign import ccall "JS_NewContext"
  jsNewContext :: Ptr JSRuntime -> IO (Ptr JSContext)

foreign import ccall "JS_NewCustomContext"
  jsNewCustomContext :: Ptr JSRuntime -> IO (Ptr JSContext)

foreign import ccall "JS_FreeContext"
  jsFreeContext :: Ptr JSContext -> IO ()

foreign import ccall "js_std_init_handlers"
  jsStdInitHandlers :: Ptr JSRuntime -> IO ()

foreign import ccall "js_std_add_helpers"
  jsStdAddHelpers :: Ptr JSContext -> Int -> Ptr (Ptr Char) -> IO ()

foreign import ccall "register_functions"
  registerFunctions :: Ptr JSContext -> IO ()

foreign import ccall "js_std_free_handlers"
  jsStdFreeHandlers :: Ptr JSRuntime -> IO ()

-- void js_eval_str(JSContext *ctx, const char *js_code) {

foreign import ccall "js_eval_str"
  jsEvalStr_ :: Ptr JSContext -> CString -> IO ()

foreign import ccall "js_eval_yomi_bytecode"
  jsEvalYomiBytecode :: Ptr JSContext -> IO ()

jsEvalStr :: Ptr JSContext -> String -> IO ()
jsEvalStr ctx str = do
  withCString str (\cstr -> jsEvalStr_ ctx cstr)

-- typedef char * (* StringToStringFunc) (const char *)
-- void set_string_processor (StringToStringFunc processor_func)

type StringToStringFunc = CString -> IO CString

-- Create a function pointer from a Haskell function
-- XXX: remember to eventually free it with `freeHaskellFunPtr` when it's no longer needed.
foreign import ccall "wrapper"
  mkStringToStringFunc :: StringToStringFunc -> IO (FunPtr StringToStringFunc)

-- Register the string processor function
foreign import ccall "set_string_processors"
  setStringProcessors ::
    FunPtr StringToStringFunc ->
    FunPtr StringToStringFunc ->
    FunPtr StringToStringFunc ->
    FunPtr StringToStringFunc ->
    FunPtr StringToStringFunc ->
    FunPtr StringToStringFunc ->
    IO ()

foreign import capi unsafe "string.h strndup"
  strndup :: CString -> CSize -> IO CString

foreign import capi unsafe "stdio.h puts"
  puts :: CString -> IO ()

-- https://www.reddit.com/r/haskell/comments/mxyt9j/comment/gvyiw32/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
copyByteString :: ByteString -> IO CString
copyByteString =
  flip unsafeUseAsCStringLen (uncurry $ (. fromIntegral) . strndup)

stringToStringFuncAdapter :: (String -> String) -> StringToStringFunc
stringToStringFuncAdapter f cstr = do
  str <- peekCString cstr
  let result = f str
  newCString result

byteStringToStringFuncAdapter :: (ByteString -> ByteString) -> StringToStringFunc
byteStringToStringFuncAdapter f cstr = do
  bs <- packCString cstr
  let result = f bs
  copyByteString result

textToStringFuncAdapter :: (Text -> Text) -> StringToStringFunc
textToStringFuncAdapter f =
  byteStringToStringFuncAdapter (\bs -> TE.encodeUtf8 (f (TE.decodeUtf8 bs)))

initJs :: IO (Ptr JSRuntime, Ptr JSContext)
initJs = do
  rt <- jsNewRuntime
  jsStdInitHandlers rt
  ctx <- jsNewCustomContext rt
  jsStdAddHelpers ctx (-1) nullPtr
  jsEvalYomiBytecode ctx
  registerFunctions ctx
  pure (rt, ctx)

freeJs :: Ptr JSRuntime -> Ptr JSContext -> IO ()
freeJs rt ctx = do
  jsStdFreeHandlers rt
  jsFreeContext ctx
  jsFreeRuntime rt
