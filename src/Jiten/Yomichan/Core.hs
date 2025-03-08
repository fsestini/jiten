{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE RecordWildCards #-}

module Jiten.Yomichan.Core where

import Control.Exception (bracket)
import qualified Control.Monad as Monad
import qualified Data.Aeson as A
import Data.ByteString (ByteString, packCString)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Text (Text, unpack)
import qualified Data.Text.Encoding as TE
import Database.SQLite.Simple (Connection)
import Foreign (Ptr, freeHaskellFunPtr)
import Foreign.C (CString, newCString, peekCString, withCString)
import Foreign.C.Types (CSize (..))
import Foreign.Ptr (FunPtr, nullPtr)
import qualified Jiten.Database as Db

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
  jsEvalStr_ :: Ptr JSContext -> CString -> IO CString

foreign import ccall "js_eval_yomi_bytecode"
  jsEvalYomiBytecode :: Ptr JSContext -> IO ()

foreign import ccall "JS_FreeCString"
  js_free_cstring :: Ptr JSContext -> CString -> IO ()

jsEvalStr :: Ptr JSContext -> String -> IO Text
jsEvalStr ctx str =
  withCString str $ \cstr -> do
    result <- jsEvalStr_ ctx cstr
    bs <- packCString result
    js_free_cstring ctx result
    pure (TE.decodeUtf8 bs)

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

byteStringToStringFuncAdapter :: (ByteString -> IO ByteString) -> StringToStringFunc
byteStringToStringFuncAdapter f cstr = do
  bs <- packCString cstr
  result <- f bs
  copyByteString result

textToStringFuncAdapter :: (Text -> IO Text) -> StringToStringFunc
textToStringFuncAdapter f =
  byteStringToStringFuncAdapter (\bs -> TE.encodeUtf8 <$> f (TE.decodeUtf8 bs))

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

withYomitan :: Connection -> IO [Db.DictionaryId] -> (Ptr JSContext -> IO a) -> IO a
withYomitan conn getEnabledDicts h =
  bracket initAll freeAll (\(_, ctx, _) -> h ctx)
  where
    mkFunPtr :: ([Text] -> [Db.DictionaryId] -> IO [a]) -> (a -> Text) -> IO (FunPtr StringToStringFunc)
    mkFunPtr f g =
      let callback txt =
            case A.decodeStrictText txt of
              Nothing -> fail ("malformed JSON list: " <> unpack txt)
              Just lst -> do
                enabledDicts <- getEnabledDicts
                results <- map g <$> f lst enabledDicts
                pure $ case results of
                  [] -> "[]"
                  rs -> "[" <> foldr1 (\x y -> x <> "," <> y) rs <> "]"
       in mkStringToStringFunc (textToStringFuncAdapter callback)
    initAll = do
      (rt, ctx) <- initJs
      findTermsPtr <- mkFunPtr (Db.findTermsBulk conn) Db.termResultToJSON
      findTermMetasPtr <- mkFunPtr (Db.findTermMetaBulk conn) Db.termMetaResultToJSON
      placeholderPtr <- mkFunPtr (\_ _ -> pure []) (const "")
      setStringProcessors
        findTermsPtr
        findTermMetasPtr
        placeholderPtr
        placeholderPtr
        placeholderPtr
        placeholderPtr
      pure (rt, ctx, [findTermsPtr, findTermMetasPtr, placeholderPtr])
    freeAll (rt, ctx, ptrs) = do
      freeJs rt ctx
      Monad.forM_ ptrs freeHaskellFunPtr
