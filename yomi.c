#include "string.h"
#include "vendor/quickjs/quickjs.h"
#include "vendor/quickjs/quickjs-libc.h"

extern const uint8_t qjsc_yomi_patched[];
extern const uint32_t qjsc_yomi_patched_size;

typedef char* (*StringToStringFunc)(const char*);

StringToStringFunc g_find_terms_bulk = NULL;
StringToStringFunc g_find_term_meta_bulk = NULL;
StringToStringFunc g_find_kanji_bulk = NULL;
StringToStringFunc g_find_kanji_meta_bulk = NULL;
StringToStringFunc g_find_tag_meta_bulk = NULL;
StringToStringFunc g_get_media = NULL;

void set_string_processors(
    StringToStringFunc find_terms_bulk,
    StringToStringFunc find_term_meta_bulk,
    StringToStringFunc find_kanji_bulk,
    StringToStringFunc find_kanji_meta_bulk,
    StringToStringFunc find_tag_meta_bulk,
    StringToStringFunc get_media
) {
    g_find_terms_bulk = find_terms_bulk;
    g_find_term_meta_bulk = find_term_meta_bulk;
    g_find_kanji_bulk = find_kanji_bulk;
    g_find_kanji_meta_bulk = find_kanji_meta_bulk;
    g_find_tag_meta_bulk = find_tag_meta_bulk;
    g_get_media = get_media;
}

#define DEFINE_JS_STRING_PROCESSOR(func_name, global_processor) \
JSValue func_name(JSContext *ctx, JSValueConst this_val, \
                  int argc, JSValueConst *argv) { \
    if (argc != 1) \
        return JS_EXCEPTION; \
    const char *input_str; \
    size_t len; \
    input_str = JS_ToCStringLen(ctx, &len, argv[0]); \
    if (!input_str) \
        return JS_EXCEPTION; \
    if (global_processor != NULL) { \
        char *result = global_processor(input_str); \
        JS_FreeCString(ctx, input_str); \
        JSValue js_result = JS_NewString(ctx, result); \
        free(result); \
        return js_result; \
    } else { \
        JS_FreeCString(ctx, input_str); \
        return JS_UNDEFINED; \
    } \
}

DEFINE_JS_STRING_PROCESSOR(find_terms_bulk, g_find_terms_bulk)
DEFINE_JS_STRING_PROCESSOR(find_term_meta_bulk, g_find_term_meta_bulk)
DEFINE_JS_STRING_PROCESSOR(find_kanji_bulk, g_find_kanji_bulk)
DEFINE_JS_STRING_PROCESSOR(find_kanji_meta_bulk, g_find_kanji_meta_bulk)
DEFINE_JS_STRING_PROCESSOR(find_tag_meta_bulk, g_find_tag_meta_bulk)
DEFINE_JS_STRING_PROCESSOR(get_media, g_get_media)

void register_functions(JSContext *ctx) {
    JSValue global_obj = JS_GetGlobalObject(ctx);

    JSValue find_terms_func = JS_NewCFunction(ctx, find_terms_bulk, "_findTermsBulk", 1);
    JS_SetPropertyStr(ctx, global_obj, "_findTermsBulk", find_terms_func);
    JSValue find_term_meta_func = JS_NewCFunction(ctx, find_term_meta_bulk, "_findTermMetaBulk", 1);
    JS_SetPropertyStr(ctx, global_obj, "_findTermMetaBulk", find_term_meta_func);
    JSValue find_kanji_func = JS_NewCFunction(ctx, find_kanji_bulk, "_findKanjiBulk", 1);
    JS_SetPropertyStr(ctx, global_obj, "_findKanjiBulk", find_kanji_func);
    JSValue find_kanji_meta_func = JS_NewCFunction(ctx, find_kanji_meta_bulk, "_findKanjiMetaBulk", 1);
    JS_SetPropertyStr(ctx, global_obj, "_findKanjiMetaBulk", find_kanji_meta_func);
    JSValue find_tag_meta_func = JS_NewCFunction(ctx, find_tag_meta_bulk, "_findTagMetaBulk", 1);
    JS_SetPropertyStr(ctx, global_obj, "_findTagMetaBulk", find_tag_meta_func);
    JSValue get_media_func = JS_NewCFunction(ctx, get_media, "_getMedia", 1);
    JS_SetPropertyStr(ctx, global_obj, "_getMedia", get_media_func);

    JS_FreeValue(ctx, global_obj);
}

void js_eval_str(JSContext *ctx, const char *js_code) {
    JSValue val = JS_Eval(ctx, js_code, strlen(js_code), "<input>", JS_EVAL_TYPE_GLOBAL);
    JS_FreeValue(ctx, val);
}

void js_eval_yomi_bytecode(JSContext *ctx) {
  js_std_eval_binary(ctx, qjsc_yomi_patched, qjsc_yomi_patched_size, 0);
}

JSContext *JS_NewCustomContext(JSRuntime *rt)
{
  JSContext *ctx = JS_NewContextRaw(rt);
  if (!ctx)
    return NULL;
  JS_AddIntrinsicBaseObjects(ctx);
  JS_AddIntrinsicDate(ctx);
  JS_AddIntrinsicEval(ctx);
  JS_AddIntrinsicStringNormalize(ctx);
  JS_AddIntrinsicRegExp(ctx);
  JS_AddIntrinsicJSON(ctx);
  JS_AddIntrinsicProxy(ctx);
  JS_AddIntrinsicMapSet(ctx);
  JS_AddIntrinsicTypedArrays(ctx);
  JS_AddIntrinsicPromise(ctx);
  JS_AddIntrinsicBigInt(ctx);
  return ctx;
}
