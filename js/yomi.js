import { Translator } from './yomitan/js/language/translator.js'

class DictionaryDatabase {
  findTermsBulk(termList, dictionaries, matchType) {
    // TODO: use other arguments
    const resultJson = _findTermsBulk(JSON.stringify(termList))
    const result = JSON.parse(resultJson)
    return result
  }

  findTermMetaBulk(termList, dictionaries) {
    // TODO: use other arguments
    const resultJson = _findTermMetaBulk(JSON.stringify(termList))
    const result = JSON.parse(resultJson)
    return result
  }
}

var db = new DictionaryDatabase()
var translator = new Translator(db);
translator.prepare();
