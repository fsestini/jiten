import { Translator } from './yomitan/js/language/translator.js'

class DictionaryDatabase {
  findTermsBulk(termList, dictionaries, matchType) {
    const args = { termList: termList, dictionaries: dictionaries, matchType: matchType }
    console.log('findTermsBulk.args: ', args)
    const argsJson = JSON.stringify(args)
    const resultJson = _findTermsBulk(argsJson)
    const result = JSON.parse(resultJson)
    console.log('findTermsBulk.result: ', result)
    return result
  }

  findTermMetaBulk(termList, dictionaries) {
    const args = { termList: termList, dictionaries: dictionaries }
    const argsJson = JSON.stringify(args)
    const resultJson = _findTermMetaBulk(argsJson)
    const result = JSON.parse(resultJson)
    return result
  }
}

var db = new DictionaryDatabase()
var translator = new Translator(db);
translator.prepare();
