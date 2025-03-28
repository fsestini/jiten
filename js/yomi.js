import { Translator } from "./yomitan/js/language/translator.js";
import { DisplayGenerator } from "./yomitan/js/display/display-generator.js";

class DictionaryDatabase {
  findTermsBulk(termList, enabledDictionaryMap, matchType) {
    const dictionaries = Array.from(enabledDictionaryMap.keys());
    const query = {
      terms: termList,
      dictionaries: dictionaries,
      matchType: matchType,
    };
    const resultJson = _findTermsBulk(JSON.stringify(query));
    const result = JSON.parse(resultJson);
    return result;
  }

  findTermMetaBulk(termList, enabledDictionaries) {
    let dictionaries;
    if (enabledDictionaries instanceof Map) {
      dictionaries = Array.from(enabledDictionaries.keys());
    } else {
      dictionaries = Array.from(enabledDictionaries);
    }
    const query = {
      terms: termList,
      dictionaries: dictionaries,
    };
    const resultJson = _findTermMetaBulk(JSON.stringify(query));
    const result = JSON.parse(resultJson);
    return result;
  }

  findTagMetaBulk(query) {
    return _findTagMetaBulk(JSON.stringify(query));
  }
}

var db = new DictionaryDatabase();
var translator = new Translator(db);
translator.prepare();

class HtmlTemplateShim {
  constructor() {}
  instantiate(name) {
    return new NodeBuilder("template:" + name);
  }
  instantiateFragment(name) {
    return new NodeBuilder("template-fragment:" + name);
  }
  loadFromFiles(files) {}
}

class ContentManager {
  constructor() {}
  prepareLink(node, href, internal) {
    if (internal) {
      node.href = href
        .replace(/^http:\/\/localhost/, ".")
        .replace(/search\.html/, "search");
    } else {
      node.href = href;
    }
  }
}

const displayGenerator = new DisplayGenerator(new ContentManager(), null);
displayGenerator._templates = new HtmlTemplateShim();
displayGenerator.prepare();
