import { Translator } from "./yomitan/js/language/translator.js";
import { DisplayGenerator } from "./yomitan/js/display/display-generator.js";

class DictionaryDatabase {
  findTermsBulk(termList, dictionaries, matchType) {
    // TODO: use other arguments
    const resultJson = _findTermsBulk(JSON.stringify(termList));
    const result = JSON.parse(resultJson);
    return result;
  }

  findTermMetaBulk(termList, dictionaries) {
    // TODO: use other arguments
    const resultJson = _findTermMetaBulk(JSON.stringify(termList));
    const result = JSON.parse(resultJson);
    return result;
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
      node.href = href.replace(/^http:\/\/localhost/, ".");
    } else {
      node.href = href;
    }
  }
}

const displayGenerator = new DisplayGenerator(new ContentManager(), null);
displayGenerator._templates = new HtmlTemplateShim();
displayGenerator.prepare();
