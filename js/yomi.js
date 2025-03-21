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

class NodeBuilder {
  constructor(name) {
    this.name = name;
    this.queried = new Map();
    this.dataset = {};
    this.textContent = null;
    this.children = [];
    this.nodeType = name == null ? 3 : 1;
  }
  appendChild(node) {
    this.children.push(node);
  }
  querySelector(selector) {
    // Check if the selector already exists in the map
    if (this.queried.has(selector)) {
      return this.queried.get(selector);
    }

    // If not, create a new NodeBuilder and store it
    var selected = new NodeBuilder(selector);
    this.queried.set(selector, selected);
    return selected;
  }
  toObject() {
    var result = {};
    result.name = this.name;
    result.dataset = this.dataset;
    result.textContent = this.textContent;
    result.children = this.children.map((n) => n.toObject());
    result.queried = [];
    for (const [k, v] of this.queried) {
      result.queried.push({ selector: k, selected: v.toObject() });
    }
    return result;
  }
}

class HtmlTemplateShim {
  constructor() {}
  instantiate(name) {
    return new NodeBuilder("template:" + name);
  }
  loadFromFiles(files) {}
}

const displayGenerator = new DisplayGenerator(null, null);
displayGenerator._templates = new HtmlTemplateShim();
displayGenerator.prepare();
