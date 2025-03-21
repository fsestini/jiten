class NodeBuilder {
  constructor(name) {
    this.tag = name;
    this.queried = new Map();
    this.dataset = {};
    this.classList = new Set();
    this.className = null;
    this.textContent = null;
    this.children = [];
    this.nodeType = name == null ? 3 : 1;
    this.attributes = new Map();
    this.style = {};
    this.href = null;
    this.target = null;
    this.rel = null;
    this.lang = null;
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
  setAttribute(key, val) {
    this.attributes.set(key, val);
  }
  toObject() {
    var result = {};
    result.tag = this.tag;
    result.dataset = this.dataset;
    result.textContent = this.textContent;
    result.children = this.children.map((n) => n.toObject());
    result.queried = [];
    for (const [k, v] of this.queried) {
      result.queried.push({ selector: k, selected: v.toObject() });
    }
    result.classList = [];
    for (const c of this.classList) {
      result.classList.push(c);
    }
    if (this.className !== null) {
      const classNames = this.className.split(" ");
      for (const className of classNames) {
        if (className.trim() !== "") {
          result.classList.push(className.trim());
        }
      }
    }
    result.attributes = [];
    for (const [k, v] of this.attributes) {
      result.attributes.push({ attrKey: k, attrVal: v });
    }
    result.style = this.style;
    result.href = this.href;
    result.target = this.target;
    result.ref = this.rel;
    result.lang = this.lang;
    return result;
  }
}

class DocumentShim {
  constructor() {}
  createElement(tag) {
    return new NodeBuilder(tag);
  }
  createTextNode(text) {
    var n = new NodeBuilder(null);
    n.textContent = text;
    return n;
  }
}

var document = new DocumentShim();
