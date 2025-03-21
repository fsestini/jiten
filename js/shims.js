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
