function Collator(locale, options) {
  return {
    compare: function (a, b) {
      const result = a.localeCompare(b, locale, options);
      if (result < 0) {
        return -1;
      } else if (result > 0) {
        return 1;
      } else {
        return 0;
      }
    },
  };
}

class Performance {
  constructor() {
    this.enabled = false;
  }

  mark(markName, markOptions) {
    if (this.enabled) {
      console.log(markName, markOptions);
    }
  }

  measure(measureName, startOrMeasureOptions, endMark) {
    if (this.enabled) {
      console.log(measureName, startOrMeasureOptions, endMark);
    }
  }

  now() {}
}

var performance = new Performance();

function mkOptions(dictionaries) {
  var edm = new Map();
  for (let i = 0; i < dictionaries.length; i++) {
    edm.set(dictionaries[i], {
      index: i,
      alias: "",
      allowSecondarySearches: false,
      partsOfSpeechFilter: false,
      useDeinflections: false,
    });
  }
  return {
    matchType: "exact",
    deinflect: true,
    removeNonJapaneseCharacters: true,
    textReplacements: [null],
    searchResolution: "letter",
    language: "ja",
    enabledDictionaryMap: edm,
    primaryReading: "",
    excludeDictionaryDefinitions: null,
  };
}

var Node = { ELEMENT_NODE: 1, TEXT_NODE: 3 };

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

function findTermsDOM(mode, text, options) {
  const results = translator.findTerms(mode, text, options);
  const entries = results.dictionaryEntries;
  const nodes = [];
  for (const entry of entries) {
    //TODO: pass dictionaryInfo
    const node = displayGenerator.createTermEntry(entry, []);
    nodes.push(node.toObject());
  }
  return nodes;
}
