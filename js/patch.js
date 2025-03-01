function Collator(locale, options) {
  return {
    compare: function(a, b) {
      const result = a.localeCompare(b, locale, options)
      if (result < 0) {
        return -1
      } else if (result > 0) {
        return 1
      } else {
        return 0
      }
    }
  };
}

var edm = new Map()

var options = {
  matchType: 'exact',
  deinflect: true,
  removeNonJapaneseCharacters: true,
  textReplacements: [null],
  searchResolution: 'letter',
  language: 'ja',
  enabledDictionaryMap: edm,
  primaryReading: '',
  excludeDictionaryDefinitions: null,
}

function setDictionary(dict) {
  edm.set(dict, {
    index: edm.size,
    alias: '',
    allowSecondarySearches: false,
    partsOfSpeechFilter: false,
    useDeinflections: false,
  })
}
