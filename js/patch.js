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

class Performance {
  mark(markName, markOptions) {
    console.log(markName, markOptions)
  }

  measure(measureName, startOrMeasureOptions, endMark) {
    console.log(measureName, startOrMeasureOptions, endMark)
  }

  now() { }
}

var performance = new Performance()

function mkOptions(dictionaries) {
  var edm = new Map()
  for (let i = 0; i < dictionaries.length; i++) {
    edm.set(dictionaries[i], {
      index: i,
      alias: '',
      allowSecondarySearches: false,
      partsOfSpeechFilter: false,
      useDeinflections: false,
    })
  }
  return {
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
}
