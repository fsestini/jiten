# jiten

CLI tool to import and query Japanese dictionaries in Yomichan format.

This is work in progress and lacks most features.

## Build

```sh
make quickjs
make extract-yomitan
make build
```

## Import dictionaries

```sh
cabal run jiten -- import /path/to/yomichan/dictionary.zip
```

Tested with:

- [x] jitendex
- [ ] JMnedict
- [x] BCCWJ_SUW_LUW_combined (frequencies)
- [x] アクセント辞典v2 (pitch accents)
- [ ] KANJIDIC_english

## Run

Run `jiten` as a web server:

```sh
cabal run jiten -- serve
```

The search page will be available at `http://localhost:3000/search`.

## Credits

- The [Yomitan](https://github.com/yomidevs/yomitan/) project.
- [QuickJS](https://github.com/bellard/quickjs).

## TODO

General:

- [x] Dictionary import
- [x] CLI search commands
- [x] Web UI search interface
- [ ] Extensive testing on full-size, comminly used Yomichan dictionaries
- [ ] CLI progress bars during import, etc.
- [ ] User profiles
- [ ] REST API
- [ ] Benchmarks

DB:

- [x] Term bulk search
- [x] Term meta bulk search
- [ ] Kanji bulk search
- [ ] Media search
