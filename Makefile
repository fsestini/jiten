.PHONY: extract-yomitan quickjs

src/Jiten/Yomichan/SearchPageTemplate.hs: vendor/yomitan/ext/search.html
	blaze-from-html -e -s vendor/yomitan/ext/search.html \
		| sed 's/vendor\/yomitan\/ext\/search :: Html/instantiate :: Html -> Html/g' \
		| sed 's/vendor\/yomitan\/ext\/search = do/instantiate results = do/g' \
		| sed '1a\module Jiten.Yomichan.SearchPageTemplate where' \
		| sed 's/"dictionary-entries" $$ mempty/"dictionary-entries" $$ results/g' \
		| sed 's/! hidden ""//g' \
		| sed 's/Yomitan Search/Jiten Search/g' \
		| sed '5,6d' \
		| sed '31d' \
		| sed '59,62d' \
		| head -n 61 \
		> src/Jiten/Yomichan/SearchPageTemplate.hs

extract-yomitan:
	git submodule update --init
	cd ./vendor/yomitan && npm ci && npm run build chrome-dev
	rm -rf ./js/yomitan
	unzip ./vendor/yomitan/builds/yomitan-chrome-dev.zip -d ./js/yomitan

quickjs:
	cd vendor/quickjs && make all

dist/yomi_bundled.js: js/yomi.js
	mkdir -p dist
	esbuild js/yomi.js --bundle --outfile=dist/yomi_bundled.js

dist/yomi_stripped.js: dist/yomi_bundled.js
	tail -n +2 dist/yomi_bundled.js | head -n -1 | sed 's/await //g' | sed 's/async //g' | sed 's/new Intl\.Collator(\([^)]*\))/Collator(\1)/g' > dist/yomi_stripped.js

dist/yomi_patched.js: js/shims.js js/patch.js dist/yomi_stripped.js
	cat js/shims.js dist/yomi_stripped.js js/patch.js > dist/yomi_patched.js

dist/yomi_compiled.c: vendor/quickjs/qjsc dist/yomi_patched.js
	./vendor/quickjs/qjsc -c -o dist/yomi_compiled.c dist/yomi_patched.js

build: src/Jiten/Yomichan/SearchPageTemplate.hs dist/yomi_compiled.c
	cabal build

test: build
	cabal test

update-golden:
	find .golden -name "*.actual" -type f -exec sh -c 'mv "$$1" "$${1%.actual}.expected"' _ {} \;

build-profile: dist/yomi_compiled.c
	cabal build --enable-profiling --profiling-detail=late exe:jiten

test/valid-dictionary1.zip:
	cd vendor/yomitan/test/data/dictionaries/valid-dictionary1 && zip -r valid-dictionary1.zip *
	mv vendor/yomitan/test/data/dictionaries/valid-dictionary1/valid-dictionary1.zip test/

clean:
	rm -r dist
	rm -f js/.yomitan-stamp
	cabal clean

download-test-dicts:
	wget -P ./test https://github.com/stephenmk/stephenmk.github.io/releases/latest/download/jitendex-yomitan.zip
