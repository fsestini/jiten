.PHONY: extract-yomitan quickjs

extract-yomitan:
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

dist/yomi_patched.js: js/patch.js dist/yomi_stripped.js
	cat dist/yomi_stripped.js js/patch.js > dist/yomi_patched.js

dist/yomi_compiled.c: vendor/quickjs/qjsc dist/yomi_patched.js
	./vendor/quickjs/qjsc -c -o dist/yomi_compiled.c dist/yomi_patched.js

build: dist/yomi_compiled.c
	cabal build

build-profile: dist/yomi_compiled.c
	cabal build --enable-profiling --profiling-detail=late exe:jiten

test/valid-dictionary1.zip:
	cd vendor/yomitan/test/data/dictionaries/valid-dictionary1 && zip -r valid-dictionary1.zip *
	mv vendor/yomitan/test/data/dictionaries/valid-dictionary1/valid-dictionary1.zip test/

clean:
	rm -r dist
	rm -f js/.yomitan-stamp
	cabal clean
