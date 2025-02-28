js/yomitan:
	cd ./vendor/yomitan && npm ci && npm run build chrome-dev
	unzip ./vendor/yomitan/builds/yomitan-chrome-dev.zip -d ./js/yomitan

build-profile:
	cabal build --enable-profiling --profiling-detail=late exe:jiten

test/valid-dictionary1.zip:
	cd vendor/yomitan/test/data/dictionaries/valid-dictionary1 && zip -r valid-dictionary1.zip *
	mv vendor/yomitan/test/data/dictionaries/valid-dictionary1/valid-dictionary1.zip test/
