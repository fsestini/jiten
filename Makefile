build-profile:
	cabal build --enable-profiling --profiling-detail=late exe:jiten

test/valid-dictionary1.zip:
	cd vendor/yomitan/test/data/dictionaries/valid-dictionary1 && zip -r valid-dictionary1.zip *
	mv vendor/yomitan/test/data/dictionaries/valid-dictionary1/valid-dictionary1.zip test/
