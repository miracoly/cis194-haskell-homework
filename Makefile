.PHONY: week1

week1:
	cabal test week1 --test-show-details=direct --test-option=--format=progress
week2:
	cabal test week2 --test-show-details=direct --test-option=--format=progress
