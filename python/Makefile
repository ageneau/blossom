PYTHON=python3

PATCH=mwmatching-patch
IN=mwmatching.py
OUT=mwmatching-patched.py

all: patch gen-unit-tests

patch:
	patch $(IN) --strip=2 --input=$(PATCH) --output=$(OUT)

gen-unit-tests:
	$(PYTHON) mwmatching-patched.py

clean:
	rm -f $(OUT)
