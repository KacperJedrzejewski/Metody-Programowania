_TESTS = $(wildcard tests/*)
TESTS = $(notdir $(_TESTS))
PG = swipl -q

test: $(TESTS)

test-human: $(filter h-%, $(TESTS))
test-generator: $(filter g-%, $(TESTS))
	
%.test:
	@echo
	@echo "Test suite: $@"
	@echo "------------------------------------------"
	@$(PG) -s test.pl -t test_all tests/$@

.PHONY: test
