
REPL_SERVER_PORT := 5555

clojure := clojure

.PHONY: clj-repl
clj-repl : ## Start a REPL server for Clojure
	@echo "Starting server on port $(REPL_SERVER_PORT)"
	@echo "Ctrl-c to quit."
	@$(clojure) -X:clj-repl-server :port $(REPL_SERVER_PORT)

.PHONY: cljs-node-repl
cljs-node-repl : ## Start a REPL server for ClojureScript (Node)
	@echo "Starting server on port $(REPL_SERVER_PORT)"
	@echo "Ctrl-c to quit."
	@$(clojure) -X:cljs-node-repl-server :port $(REPL_SERVER_PORT)

.PHONY: cljs-browser-repl
cljs-browser-repl : ## Start a REPL server for ClojureScript (browser)
	@echo "Starting server on port $(REPL_SERVER_PORT)"
	@echo "Ctrl-c to quit."
	@$(clojure) -X:cljs-browser-repl-server :port $(REPL_SERVER_PORT)

.PHONY: test-clj
test-clj : ## Run Clojure tests
	$(clojure) -M:clj-runner:test

.PHONY: test-cljs
test-cljs : ## Run ClojureScript tests
	$(clojure) -M:cljs-runner:test

.PHONY: test
test : test-clj test-cljs ## Run both Clojure and ClojureScript tests

pom.xml : deps.edn ## Generate pom.xml
	clj -Spom

.PHONY: display-dependency-updates
display-dependency-updates : pom.xml ## Report on stale dependencies
	mvn versions:display-dependency-updates

.PHONY: coverage
coverage : ## Report on unit test coverage (uses cloverage)
	$(clojure) -M:test:coverage

.PHONY: eastwood
eastwood : ## Run the Eastwood linter
	$(clojure) -M:test:eastwood

.PHONY: clean
clean : ## Remove temporary files
	rm -rf .cljs-test-runner-out
	rm -rf .cljs_node_repl
	rm -rf .cpcache
	rm -rf target
	rm -rf out
	rm -f pom.xml
	rm -rf .eastwood

.DEFAULT_GOAL := help

help:
	@grep -E '^[a-zA-Z_-]+ ?:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
