install:
	@cp bin/hocs-exe /usr/local/bin/hocs

uninstall:
	@echo "Uninstalling hocs..."
	@rm /usr/local/bin/hocs

.PHONY: install uninstall