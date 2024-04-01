all: stack-install
	@sudo make install
	@echo "hocs installed successfully"

stack-install:
	@stack install

install:
	@cp bin/hocs-exe /usr/local/bin/hocs

uninstall:
	@echo "Uninstalling hocs..."
	@rm /usr/local/bin/hocs

.PHONY: install uninstall  stack-install all