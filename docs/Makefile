# Set default values if not specified
PREFIX ?= /usr
infodir ?= $(PREFIX)/share/info

.PHONY: docs install-docs

# Build the info documentation
docs: bank-buddy.info

bank-buddy.info: bank-buddy.texi
	makeinfo --no-split $< -o $@

# Install the info documentation
install-docs: docs
	@echo "Installing info documentation..."
	install -d $(DESTDIR)$(infodir)
	install -m 644 bank-buddy.info $(DESTDIR)$(infodir)/
	install-info --info-dir=$(DESTDIR)$(infodir) bank-buddy.info
