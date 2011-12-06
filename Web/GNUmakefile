PACKAGE=RWordPress

include ../../AdminTools/GNUmakefile

TARGET_DIR=www.omegahat.org:OmegaWeb/$(PACKAGE)


ship:
	scp $(SVG_FILES:%=../tests/%.svg) examples.html $(TARGET_DIR)/tests
	scp ../inst/CSS/RSVGPlot.css $(TARGET_DIR)

update:
	scp examples.html $(TARGET_DIR)/tests



