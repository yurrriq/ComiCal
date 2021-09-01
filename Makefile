emacs.el: src/emacs.nw
	notangle -R$@ $< | cpif $@
