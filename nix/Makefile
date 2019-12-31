req.nix: version ?= 3.0.0
req.nix:
	@ cabal2nix \
		https://github.com/mrkkrp/req.git \
		--maintainer yurrriq \
		 --revision ${version} \
		>$@
