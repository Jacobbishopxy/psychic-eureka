# author:	Jacob Xie
# date:	2024/07/15 21:41:01 Monday
# brief:

# notice:
# `implicit-hie` causes problem on not giving full path on test-suite,
# so manually modify `hie.yaml` is required, for example: `- path: "test/UserServer.hs"`.
# Otherwise, test-suite cannot recognize lib `psychic-eureka`.
hie:
	gen-hie > hie.yaml

bounds:
	cabal gen-bounds > cabal.lock

freeze:
	cabal freeze

run:
	cabal run psychic-eureka

###################################################################################################
# dev

clean:
	cabal run test-clean
