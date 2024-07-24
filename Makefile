# author:	Jacob Xie
# date:	2024/07/15 21:41:01 Monday
# brief:

gen-hie:
	gen-hie > hie.yaml

bounds:
	cabal gen-bounds > cabal.lock

freeze:
	cabal freeze

run:
	cabal run psychic-eureka
