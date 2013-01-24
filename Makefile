all:
	runhaskell Main.hs
	cd util && ./makePlots.sh
