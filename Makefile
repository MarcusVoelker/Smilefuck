all :
	ghc -outputdir temp Main.hs -o Smilefuck

clean:
	rm Smilefuck && rm -rf temp
