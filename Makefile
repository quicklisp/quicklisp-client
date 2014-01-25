all:
	git archive --format=tar --prefix=quicklisp/ version-`cat version.txt` > quicklisp.tar
	gzip -c quicklisp.tar > quicklisp.tar.gz

