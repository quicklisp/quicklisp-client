all:
	git archive --format=tar version-`cat quicklisp/version.txt` quicklisp/ > quicklisp.tar
	gzip -c quicklisp.tar > quicklisp.tar.gz

clean:
	rm -f quicklisp.tar quicklisp.tar.gz

tag:
	git tag version-`cat quicklisp/version.txt`


