all:
	git archive --format=tar version-`cat quicklisp/version.txt` quicklisp/ > quicklisp.tar
	gzip -c quicklisp.tar > quicklisp.tar.gz

clean:
	rm -f quicklisp.tar quicklisp.tar.gz

tag:
	test -z "`git status -s quicklisp/ asdf.lisp setup.lisp`"
	git tag version-`cat quicklisp/version.txt`
