BUILD_DIR=_build


.PHONY : externals
externals :
	./ext-build.sh

.PHONY : dev-ext
dev-ext : clobber externals

.PHONY : clean
clean :
	-rm -rf *~

.PHONY : clobber
clobber : clean
	-rm -rf ${BUILD_DIR}


