ALL	= 

objs	= 

.SUFFIXES:	.so .o .c .f

#.o.so:
#	${LD} ${LFLAGS} -o $@ $< ${LINK_LIB}

all: ${ALL}


.PHONY: clean test doc
clean:
	$(RM) bin/*~

## You may need RUBYLIB=`pwd`/lib:$RUBYLIB
test:
	rake test

doc:
	yard doc; [[ -x ".github" && ( "README.ja.rdoc" -nt ".github/README.md" ) ]] && ( ruby -r rdoc -e 'puts RDoc::Markup::ToMarkdown.new.convert ARGF.read' < README.ja.rdoc > .github/README.md ; echo ".github/README.md is updated." ) || exit 0

