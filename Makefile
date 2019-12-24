NAME=helios

-include Makefile.conf

STATIC_MAKE_ARGS = $(MAKE_ARGS) -XHELIOS_LIBRARY_TYPE=static
SHARED_MAKE_ARGS = $(MAKE_ARGS) -XHELIOS_LIBRARY_TYPE=relocatable
SHARED_MAKE_ARGS += -XUTILADA_BASE_BUILD=relocatable -XUTIL_LIBRARY_TYPE=relocatable
SHARED_MAKE_ARGS += -XXMLADA_BUILD=relocatable
SHARED_MAKE_ARGS += -XLIBRARY_TYPE=relocatable

include Makefile.defaults

# Build executables for all mains defined by the project.
build-test::	setup
	# $(GNATMAKE) $(GPRFLAGS) -p -P$(NAME)_tests $(MAKE_ARGS)

# Build and run the unit tests
test:	build
	# bin/helios_harness -xml helios-aunit.xml

SWAGGER=java -jar openapi-generator-cli.jar

generate-swagger:
	$(SWAGGER) generate --generator-name ada -i hyperion.yaml \
            --additional-properties projectName=helios \
            --model-package Helios.Rest -o .

$(eval $(call ada_library,$(NAME)))
