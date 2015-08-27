

# Assume we're on some Linux variant by default
PLATFORM:=Linux
# but try to detect other situations of interest
ifeq ($(OS),Windows_NT)
  $(error Windows unsupported)
else
  UNAME:=$(shell uname -s)
  ifeq ($(UNAME),Darwin)
    PLATFORM:=OSX
  endif
endif

$(warning Detected platform as $(PLATFORM))

# Try to find Terra
FIND_TERRA_BIN:=$(shell which terra)
TERRA_ROOT:=$(patsubst %/bin/terra,%,$(FIND_TERRA_BIN))
ifeq ($(FIND_TERRA_BIN),)
  $(error Could not find 'terra' executable on your PATH)
endif

$(warning Detected Terra directory location as $(TERRA_ROOT))


# Tools to use
PLEXL:=../bin/plexl


# Setup the flags and options
CFLAGS:=
# add include directory
CFLAGS:=$(CFLAGS) -I$(TERRA_ROOT)/include/
# add library directory and flags
CFLAGS:=$(CFLAGS) -L$(TERRA_ROOT) -lterra
# add rpath
CFLAGS:=$(CFLAGS) -Wl,-rpath,$(TERRA_ROOT)

ifeq ($(PLATFORM),OSX)
  CFLAGS:=$(CFLAGS) -pagezero_size 10000 -image_base 100000000
endif

$(warning CFLAGS are $(CFLAGS))


all: embedded static_call

embedded: embedded_main.c
	$(CC) embedded_main.c -o embedded $(CFLAGS)

static_func.o static_func.h: static_build.t
	$(PLEXL) static_build.t

static_call: static_func.o static_func.h static_call_main.c 
	$(CC) static_call_main.c static_func.o -o static_call



clean:
	-@rm embedded
	-@rm static_call
	-@rm static_func.o
	-@rm static_func.h

