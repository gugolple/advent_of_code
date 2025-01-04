PODMAN_DESTINATION_PATH = /mnt
PODMAN_CONTAINER = rust:1.75.0
PODMAN_CMD = podman run -v ./srcs:/mnt:Z ${PODMAN_CONTAINER}

SOURCE_DIRS = $(wildcard ./p*)
SOURCES = $(wildcard *.c)

ifndef RELATIVE_PATH
.PHONY: all
%:
	for I in ${SOURCE_DIRS} ; do \
		cd $${I} ;\
		make RELATIVE_PATH=$${I} -f ../Makefile $@ ;\
		cd .. ;\
	done

else
all: check test
check:
	podman run -v .:${PODMAN_DESTINATION_PATH}:Z ${PODMAN_CONTAINER} bash -c "cd ${PODMAN_DESTINATION_PATH} ; cargo check"

test:
	podman run -v .:${PODMAN_DESTINATION_PATH}:Z ${PODMAN_CONTAINER} bash -c "cd ${PODMAN_DESTINATION_PATH} ; cargo test"

clean:
	rm -rf target/
endif
