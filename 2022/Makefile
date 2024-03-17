PODMAN_DESTINATION_PATH = /mnt
PODMAN_CONTAINER = rust:1.75.0
PODMAN_CMD = podman run -v ./srcs:/mnt:Z ${PODMAN_CONTAINER}

SOURCE_DIRS = $(wildcard ./p*)
SOURCES = $(wildcard *.c)

ifndef NPROC
NPROC = $(shell nproc --all)
endif

ifndef RELATIVE_PATH
.PHONY: all
%:
	#echo ${NRPOC} # Show number of proccessors for the exeuction
	printf "%s\0" ${SOURCE_DIRS} | xargs -P${NPROC} -0 -I{} bash -c "echo {} ; cd {} ; make RELATIVE_PATH={} -f ../Makefile $@ >result.txt 2>&1"

else
all: check test
check:
	podman run -v .:${PODMAN_DESTINATION_PATH}:Z ${PODMAN_CONTAINER} bash -c "cd ${PODMAN_DESTINATION_PATH} ; cargo check"

test:
	podman run -v .:${PODMAN_DESTINATION_PATH}:Z ${PODMAN_CONTAINER} bash -c "cd ${PODMAN_DESTINATION_PATH} ; cargo test"

clean:
	rm -rf target/
endif
