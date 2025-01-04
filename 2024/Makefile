# Configs and paths
build_image: .build_image
IMAGE_NAME=haskell-container
IMAGE_FILE=.Containerfile
CONTAINER_MOUNT_PATH=/src
BUILD_SCRIPT=.compile.sh

PODMAN_CMD=podman run -v.:${CONTAINER_MOUNT_PATH} --rm -e SRC_DIR=${CONTAINER_MOUNT_PATH}

# Execution variables
TARGETS := $(patsubst %.hs,%,$(wildcard p*/*.hs))

.build_image:
	podman build -f ${IMAGE_FILE} -t ${IMAGE_NAME}
	touch .build_image

run_interactive: .build_image
	 ${PODMAN_CMD} -it ${IMAGE_NAME}

%: %.hs .build_image
	${PODMAN_CMD} -e TGT=$< ${IMAGE_NAME} bash ${CONTAINER_MOUNT_PATH}/${BUILD_SCRIPT}

all: ${TARGETS}
