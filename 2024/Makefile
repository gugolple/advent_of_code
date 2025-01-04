build_image: .build_image
IMAGE_NAME=haskell-container

.build_image:
	podman build -f Containerfile -t ${IMAGE_NAME}
	touch .build_image

run_interactive: .build_image
	podman run -it --rm ${IMAGE_NAME}
