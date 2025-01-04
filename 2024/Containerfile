FROM debian:12
RUN apt update && apt install -y build-essential curl libffi-dev libffi8 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5
RUN adduser --disabled-password ghc
USER ghc
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
RUN echo "source /home/ghc/.ghcup/env" >> ~/.bashrc
