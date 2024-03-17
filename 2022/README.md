# Advent code of 2022 in Rust

This is me just solving the challenges in rust. A lot of fun!.

In this is also an experiment with the capabilities of utilities like
docker/podman for managing the compilation of code, that were quite
satisfactory.

# Execution

The **Makefile** has been setted up such that one can execute all the code within a
podman instance. This negates the requirement for setting up rust.

The following commands are currently accepted:
```
make check
make test
make clean
```

If one desires to change the number of parallelism only needs to set the ENV_VAR
**NPROC**, as in the example:
```
make NPROC=<Number of execution units> <command>
```


# Links!

[Advent of code 2022](https://adventofcode.com/2022)
