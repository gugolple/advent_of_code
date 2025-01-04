# Advent code of 2022

This is my small repo just solving the challenges in rust. A lot of fun!.

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
