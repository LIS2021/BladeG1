# Blade and virtual machine implementation
## Group 1

### Compiling
```
make do_blade.native run_vm.native run_interp.native
```

### Repairing programs:
```
./do_blade.native < file
```

Options:
 - `-c`: Selects cost model. Can be `uniform` (default), `linear` or `exponential`
 - `-f`: Makes fences more expensive than SLHs by a constant factor
 - `--depth-factor`: Specifies the depth cost factor for the edges in case of linear or exponential cost model (default 2)
 - `--fence-factor`: Specifies the additional cost factor for fences if enabled (default 2)

### Running programs:
```
./run_vm.native < file
```

Options:
 - `-t`: Speculator type. Can be `perfect`, `worst` or `probabilistic` (default: `perfect`)
 - `--hit-rate`: Probabilistic speculator hit rate

### Repairing and running programs:

```
(./do_blade.native | ./run_vm.native) < file
```