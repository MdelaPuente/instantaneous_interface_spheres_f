# Tests

You should run both tests manually after compilation of the `is_spheres` executable.

## For moving spheres:

Sometimes the center of mass of the droplet or cavity might drift (intentionally or not) during the simulation. This can be taken into account in the code by providing a trajectory of the center of mass of the system in xyz format (1 atom and same number of frames as the trajectory file). We provide two `.tcl` scripts that can be used to generate such files for both xyz and dcd trajectories (you can of course generate this in any other way as long as the end product is an .xyz trajectory file).

**VMD needs to be installed and in the PATH for the following commands to work!**

Go to `tests/test_xyz/` and run:
```sh
vmd -e ../../tools/extract_comOW_to_xyz-xyztraj.tcl -dispdev text
```
which will produce a `COM_OW.xyz` output file with the center of mass of water oxygens in every frame.

Then go to `tests/test_dcd/` and run:
```sh
vmd -e ../../tools/extract_comOW_to_xyz-dcdtraj.tcl -dispdev text
```
which will produce the same `COM_OW.xyz` output file in the `tests/test_dcd/` folder. These files are required for the tests to work with the input files provided 

Of course, in many simulations the center of mass of the spheres might be fixed, which makes all of this unnecessary. In such cases you can provide the x, y and z coordinates of the center of mass directly in the input file and skip the `file_com` keyword (see `src/input.f90` for details on all keywords and default values).

## Trajectory in dcd format:

Go to `tests/test_dcd` and run:
```sh
../../bin/is_sphere is_drop_dcd.in out.log
```
After about 1 min the code should produce a `out.log` file detailing the program execution and a `test_dcd-surface.xyz` file containing the instantaneous interface for the system of the `test_dcd.dcd` trajectory. Reference outputs are provided in the `refs/` folder for comparison.

Let's see the input file `is_drop_dcd.in`:
   ```sh
! This is a comment line
systype CAVITY
file_coord test_dcd.dcd
n_atoms 30000
n_frames 3
first_O_atom 2001
last_O_atom -1
xlo 0.0
xhi 61.37
ylo 0.0
yhi 61.37
zlo 0.0
zhi 61.37
R0 20.0
file_com COM_OW.xyz
   ```
This input will read the **3** first frames of a **test_dcd.dcd** file (there **must** be a **test_dcd.pdb** file in the same folder!) with **30000** atoms and will compute the instantaneous surface of a water `CAVITY` (zero density at the COM !).\
The box dimensions are set to **[61.37, 61.37, 61.37]** Å. The water oxygens are all atoms named O or OW (you can change the behavior and default names in the `read_dcd.f90` and `read_xyz.f90` files!) starting from index **2001** to the end (**30000**). The expected radius `R0` for the cavity is 20 Å, **IMPORTANT:** the spacing between grid points will depend on this parameter (and/or the `d_grid` parameter) so you must make sure that the provied value makes sense for your system (too small will lead to a very coarse and inaccurate interface, too big will massively slowdown the program). The center of mass of the spherical lattice is indicated in the `COM_OW.xyz` file (see previous section)

## Trajectory in xyz format:

Go to `tests/test_xyz` and run:
```sh
../../bin/is_sphere is_drop_xyz.in out.log
```
The output should be exactly the same as in the dcd case. No need for a pdb file if the trajectory is in xyz.
