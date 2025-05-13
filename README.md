<div id="top"></div>

<!-- PROJECT SHIELDS -->

[![GNU AGPL v3.0 License][license-shield]][license-url]

<!-- TABLE OF CONTENTS -->

<details>
  <summary>Table of Contents</summary>
  <ol>
    <li>
      <a href="#about">About The Project</a>
    </li>
    <li>
      <a href="#getting-started">Getting Started</a>
      <ul>
        <li><a href="#prerequisites">Prerequisites</a></li>
        <li><a href="#installation">Installation</a></li>
      </ul>
    </li>
    <li><a href="#usage">Usage</a></li>
    <li><a href="#license">License</a></li>
    <li><a href="#acknowledgments">Acknowledgments</a></li>
  </ol>
</details>

<!-- ABOUT THE PROJECT -->
<div id="about"></div>

## About The Project

A small Fortran program to calcultate Instantaneous Liquid Interfaces as proposed by [Willard and Chandler](https://doi.org/10.1021/jp909219k) in spherical geometries (water droplets or cavities). The code generates a quasi-uniformly paved spherical grid with a [Fibonacci lattice](https://arxiv.org/pdf/0912.4540), which should be accurate enough for most applications (provided that the spacing between grid points be small enough). It then looks for the half density iso-surface along the directions spanned by the Fibonacci sphere from a fixed or mobile position (referred to as the center of mass COM, although it does not need to correspond to a real COM).

<p align="right">(<a href="#top">back to top</a>)</p>

<!-- GETTING STARTED -->
<div id="getting-started"></div>

## Getting Started

<div id="prerequisites"></div>

### Prerequisites


**Compilation**
* gfortran (version with Fortran 2008 support)
* make
* Optional: VMD (text version is enough)

<div id="installation"></div>

### Installation

1. Clone the repository
   ```sh
   git clone https://github.com/MdelaPuente/instantaneous_interface_spheres_f.git
   ```
2. Or get a release and untar the archive (source code)
   ```sh
   tar -xzvf instantaneous_interface_spheres_f.tar.gz
   ```
3. Go into the folder and compile the program
   ```sh
   make is_spheres
   ```
   This should create a `bin/is_spheres` executable as well as a set of precompiled binaries in a `build/` directory.
4. Test the program for both xyz and dcd input trajectories (see `tests/README.md`)


<p align="right">(<a href="#top">back to top</a>)</p>

<!-- USAGE EXAMPLES -->
<div id="usage"></div>

## Usage

The `is_spheres` executable can be called directly (provided that `gfortran` remains available **in the same version used for compilation**)
   ```sh
   is_spheres input_file output_file
   ```
The first argument is the name of the input file (see below). The second argument is the name of the output file (default will be out.log if no second argument is provided).

If everything works, the program produces the instantaneous water surface for every frame of the trajectory in xyz format (the file will be named as the trajectory file with `-surface.xyz` at the end). Trajectories can be read in both xyz and dcd format. 

**IMPORTANT:** if your trajectory is in dcd format (ex: `traj.dcd`) you should provide a one frame configuration in pdb format with the exact same name (`traj.pdb`).

The `input_file` file is a formatted file that contains pairs of `keyword value` in each line. Some keywords are compulsory while others are optional (default values will be used if not provided, check `src/input.f90` to see the list of all possible keywords and their default values). The input files provided (and commented) in the `tests/` folder should provide reasonable starting points to use the code. 

### Compulsory input keywords (one per line, space between keyword and value)

| Keyword                 | Description                                                                                                                                                |
|-------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------|
| file_coord              | Name of the coordinate file, either in XYZ or DCD format, in Å<br>(if DCD, a PDB file must be present with the same name as the DCD to get the atom names) |
| n_atoms                 | Number of atoms<br>(must be constant for all frames for XYZ, optional if DCD)                                                                              |
| n_frames                | Number of frames to calculate the interface (starting from start)<br>(optional if DCD, and will do the instantaneous interfaces for all the frames)        |
| xlo,xhi,ylo,yhi,zlo,zhi | Box boundaries, in Å                                                                                                                                       |
| first_O_atom            | Index of the first oxygen water<br>(index count start from 1)<br>[default = 1]                                                                             |
| last_O_atom             | Index of the last water oxygen<br>(index count start from 1, -1 is the end)<br>[default = -1]                              

<p align="right">(<a href="#top">back to top</a>)</p>

<!-- LICENSE -->
<div id="license"></div>

## License

Distributed under the GNU Affero General Public License v3.0. See `LICENSE` for more information.

<p align="right">(<a href="#top">back to top</a>)</p>

<!-- ACKNOWLEDGMENTS -->
<div id="acknowledgments"></div>

## Acknowledgments & Sources

* Based on the Fortran code of Rolf David for planar interfaces.
* Willard, A. P.; Chandler, D. Instantaneous Liquid Interfaces. J. Phys. Chem. B 2010, 114 (5), 1954–1958. https://doi.org/10.1021/jp909219k.
* Ridders, C. A New Algorithm for Computing a Single Root of a Real Continuous Function. IEEE Trans. Circuits Syst. 1979, 26 (11), 979–980. https://doi.org/10.1109/TCS.1979.1084580.
* González, A. Measurement of areas on a sphere using Fibonacci and latitude–longitude lattices, arXiv, 2009, [https://doi.org/10.48550/arXiv.0912.4540](https://doi.org/10.48550/arXiv.0912.4540)

<p align="right">(<a href="#top">back to top</a>)</p>

<!-- MARKDOWN LINKS & IMAGES -->
<!-- https://www.markdownguide.org/basic-syntax/#reference-style-links -->
[license-shield]: https://img.shields.io/github/license/laagegroup/0_Template.svg?style=for-the-badge
[license-url]: https://github.com/laagegroup/0_Template/blob/main/LICENSE
