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

A small Fortran program to calcultate Instantaneous Liquid Interfaces proposed by [Willard and Chandler](https://doi.org/10.1021/jp909219k) in spherical geometries (water droplets or cavities). The code generates a quasi-uniformly paved spherical grid with a [Fibonacci lattice](https://arxiv.org/pdf/0912.4540), which should be accurate enough for most applications (provided that the spacing be small enough). It then looks for the half density iso-surface along the directions spanned by the Fibonacci sphere from a fixed or mobile position (referred to as the center of mass COM, although it does not need to correspond to a real COM).

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
   git clone https://github.com/laagegroup/instantaneous_interface_spheres_f.git
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

If everything works, the program produces the instantaneous water surface for every frame of the trajectory in xyz format (the file will be named as the trajectory file with `-surface.xyz` at the end).

<p align="right">(<a href="#top">back to top</a>)</p>

<!-- LICENSE -->
<div id="license"></div>

## License

Distributed under the GNU Affero General Public License v3.0. See `LICENSE` for more information.

<p align="right">(<a href="#top">back to top</a>)</p>

<!-- ACKNOWLEDGMENTS -->
<div id="acknowledgments"></div>

## Acknowledgments & Sources

*
*
*

<p align="right">(<a href="#top">back to top</a>)</p>

<!-- MARKDOWN LINKS & IMAGES -->
<!-- https://www.markdownguide.org/basic-syntax/#reference-style-links -->
[license-shield]: https://img.shields.io/github/license/laagegroup/0_Template.svg?style=for-the-badge
[license-url]: https://github.com/laagegroup/0_Template/blob/main/LICENSE
