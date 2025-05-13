import numpy as np
import os,sys

def import_xyz(xyz_in):
    if os.path.isfile(xyz_in) == False:
        sys.exit('File not found: '+xyz_in+'\nAborting...')
    xyz = open(xyz_in, 'r')
    atoms = []
    coordinates = []
    n_atom=[]
    step_atoms = []
    step_coordinates=[]
    blank=[]
    s=0
    c=0
    for line in xyz:
        line_data = line.split()
        if len(line_data) == 1:
            n_atom.append(int(line_data[0]))
        elif len(line_data) == 4:
            atom, x, y, z = line_data
            atoms.append(atom)
            coordinates.append([float(x), float(y), float(z)])
            c=c+1
            if c >= n_atom[s]:
                step_atoms.append(atoms)
                step_coordinates.append(coordinates)
                atoms=[]
                coordinates=[]
                c=0
                s=s+1
        else:
            blank.append(line)
    n_atom=np.array(n_atom)
    step_atoms=np.array(step_atoms)
    step_coordinates=np.array(step_coordinates)
    xyz.close()
    return n_atom,step_atoms,step_coordinates,blank

for surface in ["test_xyz-surface-lower", "test_xyz-surface-upper"]:
    ref_natom, ref_nameatom, ref_coordinates, blank = import_xyz("test_xyz/ref_"+surface+".xyz")
    natom, nameatom, coordinates, blank = import_xyz("test_xyz/"+surface+".xyz")
    assert np.allclose(ref_coordinates,coordinates) , "Error in "+surface+".xyz"
    del ref_natom, ref_nameatom, ref_coordinates, blank, natom, nameatom, coordinates

for surface in ["test_dcd-surface-lower", "test_dcd-surface-upper"]:
    ref_natom, ref_nameatom, ref_coordinates, blank = import_xyz("test_dcd/ref_"+surface+".xyz")
    natom, nameatom, coordinates, blank = import_xyz("test_dcd/"+surface+".xyz")
    assert np.allclose(ref_coordinates,coordinates) , "Error in "+surface+".xyz"
    del ref_natom, ref_nameatom, ref_coordinates, blank, natom, nameatom, coordinates
