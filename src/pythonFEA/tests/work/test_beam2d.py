import os
import sys
import pathlib

# path = pathlib.Path(__file__).parent.resolve()
# while os.path.basename(path) != 'pythonFEA' and os.path.basename(path) != '':
#   path = pathlib.Path(path).parent.resolve()
#   # print(os.path.basename(path))

# sys.exit(1)
# print(pathlib.Path(path).parent.resolve())
# print(os.path.basename(path))

# print(path)
# print(os.path.dirname(os.path.dirname(os.path.dirname(__file__))))
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.dirname(__file__))))
# sys.path.insert(0, path)

from structure.elements import Elements
from structure.element.beam2d import Beam2D
from structure.nodes import Nodes
from structure.node import Node2D

from property import Properties
from property import PBeam2D
from material.materials import Materials
from material.material import LinearElastic

els = Elements()
els.add(Beam2D(1, 'steel', 'ipe200', [1, 2], [[False, False, False], [False, False, False]], 'test element'))
e = els[1]
nds = Nodes()
nds.add(Node2D(1, [0, 0], 'start node'))
nds.add(Node2D(2, [5000, 5000], 'end node'))
e.link_nodes(nds)
print(f'{repr(e):s} length: {e.length}')
print(f'{repr(e):s} transformation matrix:')
print(e.t_gcs2lcs)
print(f'{repr(e):s} transposed transformation matrix:')
print(e.t_lcs2gcs)

mats = Materials()
mats.add(LinearElastic('steel', 210000., 0.3, 7.85E-9, 1.2E-5))
props = Properties()
props.add(PBeam2D('ipe200', 1000.0, 100000.0, 0.0))

e.link_mat(mats)
e.link_prop(props)

print(f'{repr(e):s} stiffness matrix:')
print(e.ke)
print(f'{repr(e):s} mass matrix:')
print(e.me())

print('\nNodes:')
print(nds)

print('\nElements:')
print(els)

print('\nProperties:')
print(props)

print('\nMaterials:')
print(mats)
