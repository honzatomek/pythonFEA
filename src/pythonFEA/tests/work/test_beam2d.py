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
print(os.path.dirname(os.path.dirname(os.path.dirname(__file__))))
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.dirname(__file__))))
# sys.path.insert(0, path)

from structure.element.beam2d import Beam2D
from structure.nodes import Nodes
from structure.node import Node2D

e = Beam2D(1, 1, 1, [1, 2], [[False, False, False], [False, False, False]], 'test element')
nds = Nodes()
nds.add(Node2D(1, [0, 0], 'start node'))
nds.add(Node2D(2, [10, 10], 'end node'))
e.link_nodes(nds)
print(f'{repr(e):s} length: {e.length}')
print(f'{repr(e):s} transformation matrix:')
print(e.t_gcs2lcs)
print(f'{repr(e):s} transposed transformation matrix:')
print(e.t_lcs2gcs)
