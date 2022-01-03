import pythonFEA

nds = pythonFEA.Nodes()
nds.add(pythonFEA.Node(1, 1.0, 2.0, 3.0, 'test node 1'))
nds.add(pythonFEA.Node(2, 10.0, 20.0, -2.0, 'test node 2'))
nds.add(pythonFEA.Node(3, 1.0, 20.0, -2.0, 'test node 3'))
nds.add(pythonFEA.Node(4, 10.0, 2.0, 2.0, 'test node 4'))
print(nds)

els = pythonFEA.Elements()
els.add(pythonFEA.Beam2D(1, 1, 1, 1, 2, label='test element 1'))
els.add(pythonFEA.Beam2D(2, 1, 1, 2, 3, label='test element 2'))
els.add(pythonFEA.Beam2D(3, 1, 1, 3, 4, label='test element 3'))
print(els)

