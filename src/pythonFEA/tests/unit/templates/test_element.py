from templates.errors import *
from structure.node import Node2D
from structure.nodes import Nodes
from templates.element import Element
from templates import logs
import defaults

import pytest

logger = logging.getLogger()
logger.disabled = True
logging.disable(logging.FATAL)

class TestElement:
  def test_init(self):
    id = 1
    mid = 'steel'
    nodes = [1]
    label = 'test element ' + str(id)
    e = Element(id, mid, nodes, label)
    assert e.id == id
    assert e.matname == mid
    assert e.nodeids == nodes
    assert e.label == label

  def test_init2(self):
    id = -1
    mid = 'steel'
    nodes = [1]
    label = 'test element ' + str(id)
    with pytest.raises(NotValidID):
      e = Element(id, mid, nodes, label)

  def test_link_nodes(self):
    id = 1
    mid = 'steel'
    nodes = [1]
    label = 'test element ' + str(id)
    e = Element(id, mid, nodes, label)
    nds = Nodes()
    for nid in nodes:
      nds.add(Node2D(nid, [0, 0]))
    e.link_nodes(nds)
    assert e.linked_nodes

  def test_link_nodes2(self):
    id = 1
    mid = 'steel'
    nodes = [1]
    label = 'test element ' + str(id)
    e = Element(id, mid, nodes, label)
    nds = Nodes()
    for nid in nodes:
      nds.add(Node2D(nid + 100, [0, 0]))
    with pytest.raises(MissingIndex):
      e.link_nodes(nds)

  def test_nodes(self):
    id = 1
    mid = 'steel'
    nodes = [1]
    label = 'test element ' + str(id)
    e = Element(id, mid, nodes, label)
    n = Node2D(nodes[0], [0, 0])
    e.nodes = n
    assert e.nodes[0].id == n.id

  def test_nodes2(self):
    id = 1
    mid = 'steel'
    nodes = [1]
    label = 'test element ' + str(id)
    e = Element(id, mid, nodes, label)
    n1 = Node2D(nodes[0], [0, 0])
    n2 = Node2D(nodes[0] + 1, [0, 0])
    with pytest.raises(NodeMissing):
      e.nodes = [n1, n2]

  def test_nodes3(self):
    id = 1
    mid = 'steel'
    nodes = [1]
    label = 'test element ' + str(id)
    e = Element(id, mid, nodes, label)
    n = Node2D(nodes[0] + 1, [0, 0])
    with pytest.raises(MissingIndex):
      e.nodes = n

  def test_t_gcs2lcs(self):
    id = 1
    mid = 'steel'
    nodes = [1]
    label = 'test element ' + str(id)
    e = Element(id, mid, nodes, label)
    with pytest.raises(NotLinked):
      t = e.t_gcs2lcs


  def test_t_gcs2lcs2(self):
    id = 1
    mid = 'steel'
    nodes = [1]
    label = 'test element ' + str(id)
    e = Element(id, mid, nodes, label)
    nds = Nodes()
    for nid in nodes:
      nds.add(Node2D(nid, [0, 0]))
    e.link_nodes(nds)
    with pytest.raises(NotImplemented):
      t = e.t_gcs2lcs

  def test_t_lcs2gcs(self):
    id = 1
    mid = 'steel'
    nodes = [1]
    label = 'test element ' + str(id)
    e = Element(id, mid, nodes, label)
    with pytest.raises(NotLinked):
      t = e.t_lcs2gcs

  def test_t_lcs2gcs2(self):
    id = 1
    mid = 'steel'
    nodes = [1]
    label = 'test element ' + str(id)
    e = Element(id, mid, nodes, label)
    nds = Nodes()
    for nid in nodes:
      nds.add(Node2D(nid, [0, 0]))
    e.link_nodes(nds)
    with pytest.raises(NotImplemented):
      t = e.t_lcs2gcs

  def test_ke(self):
    id = 1
    mid = 'steel'
    nodes = [1]
    label = 'test element ' + str(id)
    e = Element(id, mid, nodes, label)
    with pytest.raises(NotLinked):
      t = e.ke

  def test_ke2(self):
    id = 1
    mid = 'steel'
    nodes = [1]
    label = 'test element ' + str(id)
    e = Element(id, mid, nodes, label)
    nds = Nodes()
    for nid in nodes:
      nds.add(Node2D(nid, [0, 0]))
    e.link_nodes(nds)
    with pytest.raises(NotLinked):
      t = e.ke

  def test_me(self):
    id = 1
    mid = 'steel'
    nodes = [1]
    label = 'test element ' + str(id)
    e = Element(id, mid, nodes, label)
    with pytest.raises(NotLinked):
      me = e.me()

  def test_me2(self):
    id = 1
    mid = 'steel'
    nodes = [1]
    label = 'test element ' + str(id)
    e = Element(id, mid, nodes, label)
    nds = Nodes()
    for nid in nodes:
      nds.add(Node2D(nid, [0, 0]))
    e.link_nodes(nds)
    with pytest.raises(NotLinked):
      me = e.me()

  def test_lf(self):
    id = 1
    mid = 'steel'
    nodes = [1]
    label = 'test element ' + str(id)
    e = Element(id, mid, nodes, label)
    with pytest.raises(NotLinked):
      t = e.lf()

  def test_lf2(self):
    id = 1
    mid = 'steel'
    nodes = [1]
    label = 'test element ' + str(id)
    e = Element(id, mid, nodes, label)
    nds = Nodes()
    for nid in nodes:
      nds.add(Node2D(nid, [0, 0]))
    e.link_nodes(nds)
    with pytest.raises(NotImplemented):
      t = e.lf()

  def test_lt(self):
    id = 1
    mid = 'steel'
    nodes = [1]
    label = 'test element ' + str(id)
    e = Element(id, mid, nodes, label)
    with pytest.raises(NotLinked):
      t = e.lt()

  def test_lt2(self):
    id = 1
    mid = 'steel'
    nodes = [1]
    label = 'test element ' + str(id)
    e = Element(id, mid, nodes, label)
    nds = Nodes()
    for nid in nodes:
      nds.add(Node2D(nid, [0, 0]))
    e.link_nodes(nds)
    with pytest.raises(NotImplemented):
      t = e.lt()

  def test_ksig(self):
    id = 1
    mid = 'steel'
    nodes = [1]
    label = 'test eleksignt ' + str(id)
    e = Element(id, mid, nodes, label)
    with pytest.raises(NotLinked):
      t = e.ksig()

  def test_ksig2(self):
    id = 1
    mid = 'steel'
    nodes = [1]
    label = 'test eleksignt ' + str(id)
    e = Element(id, mid, nodes, label)
    nds = Nodes()
    for nid in nodes:
      nds.add(Node2D(nid, [0, 0]))
    e.link_nodes(nds)
    with pytest.raises(NotLinked):
      t = e.ksig()

  def test_postpro(self):
    id = 1
    mid = 'steel'
    nodes = [1]
    label = 'test elepostpront ' + str(id)
    e = Element(id, mid, nodes, label)
    with pytest.raises(NotLinked):
      t = e.postpro()

  def test_postpro2(self):
    id = 1
    mid = 'steel'
    nodes = [1]
    label = 'test elepostpront ' + str(id)
    e = Element(id, mid, nodes, label)
    nds = Nodes()
    for nid in nodes:
      nds.add(Node2D(nid, [0, 0]))
    e.link_nodes(nds)
    with pytest.raises(NotLinked):
      t = e.postpro()

