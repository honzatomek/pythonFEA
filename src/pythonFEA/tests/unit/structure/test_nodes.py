from templates.errors import *
from structure.nodes import *
from structure.node import *
from templates import logs
import defaults

import pytest
import numpy as np

logger = logging.getLogger()
logger.disabled = True
logging.disable(logging.FATAL)

class TestNodes:
  def test_add1(self):
    nds = Nodes()
    with pytest.raises(WrongType):
      nds.add(1)

  def test_add2(self):
    nds = Nodes()
    n1 = Node2D(1, [2, 3], 'test 1')
    n2 = Node2D(1, [2, 3], 'test 2')
    nds.add(n1)
    with pytest.raises(UsedIndex):
      nds.add(n2)

  def test_add3(self):
    nds = Nodes()
    n1 = Node2D(1, [2, 3], 'test 1')
    nds.add(n1)
    assert nds.count == 1

  def test_add4(self):
    nds = Nodes()
    n1 = Node2D(1, [2, 3], 'test 1')
    n2 = Node2D(2, [3, 4], 'test 2')
    nds.add([n1, n2])
    assert nds.count == 2

  def test_add5(self):
    nds = Nodes()
    n1 = Node2D(1, [2, 3], 'test 1')
    n2 = Node2D(2, [2, 3], 'test 2')
    nds.add((n1, n2))
    assert nds.count == 2

  def test_node_type(self):
    nds = Nodes()
    n1 = Node2D(1, [2, 3], 'test 1')
    n2 = Node(2, [2, 3, 4], 'test 2')
    nds.add(n1)
    with pytest.raises(WrongType):
      nds.add(n2)

  def test_node_type2(self):
    nds = Nodes()
    n1 = Node2D(1, [2, 3], 'test 1')
    n2 = Node(2, [2, 3, 4], 'test 2')
    with pytest.raises(WrongType):
      nds.add([n1, n2])

  def test_count(self):
    num = 4
    nds = Nodes()
    for i in range(num):
      nds.add(Node2D(i + 1, [2, 3], 'test ' + str(i + 1)))
    assert nds.count == num

  def test_id(self):
    num = 4
    nds = Nodes()
    for i in range(num):
      nds.add(Node2D(i + 1, [2, 3], 'test ' + str(i + 1)))
    assert type(nds.id(num)) is Node2D

  def test_id2(self):
    nds = Nodes()
    nds.add(Node2D(1, [2, 3], 'test 1'))
    with pytest.raises(MissingIndex):
      n = nds.id(2)

  def test_getitem(self):
    num = 4
    nds = Nodes()
    for i in range(num):
      nds.add(Node2D(i + 1, [2, 3], 'test ' + str(i + 1)))
    assert nds.id(num) is nds[num]

  def test_min(self):
    num = 4
    nds = Nodes()
    for i in range(num):
      nds.add(Node2D(i + 1, [2, 3], 'test ' + str(i + 1)))
    assert nds.min == 1

  def test_max(self):
    num = 4
    nds = Nodes()
    for i in range(num):
      nds.add(Node2D(i + 1, [2, 3], 'test ' + str(i + 1)))
    assert nds.max == num

  def test_distance(self):
    nds = Nodes()
    n1 = Node2D(1, [2, 3], 'test 1')
    n2 = Node2D(2, [2, 3], 'test 2')
    nds.add([n1, n2])
    assert nds.distance(n1.id, n2.id) == 0.0

  def test_distance2(self):
    nds = Nodes()
    n1 = Node2D(1, [0, 0], 'test 1')
    n2 = Node2D(2, [10, 0], 'test 2')
    nds.add([n1, n2])
    assert nds.distance(n1.id, n2.id) == 10.0

  def test_distance3(self):
    nds = Nodes()
    n1 = Node(1, [2, 3, 4], 'test 1')
    n2 = Node(2, [2, 3, 4], 'test 2')
    nds.add([n1, n2])
    assert nds.distance(n1.id, n2.id) == 0.0

  def test_distance4(self):
    nds = Nodes()
    n1 = Node(1, [0, 0, 0], 'test 1')
    n2 = Node(2, [10, 0, 0], 'test 2')
    nds.add([n1, n2])
    assert nds.distance(n1.id, n2.id) == 10.0

