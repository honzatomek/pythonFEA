from templates.errors import *
from structure.node import *
from templates import logs
import defaults

import pytest
import numpy as np

logger = logging.getLogger()
logger.disabled = True
logging.disable(logging.FATAL)

class TestNode2D:
  def test_init(self):
    id = 1
    x = 0.0
    y = 1.0
    label = 'test ' + str(id)
    n = Node2D(id, x, y, label)
    assert n.id == id
    assert n.x == x
    assert n.y == y
    assert n.label == label

  def test_init2(self):
    id = 0
    x = 0.0
    y = 1.0
    label = 'test ' + str(id)
    with pytest.raises(NotValidID):
      n = Node2D(id, x, y, label)

  def test_coors(self):
    id = 1
    x = 1.0
    y = 2.0
    coors = np.asarray([x, y], dtype=defaults.DEFAULT_FLOAT)
    label = 'test ' + str(id)
    n = Node2D(id, x, y, label)
    assert n.coors.any() == coors.any()

  def test_coors2(self):
    id = 1
    x = 1.0
    y = 2.0
    coors = np.asarray([x + 1.0, y + 2.0], dtype=defaults.DEFAULT_FLOAT)
    label = 'test ' + str(id)
    n = Node2D(id, x, y, label)
    n.coors = coors
    assert n.coors.any() == coors.any()


class TestNode:
  def test_init(self):
    id = 1
    x = 0.0
    y = 1.0
    z = 2.0
    label = 'test ' + str(id)
    n = Node(id, x, y, z, label)
    assert n.id == id
    assert n.x == x
    assert n.y == y
    assert n.z == z
    assert n.label == label

  def test_init2(self):
    id = 0
    x = 0.0
    y = 1.0
    z = 2.0
    label = 'test ' + str(id)
    with pytest.raises(NotValidID):
      n = Node(id, x, y, z, label)

  def test_coors(self):
    id = 1
    x = 1.0
    y = 2.0
    z = 3.0
    coors = np.asarray([x, y, z], dtype=defaults.DEFAULT_FLOAT)
    label = 'test ' + str(id)
    n = Node(id, x, y, z, label)
    assert n.coors.any() == coors.any()

  def test_coors2(self):
    id = 1
    x = 1.0
    y = 2.0
    z = 3.0
    coors = np.asarray([x + 1.0, y + 2.0, z + 3.0], dtype=defaults.DEFAULT_FLOAT)
    label = 'test ' + str(id)
    n = Node(id, x, y, z, label)
    n.coors = coors
    assert n.coors.any() == coors.any()

