from templates.errors import *
from templates.basic import *
from templates import logs

import pytest

logger = logging.getLogger()
logger.disabled = True
logging.disable(logging.FATAL)


class TestBasic:
  def test_init(self):
    id = 1
    label = 'test'
    b = Basic(id, label)
    assert b.id == id
    assert b.label == label

  def test_init2(self):
    id = 0
    label = 'test'
    with pytest.raises(NotValidID):
      b = Basic(id, label)

  def test_init3(self):
    id = 0
    label = 'test'
    with pytest.raises(NotValidID):
      b = Basic(label, id)


class TestCollection:
  def test_add(self):
    c = Collection()
    with pytest.raises(WrongType):
      c.add(1)

  def test_add2(self):
    c = Collection()
    b1 = Basic(1, 'test 1')
    b2 = Basic(1, 'test 2')
    c.add(b1)
    with pytest.raises(UsedIndex):
      c.add(b2)

  def test_add2(self):
    c = Collection()
    b1 = Basic(1, 'test 1')
    c.add(b1)
    assert c.count == 1

  def test_add3(self):
    c = Collection()
    b1 = Basic(1, 'test 1')
    b2 = Basic(2, 'test 2')
    c.add([b1, b2])
    assert c.count == 2

  def test_add4(self):
    c = Collection()
    b1 = Basic(1, 'test 1')
    b2 = Basic(2, 'test 2')
    c.add((b1, b2))
    assert c.count == 2

  def test_count(self):
    num = 4
    c = Collection()
    for i in range(num):
      c.add(Basic(i + 1, 'test ' + str(i + 1)))
    assert c.count == num

  def test_id(self):
    num = 4
    c = Collection()
    for i in range(num):
      c.add(Basic(i + 1, 'test ' + str(i + 1)))
    assert type(c.id(num)) is Basic

  def test_id2(self):
    c = Collection()
    c.add(Basic(1, 'test 1'))
    with pytest.raises(MissingIndex):
      b = c.id(2)

  def test_getitem(self):
    num = 4
    c = Collection()
    for i in range(num):
      c.add(Basic(i + 1, 'test ' + str(i + 1)))
    assert c.id(num) is c[num]

  def test_min(self):
    num = 4
    c = Collection()
    for i in range(num):
      c.add(Basic(i + 1, 'test ' + str(i + 1)))
    assert c.min == 1

  def test_max(self):
    num = 4
    c = Collection()
    for i in range(num):
      c.add(Basic(i + 1, 'test ' + str(i + 1)))
    assert c.max == num




# if __name__ == '__main__':
#   logger = logging.getLogger()
#   logger.disabled = True
#   logging.disable(logging.FATAL)
#
#   # unittest.main(verbosity=3)
