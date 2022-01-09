import logging

from templates.basic import Basic
from templates.errors import *

class Structure(Basic):
  command = 'STRUCTURE'

  def __init__(self, label, nodes = None, elements = None):
    super().__init__(label=label)
    self.nodes = nodes
    self.elements = elements

  @property
  def nodes(self):
    return self.nodes

  @nodes.setter
  def nodes(self, nodes):
    self.__nodes = nodes

  @property
  def elements(self):
    return self.__elements

  @elements.setter
  def elements(self, elements):
    self.__elements = elements

