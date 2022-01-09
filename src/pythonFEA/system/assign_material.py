import logging

# from .__dummy_assign import DummyAssignment
from templates.assign import DummyAssignment

class AssignMaterial(DummyAssignment):
  command = 'AMAT'
  assignment_type = 'Material'

