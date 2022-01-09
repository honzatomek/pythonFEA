import logging

from .__dummy_assign import DummyAssignment

class AssignProperty(DummyAssignment):
  command = 'APROP'
  assignment_type = 'Property'

