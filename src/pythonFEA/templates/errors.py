import logging

class BaseError(Exception):
  def __init__(self, message):
    self.message = message
    logging.exception(message)
    super().__init__(self.message)

class MissingIndex(BaseError):
  pass

class UsedIndex(BaseError):
  pass

class WrongType(BaseError):
  pass

class NotInitialised(BaseError):
  pass

class NotImplemented(BaseError):
  pass

class NoEntry(BaseError):
  pass

class NodeMissing(BaseError):
  pass

class ElementMissing(BaseError):
  pass

class LoadMissing(BaseError):
  pass

class PropertyMissing(BaseError):
  pass

class InvalidProperty(BaseError):
  pass

class MaterialMissing(BaseError):
  pass

class SituationMissing(BaseError):
  pass

class ConstraintDOFs(BaseError):
  pass

class MissingEntry(BaseError):
    pass

class NotValidID(BaseError):
    pass

class NotValidCoor(BaseError):
    pass

class CoordinateMissing(BaseError):
  pass
