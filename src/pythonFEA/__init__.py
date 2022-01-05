import os
import sys
import logging

logging.debug('Reading src/__init__.py')
print('Reading src/__init__.py.')
sys.path.insert(0, os.path.dirname(__file__))

