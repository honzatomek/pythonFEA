import os
import sys
import pathlib

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.dirname(__file__))))

from system import AssignProperty
from system import AssignMaterial
from system import System

aprop = AssignProperty()
pa = dict()
pa.setdefault('ipe200', [])
pa.setdefault('ipe300', [])

for i in range(100):
  pa['ipe200'].append(i + 1)
  pa['ipe300'].append(i + 101)
  # pa['ipe300'].append(i + 100)

aprop.assignment = pa

amat = AssignProperty()
ma = dict()
ma.setdefault('steel', [])
ma.setdefault('PA6-GF30-I', [])

for i in range(10):
  ma['steel'].append(i + 1)
  ma['PA6-GF30-I'].append(i + 101)

amat.assignment = ma

s = System('DEFVAR', amat, aprop)
print(str(s))

