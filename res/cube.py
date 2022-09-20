#!/usr/bin/python3

import numpy as np
from scipy import sparse
from math import sin, pi

def cube(width, depth, height, m, n, o):
  dx = width / m
  dy = depth / n
  dz = height / o

  # nodes and coors
  coors = []
  nodes = []
  for k in range(o+1):
    for j in range(n+1):
      for i in range(m+1):
        nodes.append(i + (m+1)*j + (m+1)*(n+1)*k)
        coors.append([i*dx, j*dy, k*dz])
  nodes = np.array(nodes, dtype=int)
  coors = np.array(coors, dtype=float)
  # print(nodes)
  # print(coors)

  # connectivity
  lme = np.zeros((m*n*o, 8), dtype=int)
  for k in range(o):
    for j in range(n):
      for i in range(m):
        n1 = i + (m+1)*j + (m+1)*(n+1)*k
        n2 = i + (m+1)*j + (m+1)*(n+1)*k + 1
        n3 = i + (m+1)*(j+1) + (m+1)*(n+1)*k + 1
        n4 = i + (m+1)*(j+1) + (m+1)*(n+1)*k
        n5 = i + (m+1)*j + (m+1)*(n+1)*(k+1)
        n6 = i + (m+1)*j + (m+1)*(n+1)*(k+1) + 1
        n7 = i + (m+1)*(j+1) + (m+1)*(n+1)*(k+1) + 1
        n8 = i + (m+1)*(j+1) + (m+1)*(n+1)*(k+1)
        lme[i+m*j+m*n*k] = [n1, n2, n3, n4, n5, n6, n7, n8]

  # print(lme)

  lm = sparse.lil_matrix((len(nodes), len(nodes)), dtype=int)
  for i in range(lme.shape[0]):
    for j in range(lme.shape[1]):
      for k in range(lme.shape[1]):
        lm[lme[i,j], lme[i,k]] = 1

  lm = sparse.csr_matrix(lm)
  # print(lm.toarray())
  mapping = sparse.csgraph.reverse_cuthill_mckee(lm, symmetric_mode=True)

  # print(mapping)
  pmat = sparse.lil_matrix(lm.shape, dtype=int)
  for i in range(mapping.shape[0]):
    pmat[i,mapping[i]] = 1

  pmat = sparse.csr_matrix(pmat)
  # print(pmat.toarray())

  # lm_p = pmat.T @ lm
  lm_p1 = sparse.lil_matrix(lm.shape)
  # print(lm.shape)
  # print(mapping.shape)
  # lm_p[mapping, mapping] = lm
  lm_p1 = lm[np.ix_(mapping, mapping)]
  lm_p1 = sparse.csr_matrix(lm_p1)
  # print(lm_p1.toarray())

  lm_p2 = sparse.lil_matrix(lm.shape)
  lm_p2 = pmat @ lm @ pmat.T
  lm_p2 = sparse.csr_matrix(lm_p2)
  # print(lm_p2.toarray())

  compare = lm_p1.toarray() == lm_p2.toarray()
  # print(np.array_equal(lm_p1, lm_p2))
  # print(compare)
  # print(compare.all())

  return coors, lme


def generate_unv(nodes, elements, length, scale):
  # nodes
  # print(f'{-1:6n}')
  # print(f'{15:6n}')
  # for i, n in enumerate(nodes):
  #   print('{0:10n}{1:10n}{2:10n}{3:10n} {4:13.5e} {5:13.5e} {6:13.5e}'.format(i+1, 1, 1, 1, n[0], n[1], n[2]))
  # print(f'{-1:6n}')
  print(f'{-1:6n}')
  print(f'{2411:10n}')
  for i, n in enumerate(nodes):
    print('{0:10n}{1:10n}{2:10n}{3:10n}'.format(i+1, 1, 1, 1))
    print('{0:25.16e}{1:25.16e}{2:25.16e}'.format(n[0], n[1], n[2]).replace('e', 'D'))
  print(f'{-1:6n}')

  # elements
  # print(elements)
  print(f'{-1:6n}')
  print(f'{2412:6n}')
  for i, e in enumerate(elements):
    print('{0:10n}{1:10n}{2:10n}{3:10n}{4:10n}{5:10n}'.format(i+1, 115, 0, 0, 2, 8))
    print(''.join([f'{n+1:10n}' for n in e]))
  print(f'{-1:6n}')

  # displacement
  print(f'{-1:6n}')
  print(f'{55:6n}')
  print('{0:40s}'.format('NONE').rstrip())
  print('{0:40s}'.format('NONE').rstrip())
  print('{0:40s}'.format('NONE').rstrip())
  print('{0:40s}'.format('NONE').rstrip())
  print('{0:40s}'.format('NONE').rstrip())
  print(''.join([f'{s:10n}' for s in [1, 2, 2, 8, 2, 3]]))
  # 1 -  structural
  # 2 -  normal mode
  # 2 -  3-dof global translation vector
  # 8 -  displacement
  # 2 -  real data type
  # 3 -  number of data values per node
  print(''.join([f'{s:10n}' for s in [2, 4, 1, 1]]))
  print(''.join([f'{s:13.5e}' for s in [10., 0., 0., 0.]]))
  for i, n in enumerate(nodes):
    print(f'{i+1:10n}')
    print('{0:13.5e}{1:13.5e}{2:13.5e}'.format(n[0], n[1] +  sin(n[2] / length) * scale, n[2]))
  print(f'{-1:6n}')


if __name__ == '__main__':
  nodes, elements = cube(100., 100., 1000., 1, 1, 5)
  generate_unv(nodes, elements, 1000., 10.)

