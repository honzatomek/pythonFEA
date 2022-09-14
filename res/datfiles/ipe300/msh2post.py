import meshio
import os
import sys

OUT = '.post'

def __convert__(filename):
  print(f'>>> converting {filename:s} to {os.path.splitext(filename)[0] + OUT:s}:')
  try:
    mesh = meshio.gmsh.read(filename)
    meshio.permas.write(os.path.splitext(filename)[0] + OUT, mesh)
    print('<<< done.')
  except Exception as e:
    print(e)

if __name__ == '__main__':
  args = sys.argv
  for f in args[1:]:
    __convert__(f)

