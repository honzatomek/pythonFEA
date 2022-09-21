#!/usr/bin/python3
# https://www.codeproject.com/Articles/1258509/Modal-Analysis-of-Plane-Truss-using-Python

'''
Input file format:

nn, nele, nbc nodes, nloaded nodes
x1, y1
x2, y2
x3, y3
.
.
.
xnn, ynn
n11, n12, a1, e1, rho1
n21, n22, a2, e2, rho2
n31, n32, a3, e3, rho3
n41, n42, a4, e4, rho4
.
.
.
nnele1, nnele2, anele, enele, rhonele
node_bcnode1, ux restrained (1/0), uy restrained (1/0), prescibed ux, prescribed uy
node_bcnode2, ux restrained (1/0), uy restrained (1/0), prescibed ux, prescribed uy
node_bcnode3, ux restrained (1/0), uy restrained (1/0), prescibed ux, prescribed uy
.
.
.
node_nbc nodes, ux restrained (1/0), uy restrained (1/0), prescibed ux, prescribed uy
node_loadednode1, fx, fy
node_loadednode2, fx, fy
.
.
.
node_nloadednodes, fx, fy


In the format, the indicated fields and variables are:

nn = Number of nodes in the model
nele = Number of Elements in the model
nbc (same as nbc nodes)= Total number of nodes having boundary conditions (supports)
nloads (same as nloaded nodes) = Total number of nodes on which loads have been applied
x1, y1, x2, y2... are x or y coordinates describing the nodes. Thus, after initial description of the structure, the nodal coordinates are required to be specified.
Element description follows the node coordinates. Each element (member) shall be described by node 1 (node at end i), node 2 (node at end j), area of cross section, modulus of elasticity of the material and density of the material (mass per unit volume)
Supports immediately follow the element description. Each support description includes the node on which the support is applied, next is a logic of restraint in ux direction which indicates whether displacement in u direction (ux) is restrained or not (1 to indicate restrained, 0 to indicate unrestrained), followed by the value of prescribed ux. Prescribed ux must be given. If logic of restraint = 0, the prescribed value will be ignored. Next will be logic of restraint in v direction (uy). Specify 1 to indicate restrained, 0 to indicate unrestrained translation in y direction. This shall be followed by the value of prescribed uy. Again, just like ux, this value must be specified. If restraint uy = 0, prescribed uy value will be ignored.
Finally, the loads shall be specified for each loaded node. The load description includes the node on which the load is applied, load in x direction (Fx), and load in y direction (Fy)
Please note that no blank space or comments in the input file are permitted. The Input class can be very easily modified to accept comments and/or blank lines. Also note that node numbering in the input file starts from 1 and not 0 as in the code.
'''


import math
import numpy as np
class Element(object):
  """2D Truss element. Contains various definitions required for the element and also computes
  element matrices"""

  def __init__(self, x1, y1, x2, y2, a, e, n1, n2, rho):
    self.x1 = x1
    self.y1 = y1
    self.x2 = x2
    self.y2 = y2
    self.a = a
    self.e = e
    self.n1 = n1
    self.n2 = n2
    self.l = math.sqrt((x2-x1)**2 + (y2-y1)**2)
    self.rho = rho

  def GetKe(self):
    nDofs = 4;

    c = (self.x2-self.x1)/self.l
    s = (self.y2-self.y1)/self.l

    cc = c * c
    ss = s * s
    cs = c * s

    s = (nDofs,nDofs)
    ke = np.zeros(s)
    ke[0,0] = cc
    ke[0,1] = cs
    ke[1,1] = ss
    ke[1,0] = cs

    for r in range(2,4):
      for c in range(2,4):
        ke[r,c] = ke[r-2,c-2]

    for r in range(2,4):
      for c in range(0,2):
        ke[r,c] = -ke[r-2,c]

    for r in range(0,2):
      for c in range(2,4):
        ke[r,c] = -ke[r,c-2]

    return ke

  def GetMe(self, rho):
    nDofs = 4;

    s = (nDofs,nDofs)
    me = np.zeros(s)
    me[0,0] = 2
    me[1,1] = 2
    me[2,2] = 2
    me[3,3] = 2

    me[0,2] = 1
    me[1,3] = 1

    me[2,0] = 1
    me[3,1] = 1

    me = me * (1/6 * rho * self.a * self.l)

    #transform the matrix to global coordinates
    s = (nDofs, nDofs)
    t = np.zeros(s)
    c = (self.x2-self.x1)/self.l
    s = (self.y2-self.y1)/self.l

    t[0,0] = c
    t[0,1] = s
    t[1,0] = -s
    t[1,1] = c

    t[2,2] = c
    t[2,3] = s
    t[3,2] = -s
    t[3,3] = c

    m = np.transpose(t) @ me @ t

    return m


      while converged == False:
        # Step 1: lets start with the procedure
        k += 1

        # if there are more eigenvalues and eigenvectors to be computed,
        # we apply Gram-Schmidt rpocess to compute an orthogonal trial
        # vector for eigenvector to obtain the next eigenvalue / eigenvector

        if ev > 0: # and ev < ndofs:
          uk_1 = self.GetOrthogonalVector(ndofs,uk_1,mg,ev,evec)

        # Step 2: determine the right hand side
        vk_1 = mg @ uk_1 # multiply matrix m with vector u(k-1)

        # Step 3: compute u by solving equations
        uhatk = np.linalg.solve(kg,vk_1)

        # Step 4: denote vk = mu
        vhatk = mg @ uhatk

        # Step 5: estimate eigenvalue
        uhatkt = np.transpose(uhatk)
        eigenvalue = (uhatkt @ vk_1)/(uhatkt @ vhatk)

        # Step 6: normalize eigenvector
        denominator = math.sqrt(uhatkt @ vhatk)
        uk = uhatk/denominator

        # Step 7: check for tolerance
        tol = abs((eigenvalue - eigenvalue0) / eigenvalue)
        if tol <= tolerance:
          converged = True
          evec[:,ev] = uk
          eval[ev] = eigenvalue
          print("Eigenvalue = " + str(eigenvalue) + " ... done.")
        else:
          eigenvalue0 = eigenvalue
          uk_1 = uk

          if k > 1000:
            evec[:,ev] = uk
            eval[ev] = eigenvalue
            print ("could not converge. Tolerance = " + str(tol))
            break

    self.eigenvalues = eval
    return evec


if __name__ == '__main__':
  # Read the input
  inp = Input.Input()
  inp.Read()

  nnodes= inp.nnodes
  nodes = inp.nodes

  nele = inp.nele
  elements = inp.elements
  nbcnodes = inp.nbcnodes
  supports = inp.supports
  loads = inp.loads
  filename = inp.filename

  # Solve the truss

  ndof = nnodes * 2
  assembly = Assembly.Assembly()
  assembly. GenerateKgAndMg(ndof,elements)

  # generate load vector
  assembly.GenerateLoadVector(loads)

  # apply boundary conditions
  assembly.ApplyBoundaryConditions(supports)

  # Retrieve global stiffness matrix, mass matrix and load vector from the assembly object
  kg = assembly.kg
  mg = assembly.mg
  r = assembly.r

  # perform linear solution to get deformations for applied load
  print("Performing linear solution... ", end="")
  d = np.linalg.solve(kg,r)
  print ("done.")

  # lets reshape the displacements so that u and v components can be seen separately
  disp = d.reshape(nnodes,2)

  # write the output to the outputfile
  output = Output.Output()
  output.WriteOutputFile(inp.outfilename, nodes, elements, disp)

  # write the output in matlab format so that we can plot it
  mfile = filename + ".m"
  dispscale = 400
  output.WriteModelOnMatlabFile(mfile,nodes,elements,disp)

  # compute eigenvalues and eigen vectors
  e = Eigen.Eigen()
  print("Performing eigenvalue solution... ", end="")
  evec = e.Solve(kg,mg,1e-6)
  #evec = e.RescaleEigenVectors(evec)

  Eval = e.eigenvalues
  Neval = len(eval)
  Print ("done.")
