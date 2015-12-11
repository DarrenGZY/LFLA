from InSpace import *
from AffSpace import *

### test for vector ###
def testVector():
	vecs = np.array([[1,-2,3],[0.2,23,0],[0,-4,-5.2]])


# ### test for matrix ###
def testMatrix():
	mat = [[3.3,-4,5],[5.3,-2,-2.2],[1.2,3.3,-2]]


# ### test for vector space ###
def testVecSpace():
	v1 = np.array([1,2,3])
	v2 = np.array([1,2,3])
	vecs = [v1]
	vecspace = VecSpace(vecs)
	vecspace.belongs(v2)
	v2 = np.array([10,20,30])
	vecspace.belongs(v2)
	v2 = np.array([1,20,30])
	vecspace.belongs(v2)
	print vecspace.vectors

	vs = VecSpace()
	v1 = [6, 0, 3, 1, 4, 2]
	v2 = [0, -1, 2, 7, 0, 5]
	v3 = [12, 3, 0, -19, 8, -11]
	vecs = [v1]
	vecspace = VecSpace(vecs)
	print vecspace.vectors
	# expected to be belong
	vecspace.belongs(v2)
	# expected NOT to be belong
	vecspace.belongs(v3)
	# need more test case for PLUS function
	vecspace.plus(vecspace)
	print vecspace.vectors
	print vecspace.dim()
	print vecspace.basis()


# ### test for inner space ###
def testInSpace():
	vecs = np.array([[1,-2,3],[0.2,23,0],[0,-4,-5.2]])
	mat = [[3.3,-4,5],[5.3,-2,-2.2],[1.2,3.3,-2]]

	### test for InnerSpace ###
	ins0 = InSpace()
	print ins0.vectors
	print ins0.matrix

	ins1 = InSpace(vecs, mat)
	print ins1.vectors
	print ins1.matrix

	# expected to work
	u = [0.2, 5.2, -4]
	v = [1.3, -0.4, 19]
	print ins1.product(u,v)

	# expected not to work
	# this will quit the program
	u = [0.2, -4]
	v = [1.3, -0.4, 19]
	# print ins1.product(u,v)


# ### test for affine space ###
def testAffSpace():
	v1 = [6, 0, 3, 1, 4, 2]
	v2 = [0, -1, 2, 7, 0, 5]
	v3 = [12, 3, 0, -19, 8, -11]
	vecs = [v1]
	vecspace = VecSpace(vecs)

	affspace = AffSpace()
	v = np.array([1.3, -0.4, 19, 3, -1.3, 0])
	v1 = np.array([-2, -1.4, 0, -13, 3.3, 10])
	affspace = AffSpace(v, vecspace)
	affspace.belongs(v1)
	affspace.belongs(v1)
	mat = np.array([[1,-2,3],[0.2,23,0],[0,-4,-5.2]])
	u = np.array([0.2, 5.2, -4])

	affspace = AffSpace.solve(mat,u)
	print affspace.vector



def testIssue():
	v1 = np.array([1,1,1])
	v2 = np.array([1,2,3])
	v3 = np.array([2,3,4])
	vs = VecSpace([v2])
	asp = AffSpace(v1, vs)
	asp.belongs(v3)
	print vs.basis()


def main():
	# testVector()
	# testMatrix()
	testVecSpace()
	# testInSpace()
	# testAffSpace()
	
	# testIssue()

if __name__ == '__main__':
	main()

