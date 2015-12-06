from InSpace import *
from AffSpace import *

def main():
	# vecs = [[1,-2,3],[0.2,23,0],[0,-4,-5.2]]
	# mat = [[3,4,5],[5,-2,4]]

	# ### test for InnerSpace ###
	# ins0 = InSpace()
	# print ins0.vectors
	# print ins0.matrix

	# ins1 = InSpace(vecs, mat)
	# print ins1.vectors
	# print ins1.matrix

	# # expected to work
	# u = [0.2, 5.2, -4]
	# v = [1.3, -0.4, 19]
	# print ins1.product(u,v)

	# # expected not to work
	# # this will quit the program
	# u = [0.2, -4]
	# v = [1.3, -0.4, 19]
	# print ins1.product(u,v)


	### test for vector space ###
	# v1 = [1,2,3]
	# v2 = [1,2,3]
	# vecs = [v1]
	# vecspace = VecSpace(vecs)
	# vecspace.belongs(v2)
	# v2 = [10,20,30]
	# vecspace.belongs(v2)
	# v2 = [1,20,30]
	# vecspace.belongs(v2)
	# print vecspace.vectors


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
	# print vecspace.vectors
	# print vecspace.dim()
	# print vecspace.basis()

	# ### test for affine space ###
	v = [1.3, -0.4, 19, 3, -1.3, 0]
	v1 = [-2, -1.4, 0, -13, 3.3, 10]
	affspace = AffSpace(v, vecspace)
	affspace.belongs(v1)
	affspace.belongs(v1)
	mat = [[1,-2,3],[0.2,23,0],[0,-4,-5.2]]
	u = [0.2, 5.2, -4]

	affspace = AffSpace.solve(mat,u)
	print affspace.vector


if __name__ == '__main__':
	main()

