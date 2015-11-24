from InSpace import *
from VecSpace import *

def main():
	vecs = [[1,-2,3],[0.2,23,0],[0,-4,-5.2]]
	mat = [[3,4,5],[5,-2,4]]

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
	u = [0.2, -4]
	v = [1.3, -0.4, 19]
	print ins1.product(u,v)


	### test for vector space ###
	vecspace = VecSpace([])


if __name__ == '__main__':
	main()