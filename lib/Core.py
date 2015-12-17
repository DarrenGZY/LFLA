import sys
import numpy as np

TOLERANCE = 1e-5

def linearIndependent(vecs):
	# singular value decomposition
	U, s, V = np.linalg.svd(np.asarray(vecs))
	if (np.sum(s > TOLERANCE) == len(vecs)):
		return 1
	else:
		return 0


def sqrt(var):
	return np.sqrt(var)


def ceil(var):
	return np.ceil(var)


def floor(var):
	return np.floor(var)


def dim(npArray):
	return npArray.size


def size(npArray):
	return npArray.shape


def basis(vecspace):
	return vecspace.vectors[0]


def liebracket(matA, matB):
	if (matA.shape == matB.shape):
		if (matA.shape[0] == matA.shape[1]) and \
			(matB.shape[0] == matB.shape[1]):
			return matA * matB - matB * matA 
		else:
			print "Input Matrix is not square matrix"
	else:
			print "Dimensions of two matrices dose not match"


def rank(mat):
	return np.linalg.matrix_rank(mat)


def trace(mat):
	return np.trace(mat)


def eigen(mat):
	return np.linalg.eig(mat)


# def image(mat):

# def ortho(inspace):

