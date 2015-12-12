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


def dim(vec):
	return vec.size


def size(mat):
	return mat.shape


def basis(vecspace):
	return vecspace.vectors[0]


def Liebraket(matA, matB):
	# if 
	matA * matB - matB * matA 

# def rank(mat):
# def trace(mat):
# def eigen(mat):
# def image(mat):
# def ortho(inspace):

