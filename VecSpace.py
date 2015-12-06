import numpy as np

TOLERANCE = 1e-5
def linearIndependent(vecs):
	# singular value decomposition
	U, s, V = np.linalg.svd(np.asarray(vecs))
	if (np.sum(s > TOLERANCE) == len(vecs)):
		return 1
	else:
		return 0

class VecSpace:
	"""A class of vector space

	Attributes:
		vectors: A list of vectors
	"""

	def __init__(self, vecs):
		self.vectors = []
		
		tempVecs = []
		# test for all vectors in the vecs list
		for i in range(len(vecs)):
			tempVecs = tempVecs.append(vecs[i])
			# if # of counts > # of vecs,
			# the new vector is linear independent with base vectors
			if linearIndependent(vecs):
				self.vectors.append(vecs[i])


	def belongs(self, vec):
		self.vectors.append(vec)
		if linearIndependent(self.vectors):
			print "Belongs to this vectors spaces"
			return 1
		else:
			print "Not belongs to this vectors spaces"
			del self.vectors[-1]
			return 0


	def plus(self, vecspace):
		temp = self.vectors + vecspace.vectors
		index = linearIndependent(temp)
		self.vectors = []
		# ??? not sure if this is mathmatically valid
		for i in range(len(index)):
			if index[i] > TOLERANCE:
				self.vectors.append(temp[i])
		# ??? return L(x), what does this mean?


	def dim(self):
		return len(self.vectors)


	def basis(self):
		# ??? a vector inside the vectors or whole list of vectors
		return self.vectors

