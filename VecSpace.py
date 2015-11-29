import numpy as np

TOLERANCE = 1e-5
def linearIndependent(vecs):
	# singular value decomposition
	U, s, V = np.linalg.svd(np.asarray(vecs))
	return s.tolist()


class VecSpace:
	"""A class of vector space

	Attributes:
		vectors: A list of vectors
	"""

	def __init__(self, vecs):
		self.vectors = []
		index = linearIndependent(vecs)

		# if the singular value > 0 (tolerance)
		# then it is linear independent
		for i in range(len(index)):
			if index[i] > TOLERANCE:
				self.vectors.append(vecs[i])


	def belongs(self, vec):
		self.vectors.append(vec)
		index = linearIndependent(self.vectors)
		if index[-1] <= TOLERANCE:
			del self.vectors[-1]
			print "Not belongs to this vectors spaces"
			return 0
		else:
			print "Belongs to this vectors spaces"
			return 1


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

