from VecSpace import *

class AffSpace:
	"""A class of vector space

	Attributes:
		vector: A vector
		vecspace: A vector space
	"""

	def __init__(self, vec=None, vs=None):
		if vec is None:
			self.vector = []
		else:
			self.vector = vec
		if vs is None:
			self.vecspace = []
		else:
			self.vecspace = vs


	def belongs(self, vec):
		self.vector
		if not (self.vecspace.belongs( \
			(np.asarray(vec) - np.asarray(self.vector)).tolist() ) == 1):
			print "Not belongs to this affine spaces"
			return 0
		else:
			print "Belongs to this affine spaces"
			return 1


	@staticmethod
	def solve(mat, vec):
		# return solution of mat x = vec
		x = np.linalg.solve(np.array(mat), np.array(vec))
		
		# return null space of mat x = 0
		U, s, V = np.linalg.svd(mat)
		null_space = np.compress(s <= TOLERANCE, V, axis=0)
		
		vecs = []
		if null_space.shape[0] != 0:
			for i in range(null_space.shape[1]):
				vecs.append(null_space[:,i])
		return AffSpace(x, VecSpace(vecs))


	def dim(self):
		return self.vecspace.dim()
