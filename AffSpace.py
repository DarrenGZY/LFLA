from VecSpace import *

class AffSpace:
	"""A class of vector space

	Attributes:
		vector: A vector
		vecspace: A vector space
	"""

	def __init__(self, vec, vs):
		self.vector = vec
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


	def solve(self, mat, vec):
		v = np.linalg.solve(np.array(mat), np.array(vec))
		return v
		# ??? what is Gauss elimination methods??


	def dim(self):
		# ??? what is dim of a vecspace #
		return len(self.vecspace.vectors)
