from Core import *

class VecSpace:
	"""A class of vector space

	Attributes:
		vectors: A list of vectors
	"""

	def __init__(self, vecs=None):
		if vecs is None:
			self.vectors = [] 
		else:
			# test for all vectors in the vecs list
			self.vectors = [] 
			for i in range(len(vecs)):
				self.vectors.append(vecs[i])
				# if # of counts > # of vecs,
				# the new vector is linear independent with base vectors
				if not linearIndependent(self.vectors):
					del self.vectors[-1]


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
		return VecSpace(temp)


	def dim(self):
		return len(self.vectors)


	def basis(self):
		return self.vectors

