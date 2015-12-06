from Core import *

class VecSpace:
	"""A class of vector space

	Attributes:
		vectors: A list of vectors
	"""

	def __init__(self, vecs=[]):
		vectors = []
		
		
                # test for all vectors in the vecs list
		for i in range(len(vecs)):
			vectors.append(vecs[i])
			# if # of counts > # of vecs,
			# the new vector is linear independent with base vectors
			if not linearIndependent(vectors):
				del vectors[-1]
                self.vectors = vectors


	def belongs(self, vec):
	        if self.vectors == []:
                    return 0

                self.vectors.append(vec)
		if linearIndependent(self.vectors):
			print "Not Belongs to this vectors spaces"
			return 0
		else:
			print "Belongs to this vectors spaces"
			del self.vectors[-1]
			return 1


	def __add__(self, vecspace):
		temp = self.vectors + vecspace.vectors
		return VecSpace(temp)


	def dim(self):
		return len(self.vectors)


	def basis(self):
		return self.vectors

