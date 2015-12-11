from Core import *

class InSpace:
	"""A class of inner product space

    Attributes:
        vectors: A list of vectors
        matrix: A matrix
    """
    
	def __init__(self, vecs=None, mat=None):
		if vecs is None:
			self.vectors = []
		else:
			self.vectors = vecs
		if mat is None:
			self.matrix = []
		else:
			self.matrix = mat


	def product(self, u, v):
		### Runtime check
		if len(u) != len(v):
			print "Please check dimensions of u and v"
			sys.exit(0)
		if self.vectors is None:
			print "Please initialize a list of vectors"
			sys.exit(0)
		if self.matrix is None:
			print "Please initialize matrix" 
			sys.exit(0)

		 # WARNING: this test is not so rubust
		 # when vectors has only one vector, 
		 # then the length simply return the dimension of the vector
		if len(self.vectors) != len(self.matrix[0]):
			print "Please check dimensions of vectors and matrix"
			sys.exit(0)

		lvecs = np.array(self.vectors)
		u = np.array(u)
		_u = np.linalg.solve(lvecs,u)
		v = np.array(v)
		_v = np.linalg.solve(lvecs,v)
		return np.transpose(_u) * self.matrix * _v

