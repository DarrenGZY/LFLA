from Core import *
import numpy as np

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
			self.matrix = np.matrix([[]])
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
	        
                (row, col) = self.matrix.shape
                if len(self.vectors) != row:
			print "Please check dimensions of vectors and matrix"
			sys.exit(0)

		lvecs = np.array(self.vectors)
		u = np.array(u)
		_u = np.linalg.solve(lvecs,u)
		v = np.array(v)
		_v = np.linalg.solve(lvecs,v)
		product = np.matrix(_u) * self.matrix * np.transpose( np.matrix(_v))

                (p_row, p_col) = product.shape
                if (p_row != 1) or (p_col != 1):
                    print "dimension problems in inner product"
                    sys.exit(0)
                return product.item((0,0))

	def dim(self):
		return len(self.vectors)


