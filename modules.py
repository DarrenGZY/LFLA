class VecSpace:
    def __init__(self, vectors):
        self.vectors = vectors


class InSpace:
    def __init__(self, vectors, matrix):
        self.vectors = vectors
        self.matrix = matrix

class AffSpace:
    def __init__(self, vector, vecspace):
        self.vector = vector
        self.vecspace = vecspace
