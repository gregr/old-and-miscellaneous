################################################################
# graphing dependencies (probably not needed)
# class Vertex(object):
#     def __init__(self, name):
#         self.name = name # array-index for O(1) data access
#         self.edges = []
#     def connectAndSort(self, path, sorted):
#         for e in self.edges:
#             # check in path
#             # check if already seen

# # vertices don't directly contain their data
# # instead their names are used to index into a table of data
# # this is to decouple the vertices from any given algorithm-specific data
# class ConnectSortVertexData(object): # todo: slots?
#     def __init__(self):
#         self.seen = False
#         self.component = None
#         self.pathPos = None

# class Component(object):
#     def __init__(self):
#         self.vertices = set()

# class EdgePath(object):
#     def __init__(self):
#         self.path = []
#         self.vertexPos = {}
#     def getPos(self, vert):
#         pos = self.vertexPos.get(vert)
#         if pos is None: return None
#         # do something with pos
#         # return something

# class PendingSet(object):
#     def __init__(self, pend):
#         self.pending = list(pend)
#         self.done = set()
#     def __iter__(self): return self
#     def next(self):
#         while self.pending:
#             x = self.pending.pop()
#             if x not in self.done:
#                 self.done.add(x)
#                 return x
#         raise StopIteration

# def connectAndSort(verts):
#     pending = PendingSet(verts)
#     sorts = []
#     for v in pending:
#         sorted = []
#         connected = v.connectAndSort(set(), sorted)
#         sorts.append(sorted)
