class DepTracker(object):
    def __init__(self):
        self.dependencies = {}
        self.dependents = {}
    def add(self, dependent, dependencies):
        direct = dependencies
        dependencies = reduce(set.union,
                              (self.dependents.get(dependency,
                                                   ((), set([dependency])))[1]
                               for dependency in dependencies),
                              set())
        dependencies.discard(dependent)
        prevDependents = self.dependencies.get(dependent)
        finished = set()
        if prevDependents is not None:
            del self.dependencies[dependent]
            for prev in prevDependents:
                dprev = self.dependents[prev][1]
                dprev.remove(dependent)
                dprev |= dependencies
                if not dprev: finished.add(prev)
        else: prevDependents = set()
        components = []
        if not dependencies: # extract sorted strongly-connected components
            todo = [direct]
            component = [dependent]
            while True:
                while todo:
                    direct = todo.pop()
                    for fin in direct:
                        if fin in finished:
                            finished.remove(fin)
                            component.append(fin)
                            todo.append(self.dependents[fin][0])
                            del self.dependents[fin]
                components.append(component)
                if not finished: break
                next = finished.pop()
                todo = [self.dependents[next][0]]
                del self.dependents[next]
                component = [next]
        else:
            self.dependents[dependent] = (direct, dependencies)
            for dependency in dependencies:
                dependents = self.dependencies.setdefault(dependency, set())
                dependents |= prevDependents
                dependents.add(dependent)
        return components # for which all dependencies are satisfied

def _test():
    deps = DepTracker()
    for d, ds in [
        ('dumb', []),
        ('a', ['b', 'c']),
        ('f', ['g']),
        ('g', ['c']),
        ('c', ['a']),
        ('b', ['a']),
        ]:
        print d, ds
        print deps.add(d, set(ds))
        print deps.dependents
        print
    print deps.dependencies
