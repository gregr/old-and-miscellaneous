class Location(object): # sectors, planets, stations, etc.
    def __init__(self):
        self.links = []
        self.contents = []
    def add(self, entity, created=False):
        self.contents.append(entity)
        self.onAdd(entity, created)
    def remove(self, entity, destroyed=False):
        self.contents.remove(entity)
        self.onRemove(entity, destroyed)
    def onAdd(self, obj, created):
        for entity in self.contents:
            entity.onDetectArrive(obj, created)
    def onRemove(self, obj, destroyed):
        for entity in self.contents:
            entity.onDetectDepart(obj, destroyed)

limbo = Location()

class Entity(object):
    def __init__(self, loc=limbo, **kwargs):
        self.location = loc
        self.__dict__.update(kwargs)
    def depart(self):
        self.location.remove(self)
        self.onDepart()
    def arrive(self, target):
        self.location = target
        target.add(self)
        self.onArrive(target)
    def move(self, target): # test for ability to move is done elsewhere?
        self.depart()
        self.arrive(target)
    def onDefend(self, attack):
        pass
    def onDepart(self):
        pass
    def onArrive(self):
        pass
    def onDetectArrive(self, entity, created):
        pass
    def onDetectDepart(self, entity, destroyed):
        pass

# class State(object):
#     def __init__(self, **kwargs):
#         for k, v in kwargs.iteritems():
#             setattr(self, k, [, v])

class Ship(Entity):
    def __init__(self, *args, **kwargs):
        Entity.__init__(self, *args, **kwargs)
        self.pilot = None
        self.towing = None
    def onDefend(self, attack):
        self.shields
        self.fighters
        return attack
    def onDepart(self): # tow
        if self.towing:
            self.towing.depart()
    def onArrive(self, target): # pass target in case arriving causes movement
        if self.towing:
            self.towing.arrive(target)
    def onDetectArrive(self, entity, created):
        if self.pilot:
            self.pilot.onDetectArrive(entity, created)
    def onDetectDepart(self, entity, destroyed):
        if self.pilot:
            self.pilot.onDetectDepart(entity, destroyed)

class Agent(object): # players, AI, etc.
    def __init__(self):
        pass
    def onDetectArrive(self, entity, created):
        pass
    def onDetectDepart(self, entity, destroyed):
        pass
