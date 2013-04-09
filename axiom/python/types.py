# type(lit :: t) == t

# type(var) == tenv{var}

# # demand, ultimatum, correction, assumption? not sure what to call this yet
# typeoverride x t
# type == t
# teval ==> teval(x), warn(type(x) = t)

# typeannotation x t
# type == t
# teval ==> teval(x), type(x) = t

# construct t xs
# type == taggedtype(t, type(xs))
# teval ==> teval(xs), taggedtype(t, type(xs)) = taggedtyperequired(t, type(xs))

# let x v r
# type == type(r)
# teval ==> tenv{x=?}, teval(v), teval(r), type(x) = type(v)

# abstract x r
# type == type(x) -> type(r)
# teval ==> tenv{x=?}, teval(r)

# r == apply f x
# type == type(r)
# teval ==> teval(f), teval(x), type(f) = >type(x) -> type(r)

# case x
#  p1 b1
#  p2 b2
#  ...
# type == [type(b1) type(b2) ...]
# teval ==> teval(x), teval(b...), type(x) = pattype(p...)

# r == call f xs
# type == type(r)
# teval ==> teval(f), teval(xs), type(f) = >type(xs) --> type(r)

# delay x
# type == lazy(type(x))
# teval ==> teval(x)

# force x
# type == strict(type(x))
# teval ==> teval(x)

# assign x y
# type == unit
# teval ==> teval(x), teval(y), field(x) = type(y)

# # todo: record types

# List A ::= [] | (A a) => (:) a (List A)

# (:) :: (A a) => a -> List B -> List (A||B)


# List A ::= [] | (A a) => (:) a (List A)

# (:) :: (A a) => a -> List A -> List A


# g f x y = [(x+3) (f x y)]

# x ::< Num

# ((a && b) || c) && (d && (e || (f && g)))

# f x y = show (x + y)

# a :: Num
# b :: Num
# (+) :: (Num (a b), [a b] (c)) => a -> b -> c
# show :: Show x => x -> String

# f :: x -> y -> r

# x :: x?
# y :: y?
# r :: r?
# (+) :: x -> y -> r

# Num (a b) => a -> b -> [a b] :: x -> y -> r
# Num a => a :: x
# Num b => b :: y
# Num (a b) => [a b] :: r

# x :: Num a => a
# y :: Num b => b
# r :: Num (a b) => [a b]
# f :: Num (a b) => a -> b -> [a b]

# [Int]||[String] == [Int String]
# [Int String]&&[String Char] == [String]
# _ denotes an unknown (extensible) type set
# Num :::= _||[Int Float]
# Show :::= _||[Int String Char Float]
# the _ in each case is considered different
# A :::= _
# B :::= _
# A and B denote two different type sets (even if they end up equivalent)

#def t_eq(a, b): return a.tval() is b.tval()

class Type(object):
    def __init__(self, name): self.name = name
    def __repr__(self): return '%s(%r)'%(self.__class__.__name__, self.name)
    def __str__(self): return str(self.name)
    def tval(self): return self
    def mentions(self, tv): return False
    def constrain(self, ty, attr, tenv):
        if not ty.contains(self):
            if type(ty) is ExtensibleTypeSet: tenv.addDep(self, ty, attr)
            assert False, (self, ty)
#    def unify(self, ty): assert t_eq(self, ty), (self, ty)

class TypeConstraint(Type):
    def __init__(self, name):
        Type.__init__(self, name)
        self.final = None # will point to a type or another TypeVar when unified
        self.constraint = Any
    def __repr__(self):
        return 'TypeVar(%r, %r, %r)'%(self.name, self.type, self.constraint)
    def tval(self):
        if self.final is None: return self
        return self.final.tval()
    def mentions(self, tv): return tv is self
    def constrain(self, ty, attr, tenv):
        if self.final is None:
            self.constraint = ty.intersect(self.constraint)
            assert self.constraint is not Empty, self
        else: self.final.constrain(ty, attr, tenv)

class TypeVar(Type):
    def __init__(self, name):
        Type.__init__(self, name)

# todo: type cons interaction
class TypeCons(Type):
    def __init__(self, name): Type.__init__(self, name)
    def __eq__(self, ty): return self is ty.tval()
    def toSet(self): return ConcreteTypeSet([self])
    def contains(self, ty): return self == ty.tval()
    def intersect(self, ty):
        if not ty.contains(self): return Empty
        return self
#        else: return ConcreteTypeSet([self]) # todo: type cons intersection?

class TypeCons(Type): # todo: co/contra-variance
    def __init__(self, name, args):
        self.name = name
        self.args = args
    def mentions(self, tv):
        for arg in self.args:
            if arg.mentions(tv): return True
        return False
    def unify(self, ty):
        assert isinstance(ty, TypeCons), type(ty)
        assert self.name is ty.name, (self.name, ty.name)
        assert len(self.args) == len(ty.args), (len(self.args), len(ty.args))
        for a,b in izip(self.args, ty.args): unify(a, b)
    def __repr__(self): return 'TypeCons(%r, %r)'%(self.name, self.args)
    def __str__(self): return '(%s)'%' '.join([str(self.name)]+
                                              map(str, self.args))

class TypeSet(Type):
    def toSet(self): return self

class ConcreteTypeSet(TypeSet):
    def __init__(self, members): self.members = set(members)
    def __eq__(self, ty): return ty.toSet().equiv(self.members)
    def __repr__(self): return '%s(%r)'%(self.__class__.__name__, self.members)
    def __str__(self): return '[%s]'%' '.join(str(m) for m in self.members)
    def equiv(self, mems): return self.members == mems
    def mentions(self, tv): return tv in self.members
    def constrain(self, ty):
        assert False, (self, ty)
    def contains(self, ty): # todo: type cons interaction
        if type(ty) is ConcreteTypeSet: return ty.members.issubset(self.members)
        elif type(ty) is TypeLit: return ty in self.members
        else: return False
    def intersect(self, ty):
        if type(ty) is ConcreteTypeSet:
            return ConcreteTypeSet(ty.members.intersection(self.members))
        elif type(ty) is TypeLit: return ty.intersect(self)
        else: return ty.intersect(self) #pass # todo: join with AbstractSet

Empty = ConcreteTypeSet([])

class AbstractTypeSet(TypeSet):
    def constrain(self, ty, *_): assert ty.contains(self), (self, ty)

class AnyTypeSet(AbstractTypeSet):
    def __init__(self): AbstractTypeSet.__init__(self, "Any")
    def equiv(self, _): return False
    def contains(self, ty): return True
    def intersect(self, ty): return ty
Any = AnyTypeSet() # only need one

class TypeDep(Type):
    def __init__(self, deps): self.deps = deps
    def __repr__(self): return 'TypeDep(%r)'%self.deps
    def __str__(self): return '?%s'%str(self.deps)

# testing
Int = TypeLit('Int')
Float = TypeLit('Float')
Char = TypeLit('Char')
String = TypeLit('String')
Unit = TypeLit('Unit')

Num = ExtensibleTypeSet('Num', [Int, Float])
Show = ExtensibleTypeSet('Show', [Int, Char, String])
Strange = ConcreteTypeSet([Char, Float, Unit])

# #def _test():
# t1 = ts_union(Num, Show)
# t2 = ts_union(Strange, t1)
# t3 = ts_inter(Strange, t1)
# t4 = ts_inter(Num, Show)
# t5 = ts_union(Strange, t4)
# t6 = ts_inter(Strange, t4)
# tests = [t1,t2,t3,t4,t5,t6]
# for tn in tests:
#     print tn, '==>'
#     print TypeSet(False, tn.getMembers())
# #    return tests
