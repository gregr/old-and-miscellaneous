#! /usr/bin/python

lazyEvaluation = True

class Term(object):
    def Eval(self, rest):
        raise NotImplementedError
    def __repr__(self):
        return self.value


nil = Term()
nil.value = "nil"


class Cons(Term):
    def __init__(self, head, tail=nil):
        self.head = head
        self.tail = tail
    def Eval(self, rest):
        try:
#            ListAppend(self, rest)
#            return self.head.Eval(self.tail)
            c = self.tail
            if c is not nil:
                c = CopyList(c)
                ListAppend(c, rest)
            return self.head.Eval(c)
        except AttributeError:
            raise StopIteration
        except StopIteration:
            return self
    def __repr__(self):
        return ListRepr(self)


def ListRepr(cons):
    l = []
    while cons is not nil:
        l.append(cons.head)
        cons = cons.tail
    return "(%s)" % " ".join([str(p) for p in l])

def CopyList(cons):
    if cons is nil:
        return nil
    return Cons(cons.head, CopyList(cons.tail))

def ListTail(cons):
    tail = cons.tail
    while tail is not nil:
        cons = tail
        tail = cons.tail
    return cons

def ListAppend(cons, element):
    tail = ListTail(cons)
    tail.tail = element


class Primitive(Term):
    def __init__(self, value):
        self.value = value


class Constant(Primitive):
    def __init__(self, value):
        Primitive.__init__(self, value)
    def Eval(self, rest):
        if isinstance(rest, Cons):
            return Cons(self, Cons(rest.head.Eval(nil), rest.tail))
        if rest is not nil:
            return Cons(self, rest)
        return self


def Eval_I(rest):
    if rest is nil:
        raise StopIteration
    return rest


def Eval_K(rest):
    try:
        x = rest.head
        return Cons(x, rest.tail.tail)
    except AttributeError:
        pass
    try:
        if isinstance(rest, Cons):
            return Cons(K, Cons(rest.head.Eval(nil), rest.tail))
    except AttributeError:
        pass
    raise StopIteration


if lazyEvaluation:
    def Eval_S(rest):
        try:
            x = rest.head
            y = rest.tail.head
            z = rest.tail.tail.head
            z = Cons(z)
            x = Cons(x, z)
            y = Cons(y, z)
            return Cons(x, Cons(y, rest.tail.tail.tail))
        except AttributeError:
            pass
        try:
            if isinstance(rest, Cons):
                return Cons(S, Cons(rest.head.Eval(nil), rest.tail))
            elif isinstance(rest.tail.head, Cons):
                return Cons(S
                            ,Cons(rest.head
                                  ,Cons(rest.tail.head.Eval(nil), rest.tail.tail)))
        except AttributeError:
                pass
        raise StopIteration
else:
    def Eval_S(rest):
        try:
            x = rest.head
            y = rest.tail.head
            z = rest.tail.tail.head
            if isinstance(z, Cons):
                result = z.Eval(nil)
                if result is not z:
                    return Cons(S, Cons(x, Cons(y, Cons(result, rest.tail.tail.tail))))
            z = Cons(z)
            x = Cons(x, z)
            y = Cons(y, z)
            return Cons(x, Cons(y, rest.tail.tail.tail))
        except AttributeError:
            pass
        try:
            if isinstance(rest, Cons):
                return Cons(S, Cons(rest.head.Eval(nil), rest.tail))
            elif isinstance(rest.tail.head, Cons):
                return Cons(S
                            ,Cons(rest.head
                                  ,Cons(rest.tail.head.Eval(nil), rest.tail.tail)))
        except AttributeError:
                pass
        raise StopIteration
            

I = Primitive("I")
I.Eval = Eval_I
K = Primitive("K")
K.Eval = Eval_K
S = Primitive("S")
S.Eval = Eval_S


keychars = ["(", ")"]


def Tokenize(s):
    tokens = []
    current  = ""
    for c in s:
        if c in keychars:
            if current:
                tokens.append(current)
                current = ""
            tokens.append(c)
        else:
            current += c
    if current:
        tokens.append(current)
    return tokens


def Scan(data):
    splits = data.split()
    print "split:", splits
    tokens = []
    for s in splits:
        tokens.extend(Tokenize(s))
    return tokens


def MakeTerm(tokens, index):
    l = len(tokens)
    term = []
    while index < l:
        t = tokens[index]
        if t == ")":
            if not term:
                raise RuntimeError, "empty subterm at: %s" % index
            return term, index+1
        if t == "(":
            t, index = MakeTerm(tokens, index+1)
        else:
            index += 1
        term.append(t)
    return term, index


def Parse(data):
    tokens = Scan(data)
    tokens.append(")")
    term, index = MakeTerm(tokens, 0)
    if index != len(tokens):
        raise RuntimeError, "parsing error with tokens: %s" % tokens
    return term


keywords = {"I": I, "K": K, "S": S}


def Compile(terms):
    next = nil
    terms = terms[:]
    while terms:
        term = terms.pop()
        if isinstance(term, list):
            term = Compile(term)
        elif term in keywords:
            term = keywords[term]
        else:
            term = Constant(term)
        next = Cons(term, next)
    return next


def Build(data):
    return Compile(Parse(data))


data = "(S I I) (S I I)"
term = Parse(data)
program = Compile(term)

d = "S I I alpha"

c = Build(d)
#c = program

def e():
    global c
    c = c.Eval(nil)
    print c
