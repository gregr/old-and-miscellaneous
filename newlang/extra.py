def make_seq(t, i):
    if i >= tuple_length(t):
        return null
    return make_typed(symbol("seq"), _tuple(t, i))

def _seq_tuple(s):
    return tuple_get(s, 0)

def _seq_index(s):
    return tuple_get(s, 1)

def _seq_cur(s):
    return tuple_get(_seq_tuple(s), _seq_index(s))

def _seq_nxt(s):
    return make_seq(_seq_tuple(s), _seq_index(s)+1)

def seq_cur(s):
    s = typed_value(s)
    return _seq_cur(s)

def seq_nxt(s):
    s = typed_value(s)
    return _seq_nxt(s)

def isSeq(v):
    return isNull(v) or isType(symbol("seq"), v)

# def tuple_print(a):
#     print "<tuple-%d %s>" % (tuple_length(a), a[1])

#a = make_tuple(3)
