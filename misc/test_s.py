# Copyright (C) 2007 Gregory L. Rosenblatt
# All rights reserved

# <greg.uriel@gmail.com>
# http://code.google.com/p/uriel/

# This file is part of Uriel.

# Uriel is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.

# Uriel is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this library.  If not, see <http://www.gnu.org/licenses/>

#topEnv.define(Symbol("fn"), topEnv[Symbol("strict-fn")])

#prelude = [
#    makeList(Symbol("define"), Symbol("fn"), Symbol("strict-fn"))
#    ]

#makeList(Symbol("define"), Symbol("def-fn"), )

#makeList(Symbol("macro"), makeList(Symbol("name"), Symbol("params"),
#                                   Symbol("body")),
#         makeList(Symbol("define"), Symbol("name"),
#                  makeList(Symbol("params"), Symbol("body")))

#makeList(Symbol("define"), Symbol("def-macro"),


def testNestedIfExpr(pred1=Symbol("true"), pred2=Symbol("true")):
    return makeList(Symbol("if"), pred1, "it was true first",
                    makeList(Symbol("if"), pred2,
                             "it was true second but not first",
                             "it was false both times"))

#expr = testNestedIfExpr(Symbol("false"), Symbol("true"))
#expr = makeList(Symbol("eq?"),
#                makeList(Symbol("quote"), Symbol("one")),
#                makeList(Symbol("quote"), Symbol("one")))
#expr = makeList(Symbol("equal?"), "one", "bone")
#expr = makeList(applyPrintArgs, 1, 2, 3)
#expr = makeList(Symbol("quote"), makeList(Symbol("if"), 1, Symbol("2"), 3))
equalityTest = makeList(Symbol("fn"), makeList(Symbol("a"), Symbol("b")),
                        makeList(Symbol("if"), makeList(Symbol("equal?"), Symbol("a"),
                                                   Symbol("b")),
                                 "a == b", "a != b"))

#expr = makeList(equalityTest, 2, 3)

# define? but how do you get the result into the current env...
#(syntax-fn (make-fn name-and-params body)
# (let (name (head name-and-params)) (params (tail name-and-params))
#  (letrec (name (make-fn params body))
#   name)))


factorial = makeList(Symbol("letrec"), makeList(makeList(Symbol("factorial"),
                     makeList(Symbol("fn"), makeList(Symbol("n")),
                              makeList(Symbol("if"),
                                       makeList(Symbol("<"),
                                                Symbol("n"),
                                                1),
                                       1,
                                       makeList(Symbol("*"),
                                                Symbol("n"),
                                                makeList(Symbol("factorial"),
                                                         makeList(Symbol("-"),
                                                                  Symbol("n"),
                                                                  1))))))),
                     Symbol("factorial"))

expr = makeList(factorial, 7)


lst = makeList(Symbol("fn"), Symbol("args"), Symbol("args"))

#expr = makeList(lst, 1, 2, 3)

default = makeList(Symbol("fn"), makeList(Symbol("value")),
                   makeList(Symbol("fn"), makeList(Symbol("arg"),
                                                   Symbol("env")),
                            makeList(Symbol("if"),
                                     makeList(Symbol("eq?"),
                                              Symbol("arg"), Symbol("nil")),
                                     Symbol("value"),
                                     Symbol("arg"))))

testDefaults = makeList(Symbol("fn"), makeList(Symbol("a"),
                                               makeList(Symbol("b"),
                                                        makeList(default,
                                                                 52))),
                        makeList(Symbol("pair"), Symbol("a"), Symbol("b")))

#expr = makeList(testDefaults, 1)

#expr = makeList(default, "ehh?")

#expr = makeList(Symbol("eval"), makeList(Symbol("quote"),
#                                         makeList(Symbol("quote"),
#                                         makeList(Symbol("+"), 4, 5))), topEnv)

def test():
    return True
