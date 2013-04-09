from syntax import Parser, ParseError, newOperator
from data import Env, EnvName, symbol, alias_

class ModuleError(StandardError): pass

class ModuleManager(object):
    def __init__(self):
        self.modules = {}
        self.pending = set()
    def get(self, name, rootEnv, onNew):
        if name in self.pending:
            raise ModuleError, ("module '%s' is self-dependent" % name)
        mod = self.modules.get(name)
        if mod is None:
            mod = Module(name, rootEnv)
            self.modules[name] = mod
            self.pending.add(name)
            if onNew(mod): self.finish(name)
        return mod
    def finish(self, name): self.pending.discard(name)

def tryExtend(modName, env, name, value):
    v = env.getLocal(name)
    if v is None: env.extend(name, value)
    else: raise ModuleError, ("implicit redefinition of '%s' in module '%s'"
                              % (name, modName))

class Module(object):
    public = EnvName(symbol('public'))
    private = EnvName(symbol('private'))
    modes = (public, private)
    def __init__(self, name, rootEnv):
        self.name = name
        self.currentMode = self.public
        self.env = rootEnv
        self.senv = Env()
        self.ops = Env()
        self.namesByMode = dict((mode, set()) for mode in self.modes)
    def getMode(self): return self.currentMode
    def setMode(self, mode):
        if mode not in self.namesByMode:
            raise ModuleError, ("'%s' is not a valid mode" % mode)
        self.currentMode = mode
    def addName(self, sym):
        name = EnvName(sym)
        nameSet = self.namesByMode[self.currentMode]
        if name not in nameSet: nameSet.add(name)
        return name
    def defVar(self, sym, value):
        name = self.addName(sym)
        alias = self.senv.get(name)
        if alias is None:
            alias = alias_(sym)
            self.senv.extend(name, (alias, False))
        else:
            if alias[1]: raise ModuleError, ("'%s' already defined as syntax"
                                             % name)
            alias = alias[0]
        if value is not None:
            tryExtend(self.name, self.env, EnvName(alias), value)
        return alias
    def defSyntax(self, sym, value, asSyntax=True):
        name = self.addName(sym)
        tryExtend(self.name, self.senv, name, (value, asSyntax))
    def defOp(self, sym, *args):
        self.ops.extend(EnvName(sym), newOperator(sym, *args))
    def modeSyms(self, mode=None):
        if mode is None: mode = self.public
        return [name.sym for name in self.namesByMode[mode]]
    def checkName(self, sym, srcMode=None):
        if srcMode is None: srcMode = self.public
        name = EnvName(sym)
        if name not in self.namesByMode[srcMode]:
            for mode, nameSet in self.namesByMode.iteritems():
                if mode is not srcMode and name in nameSet:
                    msg = ("'%s' is a '%s' name in module '%s'"
                           % (name, mode, self.name))
                    raise ModuleError, msg
            msg = ("module '%s' does not define the name '%s'"
                   % (self.name, name))
            raise ModuleError, msg
        return name
    def include(self, mod, syms, srcMode=None, tgtMode=None):
        if tgtMode is None: tgtMode = self.currentMode
        savedMode = self.currentMode
        self.currentMode = tgtMode
        for symSrc, symTgt in syms:
            name = mod.checkName(symSrc, srcMode)
            v = mod.senv.get(name)
            if v is not None: self.defSyntax(symTgt, *v)
            op = mod.ops.get(name)
            if op is not None: self.ops.extend(name, op)
        self.currentMode = savedMode
    def lookup(self, sym, mode=None):
        name = self.checkName(sym, mode)
        v = self.senv.get(name)
        if v is None or v[1]: return None
        return self.env.get(EnvName(v[0]))

# ideas:
# declare new env in the middle of a module (shadow module)
# re-evaluate code of specific old definitions in the new env
