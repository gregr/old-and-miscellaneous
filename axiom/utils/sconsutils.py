import sys
sys.path += ['/usr/lib/scons']
from SCons.Script import *

posix = 'PLATFORM_POSIX'
win32 = 'PLATFORM_WIN32'

gcc = 'COMPILER_GCC'
msvc = 'COMPILER_MSVC'

platforms = dict(posix=posix, win32=win32)
compilers = dict(gcc=gcc, cl=msvc)

platform = platforms[Platform().name]
compiler = compilers[Environment()['CC']]

def _gccBuildOptions(debug=False):
    opts = dict(CPPDEFINES=['_REENTRANT'],
                CCFLAGS=['-ansi', '-pedantic', '-std=c99', '-Wall', '-W',
                         '-Wstrict-aliasing=2', '-fstrict-aliasing'])
    if debug: opts['CCFLAGS'] += ['-g']
    return opts

def _msvcBuildOptions(debug=False):
    opts = dict(CPPDEFINES=[], CCFLAGS=['/W4', '/EHsc', '/GR'])
    if debug: opts['CCFLAGS'] += ['/MDd', '/Od', '/RTC1']
    else: opts['CCFLAGS'] += ['/MD']
    return opts

def _buildOptions(compiler, platform, debug=False):
    if compiler == gcc: opts = _gccBuildOptions(debug)
    elif compiler == msvc: opts = _msvcBuildOptions(debug)
    else: raise RuntimeError, ('unknown compiler: %s' % compiler)
    #if not debug: opts['CPPDEFINES'] += ['NDEBUG']
    if platform == win32: opts['CPPDEFINES'] += ["_WIN32_WINNT=0x0501"]
    return opts

class ObjCache(object):
    def __init__(self, env, format=None):
        self.env = env
        if format is None: format = '%s'
        self.format = format
        self.objs = {}
    def get(self, objDef):
        nameRoot = objDef.name.split('.c')[0]
        obj = self.objs.get(nameRoot)
        if obj is None:
            obj = objDef.build(self.env, self.format%nameRoot)
            self.objs[nameRoot] = obj
        return obj

class ObjDef(object):
    def __init__(self, name, defs=[]):
        self.name = name
        self.defs = defs
    def build(self, env, objName):
        if self.defs:
            nenv = env.Copy()
            nenv.Append(CPPDEFINES=self.defs)
        else: nenv = env
        return nenv.Object(objName, self.name)

class SrcDeps(object):
    def __init__(self):
        self.deps = {}
    def get(self, src):
        return self.deps.get(src, [])
    def add(self, src, dep):
        self.deps.setdefault(src, []).append(dep)

def makeEnv(debug=False):
    env = Environment(**_buildOptions(compiler, platform, debug))
    if debug: format = '%s-d'
    else: format = None
    env.objCache = ObjCache(env, format)
    env.libDeps = SrcDeps()
    return env

def _makeObjDefs(srcs):
    plainSrcs = tuple(ObjDef(src) for src in srcs if isinstance(src, str))
    depSrcs = tuple(ObjDef(*src) for src in srcs if type(src) in (tuple, list))
    return plainSrcs + depSrcs

def _makeLib(name, srcs, objs, env, objCache):
    srcs = _makeObjDefs(srcs)
    return env.Library(name, map(objCache.get, srcs)+list(objs))[0]

def _makeProg(name, srcs, objs, moreLibs, env, objCache, libDeps):
    srcs = _makeObjDefs(srcs)
    pathLibs = list(set(reduce((lambda a,b:a+b),
                               map(libDeps.get, [s.name for s in srcs]))))
    if pathLibs:
        paths, libs = zip(*pathLibs)
        paths = list(p for p in set(paths) if p is not None)
    else:
        paths, libs = [], []
    return env.Program(name, map(objCache.get, srcs)+list(objs),
                       LIBPATH=paths, LIBS=list(libs)+list(moreLibs))

releaseEnv = makeEnv()
debugEnv = makeEnv(True)

def getEnv(debug):
    if debug: return debugEnv
    else: return releaseEnv

def setDeps(*srcDeps):
    for sd in srcDeps:
        if len(sd) == 3:
            src, relDep, dbgDep = sd
        else:
            src, relDep = sd
            dbgDep = relDep
        releaseEnv.libDeps.add(src, relDep)
        debugEnv.libDeps.add(src, dbgDep)

def makeLib(name, srcs, objs=[], debug=False):
    e = getEnv(debug)
    return _makeLib(name, srcs, objs, e, e.objCache)

def makeProg(name, srcs, objs=[], libs=[], debug=False):
    e = getEnv(debug)
    return _makeProg(name, srcs, objs, libs, e, e.objCache, e.libDeps)

def require(sconsDir): pass
