#envstack_peek
#envstack_push env
#envstack_pop
this_env # cur_senv not needed?
env_new env
env_add env sym val
get_module
module_env

var
unboxed
newtag ... belongs with envs?
construct
unpack/pack
gettag
seq
proc
# simply group adjacent applications? primitives curried by default too
#call # only on known procs?
#apply # what data does it take?
# explicit apply could be useful as an assertion about the number of arguments
switch discriminant defaultCont [(tag|[unboxed val])* cont]*
throw
catch

expand form senv env
eval form senv env

{Unit}
{Nil}
{Cons head tail}
{Symbol symdesc}
{SynClo senv frees form}

Int
Float
Char
String

Array? # hidden?
array_new
array_length
array_unpack/pack

?
True
False

{Macro proc}
{Proc ...} # hidden?

{GlobalDB tags modules multimethod_tables?...}
# alternatively, simply have Kernel be a general dictionary with keys: tags, modules, etc. and don't call it kernel... db of some sort? parser hooks? meta-circular debugging and expansion history? tracer hooks? hooks... maybe call it Kernel after all?

# how are modules loaded?

{Context globaldb module senv env} # and use unpacking for accessors
{Module (path dependencies dependants) exports senv env} # module db?
__context
#this_mod
#this_env
#mod_senv mod
#mod_env mod
env_new env
env_add env sym val
env_get env sym conseq altern?

gensym
alias

update
 delay
 force

threads/processes/signals?

ffi?:
networking
filesystem/stdio/os

deferred compiler/interpreter (compile means eval for pure interpreters):
given a list of inputs assumed present at compile-time;
  causes some input ops to become pure
  default list includes module paths (explicit exclusion allows late-loading)
eval definitions, compute dependencies
postpone eval of pure value exprs until demanded, but compute dependencies
compile effectful exprs (after demanding their dependencies)
eval pure values when expansion requires it
no need to compile unneeded definitions/pure exprs
if expansion relies on runtime input, compile the continued expansion/eval;
  you end up having to compile interpreter/jit compiler into the executable
Kernel db usage (multimethods, record fields of tags, etc.) in exprs implies dynamic switch (can be inlined during proc specialization; can be made into static switch using type information)

