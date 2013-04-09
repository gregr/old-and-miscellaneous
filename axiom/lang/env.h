#ifndef axiom_lang_env_H_
#define axiom_lang_env_H_

#include <map>
#include <vector>
#include <string>
#include "value.h"

namespace lang {

    typedef std::string Name;

    // possibly track the stack offset difference from parent? probably not
    struct Env {
        Env(const Env* parent=0) : parent_(parent), stacked_(0) {}
        void add(const Name& name, const Val& v) {
            assert(binds_.find(name) == binds_.end()); // todo: use nicer assert
            binds_[name] = vals_.size();
            vals_.push_back(v);
        }
        const Value& get(const Name& name) const {
            EnvBinds::const_iterator itr = binds_.find(name);
            if (itr == binds_.end())
                if (parent_ != 0)
                    return parent_->get(name);
                else
                    throw runtime_error(std::string("unbound variable: ")
                                        + name);
            else
                return vals_[*itr];
        }
    private:
        typedef std::map<Name, unsigned> EnvBinds;
        typedef std::vector<Value> IndexedVals;
        EnvBinds binds_;
        IndexedVals vals_;
        const Env* parent_;
        //        unsigned stacked_; // how many are on the stack (for multi-offsets)
    };
}

#endif
