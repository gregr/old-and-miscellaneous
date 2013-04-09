#ifndef axiom_env_H_ // todo: language utils
#define axiom_env_H_

#include <map>
#include <stdexcept>

struct EnvKeyError : public std::runtime_error {
    EnvKeyError() : std::runtime_error("") {}
};

#define ENV_GET(IT_TYPE) \
    typename EnvMap::IT_TYPE itr = bindings_.find(k);   \
    if (itr == bindings_.end()) {                       \
        if (parent_ != 0) return parent_->get(k);       \
        else throw EnvKeyError();                       \
    } else return itr->second;

template <typename KeyType, typename ValType>
class Env {
    typedef std::map<KeyType, ValType> EnvMap;
    EnvMap bindings_;
    const Env* parent_;
public:
    explicit Env(const Env* parent=0) : parent_(parent) {}
    //    void add(const KeyType& k, const ValType& v) { bindings_[k] = v; }
    void add(const KeyType& k, const ValType& v) {
        typename EnvMap::const_iterator itr = bindings_.find(k);
        if (itr != bindings_.end()) throw EnvKeyError();
        bindings_[k] = v;
    }
    void forceAdd(const KeyType& k, const ValType& v) { bindings_[k] = v; }
    const ValType& get(const KeyType& k) const { ENV_GET(const_iterator) }
    //    ValType& get(const KeyType& k) { ENV_GET(iterator) }
};

#endif
