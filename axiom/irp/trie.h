#ifndef irp_trie_H_
#define irp_trie_H_

#include <istream>
#include <map>
#include <vector>
#include <string>
//#include <iostream> // testing

namespace irp {

    template <typename ValueType>
    class Trie {
        struct Branch;
        typedef std::map<char, Branch> BranchMap;
        struct Branch {
            Branch() : valid(false) {}
            BranchMap next;
            ValueType val;
            bool valid;
        };
        Branch root_;
    public:
        void add(const std::string& key, const ValueType& val) {
            Branch* cur = &root_;
            std::string::const_iterator itr = key.begin(), end = key.end();
            for (; itr != end; ++itr) {
                typename BranchMap::iterator next = cur->next.find(*itr);
                if (next == cur->next.end()) {
                    cur = &cur->next.insert(std::make_pair(*itr, Branch()))
                        .first->second;
                } else cur = &next->second;
            }
            cur->val = val;
            cur->valid = true;
        }
        ValueType* find(std::istream& in) {
            Branch* cur = &root_;
            std::vector<char> stack;
            while (in) {
                char ch = in.get();
                stack.push_back(ch);
                typename BranchMap::iterator next = cur->next.find(ch);
                if (next == cur->next.end()) {
                    while (!stack.empty()) {
                        in.putback(stack.back());
                        stack.pop_back();
                    }
                    break;
                }
                cur = &next->second;
            }
            if (cur->valid) return &cur->val;
            else return 0;
        }
    };
}

#endif
