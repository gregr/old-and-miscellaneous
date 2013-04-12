import qualified Data.Set as S

data Dg = Node {name::String, children::[Dg]}
          deriving (Show, Eq, Ord)

ta = Node "a" [tc, te]
tb = Node "b" [td]
tc = Node "c" [td, tg]
td = Node "d" [te, tf]
te = Node "e" [tf, tg]
tf = Node "f" [tg]
tg = Node "g" []

graph = [td, tg, tf, tc, te, ta, tb]

sort nodes = ordered
    where (ordered, _, _) = depthSort' nodes S.empty S.empty
          sort' nd seen finished =
              if S.member nd seen then error "cycle detected" else
                  if S.member nd finished then ([], seen, finished) else
                      (nd : nds, seen, S.insert nd fn)
                          where (nds, sn, fn) = depthSort' (children nd)
                                                (S.insert nd seen) finished
          depthSort' ns seen finished = foldl inner ([], seen, finished) ns
              where inner (nds, sn, fin) n =
--cycle detection won't work with foldl due to laziness (stack will overflow)
                        if S.member n sn then error "cycle detected"
                        else (nds'++nds, sn', fin')
                            where (nds', sn', fin') = sort' n sn fin

test = map name $ sort graph
