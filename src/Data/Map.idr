module Data.Map

export
data Map : (k : Type) -> (v : Type) -> Type where
    Nil  : Map k v
    (::) : (k,v) -> Map k v -> Map k v

export
empty : Map k v
empty = Nil

export
lku : Eq k => k -> Map k v -> Maybe v
lku _ Nil          = Nothing
lku k ((k',v')::r) = 
    if k == k' then Just v' else lku k r

export
ins : Eq k => k -> v -> Map k v -> Map k v
ins k v Nil          = [(k,v)]
ins k v ((k',v')::r) =
    if k == k' then (k,v)::r else (k',v')::ins k v r

export
del : Eq k => k -> Map k v -> Map k v
del _ Nil          = Nil
del k ((k',v')::r) =
    if k == k' then r else (k',v')::del k r