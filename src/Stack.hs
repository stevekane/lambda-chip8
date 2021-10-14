module Stack where

type Stack = []

push        = (:)
pop (a : s) = (a,s)
pop []      = error "Pop empty not supported"