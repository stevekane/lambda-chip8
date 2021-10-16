module Stack where

-- | an unsafe an silly stack implementation
type Stack = []

push        = (:)
pop (a : s) = (a,s)
pop []      = error "Pop empty not supported"