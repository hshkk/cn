||| Memoized algorithms.
||| * `f`: Fibonacci sequence (tiling a 1 x n board)
||| * `s`: Maximum sum of nonconsecutive elements
module Control.Memoization

import Control.Monad.State
import Data.Map
import Data.SortedMap

||| The memoization type.
||| @ s The type of identifier for each cached computation.
||| @ a The type of stateful computations.
export
M : (s : Type) -> (a : Type) -> Type
M s a = State (Map s a) a

||| Given a 1 x `n` board...
||| ```
||| +---+---+---------+---+---+
||| |   |   |   ...   |   |   |
||| +---+---+---------+---+---+
|||   1   2     ...    n-1  n
||| ```
||| ...and tiles of size 1 x 1 and 1 x 2, what is the total number of ways the 
||| board could be fully covered (that is, *tilings*) using 1 x 1 and 1 x 2 tiles?
|||
||| e.g., for a 1 x 3 board, we have three possible tilings `A`, `B`, and `C`.
||| ```
||| +---+---+---+
||| |███|███|███| A
||| +---+---+---+
|||   1   2   3
||| ```
||| ```
||| +---+---+---+
||| |███████|███| B
||| +---+---+---+
|||   1   2   3
||| ```
||| +---+---+---+
||| |███|███████| C
||| +---+---+---+
|||   1   2   3
||| ```
|||
||| @ n The number of columns of the board.
f : (n : Nat) -> M Nat Nat
f 0           = pure 0         -- Base I.   A 1 x 0 board has no possible tilings.
f 1           = pure 1         -- Base II.  A 1 x 1 board has one possible tiling.
f 2           = pure 2         -- Base III. A 1 x 2 board has two possible tilings.
f n@(S (S k)) = 
    get >>= \st =>
    case lku n st of
        Just r  => pure r
        Nothing =>
            f (S k) >>= \x =>
            f k     >>= \y =>
            let z=x+y in
            modify (ins n z) >>
            pure z

||| Executes `f`.
||| @ n The argument to `f`.
rf : (n : Nat) -> Nat
rf n = evalState empty (f n)

||| Given a list of `n` orderable and summable elements,
||| maximize the sum of *nonconsecutive* elements.
|||
||| e.g., for the list...
||| ```
||| +---+---+---+---+---+---+
||| | 4 | 2 | 1 | 8 | 6 | 3 |
||| +---+---+---+---+---+---+
|||   1   2   3   4   5   6
||| ```
||| ...the maximized sum of nonconsecutive elements is 15,
||| which results from adding 4, 8, and 3 together.
|||
||| xs @ A list of `n` orderable and summable elements.
s : (Ord a, Num a) => 
    (xs : List a) -> M Nat a
s []           = pure 0        -- Base I.   The maximized sum for a list of zero elements is zero.
s [x]          = pure x        -- Base II.  The maximized sum for a list of one element is the element itself.
s [x, y]       =               -- Base III. The maximized sum for a list of two elements is the maximum element
    let m = max x y in         -- between the two elements.
    modify (ins 2 m) >>
    pure m
s xs@(x::y::r) = 
    get >>= \st =>
    let n = length xs in
    case lku n st of
        Just r  => pure r
        Nothing => 
            s (y::r) >>= \u => -- Case I.  Adding x will not maximize the sum.
            s  r     >>= \v => -- Case II. Adding x will maximize the sum.
            let m=max u (x+v) in
            modify (ins n m) >>
            pure m

||| Executes `s`.
||| @ xs The argument to `s`.
rs : (Ord a, Num a) => 
     (xs : List a) -> a
rs xs = evalState empty (s xs)
