module Matrix
    (
        Matrix,
        showMat,
        idMat,
        nullMat,
        fromRows,
        fromCols,
        trp,
        dims,
        rows,
        cols,
        rowsAsMats,
        colsAsMats,
        diagMat,
        diag,
        minorMat,
        det,
        invMat,
        (@@-),
        (*|*),
        (*|),
        (|*),
        (+|+),
        (-|-),
        (|+),
        (+|)
    )
    where

import Data.Array
import Data.List
import Data.Functor
import Debug.Trace

import SupportUtils

data Matrix e = Matrix {
        elems :: Array (Int, Int) e
    }
    deriving (Eq)

instance Show e => Show (Matrix e) where
    show = showMat

instance Functor Matrix where
    fmap f m = Matrix (f <$> Matrix.elems m)

dims :: Matrix e -> (Int, Int)
dims = snd . bounds . Matrix.elems

showMat :: Show e => Matrix e -> String
showMat m = "Dims: " ++ show (r + 1, c + 1) ++ "\n" ++ intercalate "\n" (map printRow [0..r])
    where (r, c) = dims m
          es = Matrix.elems m
          printRow r = unwords $ map (\c -> printElem (r, c)) [0..c]
          printElem p = show (es ! p)

idMat :: Num e => Int -> Matrix e
idMat n = Matrix (array ((0, 0), (n - 1, n - 1)) [((i, j), f i j) | i <- [0..n-1], j <- [0..n-1]])
    where f i j | i == j = 1
                | otherwise = 0

nullMat :: Matrix e
nullMat = Matrix (array ((0, 0), (-1, -1)) [])

fromRows :: [[e]] -> Matrix e
fromRows rows | null rows || null (head rows) = nullMat
              | otherwise = Matrix (listArray ((0, 0), (length rows - 1, length (head rows) - 1)) (concat rows))

fromCols :: [[e]] -> Matrix e
fromCols = fromRows . transpose

trp :: Matrix e -> Matrix e
trp m = Matrix $ ixmap ((0, 0), swp $ dims m) swp (Matrix.elems m)
    where swp (a, b) = (b, a)

diagMat :: Num e => Int -> e -> Matrix e
diagMat n e = idMat n |* e

diag :: Num e => Matrix e -> Matrix e
diag m = Matrix (array ((0, 0), ds) [((i, j), t i j) | i <- [0..fst ds], j <- [0..snd ds]])
    where ds = dims m
          t i j | i == j = m @@- (i, j)
                | otherwise = 0

minorMat :: Matrix e -> (Int, Int) -> Matrix e
minorMat m (mr, mc) = Matrix (array ((0, 0), (dr - 1, dc - 1)) [((i, j), t i j) | i <- [0..dr - 1], j <- [0..dc - 1]])
    where (dr, dc) = dims m
          t i j = m @@- (fix i mr, fix j mc)
          fix i p | i < p = i
                  | otherwise = i + 1

det :: Num e => Matrix e -> e
det m | dims m == (0, 0) = m @@- (0, 0)
      | dims m == (1, 1) = (m @@- (0, 0)) * (m @@- (1, 1)) - (m @@- (1, 0)) * (m @@- (0, 1))
      | otherwise = sum $ map step [0..cs]
    where step' i = (m @@- (0, i)) * det (minorMat m (0, i))
          step i | i `mod` 2 == 0 = step' i 
                 | otherwise = negate $ step' i
          (rs, cs) = dims m

invMat :: (Num e, Fractional e) => Matrix e -> Matrix e
invMat m = trp m |* recip (det m)

rows :: Matrix e -> [[e]]
rows m = map getRow [0..r]
    where (r, c) = dims m
          getRow i = map snd $ filter (\((r, _), _) -> r == i) as
          es = Matrix.elems m
          as = assocs es

cols :: Matrix e -> [[e]]
cols = rows . trp

rowsAsMats :: Matrix e -> [Matrix e]
rowsAsMats = map (fromRows . listize) . rows

colsAsMats :: Matrix e -> [Matrix e]
colsAsMats = map (fromRows . listize) . cols

(@@-) :: Matrix e -> (Int, Int) -> e
(@@-) m p = Matrix.elems m ! p

(+|) :: Num e => e -> Matrix e -> Matrix e
(+|) n m = (+n) <$> m
infixl 6 +|

(|+) :: Num e => Matrix e -> e -> Matrix e
(|+) = flip (+|)
infixl 6 |+

(*|) :: Num e => e -> Matrix e -> Matrix e
(*|) n m = (*n) <$> m
infixl 7 *|

(|*) :: Num e => Matrix e -> e -> Matrix e
(|*) = flip (*|)
infixl 7 |*

(+|+) :: Num e => Matrix e -> Matrix e -> Matrix e
(+|+) l r = Matrix (array ((0, 0), d) [((i, j), les ! (i, j) + res ! (i, j)) | i <- [0..rs], j <- [0..cs]])
    where d@(rs, cs) = dims l
          les = Matrix.elems l
          res = Matrix.elems r
infixl 6 +|+

(-|-) :: Num e => Matrix e -> Matrix e -> Matrix e
(-|-) l r = l +|+ (r |* (-1))
infixl 6 -|-

(*|*) :: Num e => Matrix e -> Matrix e -> Matrix e
(*|*) l r = Matrix (array ((0, 0), (nl - 1, nr - 1)) [((i, j), mulVecs (lrows !! i) (rcols !! j)) | i <- [0..nl - 1], j <- [0..nr - 1]])
    where lrows = rows l
          rcols = cols r
          nl = length lrows
          nr = length rcols
          mulVecs v1 v2 = sum $ zipWith (*) v1 v2
infixl 7 *|*

