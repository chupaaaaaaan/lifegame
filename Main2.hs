module Main where

import Graphics.Gloss


------------------------------
-- Comonad
------------------------------
class Functor w => Comonad w where
  extract :: w b -> b
  extend :: (w b -> a) -> w b -> w a
  duplicate :: w b -> w (w b)

  duplicate = extend id
  extend f = fmap f . duplicate


------------------------------
-- List Zipper on Ring
------------------------------
--  z0 -> z1 -> z2 -> z3
--  |                  |
--  +---<-----<-----<--+
-- Z back current (next:_)
data Z a = Z a a [a] deriving Show

next :: Z a -> Z a
next (Z y x xs) = let xss = xs ++ [y]
                   in Z x (head xss) (tail xss)

iterateN :: Int -> (a -> a) -> a -> [a]
iterateN 0 _ _ = []
iterateN n f x = let z = f x
                 in z : iterateN (n-1) f z

instance Functor Z where
  fmap f (Z y x xs) = Z (f y) (f x) (fmap f xs)

instance Comonad Z where
  extract (Z _ x _) = x
  duplicate zt@(Z _ _ xs) = let ztt = (iterateN (length xs + 1) next zt)
                             in Z (last ztt) zt (init ztt)

toZ :: Int -> a -> [a] -> Z a
toZ w a xs = let (x:y:zz) = (take w $ xs ++ repeat a)
              in Z (last zz) x (init (y:zz))


------------------------------
-- 2D List Zipper on Torus
------------------------------
newtype Z2 a = Z2 (Z (Z a)) deriving Show

instance Functor Z2 where
  fmap f (Z2 zz) = Z2 (fmap (fmap f) zz)

instance Comonad Z2 where
  extract (Z2 zz) = extract (extract zz)
  duplicate (Z2 zz) = fmap Z2 . Z2 . roll . roll $ zz
    where roll zt@(Z _ (Z _ _ xs) _) = let ztt = (iterateN (length xs + 1) (fmap next) zt)
                                         in Z (last ztt) zt (init ztt)

toZ2 :: Int -> Int -> a -> [[a]] -> Z2 a
toZ2 w h a xss = Z2 $ toZ h (toZ w a []) (map (toZ w a) xss)


------------------------------
-- Life Game on Torus
------------------------------
nWidth, nHeight, cSize,  wWidth, wHeight :: Num a => a
nWidth  = 50
nHeight = 50
cSize   = 10
wWidth  = nWidth * cSize
wHeight = nHeight * cSize

-- n0 n1 n2
-- n3 _  n4
-- n5 n6 n7
countNeighbours :: Z2 Bool -> Int
countNeighbours (Z2 (Z
                      ( Z n0 n1 (n2:_))
                      ( Z n3 _  (n4:_))
                      ((Z n5 n6 (n7:_)):_))) = 
  length $ filter id [n0, n1, n2, n3, n4, n5, n6, n7]

life :: Z2 Bool -> Bool
life z = let a = extract z
             n = countNeighbours z
         in (a && (n == 2 || n == 3)) || (not a && n == 3)


type Model = Z2 Bool

draw :: Model -> Picture
draw (Z2 (Z rs xs rows)) =
  let cell = translate (-cSize / 2) (cSize / 2) $ rectangleSolid cSize cSize
      b2c b = if b then black else white
      cells = do
        ((Z r z row),h) <- zip ([xs] ++ rows ++ [rs]) [1..nHeight]
        (b          ,w) <- zip ([z] ++ row ++ [r])    [1..nWidth]
        let x = fromIntegral w * cSize - wWidth / 2
            y = wHeight / 2 - fromIntegral h * cSize
            transform = color (b2c b) . translate x y
        pure $ transform cell
  in mconcat cells

main :: IO ()
main = simulate inWindow white 20 initModel draw (\_ _ -> extend life)
  where inWindow = InWindow "No Title" (wWidth, wHeight) (100,100)
        field = [ "........."
                , "..#......"
                , "....#...."
                , ".##..###."
                , "........."
                ]
        initModel = toZ2 nWidth nHeight False $ map (map (=='#')) field
