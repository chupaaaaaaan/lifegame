module Main where

import Data.List
import Control.Monad
import Control.Concurrent


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
data Zt a = Zt a a [a] deriving Show

next :: Zt a -> Zt a
next (Zt y x xs) = let xss = xs ++ [y]
                   in Zt x (head xss) (tail xss)

iterateN :: Int -> (a -> a) -> a -> [a]
iterateN 0 _ _ = []
iterateN n f x = let z = f x
                 in z : iterateN (n-1) f z

instance Functor Zt where
  fmap f (Zt y x xs) = Zt (f y) (f x) (fmap f xs)

instance Comonad Zt where
  extract (Zt _ x _) = x
  duplicate zt@(Zt _ _ xs) = let ztt = (iterateN (length xs + 1) next zt)
                             in Zt (last ztt) zt (init ztt)

toZt :: Int -> a -> [a] -> Zt a
toZt w a xs = let (x:y:zz) = (take w $ xs ++ repeat a)
              in Zt (last zz) x (init (y:zz))


------------------------------
-- 2D List Zipper on Torus
------------------------------
newtype Z2 a = Z2 (Zt (Zt a)) deriving Show

instance Functor Z2 where
  fmap f (Z2 zz) = Z2 (fmap (fmap f) zz)

instance Comonad Z2 where
  extract (Z2 zz) = extract (extract zz)
  duplicate (Z2 zz) = fmap Z2 . Z2 . roll . roll $ zz
    where roll zt@(Zt _ (Zt _ _ xs) _) = let ztt = (iterateN (length xs + 1) (fmap next) zt)
                                         in Zt (last ztt) zt (init ztt)

toZ2 :: Int -> Int -> a -> [[a]] -> Z2 a
toZ2 w h a xss = Z2 $ toZt h (toZt w a []) (map (toZt w a) xss)


------------------------------
-- Life Game on Torus
------------------------------
-- n0 n1 n2
-- n3 _  n4
-- n5 n6 n7
countNeighbours :: Z2 Bool -> Int
countNeighbours (Z2 (Zt
                      ( Zt n0 n1 (n2:_))
                      ( Zt n3 _  (n4:_))
                      ((Zt n5 n6 (n7:_)):_))) = 
  length $ filter id [n0, n1, n2, n3, n4, n5, n6, n7]

life :: Z2 Bool -> Bool
life z = let a = extract z
             n = countNeighbours z
         in (a && (n == 2 || n == 3)) || (not a && n == 3)

showZ2 :: Z2 Char -> IO ()
showZ2 (Z2 (Zt rs xs rows)) = do
  forM_ ([xs] ++ rows ++ [rs]) $ \(Zt r x row) -> do
    putStrLn . intercalate " " . map pure $ [x] ++ row ++ [r]

main :: IO ()
main = do
  let c2b c = if c == '.' then False else True
      b2c b = if b then '#' else '.'
      (w, h) = (50, 50)
      -- field = [ ".#."
      --         , "..#"
      --         , "###"
      --         ]
      -- field = [ ".............."
      --         , ".............."
      --         , ".............."
      --         , ".............."
      --         , "...........#.."
      --         , ".....##......."
      --         , "......#...###."
      --         , ".............."
      --         , ".............."
      --         , ".............."
      --         ]
      field = [ ".............."
              , ".............."
              , ".............."
              , ".............."
              , "......#......."
              , "........#....."
              , ".....##..###.."
              , ".............."
              , ".............."
              , ".............."
              ]
      initState = fmap c2b $ toZ2 w h '.' field
      loop state = do
        let state' = extend life state
        replicateM_ h $ putStr "\ESC[A\ESC[2K"
        showZ2 (fmap b2c state)
        threadDelay 300000
        loop state'

  replicateM_ h $ putStr ""

  loop initState
