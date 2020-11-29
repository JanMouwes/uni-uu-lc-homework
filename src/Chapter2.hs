module Chapter2 where

-- Exercise 22
data Pal = DoubleA Pal | DoubleB Pal | PalA | PalB | PalEmpty

ex22_1 :: Pal -> String
ex22_1 PalEmpty = ""
ex22_1 PalA = "a"
ex22_1 PalB = "b"
ex22_1 (DoubleA p) = "a" ++ ex22_1 p ++ "a"
ex22_1 (DoubleB p) = "b" ++ ex22_1 p ++ "b"

ex22_pal1 :: Pal
ex22_pal1 = DoubleA $ DoubleB $ DoubleA PalEmpty
ex22_pal2 :: Pal
ex22_pal2 = DoubleB $ DoubleA $ PalA

ex22_2 :: Pal -> Int
ex22_2 PalEmpty = 0
ex22_2 PalA = 1
ex22_2 PalB = 0
ex22_2 (DoubleA p) = 2 + ex22_2 p
ex22_2 (DoubleB p) = 0 + ex22_2 p


-- Exercise 23
data Mir = MirDouble Letter Mir | MirEmpty
data Letter = A | B

ex23_amir1 = MirDouble A $ MirDouble B $ MirDouble A MirEmpty
ex23_amir2 = MirDouble A $ MirDouble B $ MirDouble B MirEmpty

ex23_2 :: Mir -> String
ex23_2 MirEmpty = ""
ex23_2 (MirDouble l m) = 
    let concL = case l of 
                    A -> "a"
                    B -> "b"
    in concL ++ ex23_2 m ++ concL

ex23_3 :: Mir -> Pal
ex23_3 MirEmpty = PalEmpty
ex23_3 (MirDouble l m) = case l of
    A -> DoubleA (ex23_3 m)
    B -> DoubleB (ex23_3 m)

-- Exercise 24
data Parity = Ones Parity | Left0 Parity | Right0 Parity | Empty

ex24_aEven1 :: Parity
ex24_aEven1 = Left0 $ Left0 $ Ones $ Left0 $ Empty
ex24_aEven2 :: Parity
ex24_aEven2 = Left0 $ Right0 $ Ones $ Left0 $ Empty

ex24_2 :: Parity -> String
ex24_2 Empty = ""
ex24_2 (Left0 p) = "0" ++ ex24_2 p
ex24_2 (Right0 p) = ex24_2 p ++ "0"
ex24_2 (Ones p) = "1" ++ ex24_2 p ++ "1"

-- Exercise 25
data BitList = Single Bit | List Bit BitList
data Bit = One | Zero

ex25_aBitList1 :: BitList
ex25_aBitList1 = List Zero $ List One $ Single Zero
ex25_aBitList2 :: BitList
ex25_aBitList2 = List Zero $ List Zero $ Single One

ex25_2 :: BitList -> String
ex25_2 (Single b) = case b of 
    Zero -> "0"
    One -> "1"
ex25_2 (List b l) = (case b of
    Zero -> "0" 
    One -> "1") ++ "," ++ ex25_2 l

ex25_3 :: BitList -> BitList -> BitList
ex25_3 (Single b1) l2 = List b1 l2
ex25_3 (List b1 l1) l2 = List b1 (ex25_3 l1 l2)
