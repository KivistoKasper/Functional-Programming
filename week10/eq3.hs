import Bool3

class Eq3 a where
    (===) :: a -> a -> Bool3
    (/==) :: a -> a -> Bool3
    x === y = not3 (x /== y)
    x /== y = not3 (x === y)

instance Eq3 Bool3 where
    True3 === True3 = True3
    False3 === False3 = True3
    Unk3 === _ = Unk3
    _ === Unk3 = Unk3
    _ === _ = False3

instance Eq3 Integer where
    x === y
        | x < 0 || y < 0 = Unk3
        | x == y = True3
        | otherwise = False3

instance (Eq3 a) => Eq3 (Maybe a) where
    Nothing === _ = Unk3
    _ === Nothing = Unk3
    Just x === Just y = x === y
