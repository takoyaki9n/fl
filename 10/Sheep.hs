data Sheep = Sheep (Maybe Sheep) (Maybe Sheep) deriving Show

father (Sheep f m) = f
mother (Sheep f m) = m

maternalGrandfather s = mother s >>= father >>= return
fathersMaternalGrandmother s = father s >>= mother >>= mother >>= return
