module CCAR.Model.Maturity where
import Import

type Year = Int 
type Month = Int  
type Day = Int

data Mat = MatY Year 
    | MatM Month 
    | InvalidMaturity deriving (Show, Read, Generic, Typeable)


{-- | 
 Check for the Mat and return the bounds
--}
checkBounds :: Mat -> Mat
checkBounds a =
    if a < minBound then minBound
    else if a > maxBound then maxBound
    else a


handleInvalidMaturity :: (Mat -> Mat -> Mat) -> (Mat, Mat) -> Mat
handleInvalidMaturity _ (InvalidMaturity, _)  = InvalidMaturity
handleInvalidMaturity _ (_, InvalidMaturity ) =  InvalidMaturity

plus :: Mat -> Mat -> Mat
plus a@(MatY b) c@(MatY d) = MatY (b + d)
plus a@(MatY b) c@(MatM d) = MatM (d + 12*b)
plus a@(MatM b) c@(MatY d) = plus c a
plus (MatM a) (MatM b) = MatM (a + b)
plus a b = handleInvalidMaturity plus (a, b)


minus :: Mat -> Mat -> Mat
minus a@(MatY b) c@(MatY d) = MatY $ abs (b - d)
minus a@(MatY b) c@(MatM d) = MatM $ abs (d - 12*b)
minus a@(MatM b) c@(MatY d) = minus c a
minus (MatM a) (MatM b) = MatM $ abs (a - b)
minus a b = handleInvalidMaturity minus (a, b)


mult :: Mat -> Mat -> Mat
mult a@(MatM c) b@(MatM d) = MatM (c * d)
mult a@(MatY c) b@(MatM d) = mult (MatM $ 12*c) b
mult a@(MatM c) b@(MatY d) = mult b a
mult a@(MatY c) b@(MatY d) = mult (MatM $ c * 12) (MatM $ d * 12) 
mult a b     = handleInvalidMaturity mult (a,b)

absoluteM :: Mat -> Mat
absoluteM a@(MatM b) = MatM $ abs b
absoluteM a@(MatY b) = MatY $ abs b
absoluteM InvalidMaturity = InvalidMaturity

-- These are positive numbers
negateM :: Mat -> Mat
negateM a = undefined

-- What does sign mean for a maturity:
-- maturity is unsigned?
signum :: Mat -> Mat
signum a = undefined

instance Num Mat where
    (+) = plus    
    (-) = minus
    (*) = mult
    abs = absoluteM
    negate = negateM

instance Bounded Mat where
    maxBound = MatY 30
    minBound = MatM 1


instance Eq Mat where
    (MatY a) == (MatY b) = a == b
    (MatY a) == (MatM b) = a == 12 * b
    (MatM a) == (MatY b) = 12 * a == b
    (MatM a) == (MatM b) = a == b

instance Ord Mat where
    compare (MatY a) (MatY b) = compare a b
    compare (MatM a) (MatM b) = compare a b
    compare (MatM a) (MatY b) = compare (12 *a ) b
    compare (MatY a) (MatM b) = compare a (12 *b)

instance ToJSON Mat
instance FromJSON Mat
