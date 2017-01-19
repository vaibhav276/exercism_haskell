module Triangle (TriangleType(..), triangleType) where

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  | Degenerate
                  deriving (Eq, Show)

triangleType :: Float -> Float -> Float -> TriangleType
triangleType a b c
    | a > (b+c) || b > (a+c) || c > (a+b)    = Illegal
    | a <= 0.0 || b <= 0.0 || c <= 0.0       = Illegal
    | a == (b+c) || b == (a+c) || c == (a+b) = Degenerate
    | a == b && a == c                       = Equilateral
    | a == b || a == c || b == c             = Isosceles
    | a /= b && a /= c && b /= c             = Scalene
    | otherwise                              = Illegal
