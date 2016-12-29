module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear year
    | year `mod` 100 == 0 = if (year `mod` 400 == 0) then True else False
    | year `mod` 4 == 0 = True
    | otherwise = False
