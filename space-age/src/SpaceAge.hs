module SpaceAge (Planet(..), ageOn) where

data Planet =
    Earth
    | Mercury
    | Venus
    | Mars
    | Jupiter
    | Saturn
    | Uranus
    | Neptune

-- | ageOn returns the age (in respective planet years) for a given planet and
-- given number of seconds. 1 earth year = 31557600 seconds.
ageOn :: Planet -> Float -> Float
ageOn p = (relativeYears p).(earthYears) where earthYears = (/31557600)

-- | relativeYears returns the function for converting earth years into
-- other planet years
relativeYears :: Planet -> Float -> Float
relativeYears Earth   = id
relativeYears Mercury = (/0.2408467)
relativeYears Venus   = (/0.61519726)
relativeYears Mars    = (/1.8808158)
relativeYears Jupiter = (/11.862615)
relativeYears Saturn  = (/29.447498)
relativeYears Uranus  = (/84.016846)
relativeYears Neptune = (/164.79132)
