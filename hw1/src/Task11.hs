module Task11 where

data DayOfWeek = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving Show

nextDay :: DayOfWeek -> DayOfWeek
nextDay Monday = Tuesday
nextDay Tuesday = Wednesday
nextDay Wednesday = Thursday
nextDay Thursday = Friday
nextDay Friday = Saturday
nextDay Saturday = Sunday
nextDay Sunday = Monday

afterDays :: DayOfWeek -> Integer -> DayOfWeek
afterDays day 1 = nextDay day
afterDays day a = nextDay $ afterDays day (a - 1)

isWeekend :: DayOfWeek -> Bool
isWeekend Sunday = True
isWeekend Saturday = True
isWeekend _ = False

daysToParty :: DayOfWeek -> Integer
daysToParty Friday = 0
daysToParty day = 1 + (daysToParty $ nextDay day)