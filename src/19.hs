module Main (main,euler19)
where
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
{-
	Jan. 1 1900 was a Monday

	thirty days has september, april, june and november.
	all the rest have thirty-one, saving february alone,
	which has twenty-eight, rain or shine. And on leap years
	twenty-nine.
	
	A leap year occurs on any year divisible by 4, but not on a century unless
	if it is divisible by 400.
	
	How many sundays fell on the first of a month, during the twentieth century
	( 1 Jan 1901 to 31 Dec 2000 ) ?
-}

main = print euler19
euler19 = length $ filter ( \(_,_,c) -> (c == 7) ) all_days
	where
		all_days = [ toWeekDate $ fromGregorian a b 1 | a <- [1901..2000], b <- [1..12]]
