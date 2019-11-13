-- 'closures.hs'.


adder1 x = adder1
           where adder1 y = x + y


adder2 x = let adder2 y = x + y
           in adder2


adder3 x = (+) x


adder4 x = \y -> x + y


adder5 x y = x + y
