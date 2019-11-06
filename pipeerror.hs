-- 'pipeerror.hs'.


pipeerror :: [ (a -> Either e a) ] -> a -> Either e a
pipeerror fs v = foldl call (Right v) fs
                 where call a e = case a of
                                  Left _  -> a
                                  Right r -> e r


inc :: Int -> Either String Int
inc x = Right (x + 1)


iseven :: Int -> Either String Int
iseven x = if x `mod` 2 == 0
           then Right x
           else Left "Not even"


main :: IO ()
main = case pipeerror [ inc, inc, inc, iseven, inc, inc ] 0 of
       Left l  -> putStrLn l
       Right r -> putStrLn . show $ r
