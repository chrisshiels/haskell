-- 'pipemaybe.hs'.


pipemaybe :: [ (a -> Maybe a) ] -> a -> Maybe a
pipemaybe fs v = foldl call (Just v) fs
                 where call a e = case a of
                                  Nothing -> a
                                  Just x  -> e x


inc :: Int -> Maybe Int
inc x = Just (x + 1)


iseven :: Int -> Maybe Int
iseven x = if x `mod` 2 == 0
           then Just x
           else Nothing


main :: IO ()
main = case pipemaybe [ inc, inc, inc, iseven, inc, inc ] 0 of
       Nothing -> putStrLn "Error"
       Just x  -> putStrLn . show $ x
