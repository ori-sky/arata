module ListBinding where

(>>:) :: Monad m => m a -> [a -> m b] -> m [b]
left >>: right = do
    b <- left
    mapM (\f -> f b) right
infixl 1 >>:
