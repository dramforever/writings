> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE RankNTypes #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> 
> data SKI
>   = App SKI SKI
>   | S
>   | K
>   | I
> 
> instance Show SKI where
>   show S = "S"
>   show K = "K"
>   show I = "I"
>   show (App u v) = "(" ++ show u ++ " " ++ show v ++ ")"
> 
> newtype Scope repr = Scope { getScope :: repr }
> 
> getVar :: repr -> Scope repr
> getVar iR = Scope iR
> 
> liftApp
>   :: repr
>   -> (repr -> repr -> repr)
>   -> Scope repr
>   -> Scope repr
>   -> Scope repr
> liftApp sR appR (Scope f) (Scope x) = Scope $ (sR `appR` f) `appR` x
> 
> liftConst
>   :: repr
>   -> (repr -> repr -> repr)
>   -> repr
>   -> Scope repr
> liftConst kR appR x = Scope $ kR `appR` x
> 
> class HasSKI repr where
>   s, k, i :: repr
>   (@-) :: repr -> repr -> repr
> 
> infixl 9 @-
> 
> instance HasSKI SKI where
>   s = S
>   k = K
>   i = I
>   (@-) = App
> 
> instance (HasSKI repr) => HasSKI (Scope repr) where
>   s = liftConst k (@-) s
>   k = liftConst k (@-) k
>   i = liftConst k (@-) i
>   (@-) = liftApp s (@-)
> 
> class LiftVar u v where
>   liftVar :: u -> v
> 
> instance {-# OVERLAPPABLE #-} (HasSKI v, LiftVar u v) => LiftVar u (Scope v) where
>   liftVar = liftConst k (@-) . liftVar
> 
> instance LiftVar SKI SKI where liftVar = id
> 
> instance LiftVar (Scope repr) (Scope repr) where liftVar = id
> 
> var :: HasSKI repr => Scope repr
> var = getVar i
> 
> lam :: forall repr. HasSKI repr
>     => ((forall k. LiftVar (Scope repr) k => k) -> Scope repr) -> repr
> lam f = getScope (f $ liftVar (var :: Scope repr))
> 
> -- c = \f x y -> f y x
> c :: SKI
> c = lam (\f -> lam (\x -> lam (\y -> f @- y @- x)))
