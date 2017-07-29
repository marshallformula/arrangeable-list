module ArrangeableList exposing (..)


type ArrangeableList a
    = ArrangeableList a (List a) (List a)


initialize : List a -> List a -> a -> ArrangeableList a
initialize pre post selected =
    ArrangeableList selected pre post
