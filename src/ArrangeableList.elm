module ArrangeableList
    exposing
        ( ArrangeableList
        , initialize
        , fromArray
        , fromList
        , fromListAtHead
        , fromListAtTail
        , getSelected
        , getPreList
        , getPostList
        , progress
        , retrogress
        , toList
        , toArray
        )

import Array exposing (Array)


type ArrangeableList a
    = ArrangeableList a (List a) (List a)


initialize : List a -> List a -> a -> ArrangeableList a
initialize pre post selected =
    ArrangeableList selected (List.reverse pre) post


fromArray : Array a -> Int -> Maybe (ArrangeableList a)
fromArray items idx =
    case (Array.isEmpty items) of
        True ->
            Nothing

        False ->
            case (Array.get idx items) of
                Nothing ->
                    Nothing

                Just val ->
                    let
                        pre =
                            Array.slice 0 idx items
                                |> Array.toList

                        post =
                            Array.slice (idx + 1) (Array.length items) items
                                |> Array.toList
                    in
                        Just <| initialize pre post val


fromList : List a -> Int -> Maybe (ArrangeableList a)
fromList items idx =
    fromArray (Array.fromList items) idx


fromListAtHead : List a -> a -> ArrangeableList a
fromListAtHead items selected =
    initialize [] items selected


fromListAtTail : List a -> a -> ArrangeableList a
fromListAtTail items selected =
    initialize items [] selected


getSelected : ArrangeableList a -> a
getSelected (ArrangeableList selected pre post) =
    selected


getPreList : ArrangeableList a -> List a
getPreList (ArrangeableList selected pre post) =
    List.reverse pre


getPostList : ArrangeableList a -> List a
getPostList (ArrangeableList selected pre post) =
    post


progress : ArrangeableList a -> ArrangeableList a
progress ((ArrangeableList selected pre post) as aList) =
    case (List.head post) of
        Nothing ->
            aList

        Just val ->
            ArrangeableList selected (val :: pre) (List.drop 1 post)


retrogress : ArrangeableList a -> ArrangeableList a
retrogress ((ArrangeableList selected pre post) as aList) =
    case (List.head pre) of
        Nothing ->
            aList

        Just val ->
            ArrangeableList selected (List.drop 1 pre) (val :: post)


toList : ArrangeableList a -> List a
toList (ArrangeableList selected pre post) =
    (List.reverse pre) ++ (selected :: post)


toArray : ArrangeableList a -> Array a
toArray aList =
    toList aList
        |> Array.fromList
