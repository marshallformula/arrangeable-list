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

{-| This is sort of like a Zip-List data structure - but instead of focusing on a speicific list item - it retains the focuson the same item and moves
that item around in the list.


# Constructing

@docs initialize, fromArray, fromList, fromListAtHead, fromListAtTail


# Extracting Values

@docs getSelected, getPreList, getPostList


# Manipulating the List

@docs progress, regrogress


# Transformation

@docs toList, toArray

-}

import Array exposing (Array)


type ArrangeableList a
    = ArrangeableList a (List a) (List a)


{-| Initializes an ArrageableList given a list before (pre), list after (post), and the selected (focused) item.

    initialize [] ["Batman", "Superman", "Green Lantern"] "Wonder Woman"

-}
initialize : List a -> List a -> a -> ArrangeableList a
initialize pre post selected =
    ArrangeableList selected (List.reverse pre) post


{-| Create an ArrangeableList from an Array and an integer that specifies an index in the array which should be the selected item.
This returns a Maybe Arrageablelist. A Nothing is returned if the index is invalid/out of bounds.

    myArray =
        Array.fromList ["Superman", "Batman", "Green Lantern"]

    fromArray myArray 1

-}
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


{-| Create an ArrangeableList from a List and an integer that specifies an index in the list which should be the selected item.
This returns a Maybe Arrageablelist. A Nothing is returned if the index is invalid/out of bounds.

    fromArray ["Superman", "Batman", "Green Lantern"] 0

-}
fromList : List a -> Int -> Maybe (ArrangeableList a)
fromList items idx =
    fromArray (Array.fromList items) idx


{-| Creates an ArrangeableList from a supplied List and a value - placing value at the head of the list.

    fromListAtHead ["Superman", "Batman", "Green Lantern"] "WonderWoman"

-}
fromListAtHead : List a -> a -> ArrangeableList a
fromListAtHead items selected =
    initialize [] items selected


{-| Creates and ArrangeableList from a supplied list and a value - placing the value at the tail of the list.

    fromListAtHead ["Superman", "Batman", "Green Lantern"] "WonderWoman"

-}
fromListAtTail : List a -> a -> ArrangeableList a
fromListAtTail items selected =
    initialize items [] selected


{-| Returns the currently selected value
-}
getSelected : ArrangeableList a -> a
getSelected (ArrangeableList selected pre post) =
    selected


{-| Returns the items in the list before the selected value.
-}
getPreList : ArrangeableList a -> List a
getPreList (ArrangeableList selected pre post) =
    List.reverse pre


{-| Returns the items in the list after the selected value.
-}
getPostList : ArrangeableList a -> List a
getPostList (ArrangeableList selected pre post) =
    post


{-| Moves the selected item forward in the list.

    myList =
        fromListAtHead ["Superman", "Batman", "Green Lantern"] "WonderWoman"

    progress myList
        |> toList -- ["Superman", "WonderWoman", "Batman", "Green Lantern"]

-}
progress : ArrangeableList a -> ArrangeableList a
progress ((ArrangeableList selected pre post) as aList) =
    case (List.head post) of
        Nothing ->
            aList

        Just val ->
            ArrangeableList selected (val :: pre) (List.drop 1 post)


{-| Moves the selected item backward in the list.

    myList =
        fromListAtTail ["Superman", "Batman", "Green Lantern"] "WonderWoman"

    retrogress myList
        |> toList -- ["Superman", "Batman", "WonderWoman", "Green Lantern"]

-}
retrogress : ArrangeableList a -> ArrangeableList a
retrogress ((ArrangeableList selected pre post) as aList) =
    case (List.head pre) of
        Nothing ->
            aList

        Just val ->
            ArrangeableList selected (List.drop 1 pre) (val :: post)


{-| Returns a regular List of the items that compose the ArrangeableList.
-}
toList : ArrangeableList a -> List a
toList (ArrangeableList selected pre post) =
    (List.reverse pre) ++ (selected :: post)


{-| Returns an Array of the items that compose the ArrangeableList.
-}
toArray : ArrangeableList a -> Array a
toArray aList =
    toList aList
        |> Array.fromList
