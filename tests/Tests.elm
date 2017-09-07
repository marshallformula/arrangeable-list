module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, string)
import ArrangeableList exposing (..)
import Array


emptyAList : ArrangeableList String
emptyAList =
    initialize [] [] "single element"


excellentList : List String
excellentList =
    [ "Bill", "Ted", "Beethoven" ]


singleElementTests : Test
singleElementTests =
    describe "Empty list tests"
        [ test "empty pre/post is valid" <|
            \_ ->
                emptyAList
                    |> toList
                    |> List.length
                    |> Expect.equal 1
        , test "progress does nothing" <|
            \_ ->
                emptyAList
                    |> progress
                    |> getSelected
                    |> Expect.equal "single element"
        , test "retrogress does nothing" <|
            \_ ->
                emptyAList
                    |> retrogress
                    |> getSelected
                    |> Expect.equal "single element"
        ]


fromListTests : Test
fromListTests =
    describe "fromList tests"
        [ test "Nothing from empty list" <|
            \_ ->
                case (fromList [] 0) of
                    Nothing ->
                        Expect.pass

                    _ ->
                        Expect.fail "fromList [] should return Nothing"
        , test "index out of bounds gives Nothing" <|
            \_ ->
                case (fromList excellentList 5) of
                    Nothing ->
                        Expect.pass

                    _ ->
                        Expect.fail "fromList out of bounds should return Nothing"
        , test "fromList gives Just when valid list & index" <|
            \_ ->
                case (fromList excellentList 1) of
                    Nothing ->
                        Expect.fail "Valid list & index shouldn't give Nothing"

                    Just aList ->
                        toList aList
                            |> Expect.equal excellentList
        , test "selection works with valid index" <|
            \_ ->
                case (fromList excellentList 2) of
                    Nothing ->
                        Expect.fail "Valid list & index shouldn't give Nothing"

                    Just aList ->
                        getSelected aList
                            |> Expect.equal "Beethoven"
        ]


fromArrayTests : Test
fromArrayTests =
    describe "fromArray tests"
        [ test "Nothing from empty array" <|
            \_ ->
                case (fromArray Array.empty 0) of
                    Nothing ->
                        Expect.pass

                    _ ->
                        Expect.fail "Empty array should give Nothing"
        , test "Nothing from out of bounds array" <|
            \_ ->
                case (fromArray (Array.repeat 1 "DiscoStu") 8) of
                    Nothing ->
                        Expect.pass

                    _ ->
                        Expect.fail "Out of bounds index should return Nothing"
        , test "fromArray gives Just when valid array and index" <|
            \_ ->
                case (fromArray (Array.fromList excellentList) 1) of
                    Nothing ->
                        Expect.fail "Valid array and index should be a Just"

                    Just aList ->
                        toList aList
                            |> Expect.equal excellentList
        , test "selection works with valid index" <|
            \_ ->
                case (fromArray (Array.fromList excellentList) 1) of
                    Nothing ->
                        Expect.fail "Valid array and index should be a Just"

                    Just aList ->
                        getSelected aList
                            |> Expect.equal "Ted"
        ]


fromListAtHeadTests : Test
fromListAtHeadTests =
    describe "fromListAtHead tests"
        [ test "fromListAtHead works correctly" <|
            \_ ->
                fromListAtHead excellentList "Napoleon"
                    |> getPreList
                    |> Expect.equal []
        , fuzz (Fuzz.list Fuzz.string) "fromListAtHead works correctly for all lists" <|
            \fuzzedList ->
                fromListAtHead fuzzedList "steve"
                    |> getSelected
                    |> Expect.equal "steve"
        ]


fromListAtTailTests : Test
fromListAtTailTests =
    describe "fromListAtTail tests"
        [ test "fromListAtTail works correctly" <|
            \_ ->
                fromListAtTail excellentList "Napoleon"
                    |> getPostList
                    |> Expect.equal []
        , fuzz (Fuzz.list Fuzz.string) "fromListAtTail works correclty for all lists" <|
            \fuzzedList ->
                fromListAtTail fuzzedList "steve"
                    |> getSelected
                    |> Expect.equal "steve"
        ]


progressTests : Test
progressTests =
    describe "progress tests"
        [ describe "progress works correctly" <|
            let
                progressed =
                    progress <| fromListAtHead excellentList "Napoleon"
            in
                [ test "arranged list is correct" <|
                    \_ ->
                        Expect.equal (toList progressed) [ "Bill", "Napoleon", "Ted", "Beethoven" ]
                , test "multiple progresses moves napoleon correctly" <|
                    \_ ->
                        progressed
                            |> progress
                            |> progress
                            |> toList
                            |> Expect.equal [ "Bill", "Ted", "Beethoven", "Napoleon" ]
                ]
        , test "progress past tail does nothing" <|
            \_ ->
                fromListAtTail excellentList "Napoleon"
                    |> progress
                    |> progress
                    |> progress
                    |> progress
                    |> toList
                    |> Expect.equal [ "Bill", "Ted", "Beethoven", "Napoleon" ]
        ]


retrogressTests : Test
retrogressTests =
    describe "retrogress tests"
        [ describe "retrogress works correctly" <|
            let
                retrogressed =
                    retrogress <| fromListAtTail excellentList "Napoleon"
            in
                [ test "arranged list is correct" <|
                    \_ ->
                        Expect.equal (toList retrogressed) [ "Bill", "Ted", "Napoleon", "Beethoven" ]
                , test "multiple retrogresses moves napoleon correctly" <|
                    \_ ->
                        retrogressed
                            |> retrogress
                            |> retrogress
                            |> toList
                            |> Expect.equal [ "Napoleon", "Bill", "Ted", "Beethoven" ]
                ]
        , test "retrogress past head does nothing" <|
            \_ ->
                fromListAtHead excellentList "Napoleon"
                    |> retrogress
                    |> retrogress
                    |> retrogress
                    |> retrogress
                    |> retrogress
                    |> toList
                    |> Expect.equal [ "Napoleon", "Bill", "Ted", "Beethoven" ]
        ]


isSelectedTest : Test
isSelectedTest =
    describe "tests that item supplied is the currently selected on in the arrangeable list"
        [ test "correctly tests selection" <|
            \_ ->
                fromListAtHead excellentList "Napoleon"
                    |> isSelectedItem "Napoleon"
                    |> Expect.true "Napoleon should be selected"
        , test "correctly reports non-correct selection" <|
            \_ ->
                fromListAtHead excellentList "Napoleon"
                    |> isSelectedItem "Bill"
                    |> Expect.false "Bill should not be selected"
        ]


mapTest : Test
mapTest =
    test "tests that the function is applied to all items" <|
        \_ ->
            fromListAtHead excellentList "Napoleon"
                |> map String.toUpper
                |> progress
                |> toList
                |> Expect.equal [ "BILL", "NAPOLEON", "TED", "BEETHOVEN" ]
