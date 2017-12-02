module SortedLabels exposing (SortedLabels, asList, ascendingLabels, descendingLabels)

{-
   A type for sorted lists of strings

   Invariants
   - The list is always sorted in the specified order
-}


type SortedLabels
    = SortedLabels (List String) SortDirection


type SortDirection
    = Asc
    | Desc


ascendingLabels : List String -> SortedLabels
ascendingLabels labels =
    SortedLabels labels Asc


descendingLabels : List String -> SortedLabels
descendingLabels labels =
    SortedLabels labels Desc


asList : SortedLabels -> List String
asList sortedLabels =
    case sortedLabels of
        SortedLabels labels Asc ->
            List.sort labels

        SortedLabels labels Desc ->
            labels
                |> List.sort
                |> List.reverse
