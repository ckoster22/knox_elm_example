module SortedLabels exposing (SortDirection(..), SortedLabels, asList)

{-
   A type for sorted lists of strings

   Invariants
   - The list is always sorted in the specified order
-}


type alias SortedLabels =
    { labels : List String
    , direction : SortDirection
    }


type SortDirection
    = Asc
    | Desc


asList : SortedLabels -> List String
asList sortedLabels =
    case sortedLabels.direction of
        Asc ->
            List.sort sortedLabels.labels

        Desc ->
            sortedLabels.labels
                |> List.sort
                |> List.reverse
