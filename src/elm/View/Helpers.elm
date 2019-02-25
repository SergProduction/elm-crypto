module View.Helpers exposing (getRedOrGreenClass)


getRedOrGreenClass : String -> String -> ( String, Bool )
getRedOrGreenClass prev next =
    let
        res =
            Maybe.map2
                (\p n ->
                    if p > n then
                        "green"

                    else if p < n then
                        "red"

                    else
                        ""
                )
                (String.toFloat prev)
                (String.toFloat next)
    in
    case res of
        Nothing ->
            ( "", False )

        Just x ->
            ( x, True )
