
booleanFn a b c =
    if a == True
    then if b == False then c else False
    else if b || c == False then False else True