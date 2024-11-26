credits :: (Char, Integer) -> (Char, Integer) -> Integer
credits (c1, n1) (c2, n2) =
    if (c1 == 's' && n1 == 14 ) || (c2 == 's' && n2 == 14) then 14
    else if c1 == c2 && (n1+1 == n2 || n1-1 == n2) then 8
    else if n1 == n2 && c1 /= c2 then 6
    else if c1 /= c2 && (n1+1 == n2 || n1-1 == n2) then 4 
    else if c1 == c2 then 2
    else 0