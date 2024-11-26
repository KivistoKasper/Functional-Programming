maxhr :: Float -> Float 
maxhr x = 
    if x > 40 then 207-0.7*x
    else 220-x
