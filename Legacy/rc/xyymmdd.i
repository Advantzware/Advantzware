    if {1} = "000000" or {1} = "      " or {1} = "00000000" then {2} = ?.
    else do:
        if length({1}) <= 6 then do:
        yr = integer(substring({1},1,2)) + {rc/century.i}.
        mo = integer(substring({1},3,2)).
        da = integer(substring({1},5,2)).
        end.
        else if length({1}) <= 8 then do:
        yr = integer(substring({1},1,4)).
        mo = integer(substring({1},5,2)).
        da = integer(substring({1},7,2)).
        end.
        {2} = date(mo,da,yr) no-error.
    end.
