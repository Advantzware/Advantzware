DEF input param report-id AS CHAR FORMAT "x(12)" NO-UNDO.
    if opsys BEGINS "win" then do:
        dos silent start value(report-id).
    end.
    else 
    if opsys = "msdos" then do:
        dos silent edit value(report-id).
    end.
    else if opsys = "unix"
    then unix silent pg value(report-id).
    else if opsys = "ctos" or opsys = "btos"
    then ctos silent edit value(report-id).
    else do:
        run rc/notimpl.p.
    end.    
