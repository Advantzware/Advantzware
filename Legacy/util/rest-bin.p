FOR EACH res.fg-bin WHERE res.fg-bin.i-no = "".
    FIND FIRST asi.fg-bin WHERE asi.fg-bin.company = res.fg-bin.company AND
        asi.fg-bin.i-no = res.fg-bin.i-no             AND
         asi.fg-bin.loc = res.fg-bin.loc AND
                     asi.fg-bin.loc-bin = res.fg-bin.loc-bin 
                      NO-LOCK NO-ERROR.

    IF NOT AVAIL asi.fg-bin THEN DO:
       DISP res.fg-bin.loc res.fg-bin.loc-bin res.fg-bin.qty.  
       CREATE asi.fg-bin.
       BUFFER-COPY res.fg-bin TO asi.fg-bin.

    END.

        
END.
