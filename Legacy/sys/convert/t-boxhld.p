/* t-boxhld.p  Load box-design-hdr record */
DEF VAR ii AS INT NO-UNDO.

DEF TEMP-TABLE tt-bhdr like box-design-hdr.

INPUT FROM d:\datadump\boxheadr.d NO-ECHO.

REPEAT :
    CREATE tt-bhdr.
    IMPORT tt-bhdr.
    IF tt-bhdr.e-num = 0 THEN NEXT.
    
    if tt-bhdr.company = "" then tt-bhdr.company = "001".    
    FIND FIRST eb WHERE eb.company = tt-bhdr.company
                    and eb.e-num = tt-bhdr.e-num NO-LOCK NO-ERROR.
    IF AVAIL eb THEN ASSIGN  tt-bhdr.company = eb.company
                             tt-bhdr.est-no = eb.est-no
                             tt-bhdr.eqty = eb.eqty
                             /*tt-bhdr.form-no = eb.form-no
                             tt-bhdr.blank-no = eb.blank-no*/ .
                            
    /*                        
    FIND FIRST box-design-hdr WHERE box-design-hdr.company = tt-bhdr.company
                                AND box-design-hdr.design-no = tt-bhdr.design-no
                                AND box-design-hdr.est-no = tt-bhdr.est-no
                                AND box-design-hdr.eqty = tt-bhdr.eqty
                                AND box-design-hdr.form-no = tt-bhdr.form-no
                                AND box-design-hdr.blank-no = tt-bhdr.blank-no
                                NO-LOCK NO-ERROR.
    */
    find first box-design-hdr where box-design-hdr.e-num = tt-bhdr.e-num
                                and box-design-hdr.form-no = tt-bhdr.form-no
                                and box-design-hdr.blank-no = tt-bhdr.blank-no
                                no-lock no-error.
    IF NOT AVAIL box-design-hdr THEN DO:
       CREATE box-design-hdr.
       BUFFER-COPY tt-bhdr TO box-design-hdr.
    END.
    

                           
    ii = ii + 1.
    DISP ii tt-bhdr.est-no tt-bhdr.eqty form "->>>,>>>,>>9.99"
       tt-bhdr.form-no .
    PAUSE 0.


    delete tt-bhdr.
END.

