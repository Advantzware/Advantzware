/* t-boxlld.p  Load box-design-line record */
DEF VAR ii AS INT NO-UNDO.

DEF TEMP-TABLE tt-bline like box-design-line.

INPUT FROM d:\datadump\boxline.d NO-ECHO.

REPEAT :
    CREATE tt-bline.
    IMPORT tt-bline.
    IF tt-bline.e-num = 0 THEN NEXT.

    if tt-bline.company = "" then tt-bline.company = "001".
    
/*    
    FIND FIRST eb WHERE eb.company = tt-bline.company
                     and  eb.e-num = tt-bline.e-num NO-LOCK NO-ERROR.
    IF AVAIL eb THEN ASSIGN  tt-bline.company = eb.company
                             tt-bline.est-no = eb.est-no
                             tt-bline.eqty = eb.eqty
                             tt-bline.form-no = eb.form-no
                             tt-bline.blank-no = eb.blank-no.
                                                        
    FIND FIRST box-design-line WHERE box-design-line.company = tt-bline.company
                                AND box-design-line.design-no = tt-bline.design-no
                                AND box-design-line.est-no = tt-bline.est-no
                                AND box-design-line.eqty = tt-bline.eqty
                                AND box-design-line.form-no = tt-bline.form-no
                                AND box-design-line.blank-no = tt-bline.blank-no 
                                and box-design-line.line-no = tt-bline.line-no                                                               
                                NO-LOCK NO-ERROR.
    IF NOT AVAIL box-design-line THEN DO:
       CREATE box-design-line.
       BUFFER-COPY tt-bline TO box-design-line.
       find box-design-hdr of box-design-line no-error.
       if avail box-design-hdr then 
          box-design-hdr.box-text = box-design-hdr.box-text +
                            box-design-line.line-text + chr(13).
                                    
       ii = ii + 1.
    END.
    
*/  
    
    find first box-design-hdr where box-design-hdr.e-num = tt-bline.e-num
                                and box-design-hdr.form-no = tt-bline.form-no
                                and box-design-hdr.blank-no = tt-bline.blank-no
                                no-error.                       
    if avail box-design-hdr then do:
       assign tt-bline.company = box-design-hdr.company
              tt-bline.est-no = box-design-hdr.est-no
              tt-bline.eqty = box-design-hdr.eqty
              .   
              
       CREATE box-design-line.
       BUFFER-COPY tt-bline TO box-design-line.
       box-design-hdr.box-text = box-design-hdr.box-text +
                                 box-design-line.line-text + chr(13).
       
       ii = ii + 1.
    end.
    
    DISP ii tt-bline.est-no tt-bline.eqty form "->>>,>>>,>>>9.99"
         tt-bline.form-no tt-bline.line-no .
    PAUSE 0.
    delete tt-bline.
END.



