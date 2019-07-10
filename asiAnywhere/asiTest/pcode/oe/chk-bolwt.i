   

IF BOLWt-log THEN DO:
  FOR EACH oe-rell
    WHERE oe-rell.company EQ oe-relh.company
      AND oe-rell.r-no    EQ oe-relh.r-no
    USE-INDEX r-no NO-LOCK
     BY oe-rell.ord-no
     BY SUBSTR(oe-rell.rec_key,5,4)
     BY SUBSTR(oe-rell.rec_key,1,4)
     BY SUBSTR(oe-rell.rec_key,10,100):

    IF TRIM(oe-rell.tag) EQ "" THEN NEXT.

    FIND FIRST oe-boll EXCLUSIVE-LOCK
      WHERE oe-boll.company EQ oe-bolh.company
        AND oe-boll.b-no    EQ oe-bolh.b-no
        AND oe-boll.i-no    EQ oe-rell.i-no
        AND oe-boll.ord-no  EQ oe-rell.ord-no
        AND oe-boll.tag     EQ oe-rell.tag
      USE-INDEX b-no NO-ERROR.
    IF AVAIL oe-boll THEN DO:    

      FIND FIRST loadtag NO-LOCK 
        WHERE loadtag.company = g_company
          AND loadtag.tag-no  = oe-rell.tag NO-ERROR.
       IF AVAIL loadtag           
         THEN ASSIGN oe-boll.weight = (
                                       (oe-rell.cases * loadtag.misc-dec[1]) +
                                        loadtag.misc-dec[3]
                                      ).
    END.    
  END.
END.
