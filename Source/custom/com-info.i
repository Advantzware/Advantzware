
DEF INPUT  PARAM ip-company LIKE sman.company   NO-UNDO.
DEF INPUT  PARAM ip-sman    LIKE sman.sman      NO-UNDO.
DEF INPUT  PARAM ip-type    LIKE cust.type      NO-UNDO.
DEF INPUT  PARAM ip-procat  LIKE procat.procat  NO-UNDO.
DEF INPUT  PARAM ip-net-p   AS   DEC            NO-UNDO.
DEF INPUT  PARAM ip-cust-no AS   CHAR           NO-UNDO.

DEF OUTPUT PARAM op-{1}     LIKE sman.{1}       NO-UNDO.

DEF VAR li AS INT NO-UNDO.
DEF VAR lv AS CHAR NO-UNDO.

FIND FIRST sman
    WHERE sman.company EQ ip-company
      AND sman.sman    EQ ip-sman
    NO-LOCK NO-ERROR.
  
IF AVAIL sman THEN DO:
  lv = STRING(sman.{1}).

  IF "{1}" NE "scomm" OR sman.commbasis NE 'M' THEN
  DO:
    IF NOT CAN-FIND(FIRST smanmtrx
                    WHERE smanmtrx.company EQ ip-company
                      AND smanmtrx.sman    EQ ip-sman
                      AND smanmtrx.custype EQ ip-type
                      AND smanmtrx.procat  EQ ip-procat) THEN ip-procat = "".
   
    FOR EACH smanmtrx NO-LOCK
        WHERE smanmtrx.company EQ ip-company
          AND smanmtrx.sman    EQ ip-sman
          AND smanmtrx.custype EQ ip-type
          AND smanmtrx.procat  EQ ip-procat
        BREAK BY smanmtrx.netpct:
   
      IF LAST(smanmtrx.netpct)       OR
         smanmtrx.netpct GE ip-net-p OR
         smanmtrx.commbasis NE "N"   THEN DO:
        lv = IF "{1}" EQ "scomm" THEN STRING(smanmtrx.comm)
                                 ELSE smanmtrx.commbasis.
        LEAVE.
      END.
    END.
  END.
  ELSE
  DO:     
    FIND FIRST cust WHERE
         cust.company EQ ip-company AND
         cust.cust-no EQ ip-cust-no
         NO-LOCK NO-ERROR.

    IF AVAIL cust THEN
    DO:
       ASSIGN lv = STRING(cust.flatCommPct).
       IF DEC(lv) = 0 THEN DO: 
           FIND FIRST smanmtrx WHERE
                smanmtrx.company EQ ip-company AND
                smanmtrx.sman    EQ ip-sman AND
                smanmtrx.custype EQ ip-type AND
                smanmtrx.procat  EQ "" AND
                smanmtrx.commbasis EQ "M" AND
                smanmtrx.netpct  EQ cust.markup
                NO-LOCK NO-ERROR.

           IF AVAIL smanmtrx THEN
              lv = STRING(smanmtrx.comm).
           ELSE
           DO:
              FIND LAST smanmtrx WHERE
                   smanmtrx.company EQ ip-company AND
                   smanmtrx.sman    EQ ip-sman AND
                   smanmtrx.custype EQ ip-type AND
                   smanmtrx.procat  EQ "" AND
                   smanmtrx.commbasis EQ "M" AND
                   smanmtrx.netpct  LT cust.markup
                   NO-LOCK NO-ERROR.

              IF AVAIL smanmtrx THEN
                 lv = STRING(smanmtrx.comm).
           END.
       END.
    END.     
  END.
END.
