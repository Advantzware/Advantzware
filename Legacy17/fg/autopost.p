
DEF INPUT  PARAM ip-rowid   AS   ROWID          NO-UNDO.
DEF INPUT  PARAM ip-job-no  LIKE fg-bin.job-no  NO-UNDO.
DEF INPUT  PARAM ip-job-no2 LIKE fg-bin.job-no2 NO-UNDO.
DEF OUTPUT PARAM op-loc     LIKE fg-bin.loc     NO-UNDO.
DEF OUTPUT PARAM op-loc-bin LIKE fg-bin.loc-bin NO-UNDO.

DEF VAR ll-shipto AS LOG NO-UNDO.

{sys/inc/var.i NEW SHARED}


FIND itemfg WHERE ROWID(itemfg) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL itemfg THEN DO:
  cocode = itemfg.company.

  {sys/inc/fgwhsbin.i}

  IF LOOKUP(fgwhsbin-cha,"AUTOPOST,FGITEM") GT 0 THEN DO:
    ASSIGN
     op-loc     = itemfg.def-loc
     op-loc-bin = itemfg.def-loc-bin.

    IF fgwhsbin-cha EQ "AUTOPOST" THEN DO:
      FIND FIRST fg-bin
          WHERE fg-bin.company EQ itemfg.company
            AND fg-bin.i-no    EQ itemfg.i-no
            AND fg-bin.job-no  EQ ip-job-no
            AND ((ip-job-no NE "" AND
                  fg-bin.job-no2 EQ ip-job-no2) OR
                 ip-job-no EQ "")
          NO-LOCK NO-ERROR.

      IF AVAIL fg-bin THEN
        ASSIGN
         op-loc     = fg-bin.loc
         op-loc-bin = fg-bin.loc-bin.

      FIND FIRST sys-ctrl
          WHERE sys-ctrl.company EQ itemfg.company
            AND sys-ctrl.name    EQ "AUTOPOST"
          NO-LOCK NO-ERROR.

      ll-shipto = NOT AVAIL sys-ctrl OR sys-ctrl.char-fld NE "FGFile".

      /*get warehouse and bin from shipto file*/
      IF ll-shipto THEN DO:
        /*get estimate blank file from finished goods item file*/
        FIND FIRST eb
            WHERE eb.company  EQ itemfg.company
              AND eb.est-no   EQ itemfg.est-no
              AND eb.stock-no EQ itemfg.i-no
            USE-INDEX est-no NO-LOCK NO-ERROR.

        IF AVAIL eb THEN
        /*get customer file from estimate blank file*/
        FIND FIRST cust
            WHERE cust.company EQ eb.company
              AND cust.cust-no EQ eb.cust-no
            NO-LOCK NO-ERROR.
      END. /* sys-ctrl.char-fld eq "ShipTo" */
    END.

    /*if bin and warehouse are blank, goto cust "X" shipto file*/
    IF (op-loc EQ "" AND op-loc-bin EQ "") OR ll-shipto THEN DO:
      IF NOT AVAIL cust THEN
      FIND FIRST cust
          WHERE cust.company EQ itemfg.company
            AND cust.active  EQ "X"
          NO-LOCK NO-ERROR.
      
      IF AVAIL cust THEN
      FIND FIRST shipto
          WHERE shipto.company EQ cust.company
            AND shipto.cust-no EQ cust.cust-no
          NO-LOCK NO-ERROR.
 
      IF AVAIL shipto AND shipto.loc NE "" AND shipto.loc-bin NE "" THEN DO:
        ASSIGN
         op-loc     = shipto.loc
         op-loc-bin = shipto.loc-bin.

        RELEASE fg-bin.
      END.
    END.
  END.
      
  ELSE
    ASSIGN
     op-loc     = SUBSTR(fgwhsbin-cha,1,5)
     op-loc-bin = SUBSTR(fgwhsbin-cha,6,8).

  IF fgwhsbin-cha NE "FGITEM" THEN DO:
    IF NOT AVAIL fg-bin THEN
    FIND FIRST fg-bin
        WHERE fg-bin.company EQ itemfg.company
          AND fg-bin.i-no    EQ ""
          AND fg-bin.loc     EQ op-loc
          AND fg-bin.loc-bin EQ op-loc-bin
        NO-LOCK NO-ERROR.

    IF NOT AVAIL fg-bin OR op-loc EQ "" OR op-loc-bin EQ "" THEN
      ASSIGN
       op-loc     = ""
       op-loc-bin = "".
  END.
END.
