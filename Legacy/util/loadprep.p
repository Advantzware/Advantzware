
DEF VAR ll AS LOG INIT NO NO-UNDO.

DEF BUFFER b-eb FOR eb.
DEF BUFFER b-prep FOR prep.


MESSAGE "Are you ready to load the prep file from all estimates?"
    VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll.

IF ll THEN DO:
  FOR EACH eb
      WHERE eb.form-no NE 0
        AND (TRIM(eb.die-no) GT "" OR TRIM(eb.plate-no) GT "")
      NO-LOCK,

      FIRST ef
      WHERE ef.company EQ eb.company
        AND ef.est-no  EQ eb.est-no
        AND ef.form-no EQ eb.form-no
      NO-LOCK,

      FIRST est
      WHERE est.company EQ ef.company
        AND est.est-no  EQ ef.est-no
      NO-LOCK

      TRANSACTION

      WITH FRAME prep WITH DOWN:

    DISPLAY eb.company      LABEL "Company"
            TRIM(eb.est-no) LABEL "Est#"        FORMAT "x(10)"
            eb.form-no      LABEL "Form#"
            eb.blank-no     LABEL "Blank#".

    IF TRIM(eb.die-no)   GT "" THEN RUN update-prep (1).
    IF TRIM(eb.plate-no) GT "" THEN RUN update-prep (2).
  END.

  HIDE FRAME prep NO-PAUSE.

  MESSAGE "Procedure has completed..." VIEW-AS ALERT-BOX.
END.

RETURN.

PROCEDURE update-prep.
  DEF INPUT PARAM ip-type AS INT NO-UNDO.

  DEF VAR lv-code LIKE prep.code NO-UNDO.
  DEF VAR lv-date AS DATE NO-UNDO.


  lv-code = IF ip-type EQ 1 THEN eb.die-no ELSE eb.plate-no.

  FIND FIRST prep
      WHERE prep.company EQ eb.company
        AND prep.code    EQ lv-code
      NO-ERROR.
      
  IF NOT AVAIL prep THEN DO:
    CREATE prep.
    ASSIGN
     prep.company     = eb.company
     prep.loc         = eb.loc
     prep.code        = lv-code
     prep.dscr        = IF ip-type EQ 1 THEN "Cutting Die" ELSE "Printing Plate"
     prep.mat-type    = IF ip-type EQ 1 THEN "D" ELSE "P"
     prep.cost-type   = "MIS"
     prep.uom         = "EA"
     prep.last-date   = est.mod-date.

    FOR EACH cust
        WHERE cust.company EQ prep.company
          AND cust.active  EQ "X"
        NO-LOCK
        BY cust.rec_key:
      ASSIGN
       prep.owner[1]   = cust.cust-no
       prep.owner-%[1] = 100.
      LEAVE.
    END.

    FOR EACH b-prep
        WHERE b-prep.company  EQ prep.company
          AND b-prep.mat-type EQ prep.mat-type
        NO-LOCK
        BY b-prep.rec_key:
      ASSIGN
       prep.cost      = b-prep.cost
       prep.ml        = b-prep.ml
       prep.amtz      = b-prep.amtz
       prep.actnum    = b-prep.actnum
       prep.fgcat     = b-prep.fgcat
       prep.loc-bin   = b-prep.loc-bin.
      LEAVE.
    END.
  END.

  lv-date = est.mod-date.

  RELEASE oe-ord.
  IF eb.ord-no NE 0 THEN
  FIND FIRST oe-ord
      WHERE oe-ord.company  EQ eb.company
        AND oe-ord.ord-no   EQ eb.ord-no
      NO-LOCK NO-ERROR.
  IF AVAIL oe-ord AND oe-ord.ord-date GT lv-date THEN
    lv-date = oe-ord.ord-date.

  IF NEW prep OR lv-date GT prep.last-date THEN DO:
    ASSIGN
     prep.cust-no     = eb.cust-no
     prep.box-style   = eb.style
     prep.carton-w    = eb.wid
     prep.carton-l    = eb.len
     prep.carton-d    = eb.dep
     prep.die-w       = ef.trim-w
     prep.die-l       = ef.trim-l
     prep.last-est-no = eb.est-no
     prep.last-date   = lv-date.

    FOR EACH b-eb
        WHERE b-eb.company EQ ef.company
          AND b-eb.est-no  EQ ef.est-no
          AND b-eb.form-no EQ ef.form-no
        NO-LOCK:
      prep.number-up = prep.number-up + b-eb.num-up.
    END.

    IF prep.cust-no NE "" THEN
    FOR EACH cust
        WHERE cust.company EQ prep.company
          AND cust.cust-no EQ prep.cust-no
        NO-LOCK:
      prep.cust-name = cust.name.
      LEAVE.
    END.
  END.

END PROCEDURE.
