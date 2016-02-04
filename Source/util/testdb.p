DISABLE TRIGGERS FOR LOAD OF phone.
DISABLE TRIGGERS FOR LOAD OF emp_type.
DISABLE TRIGGERS FOR LOAD OF asihlp.hlp-head.
DISABLE TRIGGERS FOR LOAD OF uom.
DEF VAR vWereFound AS LOG NO-UNDO.
MESSAGE "db1" dbname(LDBNAME(1)) SKIP 
    "db2" LDBNAME(2) SKIP
    "db3" LDBNAME(3) SKIP
    "db4" LDBNAME(4) SKIP
    "db5" LDBNAME(5) SKIP
    VIEW-AS ALERT-BOX.
REPEAT:
    MESSAGE "Enter 0) Quit 1) Create Recs 2) Delete Recs 3) Test Recs"
        UPDATE vAns AS INT.

    CASE vAns:
        WHEN 0 THEN
            LEAVE.
        WHEN 1 THEN DO:
            RUN CREATE_recs.
        END.
        WHEN 2  THEN DO:
            RUN DELETE_recs.
        END.
        WHEN 3 THEN DO:
            RUN FIND_recs (OUTPUT vWereFound).
            IF vWereFound THEN
                MESSAGE "All Test Records Found!" VIEW-AS ALERT-BOX.
            ELSE
                MESSAGE "Not all Records were Found!"
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.
    END CASE.
END.
PROCEDURE CREATE_recs.
CREATE nosweat.phone.
ASSIGN nosweat.phone.TABLE_rec_key = "12345789"
       nosweat.phone.attention = "ASITEST".

CREATE emptrack.emp_type.
ASSIGN emptrack.emp_type.emp_type = "ASITEST"
       emptrack.emp_type.DESCRIPTION = "ASITEST"
       emptrack.emp_type.rec_key = "123456789".

CREATE asihlp.hlp-head.
ASSIGN asihlp.hlp-head.db-name = "ASTTEST"
       asihlp.hlp-head.frm-name = "ASTTEST"
       asihlp.hlp-head.usr-txt = YES
       asihlp.hlp-head.field-ord = 1 .

CREATE asi.uom.
ASSIGN asi.uom.uom = "ASI".
END PROCEDURE. /* create_recs */
       
PROCEDURE DELETE_recs:
FIND FIRST nosweat.phone WHERE nosweat.phone.TABLE_rec_key = "12345789"
       AND nosweat.phone.attention = "ASITEST"
    EXCLUSIVE-LOCK NO-ERROR.
IF AVAIL nosweat.phone THEN
    DELETE nosweat.phone.

FIND FIRST emptrack.emp_type WHERE
     emptrack.emp_type.emp_type = "ASITEST"
     AND   emptrack.emp_type.DESCRIPTION = "ASITEST"
     AND emptrack.emp_type.rec_key = "123456789"
    EXCLUSIVE-LOCK NO-ERROR.
IF AVAIL emptrack.emp_type THEN
    DELETE emptrack.emp_type.

FIND FIRST asihlp.hlp-head WHERE 
       asihlp.hlp-head.db-name = "ASTTEST"
       AND asihlp.hlp-head.frm-name = "ASTTEST"
       AND asihlp.hlp-head.usr-txt = YES
       AND asihlp.hlp-head.field-ord = 1 
    EXCLUSIVE-LOCK NO-ERROR.
IF AVAIL asihlp.hlp-head THEN
    DELETE asihlp.hlp-head.

FIND FIRST asi.uom
 WHERE asi.uom.uom = "ASI"
    EXCLUSIVE-LOCK NO-ERROR.

IF AVAIL asi.uom THEN
    DELETE asi.uom.
END PROCEDURE /* delete recs */.

PROCEDURE find_recs:
DEF OUTPUT PARAMETER oplWereFound AS LOG.
DEF VAR lFound AS LOG.

lFound = YES.
FIND FIRST nosweat.phone WHERE nosweat.phone.TABLE_rec_key = "12345789"
       AND nosweat.phone.attention = "ASITEST"
    NO-LOCK NO-ERROR.
IF NOT AVAIL nosweat.phone THEN
    lFound = NO.

FIND FIRST emptrack.emp_type WHERE
     emptrack.emp_type.emp_type = "ASITEST"
     AND   emptrack.emp_type.DESCRIPTION = "ASITEST"
     AND emptrack.emp_type.rec_key = "123456789"
    NO-LOCK NO-ERROR.
IF NOT AVAIL emptrack.emp_type THEN
    lFound = NO.

FIND FIRST asihlp.hlp-head WHERE 
       asihlp.hlp-head.db-name = "ASTTEST"
       AND asihlp.hlp-head.frm-name = "ASTTEST"
       AND asihlp.hlp-head.usr-txt = YES
       AND asihlp.hlp-head.field-ord = 1 
    NO-LOCK NO-ERROR.
IF NOT AVAIL asihlp.hlp-head THEN
    lFound = NO.

FIND FIRST asi.uom
 WHERE asi.uom.uom = "ASI"
    NO-LOCK NO-ERROR.

IF NOT AVAIL asi.uom THEN
    lFound = NO.
oplWereFound = lFound.

END PROCEDURE /* delete recs */.
