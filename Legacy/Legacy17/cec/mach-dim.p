
DEF INPUT  PARAM ip-rowid1  AS ROWID NO-UNDO.
DEF INPUT  PARAM ip-rowid2  AS ROWID NO-UNDO.
DEF INPUT  PARAM ip-len     AS DEC   NO-UNDO.
DEF INPUT  PARAM ip-wid     AS DEC   NO-UNDO.
DEF INPUT  PARAM ip-dep     AS DEC   NO-UNDO.
DEF INPUT  PARAM ip-qty     AS DEC   NO-UNDO.
DEF OUTPUT PARAM op-reason  AS CHAR  NO-UNDO.

{est/d-machex.i}

DEF BUFFER set-eb FOR eb.


FIND tt-mach-exc WHERE ROWID(tt-mach-exc) EQ ip-rowid1 NO-ERROR.

IF AVAIL tt-mach-exc THEN
FIND eb WHERE ROWID(eb) EQ ip-rowid2 NO-LOCK NO-ERROR.

IF AVAIL eb THEN
FIND FIRST mach NO-LOCK
    WHERE mach.company EQ eb.company
      AND mach.m-code  EQ tt-mach-exc.m-code
    NO-ERROR.

IF AVAIL mach THEN DO:
  IF CAN-DO("A,P",mach.p-type)                AND
     CAN-FIND(FIRST style
              WHERE style.company EQ eb.company
                AND style.style   EQ eb.style
                AND CAN-DO("P,R",style.type)) THEN
  FIND FIRST set-eb NO-LOCK
      WHERE set-eb.company EQ eb.company
        AND set-eb.est-no  EQ eb.est-no
        AND set-eb.form-no EQ 0
      NO-ERROR.
  IF AVAIL set-eb THEN
    ASSIGN
     ip-len = set-eb.len
     ip-wid = set-eb.wid
     ip-dep = set-eb.dep.

  IF mach.min-len GT ip-len THEN op-reason = "Minimum Front-To-Back".
  ELSE
  IF mach.max-len LT ip-len THEN op-reason = "Maximum Front-To-Back".
  ELSE
  IF mach.min-wid GT ip-wid THEN op-reason = "Minimum Side-To-Side".
  ELSE
  IF mach.max-wid LT ip-wid THEN op-reason = "Maximum Side-To-Side".
  ELSE
  IF mach.min-cal GT ip-dep THEN op-reason = "Minimum Caliper/Depth".
  ELSE
  IF mach.max-cal LT ip-dep THEN op-reason = "Maximum Caliper/Depth".
  ELSE
  IF mach.min-run GT ip-qty THEN op-reason = "Minimum Run Qty".
  ELSE
  IF mach.max-run LT ip-qty THEN op-reason = "Maximum Run Qty".
END.
