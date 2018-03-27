
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

{sys/inc/var.i NEW SHARED}
DEF VAR v-is-component AS LOG NO-UNDO.
DEF BUFFER bf-eb FOR eb .

FIND itemfg WHERE ROWID(itemfg) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL itemfg THEN DO:
  cocode = itemfg.company.

  DO TRANSACTION:
    {sys/inc/prodcode.i}
  END.

  IF prodcode-log AND itemfg.prod-code EQ "" THEN DO TRANSACTION:
    FIND CURRENT itemfg.
    itemfg.prod-code = "NEW".

    FOR EACH job-hdr
        WHERE job-hdr.company EQ itemfg.company
          AND job-hdr.i-no    EQ itemfg.i-no
        NO-LOCK
        BREAK BY job-hdr.i-no:
      IF NOT FIRST(job-hdr.i-no) THEN DO:
        itemfg.prod-code = "REPEAT".
        LEAVE.
      END.
    END.
  END.
END.
