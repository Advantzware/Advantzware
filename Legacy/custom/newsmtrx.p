
DEF INPUT  PARAM ip-company LIKE sman.company   NO-UNDO.
DEF INPUT  PARAM ip-sman    LIKE sman.sman      NO-UNDO.
DEF INPUT  PARAM ip-type    LIKE cust.type      NO-UNDO.
DEF INPUT  PARAM ip-procat  LIKE procat.procat  NO-UNDO.
DEF INPUT  PARAM ip-field   AS   CHAR           NO-UNDO.
DEF OUTPUT PARAM op-value   AS   CHAR           NO-UNDO.


FOR EACH smanmtrx NO-LOCK
    WHERE smanmtrx.company EQ ip-company
      AND smanmtrx.sman    EQ ip-sman
      AND smanmtrx.custype EQ ip-type
      AND smanmtrx.procat  EQ ip-procat:

  op-value = IF ip-field EQ "scomm" THEN STRING(smanmtrx.comm)
                                    ELSE smanmtrx.commbasis.
  LEAVE.
END.
