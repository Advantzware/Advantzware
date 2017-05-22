
DEF INPUT PARAM ip-company      LIKE oe-ord.company     NO-UNDO.
DEF INPUT PARAM ip-ord-no       LIKE oe-ord.ord-no      NO-UNDO.
DEF INPUT PARAM ip-update       AS   LOG                NO-UNDO.

{sys/inc/var.i NEW SHARED}


cocode = ip-company.

{sys/inc/oeuserid.i}

DISABLE TRIGGERS FOR LOAD OF oe-ord.

IF oeuserid-log OR ip-update THEN
FOR EACH oe-ord
    WHERE oe-ord.company EQ ip-company
      AND oe-ord.ord-no  EQ ip-ord-no:
  oe-ord.user-id = USERID("nosweat").
END.
