
DEF INPUT PARAM ip-company LIKE smanmtrx.company NO-UNDO.
DEF INPUT PARAM ip-type    AS INT                NO-UNDO.
DEF INPUT PARAM ip-field   AS CHAR               NO-UNDO.


IF ip-type EQ 1 THEN
FOR EACH smanmtrx WHERE smanmtrx.company EQ ip-company:
  DELETE smanmtrx.
END.

ELSE
IF ip-type EQ 2 THEN
FOR EACH smanmtrx
    WHERE smanmtrx.company EQ ip-company
      AND smanmtrx.sman    EQ ip-field:
  DELETE smanmtrx.
END.

ELSE
IF ip-type EQ 3 THEN
FOR EACH smanmtrx
    WHERE smanmtrx.company EQ ip-company
      AND smanmtrx.procat  EQ ip-field:
  DELETE smanmtrx.
END.

ELSE
IF ip-type EQ 4 THEN
FOR EACH smanmtrx
    WHERE smanmtrx.company EQ ip-company
      AND smanmtrx.custype EQ ip-field:
  DELETE smanmtrx.
END.
