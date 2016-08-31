
DEF VAR relmerge-log AS LOG NO-UNDO.
DEF VAR relmerge-int AS INT NO-UNDO.
DEF VAR relmerge-chr AS CHAR NO-UNDO.


FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "relmerge"
    NO-LOCK NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company = cocode
   sys-ctrl.name    = "RELMERGE"
   sys-ctrl.descrip = "When creating actual releases, prompt to merge into printed release?"
   sys-ctrl.int-fld = 1.
  
end.
assign
 relmerge-log = sys-ctrl.log-fld
 relmerge-int = sys-ctrl.int-fld
 relmerge-chr = sys-ctrl.char-fld.
