/* sys/inc/xmlorder.i */

DEF VAR xmlorder-log LIKE sys-ctrl.log-fld INIT NO NO-UNDO.
DEF VAR xmlorder-chr LIKE sys-ctrl.char-fld INIT "" NO-UNDO.
DEF VAR xmlorder-int LIKE sys-ctrl.int-fld INIT 0 NO-UNDO.
DEF VAR xmlorder-dec LIKE sys-ctrl.dec-fld INIT 0 NO-UNDO.

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "xmlorder"
    NO-LOCK NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company = cocode
   sys-ctrl.name    = "XMLOrder"
   sys-ctrl.log-fld = yes
   sys-ctrl.descrip = "XML Order Release creation"
   sys-ctrl.char-fld = "c:\tmp\" .
END.

ASSIGN
 xmlorder-log = sys-ctrl.log-fld
 xmlorder-chr = sys-ctrl.char-fld.

/*IF xmlorder-log THEN xmlorder-int = sys-ctrl.int-fld.
*/
