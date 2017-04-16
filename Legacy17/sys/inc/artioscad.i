/* sys/inc/artios.i*/

DEF VAR artioscad-log LIKE sys-ctrl.log-fld INIT NO NO-UNDO.
DEF VAR artioscad-chr LIKE sys-ctrl.char-fld INIT "" NO-UNDO.
DEF VAR artioscad-int LIKE sys-ctrl.int-fld INIT 0 NO-UNDO.
DEF VAR artioscad-dec LIKE sys-ctrl.dec-fld INIT 0 NO-UNDO.

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "artiosCAD"
    NO-LOCK NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company = cocode
   sys-ctrl.name    = "ArtiosCAD"
   sys-ctrl.log-fld = YES
   sys-ctrl.descrip = "Artios Cad Integrated into Estimating"
   sys-ctrl.descrip = "c:\artios\asi".  
END.

ASSIGN
 artioscad-log = sys-ctrl.log-fld
 artioscad-chr = sys-ctrl.char-fld
 artioscad-dec = sys-ctrl.dec-fld
 artioscad-int = sys-ctrl.int-fld   .

