/* sys/inc/sspostvt.i SSPostVenTag */
  
DEF VAR SSPostFGVT-log  LIKE sys-ctrl.log-fld   NO-UNDO.
DEF VAR SSPostFGVT-Char LIKE sys-ctrl.char-fld  NO-UNDO.
DEF VAR SSPostFGVT-int  LIKE sys-ctrl.int-fld   NO-UNDO.

FIND FIRST sys-ctrl NO-LOCK
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "SSPostVenTag"
    NO-ERROR.

IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "SSPostVenTag"
   sys-ctrl.log-fld  = NO
   sys-ctrl.descrip  = "SS Raw Materials Scan Vendor Tags to Post to Inventory Immediately".
END.

ASSIGN SSPostFGVT-log  = sys-ctrl.log-fld
       SSPostFGVT-char = sys-ctrl.char-fld
       SSPostFGVT-int  = sys-ctrl.int-fld.
 
