/* sys/inc/SelRptCol.i       SelectRptColumn => Reports */

DEFINE VARIABLE SelectRptColumn-log LIKE sys-ctrl.log-fld  NO-UNDO.
DEFINE VARIABLE SelectRptColumn-cha LIKE sys-ctrl.char-fld NO-UNDO.


FIND FIRST sys-ctrl NO-LOCK
     WHERE sys-ctrl.company EQ cocode
       AND sys-ctrl.name    EQ "Reports" /*"SelectRptColumn"*/
     NO-ERROR.
IF NOT AVAILABLE sys-ctrl THEN DO :
    CREATE sys-ctrl.
    ASSIGN 
        sys-ctrl.company  = cocode
        sys-ctrl.name     = "Reports" /*SelectRptColumn"*/
        sys-ctrl.log-fld  = NO
        sys-ctrl.char-fld = ""
        sys-ctrl.descrip  = "Selectable Report Columns"
        .
 
END.

ASSIGN 
    SelectRptColumn-log = sys-ctrl.log-fld
    SelectRptColumn-cha = sys-ctrl.char-fld
    .

FIND FIRST sys-ctrl-shipto OF sys-ctrl NO-LOCK
     WHERE sys-ctrl-shipto.char-fld = "{1}"
     NO-ERROR.
IF NOT AVAILABLE sys-ctrl-shipto THEN DO :   /* Task 01201407  */
    CREATE sys-ctrl-shipto.
    ASSIGN 
        sys-ctrl-shipto.company  = cocode
        sys-ctrl-shipto.name     = "Reports" /*SelectRptColumn"*/
        sys-ctrl-shipto.log-fld  = NO
        sys-ctrl-shipto.char-fld = "{1}"
        sys-ctrl-shipto.descrip  = "Selectable Report Columns"
        .
END.

IF AVAILABLE sys-ctrl-shipto THEN
ASSIGN 
    SelectRptColumn-log = sys-ctrl-ship.log-fld
    SelectRptColumn-cha = sys-ctrl.char-fld
    .
