/* NK1Values.p */

DEFINE INPUT PARAMETER ipCompany AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opNK1Values AS CHARACTER NO-UNDO.

FIND FIRST sys-ctrl NO-LOCK WHERE sys-ctrl.company EQ ipCompany
                              AND sys-ctrl.name EQ 'Graphic' NO-ERROR.
IF NOT AVAILABLE sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
    sys-ctrl.company = ipCompany
    sys-ctrl.name = 'Graphic'
    sys-ctrl.descrip = 'Item Images Location'
    sys-ctrl.char-fld = 'r:\rcode\itemImages'.
END.
opNK1Values = sys-ctrl.char-fld.

FIND FIRST sys-ctrl NO-LOCK WHERE sys-ctrl.company EQ ipCompany
                              AND sys-ctrl.name EQ 'tempImg' NO-ERROR.
IF NOT AVAILABLE sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
    sys-ctrl.company = ipCompany
    sys-ctrl.name = 'tempImg'
    sys-ctrl.descrip = 'Item Images Temp. Location'
    sys-ctrl.char-fld = 'r:\rcode\tempImages'
    sys-ctrl.int-fld = 5.
END.
opNK1Values = opNK1Values + ',' + sys-ctrl.char-fld + ',' + STRING(sys-ctrl.int-fld).
