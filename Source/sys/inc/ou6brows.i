DEFINE VARIABLE ou6brows AS CHARACTER NO-UNDO.

FIND FIRST sys-ctrl NO-LOCK
     WHERE sys-ctrl.company EQ cocode
       AND sys-ctrl.name EQ 'OU6BROWS' NO-ERROR. 
IF NOT AVAILABLE sys-ctrl THEN DO TRANSACTION:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company = cocode
   sys-ctrl.name = 'OU6BROWS'
   sys-ctrl.descrip = 'Sort by Order# or Order Inquiry?'
   sys-ctrl.char-fld = 'Order Inquiry'.
END.
ou6brows = sys-ctrl.char-fld.
