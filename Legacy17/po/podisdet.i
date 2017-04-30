/* po/podisdet.i */
{po/podisdet2.i  "STRING" ":SCREEN-VALUE" "INPUT"}

scr-cons-uom:SCREEN-VALUE = po-ordl.cons-uom:SCREEN-VALUE IN FRAME {&FRAME-NAME}.

DISP v-tot-msf WITH FRAME {&FRAME-NAME}.

IF po-ordl.item-type:SCREEN-VALUE = "RM" AND
   po-ordl.i-no:SCREEN-VALUE NE "" THEN
   DO:
      FIND FIRST buf-item WHERE
           buf-item.company EQ cocode AND
           buf-item.i-no EQ po-ordl.i-no:SCREEN-VALUE
           NO-LOCK NO-ERROR.
     
      IF AVAIL buf-item AND buf-item.mat-type EQ "B" THEN
         ASSIGN
             v-tonnage:HIDDEN = NO
             v-tonnage:SCREEN-VALUE = STRING(v-tot-msf * buf-item.basis-w / 2000).
      ELSE
        v-tonnage:HIDDEN = YES.
   END.
   ELSE
      v-tonnage:HIDDEN = YES.


