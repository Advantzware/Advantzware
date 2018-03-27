/* oe/oescomm.i   calc salesman commission */

DEF VAR lv-{2} AS DEC DECIMALS 10 NO-UNDO.
DEF VAR lv-{2}-cha AS CHAR NO-UNDO.

IF {1} NE "" THEN DO:
   lv-{2}-cha = {1}.

   RUN sys/inc/getsmncm.p (INPUT oe-ordl.cust-no,
                           INPUT-OUTPUT lv-{2}-cha,
                           INPUT itemfg.procat,
                           0,
                           OUTPUT lv-{2}).

   /*IF lv-{2} NE 0 THEN*/ oe-ordl.s-comm[{2}]:SCREEN-VALUE = STRING(lv-{2}).
END.
