/* itemfg.i */

&IF '{&itemfg-maint}' NE '' &THEN
    RUN ENABLE-itemfg-field .  /* in itemfg's viewer */
&ENDIF
&IF '{&itemfgQty-maint}' NE '' &THEN
IF cbLoc NE "ALL" THEN DO WITH FRAME {&FRAME-NAME}:
    DISABLE {&List-nonreord} .
END.
ELSE DO WITH FRAME {&FRAME-NAME}:
    ENABLE {&List-nonreord} .
END.
&ENDIF
