
FIND FIRST bf-period                   
     WHERE bf-period.company EQ cocode
     AND bf-period.pst     LE date({1})
     AND bf-period.pend    GE date({1})
     AND bf-period.pnum   EQ MONTH(DATE({2}))
     NO-LOCK NO-ERROR.

IF NOT AVAIL bf-period THEN
DO:     
     MESSAGE (IF INTEGER({3}) EQ 1 THEN "Invoice" ELSE "Receipt") + " date must be in posted date period." VIEW-AS ALERT-BOX ERROR.
     RETURN NO-APPLY.
END.