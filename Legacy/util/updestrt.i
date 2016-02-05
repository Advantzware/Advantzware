FOR EACH est
    WHERE est.company EQ cocode
      AND est.est-no GE FILL(" ",8 - LENGTH(fest)) + TRIM(fest)
      AND est.est-no LE FILL(" ",8 - LENGTH(test)) + TRIM(test) 
    NO-LOCK,
    EACH est-op
    WHERE est-op.company EQ est.company
      AND est-op.est-no EQ est.est-no
      AND est-op.m-code GE fmch
      AND est-op.m-code LE tmch,
    FIRST mach
    WHERE mach.company EQ est-op.company
      AND mach.m-code EQ est-op.m-code
    NO-LOCK:
    
    STATUS DEFAULT " Processing...    Est: " + TRIM(STRING(est.est-no)) +
			     "  Machine: " + est-op.m-code.
    ASSIGN
        est-op.op-rate[1] = mach.mr-trate
        est-op.op-rate[2] = mach.run-trate
        .
END.

STATUS DEFAULT "".

