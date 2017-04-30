/* ------------------------------------------------ util/glprevyr.p 04/98 JLF */
/* Update Previous Year Closed? Flag                                          */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i NEW SHARED}
{sys/form/s-top.f}

DEF VAR v-prev-yr AS INT FORMAT "9999".


FOR EACH company:
  RELEASE period.

  FIND FIRST period
      WHERE period.company EQ company.company
	    AND period.pstat   EQ YES
      NO-LOCK NO-ERROR.

  IF AVAIL period THEN v-prev-yr = period.yr - 1.

  ELSE DO:
    FIND LAST period WHERE period.company EQ company.company NO-LOCK NO-ERROR.
    v-prev-yr = period.yr.
  END.

  IF AVAIL period THEN
    MESSAGE "Company: " company.company         SKIP
	  	    "Previous Fiscal Year: " v-prev-yr  SKIP(1)
            "Is the previous fiscal year closed?:"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE company.yend-per.

  ELSE company.yend-per = YES.

  v-prev-yr = 0.
END.

/* end ---------------------------------- copr. 1998  advanced software, inc. */
