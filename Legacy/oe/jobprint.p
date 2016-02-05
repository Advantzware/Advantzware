/* oe/jobprint.p test print ticket will be from v-ord.w ord-from-est procedure or
                          jc/jc-calc.p */

{sys/inc/VAR.i NEW SHARED}


    {jcrep/r-ticket.i "new shared"}

    {custom/xprint.i}

    DEF NEW SHARED VAR lv-qty AS int NO-UNDO.
    DEF NEW SHARED VAR qty AS INT NO-UNDO.
    DEF NEW SHARED VAR v-shared-rel AS INT NO-UNDO.

    DEF NEW SHARED VAR s-prt-fgimage AS LOG NO-UNDO.
    DEF VAR is-xprint-form AS LOG NO-UNDO.
    DEF VAR ls-fax-file AS cha NO-UNDO.

    DEF NEW SHARED VAR s-prt-mstandard AS LOG NO-UNDO.
    DEF NEW SHARED VAR s-prt-shipto AS LOG NO-UNDO.
    DEF NEW SHARED VAR s-prt-sellprc AS LOG NO-UNDO.
    DEF NEW SHARED VAR s-run-speed AS LOG NO-UNDO.


DEF BUFFER xest FOR est.

ASSIGN
cocode = "001"
locode = "main"
reprint  = yes.

FIND FIRST job  NO-LOCK WHERE job-no = "   704" NO-ERROR.
MESSAGE AVAIL job job.job-no VIEW-AS ALERT-BOX.


IF AVAIL job THEN DO:
     ASSIGN
     fjob-no   = FILL(" ",6 - LENGTH(TRIM(job.job-no))) +
                 TRIM(job.job-no)
     tjob-no   = FILL(" ",6 - LENGTH(TRIM(job.job-no))) +
                 trim(job.job-no)
     fjob-no2  = job.job-no2
     tjob-no2  = job.job-no2
     fjob-no   = FILL(" ",6 - LENGTH(TRIM(fjob-no))) + TRIM(fjob-no) +
                 STRING(fjob-no2,"99")
     tjob-no   = FILL(" ",6 - LENGTH(TRIM(tjob-no))) + TRIM(tjob-no) +
                 STRING(tjob-no2,"99").

  {oe/jobprint.i}

END.

