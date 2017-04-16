
&SCOPED-DEFINE fix job-no = FILL(" ",6 - LENGTH(TRIM(job-no))) + TRIM(job-no).
&SCOPED-DEFINE where-phrase WHERE SUBSTR(job-no,6,1) EQ ""


SESSION:SET-WAIT-STATE ("general").

STATUS DEFAULT "Processing ar-inv...".

FOR EACH ar-inv {&where-phrase} EXCLUSIVE TRANSACTION:
  {&fix}
END.

STATUS DEFAULT "Processing ar-invl...".

FOR EACH ar-invl {&where-phrase} EXCLUSIVE TRANSACTION:
  {&fix}
END.

STATUS DEFAULT "Processing fg-act...".

FOR EACH fg-act {&where-phrase} EXCLUSIVE TRANSACTION:
  {&fix}
END.

STATUS DEFAULT "Processing fg-bin...".

FOR EACH fg-bin {&where-phrase} EXCLUSIVE TRANSACTION:
  {&fix}
END.

STATUS DEFAULT "Processing fg-hist...".

FOR EACH fg-hist {&where-phrase} EXCLUSIVE TRANSACTION:
  {&fix}
END.

STATUS DEFAULT "Processing fg-rcpth...".

FOR EACH fg-rcpth {&where-phrase} EXCLUSIVE TRANSACTION:
  {&fix}
END.

STATUS DEFAULT "Processing fg-rcpts...".

FOR EACH fg-rcpts {&where-phrase} EXCLUSIVE TRANSACTION:
  {&fix}
END.

STATUS DEFAULT "Processing fg-rctd...".

FOR EACH fg-rctd {&where-phrase} EXCLUSIVE TRANSACTION:
  {&fix}
END.

STATUS DEFAULT "Processing inv-line...".

FOR EACH inv-line {&where-phrase} EXCLUSIVE TRANSACTION:
  {&fix}
END.

STATUS DEFAULT "Processing job...".

FOR EACH job {&where-phrase} EXCLUSIVE TRANSACTION:
  {&fix}
END.

STATUS DEFAULT "Processing job-all...".

FOR EACH job-all {&where-phrase} EXCLUSIVE TRANSACTION:
  {&fix}
END.

STATUS DEFAULT "Processing job-brd...".

FOR EACH job-brd {&where-phrase} EXCLUSIVE TRANSACTION:
  {&fix}
END.

STATUS DEFAULT "Processing job-hdr...".

FOR EACH job-hdr {&where-phrase} EXCLUSIVE TRANSACTION:
  {util/dljobkey.i}

  {&fix}

  FIND FIRST job
      WHERE job.company EQ job-hdr.company
        AND job.job     EQ job-hdr.job
        AND job.job-no  EQ job-hdr.job-no
        AND job.job-no2 EQ job-hdr.job-no2
      NO-LOCK NO-ERROR.
  IF AVAIL job THEN DO:
    {util/mkjobkey.i}
  END.
END.

STATUS DEFAULT "Processing job-mat...".

FOR EACH job-mat {&where-phrase} EXCLUSIVE TRANSACTION:
  {&fix}
END.

STATUS DEFAULT "Processing job-mch...".

FOR EACH job-mch {&where-phrase} EXCLUSIVE TRANSACTION:
  {&fix}
END.

STATUS DEFAULT "Processing job-prep...".

FOR EACH job-prep {&where-phrase} EXCLUSIVE TRANSACTION:
  {&fix}
END.

STATUS DEFAULT "Processing job-sch...".

FOR EACH job-sch {&where-phrase} EXCLUSIVE TRANSACTION:
  {&fix}
END.

STATUS DEFAULT "Processing loadtag...".

FOR EACH loadtag {&where-phrase} EXCLUSIVE TRANSACTION:
  {&fix}
END.

STATUS DEFAULT "Processing mat-act...".

FOR EACH mat-act {&where-phrase} EXCLUSIVE TRANSACTION:
  {&fix}
END.

STATUS DEFAULT "Processing mch-act...".

FOR EACH mch-act {&where-phrase} EXCLUSIVE TRANSACTION:
  {&fix}
END.

STATUS DEFAULT "Processing mch-srt...".

FOR EACH mch-srt {&where-phrase} EXCLUSIVE TRANSACTION:
  {&fix}
END.

STATUS DEFAULT "Processing misc-act...".

FOR EACH misc-act {&where-phrase} EXCLUSIVE TRANSACTION:
  {&fix}
END.

STATUS DEFAULT "Processing oe-boll...".

FOR EACH oe-boll {&where-phrase} EXCLUSIVE TRANSACTION:
  {&fix}
END.

STATUS DEFAULT "Processing oe-ord...".

FOR EACH oe-ord {&where-phrase} EXCLUSIVE TRANSACTION:
  {&fix}
END.

STATUS DEFAULT "Processing oe-ordl...".

FOR EACH oe-ordl {&where-phrase} EXCLUSIVE TRANSACTION:
  {&fix}
END.

STATUS DEFAULT "Processing oe-rell...".

FOR EACH oe-rell {&where-phrase} EXCLUSIVE TRANSACTION:
  {&fix}
END.

STATUS DEFAULT "Processing oe-retl...".

FOR EACH oe-retl {&where-phrase} EXCLUSIVE TRANSACTION:
  {&fix}
END.

STATUS DEFAULT "Processing pc-misc...".

FOR EACH pc-misc {&where-phrase} EXCLUSIVE TRANSACTION:
  {&fix}
END.

STATUS DEFAULT "Processing pc-prdd...".

FOR EACH pc-prdd {&where-phrase} EXCLUSIVE TRANSACTION:
  {&fix}
END.

STATUS DEFAULT "Processing po-all...".

FOR EACH po-all {&where-phrase} EXCLUSIVE TRANSACTION:
  {&fix}
END.

STATUS DEFAULT "Processing po-ordl...".

FOR EACH po-ordl {&where-phrase} EXCLUSIVE TRANSACTION:
  {&fix}
END.

STATUS DEFAULT "Processing po-rcpts...".

FOR EACH po-rcpts {&where-phrase} EXCLUSIVE TRANSACTION:
  {&fix}
END.

STATUS DEFAULT "Processing rm-rcpt...".

FOR EACH rm-rcpt {&where-phrase} EXCLUSIVE TRANSACTION:
  {&fix}
END.

STATUS DEFAULT "Processing rm-rcpth...".

FOR EACH rm-rcpth {&where-phrase} EXCLUSIVE TRANSACTION:
  {&fix}
END.

STATUS DEFAULT "Processing rm-rctd...".

FOR EACH rm-rctd {&where-phrase} EXCLUSIVE TRANSACTION:
  {&fix}
END.

STATUS DEFAULT "Processing rm-rdtl...".

FOR EACH rm-rdtl {&where-phrase} EXCLUSIVE TRANSACTION:
  {&fix}
END.

STATUS DEFAULT "Processing rm-rdtlh...".

FOR EACH rm-rdtlh {&where-phrase} EXCLUSIVE TRANSACTION:
  {&fix}
END.

STATUS DEFAULT "Processing rm-receipts...".

FOR EACH rm-receipts {&where-phrase} EXCLUSIVE TRANSACTION:
  {&fix}
END.

STATUS DEFAULT "".

SESSION:SET-WAIT-STATE ("").
