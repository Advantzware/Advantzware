/*  Mod: Ticket - 103137 Format Change for Order No. and Job No.       */

{sys/inc/var.i}

&SCOPED-DEFINE fix job-no = FILL(" ", iJobLen - LENGTH(TRIM(job-no))) + TRIM(job-no).
&SCOPED-DEFINE fix-jobNo jobNo = FILL(" ", iJobLen - LENGTH(TRIM(jobNo))) + TRIM(jobNo).
&SCOPED-DEFINE fix-job-number job_number = FILL(" ", iJobLen - LENGTH(TRIM(job_number))) + TRIM(job_number).
&SCOPED-DEFINE fix-jobID jobID = FILL(" ", iJobLen - LENGTH(TRIM(jobID))) + TRIM(jobID).
&SCOPED-DEFINE fix-vend-job-no vend-job-no = FILL(" ", iJobLen - LENGTH(TRIM(vend-job-no))) + TRIM(vend-job-no).
&SCOPED-DEFINE where-phrase WHERE SUBSTR(job-no,9,1) EQ ""
&SCOPED-DEFINE where-phrase-jobNo WHERE SUBSTR(jobNo,9,1) EQ ""
&SCOPED-DEFINE where-phrase-job-number WHERE SUBSTR(job_number,9,1) EQ ""
&SCOPED-DEFINE where-phrase-jobID WHERE SUBSTR(jobID,9,1) EQ ""
&SCOPED-DEFINE where-phrase-vend-job-no WHERE SUBSTR(vend-job-no,9,1) EQ ""

{sys/inc/var.i}

SESSION:SET-WAIT-STATE ("general").

STATUS DEFAULT "Processing asi2corr...".

FOR EACH asi2corr {&where-phrase} EXCLUSIVE TRANSACTION:
  {&fix}
END.

STATUS DEFAULT "Processing ar-inv...".

FOR EACH ar-inv {&where-phrase} EXCLUSIVE TRANSACTION:
  {&fix}
END.

STATUS DEFAULT "Processing ar-invl...".

FOR EACH ar-invl {&where-phrase} EXCLUSIVE TRANSACTION:
  {&fix}
END.

STATUS DEFAULT "Processing cmpltjob...".

FOR EACH cmpltjob {&where-phrase-job-number} EXCLUSIVE TRANSACTION:
  {&fix-job-number}
END.

STATUS DEFAULT "Processing costHeader...".

FOR EACH costHeader {&where-phrase-jobNo} EXCLUSIVE TRANSACTION:
  {&fix-jobNo}
END.

STATUS DEFAULT "Processing estCostHeader...".

FOR EACH estCostHeader {&where-phrase-jobID} EXCLUSIVE TRANSACTION:
  {&fix-jobID}
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
 
STATUS DEFAULT "Processing fg-rdtlh...".

FOR EACH fg-rdtlh {&where-phrase} EXCLUSIVE TRANSACTION:
  {&fix}
END.

STATUS DEFAULT "Processing inv-line...".

FOR EACH inv-line {&where-phrase} EXCLUSIVE TRANSACTION:
  {&fix}
END.

STATUS DEFAULT "Processing inventoryStock...".

FOR EACH inventoryStock {&where-phrase-jobID} EXCLUSIVE TRANSACTION:
  {&fix-jobID}
END.

STATUS DEFAULT "Processing inventoryStockSnapshot...".

FOR EACH inventoryStockSnapshot {&where-phrase-jobID} EXCLUSIVE TRANSACTION:
  {&fix-jobID}
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

STATUS DEFAULT "Processing job-farm...".

FOR EACH job-farm {&where-phrase} EXCLUSIVE TRANSACTION:
  {&fix}
END.

STATUS DEFAULT "Processing job-farm-rctd...".

FOR EACH job-farm-rctd {&where-phrase} EXCLUSIVE TRANSACTION:
  {&fix}
END.

STATUS DEFAULT "Processing job-hdr...".

FOR EACH job-hdr {&where-phrase} EXCLUSIVE TRANSACTION:
  {&fix}

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

STATUS DEFAULT "Processing sbNote...".

FOR EACH sbNote {&where-phrase} EXCLUSIVE TRANSACTION:
  {&fix}
END.

STATUS DEFAULT "Processing sbStatus...".

FOR EACH sbStatus {&where-phrase} EXCLUSIVE TRANSACTION:
  {&fix}
END.

STATUS DEFAULT "Processing ssrelbol...".

FOR EACH ssrelbol {&where-phrase} EXCLUSIVE TRANSACTION:
  {&fix}
END.
       
STATUS DEFAULT "Processing vend-whse-trans...".

FOR EACH vend-whse-trans {&where-phrase-vend-job-no} EXCLUSIVE TRANSACTION:
  {&fix-vend-job-no}
END.

STATUS DEFAULT "Processing vend-whse-trans-hist...".

FOR EACH vend-whse-trans-hist {&where-phrase-vend-job-no} EXCLUSIVE TRANSACTION:
  {&fix-vend-job-no}
END.


STATUS DEFAULT "".

SESSION:SET-WAIT-STATE ("").
