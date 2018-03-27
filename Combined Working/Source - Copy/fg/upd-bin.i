
IF {1}.pur-uom EQ "" THEN {1}.pur-uom = itemfg.prod-uom.

ASSIGN
 v-binqty = {1}.qty   * (IF {1}.qty   LT 0 THEN -1 ELSE 1)
 v-qty    = {4}.t-qty * (IF {4}.t-qty LT 0 THEN -1 ELSE 1).

IF {1}.pur-uom EQ "M" THEN
  v-tagcost = {1}.std-tot-cost.
ELSE
  RUN sys/ref/convcuom.p({1}.pur-uom, "M", 0, 0, 0, 0,
                         {1}.std-tot-cost, OUTPUT v-tagcost).

IF {2} EQ "M" THEN
  v-cost = {3}.
ELSE
  RUN sys/ref/convcuom.p({2}, "M", 0, 0, 0, 0, {3}, OUTPUT v-cost).

ASSIGN
 {1}.qty           = {1}.qty + {4}.t-qty
 {1}.partial-count = {1}.partial-count + {4}.partial.

IF {1}.qty EQ 0 THEN v-binqty = 0.

IF {4}.rita-code NE "A" OR v-cost NE 0 THEN DO:
  IF {4}.rita-code EQ "A" AND v-cost NE 0 THEN
    {1}.std-tot-cost = ((v-cost * v-binqty) + (v-cost * v-qty)) /
                       (v-binqty + v-qty).
  ELSE
    {1}.std-tot-cost = ((v-tagcost * v-binqty) + (v-cost * v-qty)) /
                       (v-binqty + v-qty).

  IF itemfg.prod-uom NE "M" THEN
    RUN sys/ref/convcuom.p("M", itemfg.prod-uom, 0, 0, 0, 0,
                           {1}.std-tot-cost, OUTPUT {1}.std-tot-cost).
END.

{1}.pur-uom = itemfg.prod-uom.

RELEASE reftable.

IF {4}.job-no NE "" AND NOT AVAIL job-hdr THEN DO:
  FIND FIRST job
      WHERE job.company EQ {4}.company
        AND job.job-no  EQ {4}.job-no
        AND job.job-no2 EQ {4}.job-no2
      NO-LOCK NO-ERROR.
  IF AVAIL job THEN
  FIND FIRST reftable
      WHERE reftable.reftable EQ "jc/jc-calc.p"
        AND reftable.company  EQ job.company
        AND reftable.loc      EQ ""
        AND reftable.code     EQ STRING(job.job,"999999999")
        AND reftable.code2    EQ {4}.i-no
      USE-INDEX reftable NO-LOCK NO-ERROR.
END.

IF AVAIL reftable AND reftable.val[5] NE 0 THEN
  ASSIGN
   v-cost           = reftable.val[5]
   {1}.std-mat-cost = {1}.std-tot-cost * (reftable.val[2] / v-cost)
   {1}.std-lab-cost = {1}.std-tot-cost * (reftable.val[1] / v-cost)
   {1}.std-var-cost = {1}.std-tot-cost * (reftable.val[3] / v-cost)
   {1}.std-fix-cost = {1}.std-tot-cost * (reftable.val[4] / v-cost).

ELSE
IF AVAIL job-hdr AND job-hdr.std-mat-cost + job-hdr.std-lab-cost +
                      job-hdr.std-var-cost + job-hdr.std-fix-cost NE 0 THEN
  ASSIGN
   v-cost           = job-hdr.std-mat-cost + job-hdr.std-lab-cost +
                      job-hdr.std-var-cost + job-hdr.std-fix-cost
   {1}.std-mat-cost = {1}.std-tot-cost * (job-hdr.std-mat-cost / v-cost)
   {1}.std-lab-cost = {1}.std-tot-cost * (job-hdr.std-lab-cost / v-cost)
   {1}.std-var-cost = {1}.std-tot-cost * (job-hdr.std-var-cost / v-cost)
   {1}.std-fix-cost = {1}.std-tot-cost * (job-hdr.std-fix-cost / v-cost).

ELSE
  ASSIGN
   {1}.std-mat-cost = {1}.std-tot-cost
   {1}.std-lab-cost = 0
   {1}.std-var-cost = 0
   {1}.std-fix-cost = 0.

{1}.std-tot-cost = {1}.std-mat-cost + {1}.std-lab-cost +
                   {1}.std-var-cost + {1}.std-fix-cost.
