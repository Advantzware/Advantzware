/* -------------------------------------------------- cec/probbox.p 10/02 YSK */
/* 'What if' UPDATE       box print is added to probeu1.p                     */
/* -------------------------------------------------------------------------- */

def input param v-recid as recid.

{sys/inc/var.i shared}
{sys/form/s-top.f}

/* for box print*/
{jcrep/r-ticket.i "shared"}
{cecrep/jobtick.i "shared"}

def new shared var v-out1-id       as   recid    no-undo.  /* YSK 06/08/01  was~ local var */
def new shared var v-out2-id       as   recid    no-undo.  /* YSK 06/08/01  was~ local var */

DEF VAR qty AS INT NO-UNDO.


DEF BUFFER b-probe FOR probe.

def shared var tmp-dir as cha no-undo.
DEF VAR is-print-box AS LOG NO-UNDO.
DEF VAR v-probe-fmt AS CHAR NO-UNDO.
DEF VAR v-mch-list AS CHAR NO-UNDO.

is-print-box = YES.

FIND probe WHERE RECID(probe) EQ v-recid NO-LOCK NO-ERROR.

v-probe-fmt = IF probe.LINE LT 100 THEN "99" ELSE "999".

IF xest.est-type = 6 then
   output to value(tmp-dir + trim(xest.est-no) + "-01.b"
      				       + string(probe.line,v-probe-fmt)) .
ELSE
   output to value(tmp-dir + trim(xest.est-no) + ".b"
					       + string(probe.line,v-probe-fmt)) .

/* display job-mch.m-code list */
ASSIGN v-mch-list = "".
FOR EACH est-op WHERE
    est-op.company EQ xest.company AND
    est-op.est-no EQ xest.est-no AND
    est-op.LINE LT 500
    NO-LOCK
    /*BREAK BY est-op.m-code*/:

    IF LOOKUP(est-op.m-code,v-mch-list) = 0 THEN
       v-mch-list = v-mch-list + "," + est-op.m-code.

END.

IF LENGTH(v-mch-list) > 0 THEN
   ASSIGN v-mch-list = SUBSTR(v-mch-list,2).

FIND FIRST cust WHERE cust.company = xeb.company 
                         AND cust.cust-no = xeb.cust-no NO-LOCK NO-ERROR.
PUT "Estimate#: " TRIM(xef.est-no) FORMAT "x(8)" SKIP
    "Customer: " xeb.cust-no cust.NAME SKIP
    v-mch-list FORM "x(100)" SKIP.

RUN cec/desprnt2.p (recid(xef),
                         input-output v-lines,
                         recid(xest)).
OUTPUT CLOSE.
/* end ---------------------------------- copr. 1997  advanced software, inc. */
