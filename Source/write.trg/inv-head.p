/* Be sure whatever mods you make take into account multi-invoice customers  */
&Scoped-define ACTION UPDATE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME inv-head

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}

{custom/globdefs.i}
{sys/inc/var.i NEW SHARED}
{sys/inc/varasgn.i}

DISABLE TRIGGERS FOR LOAD OF inv-line.

DEFINE VARIABLE dInvoiceTotal    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dInvoiceSubTotal AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dTotalTax        AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lSuccess         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage         AS CHARACTER NO-UNDO.

FOR EACH inv-line WHERE inv-line.r-no EQ {&TABLENAME}.r-no:
  inv-line.cust-no = {&TABLENAME}.cust-no.
END.

IF {&TABLENAME}.company NE "" AND {&TABLENAME}.inv-no NE 0 THEN
  RUN oe/checkinv#.p (BUFFER {&TABLENAME}).

RELEASE cust.
FIND FIRST cust NO-LOCK
    WHERE cust.company EQ {&TABLENAME}.company
      AND cust.cust-no EQ {&TABLENAME}.cust-no
    NO-ERROR.
IF AVAIL cust THEN {&TABLENAME}.curr-code[1] = cust.curr-code.

RUN oe/updmulti.p (BUFFER {&TABLENAME}, BUFFER old-{&TABLENAME}).

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.

IF old-{&TABLENAME}.t-inv-tax     NE {&TABLENAME}.t-inv-tax     OR
   old-{&TABLENAME}.t-inv-rev     NE {&TABLENAME}.t-inv-rev     OR
   old-{&TABLENAME}.t-inv-cost    NE {&TABLENAME}.t-inv-cost    OR
   old-{&TABLENAME}.t-inv-freight NE {&TABLENAME}.t-inv-freight OR
   old-{&TABLENAME}.tax-gr        NE {&TABLENAME}.tax-gr        OR
   old-{&TABLENAME}.frt-pay       NE {&TABLENAME}.frt-pay THEN DO:
    RUN Tax_CalculateForInvHead  (
        INPUT  ROWID({&TABLENAME}),
        INPUT  locode,
        INPUT  "QUOTATION",    /*  Message Type "INVOICE" or "QUOTATION" */
        INPUT  FALSE,          /* Post To journal */
        INPUT  "GetTaxAmount", /* Trigger ID */
        OUTPUT dTotalTax,
        OUTPUT dInvoiceTotal,
        OUTPUT dinvoiceSubTotal,
        OUTPUT lSuccess,
        OUTPUT cMessage
        ).
  
    ASSIGN 
        {&TABLENAME}.t-inv-tax = dTotalTax
        {&TABLENAME}.t-inv-rev = dInvoiceTotal
        .
END.
