
/*------------------------------------------------------------------------
    File        : r-prfitmN-temp-arcashl.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Mon Jan 08 14:34:14 EST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/*------------------------------------------------------------------------
    File        : r-prfitmN-temp.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Mon Jan 08 11:33:40 EST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE  INPUT PARAMETER  fCompany        AS CHARACTER NO-UNDO.
DEFINE  INPUT PARAMETER  tCompany        AS CHARACTER NO-UNDO.
DEFINE  INPUT PARAMETER  fcust           AS ch        INIT " ".
DEFINE  INPUT PARAMETER  tcust           LIKE fcust INIT "zzzzzzzz".
DEFINE  INPUT PARAMETER  fitem           LIKE itemfg.i-no INIT " ".
DEFINE  INPUT PARAMETER  titem           LIKE fitem INIT "zzzzzzzzzzzzzzzzzzz".
DEFINE  INPUT PARAMETER  fpcat           LIKE itemfg.procat INIT " ".
DEFINE  INPUT PARAMETER  tpcat           LIKE fpcat INIT "zzzzzzzzzzzzzzzzzzz".
DEFINE  INPUT PARAMETER  fdate           AS DATE      FORMAT "99/99/9999".
DEFINE  INPUT PARAMETER  tdate           LIKE fdate.

DEFINE VARIABLE sort-by-cust      AS LOG       FORMAT "Customer/ProductCategory" INIT YES.
DEFINE VARIABLE v-inc-fc          AS LOG       INIT NO.

DEFINE VARIABLE v-date            AS CHARACTER FORMAT "x(10)" NO-UNDO.
DEFINE VARIABLE v-pric            LIKE ar-invl.unit-pr.
DEFINE VARIABLE v-uom             LIKE ar-invl.pr-uom.
DEFINE VARIABLE v-brdc            AS DECIMAL.
DEFINE VARIABLE v-ordc            AS DECIMAL.
DEFINE VARIABLE v-invc            AS DECIMAL.
DEFINE VARIABLE v-marg            AS DECIMAL.
DEFINE VARIABLE v-brdp            AS DECIMAL.
DEFINE VARIABLE v-$msf            AS DECIMAL.

DEFINE VARIABLE v-qty             AS INTEGER   EXTENT 4.
DEFINE VARIABLE v-msf             AS DECIMAL   EXTENT 4.
DEFINE VARIABLE v-cst             AS DECIMAL   EXTENT 4.
DEFINE VARIABLE v-cst1            AS DECIMAL   EXTENT 4.
DEFINE VARIABLE v-cst2            AS DECIMAL   EXTENT 4.
DEFINE VARIABLE v-amt             AS DECIMAL   EXTENT 4.

DEFINE VARIABLE v-cust-no         LIKE cust.cust-no.
DEFINE VARIABLE v-order-date      AS CHARACTER FORMAT "x(10)" NO-UNDO.
DEFINE VARIABLE v-job-no          LIKE job.job-no.
DEFINE VARIABLE v-job-no2         LIKE job.job-no2.
DEFINE VARIABLE v-po-no-po        LIKE ar-invl.po-no-po.
DEFINE VARIABLE v-fac             AS INTEGER.
DEFINE VARIABLE v-cost            AS DECIMAL.
DEFINE VARIABLE v-job-qty         AS INTEGER.
DEFINE VARIABLE v-exc             AS LOG.
DEFINE VARIABLE tb_prep           AS LOGICAL   NO-UNDO.
DEFINE VARIABLE v-weight          AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-prodcode        AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-invLineNum      AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-brd-grade       AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-brd-paper-combo AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-deliveryType    AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-shiptoName      AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-shiptoStreet    AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-shiptoCity      AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-shiptoState     AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-shiptoZip       AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-designNums      AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-cutDieNums      AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-printPlateNums  AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-machRoute       AS CHARACTER NO-UNDO.



DEFINE VARIABLE v-cuom            LIKE po-ordl.cons-uom.
DEFINE VARIABLE v-unit            AS CHARACTER FORMAT "!" INIT "U".
DEFINE VARIABLE v-cost2           AS LOG       INIT YES.
DEFINE VARIABLE v-cost3           AS CHARACTER FORMAT "!" INIT "C".
DEFINE VARIABLE v-label1          AS CHARACTER FORMAT "x(15)".
DEFINE VARIABLE v-label2          AS CHARACTER FORMAT "x(4)".
DEFINE VARIABLE v-label3          AS CHARACTER FORMAT "x(11)".
DEFINE VARIABLE v-label4          AS CHARACTER FORMAT "x(11)".
DEFINE VARIABLE v-label5          AS CHARACTER FORMAT "x(9)".
DEFINE VARIABLE v-label6          AS CHARACTER FORMAT "X(15)".
DEFINE VARIABLE v-label7          AS CHARACTER FORMAT "X(15)".
DEFINE VARIABLE v-label8          AS CHARACTER FORMAT "X(15)".
DEFINE VARIABLE v-label9          AS CHARACTER FORMAT "X(9)".

DEFINE VARIABLE v-color           AS INTEGER   FORMAT ">9".
DEFINE VARIABLE x-v-color         AS CHARACTER FORMAT "x(2)".
DEFINE VARIABLE v-style           AS CHARACTER FORMAT "X(6)".
DEFINE VARIABLE v-flute           AS CHARACTER FORMAT "XXX".
DEFINE VARIABLE v-est-no          AS CHARACTER FORMAT "X(5)".
DEFINE VARIABLE v-test            AS CHARACTER FORMAT "X(6)".
DEFINE VARIABLE v-len             AS DECIMAL   FORMAT ">>9.99".
DEFINE VARIABLE v-wid             AS DECIMAL   FORMAT ">>9.99".
DEFINE VARIABLE v-dep             AS DECIMAL   FORMAT ">>9.99".
DEFINE VARIABLE v-space           AS CHARACTER FORMAT "X(2)".
DEFINE VARIABLE v-cust-part-no    AS CHARACTER FORMAT "X(15)".
DEFINE VARIABLE v-cust-part-no2   AS CHARACTER FORMAT "X(15)".
DEFINE VARIABLE v-period          AS INTEGER   NO-UNDO.  
DEFINE VARIABLE lv-r-no           LIKE oe-retl.r-no NO-UNDO.
DEFINE VARIABLE lv-type           AS CHARACTER NO-UNDO.

DEFINE VARIABLE cDisplay          AS cha       NO-UNDO.
DEFINE VARIABLE cExcelDisplay     AS cha       NO-UNDO.
DEFINE VARIABLE hField            AS HANDLE    NO-UNDO.
DEFINE VARIABLE cTmpField         AS CHA       NO-UNDO.
DEFINE VARIABLE cVarValue         AS cha       NO-UNDO.
DEFINE VARIABLE cExcelVarValue    AS cha       NO-UNDO.
DEFINE VARIABLE cSelectedList     AS cha       NO-UNDO.
DEFINE VARIABLE cFieldName        AS cha       NO-UNDO.
DEFINE VARIABLE str-tit4          AS cha       FORM "x(200)" NO-UNDO.
DEFINE VARIABLE str-tit5          AS cha       FORM "x(200)" NO-UNDO.
DEFINE VARIABLE str-line          AS cha       FORM "x(350)" NO-UNDO.
DEFINE VARIABLE lSelected         AS LOG       INIT YES NO-UNDO.

/*
DEFINE VARIABLE v-prodcode        AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-invLineNum      AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-brd-grade       AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-brd-paper-combo AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-deliveryType    AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-shiptoName      AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-shiptoStreet    AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-shiptoCity      AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-shiptoState     AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-shiptoZip       AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-designNums      AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-cutDieNums      AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-printPlateNums  AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-machRoute       AS CHARACTER NO-UNDO.
*/
DEFINE VARIABLE excelheader       AS CHARACTER NO-UNDO.



DEFINE TEMP-TABLE tt-report LIKE report.

DEFINE BUFFER xtt-report FOR tt-report.

DEFINE TEMP-TABLE w-data NO-UNDO
    FIELD sorter AS CHARACTER
    FIELD i-no   LIKE ar-invl.i-no COLUMN-LABEL "FG Item"
    FIELD inv-no LIKE ar-invl.inv-no COLUMN-LABEL "Invoice!Number"
    FIELD rec-id AS RECID.

{sys/inc/var.i "new shared"}
{sys/form/s-top.f}
{sys/ref/CustList.i NEW}
DEFINE VARIABLE glCustListActive AS LOGICAL NO-UNDO.
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
lSelected = NO.
IF cocode EQ '' THEN 
  cocode = '001'.
FIND FIRST fg-ctrl WHERE fg-ctrl.company EQ cocode NO-LOCK.
DEFINE VARIABLE cnt1 AS INTEGER.
DEFINE VARIABLE cnt2 AS INTEGER.
DEFINE VARIABLE cnt3 AS INTEGER.
DEFINE VARIABLE cnt4 AS INTEGER.

FOR EACH cust
    WHERE cust.company EQ cocode
    AND cust.cust-no GE fcust
    AND cust.cust-no LE tcust
    AND (IF lselected THEN CAN-FIND(FIRST ttCustList WHERE ttCustList.cust-no EQ cust.cust-no
    AND ttCustList.log-fld NO-LOCK) ELSE TRUE)
    NO-LOCK:
        
    FOR EACH ar-cash
        WHERE ar-cash.company    EQ cocode
        AND ar-cash.cust-no    EQ cust.cust-no
        AND ar-cash.check-date GE fdate
        AND ar-cash.check-date LE tdate
        AND ar-cash.posted     EQ YES
        NO-LOCK,

        EACH ar-cashl
        WHERE ar-cashl.c-no    EQ ar-cash.c-no
        AND ar-cashl.posted  EQ YES
        AND ar-cashl.memo    EQ YES
        AND CAN-FIND(FIRST account
        WHERE account.company EQ ar-cashl.company
        AND account.actnum  EQ ar-cashl.actnum
        AND account.type    EQ "R")
        NO-LOCK
    
        TRANSACTION:
        
        /* If Created via ar-inv, type is blank */
        FIND FIRST invoiceLines NO-LOCK WHERE invoiceLines.company EQ ar-cashl.company 
          AND invoiceLines.invoiceNumber EQ ar-cashl.inv-no 
          AND invoiceLines.type NE "" no-error.
        IF AVAIL invoiceLines THEN 
            NEXT.
            
        cnt1 = cnt1 + 1.
        CREATE tt-report.
        ASSIGN
            tt-report.term-id = ""
            tt-report.key-09  = cust.cust-no
            tt-report.key-10  = "ar-cashl"
            tt-report.rec-id  = RECID(ar-cashl).
    END.
END. 

FOR EACH tt-report
    WHERE tt-report.term-id EQ ""
    AND tt-report.key-01  EQ ""
    AND tt-report.key-02  EQ ""
    AND tt-report.key-03  EQ ""
    AND tt-report.key-04  EQ ""
    AND tt-report.key-05  EQ ""
    AND tt-report.key-06  EQ ""
    AND tt-report.key-07  EQ ""
    AND tt-report.key-08  EQ ""

    TRANSACTION:
        
    IF tt-report.key-10 EQ "ar-cashl" THEN 
    DO:
        FIND ar-cashl WHERE RECID(ar-cashl) EQ tt-report.rec-id NO-LOCK.
        FIND ar-cash  WHERE ar-cash.c-no    EQ ar-cashl.c-no NO-LOCK.
        /* If Created via ar-inv, type is blank */
        FIND FIRST invoiceLines NO-LOCK WHERE invoiceLines.company EQ ar-cashl.company 
            AND invoicesLine.invoiceNumber EQ ar-cashl.inv-no 
            AND invoiceLines.type NE "" no-error.
        IF AVAIL invoiceLines THEN 
            NEXT.        
        cnt2 = cnt2 + 1.
        ASSIGN
            v-exc            = YES
            tt-report.key-01 = IF sort-by-cust THEN tt-report.key-09 ELSE ""
            tt-report.key-02 = ""
            tt-report.key-03 = STRING(ar-cashl.inv-no,"999999").

        RUN getoeret (ROWID(ar-cashl), BUFFER reftable, BUFFER oe-retl).

        ASSIGN
            lv-r-no = 0
            lv-type = "".
          
        IF AVAILABLE reftable THEN
            ASSIGN
                lv-r-no = reftable.val[1]
                lv-type = reftable.dscr.
        ELSE
            IF ar-cashl.dscr MATCHES "*OE RETURN*" THEN
                ASSIGN
                    lv-r-no = INT(SUBSTR(ar-cashl.dscr,INDEX(ar-cashl.dscr,"oe return") + 25,12))
                    lv-type = TRIM(SUBSTR(ar-cashl.dscr,INDEX(ar-cashl.dscr,"oe return") + 12,10)).

        IF lv-r-no NE 0 THEN 
        DO:
            IF lv-type EQ "items" THEN 
            DO:
                RELEASE ar-invl.
                FIND FIRST oe-retl
                    WHERE oe-retl.company EQ cocode
                    AND oe-retl.r-no    EQ lv-r-no
                    AND oe-retl.line    EQ ar-cashl.line
                    AND oe-retl.i-no    GE fitem
                    AND oe-retl.i-no    LE titem
                    NO-LOCK NO-ERROR.
                IF AVAILABLE oe-retl THEN
                    FIND FIRST ar-invl
                        WHERE ar-invl.company EQ cocode
                        AND ar-invl.cust-no EQ ar-cash.cust-no
                        AND ar-invl.inv-no  EQ ar-cashl.inv-no
                        AND ar-invl.i-no    EQ oe-retl.i-no
                        AND ((tb_prep AND ar-invl.billable) OR NOT ar-invl.misc)
                        NO-LOCK NO-ERROR.
                IF AVAILABLE ar-invl THEN 
                DO:
                    FIND FIRST itemfg
                        WHERE itemfg.company EQ cocode
                        AND itemfg.i-no    EQ ar-invl.i-no
                        AND itemfg.procat  GE fpcat
                        AND itemfg.procat  LE tpcat
                        NO-LOCK NO-ERROR.

                    CREATE xtt-report.
                    ASSIGN
                        v-exc            = NO
                        tt-report.key-01 = IF sort-by-cust THEN tt-report.key-09
                                ELSE IF AVAILABLE itemfg THEN itemfg.procat ELSE ""
                        tt-report.key-02 = oe-retl.i-no.
                END.
            END.

            ELSE
                IF lv-type   EQ "freight"                  AND
                    "freight" GE fitem                      AND
                    "freight" LE titem                      AND
                    "frght"   GE fpcat                      AND
                    "frght"   LE tpcat                      THEN
                    ASSIGN
                        v-exc            = NO
                        tt-report.key-01 = "FRGHT"
                        tt-report.key-02 = "FREIGHT".

                ELSE
                    IF lv-type EQ "tax"                    AND
                        "tax" GE fitem                      AND
                        "tax" LE titem                      AND
                        "tax" GE fpcat                      AND
                        "tax" LE tpcat                      THEN
                        ASSIGN
                            v-exc            = NO
                            tt-report.key-01 = "TAX"
                            tt-report.key-02 = "TAX".

                    ELSE
                        IF "" GE fitem AND
                            "" LE titem THEN v-exc = NO.
        END.

        ELSE
            IF "" GE fitem AND
                "" LE titem THEN v-exc = NO.

        IF v-exc THEN DELETE tt-report.
    END.

        
END.

FOR EACH tt-report
    WHERE tt-report.term-id EQ ""

    BREAK BY tt-report.key-01
    BY tt-report.key-02
    BY tt-report.key-03

    WITH FRAME itemx DOWN

    TRANSACTION:

    CREATE w-data.
    ASSIGN
        w-data.i-no   = tt-report.key-02
        w-data.inv-no = int(tt-report.key-03)
        w-data.rec-id = tt-report.rec-id.

    ASSIGN
        v-job-no     = ""
        v-job-no2    = 0
        v-msf[1]     = 0
        v-cst[1]     = 0
        v-cst1[1]    = 0
        v-cst2[1]    = 0
        v-po-no-po   = 0
        v-style      = ""
        v-test       = ""
        v-flute      = ""
        v-est-no     = ""
        v-len        = 0
        v-wid        = 0
        v-dep        = 0
        v-date       = "" 
        v-order-date = "".
   cnt3 = cnt3 + 1.
    FIND FIRST itemfg
        WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ w-data.i-no
        NO-LOCK NO-ERROR.

    v-color = 0.
   IF AVAILABLE itemfg THEN DO:       
      FOR EACH itemfg-ink OF itemfg WHERE 
          itemfg-ink.i-no EQ itemfg.i-no NO-LOCK, 
         EACH item WHERE item.company EQ itemfg-ink.company AND 
          item.i-no EQ itemfg-ink.rm-i-no 
          NO-LOCK: 
    
          IF AVAILABLE (itemfg-ink) THEN 
          DO:
              v-color   = v-color + itemfg-ink.occurs.
          END.  
        END.
    END.

    IF AVAILABLE(itemfg) THEN 
    DO:
        /* IF RS_fgitem-cust-part-no = "Cust Part no" THEN */
        ASSIGN
            v-cust-part-no = itemfg.part-no.
        /* ELSE */
        ASSIGN
            v-cust-part-no2 = w-data.i-no.
       
        /*  IF TB_style-flute-test-lwd = YES THEN DO:*/
        ASSIGN
            v-style = itemfg.style
            v-len   = itemfg.l-score[50]
            v-wid   = itemfg.w-score[50]
            v-dep   = itemfg.d-score[50].
        FOR EACH eb FIELDS(test flute est-no) WHERE eb.company  EQ cocode
            AND eb.est-no   EQ itemfg.est-no
            AND eb.stock-no EQ itemfg.i-no NO-LOCK:
            ASSIGN 
                v-test  = eb.test
                v-flute = eb.flute.
            LEAVE.
        END.
            
    END.
    
    
    IF tt-report.key-10 EQ "ar-cashl" THEN 
    DO:
        cnt3 = cnt3 + 1.
        FIND FIRST ar-cashl WHERE RECID(ar-cashl) EQ w-data.rec-id NO-LOCK.
        FIND FIRST ar-cash  WHERE ar-cash.c-no    EQ ar-cashl.c-no NO-LOCK.
        ASSIGN
            v-cust-no = ar-cash.cust-no
            v-date    = STRING(ar-cash.check-date)
            v-pric    = ar-cashl.amt-paid - ar-cashl.amt-disc
            v-uom     = ""
            v-qty[1]  = 0
            v-cst[1]  = 0
            v-cst1[1] = 0
            v-cst2[1] = 0
            v-amt[1]  = ar-cashl.amt-paid - ar-cashl.amt-disc.

        RUN getoeret (ROWID(ar-cashl), BUFFER reftable, BUFFER oe-retl).

        ASSIGN
            lv-r-no = 0
            lv-type = "".
          
        IF AVAILABLE reftable THEN
            ASSIGN
                lv-r-no = reftable.val[1]
                lv-type = reftable.dscr.
        ELSE
            IF ar-cashl.dscr MATCHES "*OE RETURN*" THEN
                ASSIGN
                    lv-r-no = INT(SUBSTR(ar-cashl.dscr,INDEX(ar-cashl.dscr,"oe return") + 25,12))
                    lv-type = TRIM(SUBSTR(ar-cashl.dscr,INDEX(ar-cashl.dscr,"oe return") + 12,10)).
                    
        IF lv-type EQ "" AND ar-cash.memo THEN 
          lv-type = "CM".
          
        IF lv-type EQ "" THEN 
          lv-type = ar-cashl.dscr.
          
        IF lv-r-no NE 0 THEN 
        DO:
            FIND FIRST oe-reth
                WHERE oe-reth.company EQ cocode
                AND oe-reth.r-no    EQ lv-r-no
                NO-LOCK NO-ERROR.
            IF AVAILABLE oe-reth THEN
                FOR EACH ar-inv
                    WHERE ar-inv.company EQ cocode
                    AND ar-inv.cust-no EQ oe-reth.cust-no
                    AND ar-inv.inv-no  EQ oe-reth.inv-no
                    NO-LOCK,
                    EACH ar-invl
                    WHERE ar-invl.x-no EQ ar-inv.x-no
                    AND ar-invl.i-no EQ w-data.i-no
                    NO-LOCK:
                    v-po-no-po = ar-invl.po-no-po.

                    FIND FIRST oe-ord WHERE oe-ord.company = cocode 
                        AND oe-ord.ord-no  = ar-inv.ord-no NO-LOCK NO-ERROR.
                    IF AVAILABLE oe-ord THEN ASSIGN v-order-date = STRING(oe-ord.ord-date) .
                    LEAVE.
                END.

            FIND FIRST oe-retl
                WHERE oe-retl.company EQ cocode
                AND oe-retl.r-no    EQ lv-r-no
                AND oe-retl.line    EQ ar-cashl.line
                NO-LOCK NO-ERROR.

            IF AVAILABLE oe-retl THEN 
            DO:
                ASSIGN
                    v-pric    = oe-retl.unit-pr
                    v-uom     = oe-retl.uom
                    v-job-no  = oe-retl.job-no
                    v-job-no2 = oe-retl.job-no2
                    v-qty[1]  = - oe-retl.tot-qty-return
                    v-cst[1]  = oe-retl.cost * v-qty[1] / 1000
                    v-cst1[1] = oe-retl.cost * v-qty[1] / 1000
                    v-cst2[1] = oe-retl.cost * v-qty[1] / 1000.

                FIND FIRST job-hdr WHERE job-hdr.company = cocode
                    AND job-hdr.job-no  = oe-retl.job-no
                    AND job-hdr.job-no2 = oe-retl.job-no2 
                    AND job-hdr.i-no    = w-data.i-no NO-LOCK NO-ERROR.
                IF AVAILABLE(job-hdr) AND job-hdr.est-no <> "" THEN 
                    v-est-no = TRIM(job-hdr.est-no).
                ELSE 
                DO:
                    IF AVAILABLE itemfg THEN
                        v-est-no = TRIM(itemfg.est-no) .
                END.

                FIND FIRST ar-invl
                    WHERE ar-invl.company EQ cocode
                    AND ar-invl.cust-no EQ ar-cash.cust-no
                    AND ar-invl.inv-no  EQ ar-cashl.inv-no
                    AND ar-invl.i-no    EQ oe-retl.i-no
                    NO-LOCK NO-ERROR.
                IF AVAILABLE ar-invl THEN 
                DO: 
                    FIND FIRST ar-inv NO-LOCK 
                        WHERE ar-inv.x-no EQ ar-invl.x-no NO-ERROR.
                    IF AVAILABLE ar-inv THEN 
                    DO:
                        FIND FIRST period NO-LOCK
                            WHERE period.company EQ cocode
                            AND period.pst     LE ar-inv.inv-date
                            AND period.pend    GE ar-inv.inv-date
                            AND period.pstat 
							NO-ERROR.
                    END.  
                    IF AVAILABLE period THEN       
                        v-period = period.pnum.
                    
                    v-pric = ar-invl.unit-pr.
                    RUN salecost ("4",
                        ROWID(ar-invl),
                        v-job-no,
                        v-job-no2,
                        v-qty[1],
                        OUTPUT v-cst[1]).

                    RUN salecost ("2",
                        ROWID(ar-invl),
                        v-job-no,
                        v-job-no2,
                        v-qty[1],
                        OUTPUT v-cst1[1]).

                    RUN salecost ("3",
                        ROWID(ar-invl),
                        v-job-no,
                        v-job-no2,
                        v-qty[1],
                        OUTPUT v-cst2[3]).
            
                END. /* avail ar-invl */
            END. /* avail oe-retl */
        END. /* if lv-no gt 0 */
    END. /* if ar-cashl */
    
    /* Print */
 cnt4 = cnt4 + 1.
    CREATE InvoiceLines.    
    ASSIGN
        InvoiceLines.AdhesiveCost       = 0
        InvoiceLines.BoardCost          = 0 /* v-brdc */
        InvoiceLines.BoardGrade         = ""
        InvoiceLines.CoatingsCost       = 0
        InvoiceLines.company            = ar-cashl.company
        InvoiceLines.ConsolCustName     = ""
        InvoiceLines.CustomerName       = v-cust-no
        InvoiceLines.CustomerPONumber   = "" /* ar-cashl.po-no */
        InvoiceLines.CuttingDieNumbers  = ""
        InvoiceLines.DeliveryCost       = 0
        InvoiceLines.DeliveryType       = ""
        InvoiceLines.DesignNumber       = ""
        InvoiceLines.DirectCosts        = 0
        InvoiceLines.DirectOverheadCost = 0
        InvoiceLines.EquipmentRecovery  = 0
        InvoiceLines.FixedCosts         = (IF AVAILABLE itemfg THEN itemfg.std-fix-cost ELSE 0)
        InvoiceLines.FixedOverHeadCosts = 0
        InvoiceLines.GlueCost           = 0
        InvoiceLines.InkCost            = 0
        .
    assign
        InvoiceLines.InvoiceDate            = ar-cashl.inv-date
        InvoiceLines.InvoiceLineNumber      = 0 /* ar-cashl.inv-line */
        InvoiceLines.InvoiceMonthChar       = ""
        InvoiceLines.InvoiceNumber          = w-data.inv-no
        InvoiceLines.InvoiceType            = lv-type
        InvoiceLines.LabelsCost             = 0
        InvoiceLines.LabourCost             = (IF AVAILABLE itemfg THEN itemfg.std-lab-cost ELSE 0)
        InvoiceLines.LabourDirectCost       = 0 
        InvoiceLines.LabourIndirectCost     = 0
        InvoiceLines.MachineRouting         = ""
        InvoiceLines.MaintenancePartscost   = 0
        InvoiceLines.MaterialsCost          = (IF AVAILABLE itemfg THEN itemfg.std-mat-cost ELSE 0)
        InvoiceLines.MSFPerInvoice          = v-msf[1]
        InvoiceLines.OrderNumber            = 0 /* ar-cashl.ord-no */
        InvoiceLines.PalletsCost            = 0
        InvoiceLines.PaperCost              = 0
        InvoiceLines.PostingPeriodChar      = "" 
        InvoiceLines.PrintPlateNumbers      = ""
        InvoiceLines.ProductCode            = w-data.i-no
        InvoiceLines.ProductCodeDescription = ""
        InvoiceLines.ProductID              = v-cust-part-no
        InvoiceLines.ProductIdDescription   = ""
        InvoiceLines.QuantitySold           = v-qty[1]
        InvoiceLines.Rebates                = 0
        InvoiceLines.SalesDollsPerInvoice   = ar-cashl.amt-due
        InvoiceLines.SGACosts               = (IF AVAILABLE itemfg THEN itemfg.std-var-cost ELSE 0 )
        InvoiceLines.ShipToCity             = v-shiptoCity
        InvoiceLines.ShipToCountry          = ""
        InvoiceLines.ShipToName             = v-shiptoName
        InvoiceLines.ShipToState            = v-shiptoState
        InvoiceLines.ShipToStreet           = v-shiptoStreet
        InvoiceLines.ShipToZip              = v-ShiptoZip
        InvoiceLines.StrappingBundlingCost  = 0
        InvoiceLines.StretchWrapCost        = 0
        .
    assign
        InvoiceLines.TermsDiscount = 0
        InvoiceLines.ToolingCost   = 0
        InvoiceLines.TotalCost     = (IF AVAILABLE itemfg THEN itemfg.total-std-cost else 0) /* v-invc */
        InvoiceLines.UnitPrice     = (if v-unit EQ "E" then v-amt[1] else v-pric)
        InvoiceLines.UOM           = (if v-unit EQ "E" then "" else v-uom)
        InvoiceLines.WarehouseBin  = ""
        InvoiceLines.WarehouseCode = ""
        InvoiceLines.WarehouseCost = 0
        InvoiceLines.WeightLbs     = 0
        .                                

END.
MESSAGE "cnt1" cnt1 SKIP 
"cnt2" cnt2 SKIP 
"cnt3" cnt3 SKIP 
"cnt4" cnt4 SKIP  
VIEW-AS ALERT-BOX.

PROCEDURE salecost:

    DEFINE INPUT  PARAMETER ip-type     AS   INTEGER NO-UNDO.
    DEFINE INPUT  PARAMETER ip-rowid    AS   ROWID NO-UNDO.
    DEFINE INPUT  PARAMETER ip-job-no   LIKE job.job-no NO-UNDO.
    DEFINE INPUT  PARAMETER ip-job-no2  LIKE job.job-no2 NO-UNDO.
    DEFINE INPUT  PARAMETER ip-qty      AS   INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER op-cost     AS   DECIMAL NO-UNDO.

    DEFINE VARIABLE lv-uom   LIKE itemfg.prod-uom NO-UNDO.
    DEFINE VARIABLE v-bol-no LIKE oe-boll.bol-no NO-UNDO.

    v-bol-no = 0.
    FIND ar-invl WHERE ROWID(ar-invl) EQ ip-rowid NO-LOCK NO-ERROR.

    IF AVAILABLE ar-invl THEN 
    DO:
        v-bol-no = ar-invl.bol-no.
        FIND fg-ctrl WHERE fg-ctrl.company EQ ar-invl.company NO-LOCK NO-ERROR.

        FIND FIRST itemfg NO-LOCK
            WHERE itemfg.company EQ ar-invl.company
            AND itemfg.i-no    EQ ar-invl.i-no
            NO-ERROR.

        IF ip-type EQ 3 AND ar-invl.t-cost NE ? THEN
        DO:
            IF ar-invl.dscr[1] EQ "M" OR ar-invl.dscr[1] EQ "" THEN
                op-cost = ip-qty / 1000 * ar-invl.cost.
            ELSE
                op-cost = ip-qty * ar-invl.cost.
        END.

        IF ip-type EQ 2 THEN 
        DO:
            FIND FIRST oe-ordl NO-LOCK
                WHERE oe-ordl.company EQ ar-invl.company
                AND oe-ordl.ord-no  EQ ar-invl.ord-no
                AND oe-ordl.i-no    EQ ar-invl.i-no
                NO-ERROR.
            IF AVAILABLE oe-ordl THEN op-cost = oe-ordl.cost * ip-qty / 1000.
        END.

        IF ip-type EQ 1 THEN
            RUN bordcost (ip-job-no,
                ip-job-no2,                            
                ar-invl.i-no,
                v-bol-no,
                ip-qty,
                YES,
                OUTPUT op-cost).
        IF ip-type EQ 4 THEN
            RUN bordcostM (ip-job-no,
                ip-job-no2,                            
                ar-invl.i-no,
                v-bol-no,
                ip-qty,
                YES,
                OUTPUT op-cost).

        IF op-cost EQ ? THEN op-cost = 0.

        IF op-cost EQ 0 THEN 
        DO:
            FIND FIRST po-ordl NO-LOCK
                WHERE po-ordl.company   EQ ar-invl.company
                AND po-ordl.po-no     EQ ar-invl.po-no-po
                AND po-ordl.i-no      EQ ar-invl.i-no
                AND po-ordl.deleted   EQ NO
                AND po-ordl.item-type EQ NO
                AND po-ordl.job-no    EQ ip-job-no
                AND po-ordl.job-no2   EQ ip-job-no2
                USE-INDEX po-no NO-ERROR.
            IF NOT AVAILABLE po-ordl THEN
                FOR EACH po-ordl NO-LOCK
                    WHERE po-ordl.company   EQ ar-invl.company
                    AND po-ordl.i-no      EQ ar-invl.i-no
                    AND po-ordl.deleted   EQ NO
                    AND po-ordl.item-type EQ NO
                    AND po-ordl.job-no    EQ ip-job-no
                    AND po-ordl.job-no2   EQ ip-job-no2
                    USE-INDEX item
                    BY po-ordl.po-no DESCENDING:
                    LEAVE.
                END.
            IF NOT AVAILABLE po-ordl THEN
                FOR EACH po-ordl NO-LOCK
                    WHERE po-ordl.company   EQ ar-invl.company
                    AND po-ordl.i-no      EQ ar-invl.i-no
                    AND po-ordl.deleted   EQ NO
                    AND po-ordl.item-type EQ NO
                    USE-INDEX item
                    BY po-ordl.po-no DESCENDING:
                    LEAVE.
                END.
            IF AVAILABLE po-ordl THEN
                ASSIGN
                    op-cost = po-ordl.cons-cost
                    lv-uom  = po-ordl.cons-uom.

            IF op-cost EQ 0 AND AVAILABLE itemfg THEN
                ASSIGN
                    op-cost = IF itemfg.i-code EQ "C" AND ip-type EQ 2 THEN
                                itemfg.std-mat-cost ELSE
                                IF fg-ctrl.inv-meth EQ "A" THEN itemfg.avg-cost
                                            ELSE itemfg.last-cost
                                 lv-uom  = itemfg.prod-uom.

            op-cost = op-cost * ip-qty /
                (IF lv-uom EQ "C"  THEN 100               ELSE
                IF lv-uom EQ "M"  THEN 1000              ELSE
                IF lv-uom EQ "CS" AND AVAILABLE itemfg AND
                itemfg.case-count NE 0 THEN itemfg.case-count ELSE 1).
        END.

        IF op-cost EQ ? THEN op-cost = 0.
    END.
END PROCEDURE.

PROCEDURE bordcost:
    /* --------------------------------------------- sys/inc/bordcost.p 01/02 JLF */
    /* Calculate the total cost of the board received for a job/item              */
    /* -------------------------------------------------------------------------- */

    DEFINE INPUT  PARAMETER v-job-no   LIKE job.job-no.
    DEFINE INPUT  PARAMETER v-job-no2  LIKE job.job-no2.
    DEFINE INPUT  PARAMETER v-i-no     LIKE itemfg.i-no.
    DEFINE INPUT  PARAMETER  v-bol-no   LIKE oe-boll.bol-no.
    DEFINE INPUT  PARAMETER v-qty      AS   INTEGER.
    DEFINE INPUT  PARAMETER v-act-cost AS   LOG.
    DEFINE OUTPUT PARAMETER v-tot-cost AS   DECIMAL.

    DEFINE VARIABLE v-job-qty   LIKE job-hdr.qty.
    DEFINE VARIABLE v-cost      AS DECIMAL.
    DEFINE VARIABLE lv-est-type LIKE est.est-type.


    IF v-job-no EQ "" THEN 
    DO:

        /* Find BOL for item. */
        FIND FIRST oe-boll NO-LOCK WHERE
            oe-boll.company = cocode AND
            oe-boll.bol-no = v-bol-no AND
            oe-boll.i-no = v-i-no NO-ERROR.

        IF AVAILABLE oe-boll THEN
            ASSIGN v-job-no  = oe-boll.job-no
                v-job-no2 = oe-boll.job-no2.


    END. /* if v-job-no eq "" */

    FIND FIRST job
        WHERE job.company EQ cocode
        AND job.job-no  EQ v-job-no
        AND job.job-no2 EQ v-job-no2
        NO-LOCK NO-ERROR.
            
    IF AVAILABLE job THEN
        FIND FIRST job-hdr
            WHERE job-hdr.company EQ cocode
            AND job-hdr.job     EQ job.job
            AND job-hdr.job-no  EQ job.job-no
            AND job-hdr.job-no2 EQ job.job-no2
            AND job-hdr.i-no    EQ v-i-no
            NO-LOCK NO-ERROR.

    IF AVAILABLE job-hdr THEN 
    DO:
        IF job.est-no NE "" THEN
            FIND FIRST est
                WHERE est.company EQ job.company
                AND est.est-no  EQ job.est-no
                NO-LOCK NO-ERROR.
        lv-est-type = IF AVAILABLE est THEN est.est-type ELSE 1.
        IF lv-est-type GT 4 THEN lv-est-type = lv-est-type - 4.

        FOR EACH fg-rcpth
            WHERE fg-rcpth.company   EQ cocode
            AND fg-rcpth.job-no    EQ job-hdr.job-no
            AND fg-rcpth.job-no2   EQ job-hdr.job-no2
            AND fg-rcpth.i-no      EQ job-hdr.i-no
            AND fg-rcpth.rita-code EQ "R"
            NO-LOCK,

            EACH fg-rdtlh
            WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no
            NO-LOCK:

            v-job-qty = v-job-qty + fg-rdtlh.qty.
        END.

        IF v-act-cost THEN 
        DO:
            FOR EACH mat-act
                WHERE mat-act.company EQ cocode
                AND mat-act.job     EQ job-hdr.job
                AND mat-act.job-no  EQ job-hdr.job-no
                AND mat-act.job-no2 EQ job-hdr.job-no2
                AND (mat-act.s-num  EQ job-hdr.frm OR lv-est-type EQ 2)
                NO-LOCK,

                FIRST job-mat
                WHERE job-mat.company  EQ cocode
                AND job-mat.job      EQ mat-act.job
                AND job-mat.frm      EQ mat-act.s-num
                AND job-mat.blank-no EQ mat-act.b-num
                AND job-mat.i-no     EQ mat-act.i-no
                NO-LOCK,

                FIRST item
                WHERE item.company EQ cocode
                AND item.i-no    EQ mat-act.i-no
                AND index("BA",item.mat-type) GT 0
                NO-LOCK:
      
                IF item.r-wid EQ 0 THEN
                    RUN sys/ref/convcuom.p(job-mat.sc-uom, mat-act.qty-uom,
                        (IF job-mat.basis-w  NE 0 THEN
                        job-mat.basis-w ELSE item.basis-w),
                        (IF job-mat.len      NE 0 THEN
                        job-mat.len ELSE item.s-len),
                        (IF job-mat.wid      NE 0 THEN
                        job-mat.wid ELSE item.s-wid),
                        item.s-dep,   
                        mat-act.cost, OUTPUT v-cost).

                ELSE
                    RUN sys/ref/convcuom.p(job-mat.sc-uom, mat-act.qty-uom,
                        (IF job-mat.basis-w  NE 0 THEN
                        job-mat.basis-w ELSE item.basis-w),
                        job-mat.len,
                        (IF job-mat.wid      NE 0 THEN
                        job-mat.wid ELSE item.r-wid),
                        item.s-dep,   
                        mat-act.cost, OUTPUT v-cost).

                IF mat-act.ext-cost EQ 0 OR mat-act.ext-cost EQ ? THEN
                    v-tot-cost = v-tot-cost + (v-cost * mat-act.qty).
                ELSE
                    v-tot-cost = v-tot-cost + mat-act.ext-cost.
            END.
    
            ASSIGN
                v-tot-cost = v-tot-cost * (job-hdr.qty / v-job-qty)
                v-tot-cost = v-tot-cost / job-hdr.qty * v-qty.
        END. /* if v-act-cost */
  
        ELSE
            FOR EACH job-mat
                WHERE job-mat.company EQ cocode
                AND job-mat.job     EQ job-hdr.job
                AND job-mat.job-no  EQ job-hdr.job-no
                AND job-mat.job-no2 EQ job-hdr.job-no2
                AND (job-mat.frm    EQ job-hdr.frm OR lv-est-type EQ 2)
                NO-LOCK,

                FIRST item
                WHERE item.company EQ cocode
                AND item.i-no    EQ job-mat.i-no
                AND index("BA",item.mat-type) GT 0
                NO-LOCK
                BREAK BY job-mat.frm 
                BY item.mat-type:
      
                IF item.mat-type NE "B"    OR
                    FIRST-OF(item.mat-type) THEN
                    v-tot-cost = v-tot-cost + (job-mat.cost-m * v-qty / 1000).
      
            END.
    END.


END PROCEDURE.


PROCEDURE bordcostM:
    /* --------------------------------------------- sys/inc/bordcost.p 01/02 JLF */
    /* Calculate the total cost of the board received for a job/item              */
    /* -------------------------------------------------------------------------- */

    DEFINE INPUT  PARAMETER v-job-no   LIKE job.job-no.
    DEFINE INPUT  PARAMETER v-job-no2  LIKE job.job-no2.
    DEFINE INPUT  PARAMETER v-i-no     LIKE itemfg.i-no.
    DEFINE INPUT PARAMETER  v-bol-no   LIKE oe-boll.bol-no.
    DEFINE INPUT  PARAMETER v-qty      AS   INTEGER.
    DEFINE INPUT  PARAMETER v-act-cost AS   LOG.
    DEFINE OUTPUT PARAMETER v-tot-cost AS   DECIMAL.



    DEFINE VARIABLE v-fgrec-qty  LIKE job-hdr.qty.
    DEFINE VARIABLE v-cost       AS DECIMAL.
    DEFINE VARIABLE lv-est-type  LIKE est.est-type.
    DEFINE VARIABLE v-po-cost    AS DECIMAL NO-UNDO .
    DEFINE VARIABLE v-issued-qty AS INTEGER NO-UNDO .
    DEFINE VARIABLE v-rec-qty    AS INTEGER NO-UNDO .
    DEFINE VARIABLE v-sub-tot    AS DECIMAL NO-UNDO .
    DEFINE VARIABLE v-sub-qty    AS DECIMAL NO-UNDO .
    DEFINE VARIABLE v-req-qty    AS INTEGER NO-UNDO .


    /* Find BOL for item. */
    FOR EACH oe-boll NO-LOCK WHERE
        oe-boll.company = cocode AND
        oe-boll.bol-no = v-bol-no AND
        oe-boll.i-no = v-i-no :

        ASSIGN 
            v-job-no    = oe-boll.job-no
            v-job-no2   = oe-boll.job-no2
            v-fgrec-qty = 0
            v-sub-tot   = 0 
            v-req-qty   = 0
            v-sub-qty   = 0.


        FIND FIRST job NO-LOCK
            WHERE job.company EQ cocode
            AND job.job-no  EQ v-job-no
            AND job.job-no2 EQ v-job-no2
            NO-ERROR.
    
        IF AVAILABLE job THEN
            FIND FIRST job-hdr NO-LOCK
                WHERE job-hdr.company EQ cocode
                AND job-hdr.job     EQ job.job
                AND job-hdr.job-no  EQ job.job-no
                AND job-hdr.job-no2 EQ job.job-no2
                AND job-hdr.i-no    EQ v-i-no
                NO-ERROR.


        IF AVAILABLE job-hdr THEN 
        DO:
            IF job.est-no NE "" THEN
                FIND FIRST est
                    WHERE est.company EQ job.company
                    AND est.est-no  EQ job.est-no
                    NO-LOCK NO-ERROR.
            lv-est-type = IF AVAILABLE est THEN est.est-type ELSE 1.
            IF lv-est-type GT 4 THEN lv-est-type = lv-est-type - 4.

            FOR EACH fg-rcpth
                WHERE fg-rcpth.company   EQ cocode
                AND fg-rcpth.job-no    EQ job-hdr.job-no
                AND fg-rcpth.job-no2   EQ job-hdr.job-no2
                AND fg-rcpth.i-no      EQ job-hdr.i-no
                AND fg-rcpth.rita-code EQ "R"
                NO-LOCK,

                EACH fg-rdtlh
                WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no
                NO-LOCK:

                v-fgrec-qty = v-fgrec-qty + fg-rdtlh.qty.
            END.

     
            FOR EACH mat-act
                WHERE mat-act.company EQ cocode
                AND mat-act.job     EQ job-hdr.job
                AND mat-act.job-no  EQ job-hdr.job-no
                AND mat-act.job-no2 EQ job-hdr.job-no2
                AND (mat-act.s-num  EQ job-hdr.frm OR lv-est-type EQ 2)
                NO-LOCK,

                FIRST job-mat
                WHERE job-mat.company  EQ cocode
                AND job-mat.job      EQ mat-act.job
                AND job-mat.frm      EQ mat-act.s-num
                AND job-mat.blank-no EQ mat-act.b-num
                AND job-mat.i-no     EQ mat-act.i-no
                NO-LOCK,

                FIRST item
                WHERE item.company EQ cocode
                AND item.i-no    EQ mat-act.i-no
                AND index("BA",item.mat-type) GT 0
                NO-LOCK BREAK BY job-mat.i-no:

                FIND FIRST job-mch NO-LOCK WHERE  job-mch.company = job.company 
                    AND job-mch.job = job.job 
                    AND job-mch.job-no = job.job-no 
                    AND job-mch.job-no2 = job.job-no2 
                    USE-INDEX line-idx NO-ERROR  .

                IF AVAILABLE job-mch  THEN
                    v-req-qty = job-mch.run-qty .
          
                FOR EACH po-ordl NO-LOCK
                    WHERE po-ordl.company   EQ cocode
                    AND po-ordl.i-no      EQ job-mat.i-no
                    AND po-ordl.job-no    EQ v-job-no
                    AND po-ordl.job-no2   EQ v-job-no2
                    USE-INDEX item 
                    BY po-ordl.po-no DESCENDING:
                    LEAVE.
                END.
        
                IF AVAILABLE po-ordl THEN 
                DO:
                    ASSIGN
                        v-po-cost    = po-ordl.cons-cost * 1000  /* po-ordl.t-cost */
                        v-issued-qty = po-ordl.t-rec-qty.
             
                    v-sub-tot = v-sub-tot + (v-issued-qty / 1000 * v-po-cost ) .
                    v-sub-qty = ( v-sub-tot / ( MIN( v-issued-qty / v-req-qty , 1) * job-hdr.qty ) * v-fgrec-qty ) .
                    v-tot-cost = v-tot-cost + ( v-sub-qty / 1000 * oe-boll.qty ).
                END.
                ELSE 
                DO:
                    IF FIRST-OF(job-mat.i-no) THEN
                        ASSIGN v-tot-cost = v-tot-cost + oe-boll.qty / 1000 * job-mat.cost-m .
                END.
            END.  /* job-mat */
        END. /* avail job-hdr */
    END.  /* for each oe-boll */    
END PROCEDURE.

PROCEDURE getoeret.
    DEFINE INPUT PARAMETER   ip-rowid AS ROWID NO-UNDO.
    DEFINE PARAMETER BUFFER io-ref-tab FOR reftable.
    DEFINE PARAMETER BUFFER io-oe-retl FOR oe-retl.

    DEFINE VARIABLE lv-r-no LIKE oe-retl.r-no NO-UNDO.


    RELEASE io-ref-tab.
    RELEASE io-oe-retl.

    FIND ar-cashl WHERE ROWID(ar-cashl) EQ ip-rowid NO-LOCK NO-ERROR.

    IF AVAILABLE ar-cashl THEN 
    DO:
        FIND FIRST io-ref-tab
            WHERE io-ref-tab.reftable EQ "ar-cashl.return"
            AND io-ref-tab.company  EQ ar-cashl.company
            AND io-ref-tab.loc      EQ ""
            AND io-ref-tab.code     EQ STRING(ar-cashl.c-no,"9999999999")
            AND io-ref-tab.code2    EQ STRING(ar-cashl.line,"9999999999")
            NO-LOCK NO-ERROR.

        IF AVAILABLE io-ref-tab AND io-ref-tab.dscr EQ "ITEMS" THEN
            lv-r-no = io-ref-tab.val[1].

        ELSE
            IF ar-cashl.dscr MATCHES "*oe return*"                                      AND
                SUBSTR(ar-cashl.dscr,INDEX(ar-cashl.dscr,"oe return") + 12,5) EQ "ITEMS" THEN
                lv-r-no = INT(SUBSTR(ar-cashl.dscr,INDEX(ar-cashl.dscr,"oe return") + 25,12)) NO-ERROR.

        IF ERROR-STATUS:ERROR THEN lv-r-no = 0.

        IF lv-r-no NE 0 THEN
            FIND FIRST io-oe-retl
                WHERE io-oe-retl.company EQ ar-cashl.company
                AND io-oe-retl.r-no    EQ lv-r-no
                AND io-oe-retl.line    EQ ar-cashl.line
                NO-LOCK NO-ERROR.
    END.
END PROCEDURE.
    /*
        DISPLAY 
            v-cust-no
            w-data.inv-no
                
            w-data.i-no       
            v-cust-part-no    
               
                
            itemfg.procat     
            WHEN AVAILABLE itemfg
            v-qty[1]                                        /* qty-ship */
            itemfg.t-sqft     
            WHEN AVAILABLE itemfg                           /* i-sq */
            v-msf[1]                                        /* total msf */
            v-$msf                                          /* msf       */
                
            v-pric           
            v-amt[1]          
            WHEN v-unit EQ "E" @ v-pric   /* unit price */
                
            v-uom             
            WHEN v-unit NE "E"            /* uom */
            v-brdc            
            WHEN v-cost2                  /* cost m */
            v-marg            
            WHEN v-cost2                  /* margin m */
            v-brdp            
            WHEN v-cost2                  /* cost */
            x-v-color.      
              */ 