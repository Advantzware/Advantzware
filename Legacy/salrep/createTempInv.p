
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
DEFINE INPUT PARAMETER fCompany        AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER tCompany        AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER fcust           AS ch        INIT " ".
DEFINE INPUT PARAMETER tcust           LIKE fcust INIT "zzzzzzzz".
DEFINE INPUT PARAMETER fitem           LIKE itemfg.i-no INIT " ".
DEFINE INPUT PARAMETER titem           LIKE fitem INIT "zzzzzzzzzzzzzzzzzzz".
DEFINE INPUT PARAMETER fpcat           LIKE itemfg.procat INIT " ".
DEFINE INPUT PARAMETER tpcat           LIKE fpcat INIT "zzzzzzzzzzzzzzzzzzz".
DEFINE INPUT PARAMETER fdate           AS DATE      FORMAT "99/99/9999".
DEFINE INPUT PARAMETER tdate           LIKE fdate.

DEFINE VARIABLE sort-by-cust    AS LOG       FORMAT "Customer/ProductCategory" INIT YES.
DEFINE VARIABLE v-inc-fc        AS LOG       INIT NO.

DEFINE VARIABLE v-date          AS CHARACTER FORMAT "x(10)" NO-UNDO.
DEFINE VARIABLE v-pric          LIKE ar-invl.unit-pr.
DEFINE VARIABLE v-uom           LIKE ar-invl.pr-uom.
DEFINE VARIABLE v-brdc          AS DECIMAL.
DEFINE VARIABLE v-ordc          AS DECIMAL.
DEFINE VARIABLE v-invc          AS DECIMAL.
DEFINE VARIABLE v-marg          AS DECIMAL.
DEFINE VARIABLE v-brdp          AS DECIMAL.
DEFINE VARIABLE v-$msf          AS DECIMAL.

DEFINE VARIABLE v-qty           AS INTEGER   EXTENT 4.
DEFINE VARIABLE v-msf           AS DECIMAL   EXTENT 4.
DEFINE VARIABLE v-cst           AS DECIMAL   EXTENT 4.
DEFINE VARIABLE v-cst1          AS DECIMAL   EXTENT 4.
DEFINE VARIABLE v-cst2          AS DECIMAL   EXTENT 4.
DEFINE VARIABLE v-amt           AS DECIMAL   EXTENT 4.

DEFINE VARIABLE v-cust-no       LIKE cust.cust-no.
DEFINE VARIABLE v-order-date    AS CHARACTER FORMAT "x(10)" NO-UNDO.
DEFINE VARIABLE v-job-no        LIKE job.job-no.
DEFINE VARIABLE v-job-no2       LIKE job.job-no2.
DEFINE VARIABLE v-po-no-po      LIKE ar-invl.po-no-po.
DEFINE VARIABLE v-fac           AS INTEGER.
DEFINE VARIABLE v-cost          AS DECIMAL.
DEFINE VARIABLE v-job-qty       AS INTEGER.
DEFINE VARIABLE v-exc           AS LOG.
DEFINE VARIABLE tb_prep         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE v-weight        AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-prodcode      AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-invLineNum    AS INTEGER NO-UNDO.
DEFINE VARIABLE v-brd-grade     AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-brd-paper-combo  AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-deliveryType  AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-shiptoName    AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-shiptoStreet  AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-shiptoCity    AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-shiptoState   AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-shiptoZip     AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-designNums    AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-cutDieNums    AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-printPlateNums AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-machRoute     AS CHARACTER NO-UNDO.



DEFINE VARIABLE v-cuom          LIKE po-ordl.cons-uom.
DEFINE VARIABLE v-unit          AS CHARACTER FORMAT "!" INIT "U".
DEFINE VARIABLE v-cost2         AS LOG       INIT YES.
DEFINE VARIABLE v-cost3         AS CHARACTER FORMAT "!" INIT "C".
DEFINE VARIABLE v-label1        AS CHARACTER FORMAT "x(15)".
DEFINE VARIABLE v-label2        AS CHARACTER FORMAT "x(4)".
DEFINE VARIABLE v-label3        AS CHARACTER FORMAT "x(11)".
DEFINE VARIABLE v-label4        AS CHARACTER FORMAT "x(11)".
DEFINE VARIABLE v-label5        AS CHARACTER FORMAT "x(9)".
DEFINE VARIABLE v-label6        AS CHARACTER FORMAT "X(15)".
DEFINE VARIABLE v-label7        AS CHARACTER FORMAT "X(15)".
DEFINE VARIABLE v-label8        AS CHARACTER FORMAT "X(15)".
DEFINE VARIABLE v-label9        AS CHARACTER FORMAT "X(9)".

DEFINE VARIABLE v-color         AS INTEGER   FORMAT ">9".
DEFINE VARIABLE x-v-color       AS CHARACTER FORMAT "x(2)".
DEFINE VARIABLE v-style         AS CHARACTER FORMAT "X(6)".
DEFINE VARIABLE v-flute         AS CHARACTER FORMAT "XXX".
DEFINE VARIABLE v-est-no        AS CHARACTER FORMAT "X(5)".
DEFINE VARIABLE v-test          AS CHARACTER FORMAT "X(6)".
DEFINE VARIABLE v-len           AS DECIMAL   FORMAT ">>9.99".
DEFINE VARIABLE v-wid           AS DECIMAL   FORMAT ">>9.99".
DEFINE VARIABLE v-dep           AS DECIMAL   FORMAT ">>9.99".
DEFINE VARIABLE v-space         AS CHARACTER FORMAT "X(2)".
DEFINE VARIABLE v-cust-part-no  AS CHARACTER FORMAT "X(15)".
DEFINE VARIABLE v-cust-part-no2 AS CHARACTER FORMAT "X(15)".
DEFINE VARIABLE v-period        AS INTEGER   NO-UNDO.  
DEFINE VARIABLE lv-r-no         LIKE oe-retl.r-no NO-UNDO.
DEFINE VARIABLE lv-type         AS CHARACTER NO-UNDO.

DEFINE VARIABLE cDisplay        AS cha       NO-UNDO.
DEFINE VARIABLE cExcelDisplay   AS cha       NO-UNDO.
DEFINE VARIABLE hField          AS HANDLE    NO-UNDO.
DEFINE VARIABLE cTmpField       AS CHA       NO-UNDO.
DEFINE VARIABLE cVarValue       AS cha       NO-UNDO.
DEFINE VARIABLE cExcelVarValue  AS cha       NO-UNDO.
DEFINE VARIABLE cSelectedList   AS cha       NO-UNDO.
DEFINE VARIABLE cFieldName      AS cha       NO-UNDO.
DEFINE VARIABLE str-tit4        AS cha       FORM "x(200)" NO-UNDO.
DEFINE VARIABLE str-tit5        AS cha       FORM "x(200)" NO-UNDO.
DEFINE VARIABLE str-line        AS cha       FORM "x(350)" NO-UNDO.
DEFINE VARIABLE lSelected       AS LOG       INIT YES NO-UNDO.
DEFINE VARIABLE ttRecsCreated   AS INTEGER NO-UNDO.
DEFINE VARIABLE ttLineRecsCreated   AS INTEGER NO-UNDO.

DEFINE VARIABLE excelheader     AS CHARACTER NO-UNDO.
DEFINE STREAM sDebug.
OUTPUT STREAM sDebug TO c:\temp\asi\tempInvDebug.txt.


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

/* ************************  Function Prototypes ********************** */


FUNCTION fExtCost RETURNS DECIMAL 
    (INPUT dCost AS DECIMAL, INPUT cUom AS CHARACTER , INPUT dQty AS decimal  ) FORWARD.


/* ***************************  Main Block  *************************** */
lSelected = NO.
IF cocode EQ "" THEN
  cocode = "001".
FIND FIRST fg-ctrl WHERE fg-ctrl.company EQ cocode NO-LOCK.


/* sa/sa-sls03.i */
FOR EACH cust
    WHERE cust.company EQ cocode
    AND cust.cust-no GE fcust
    AND cust.cust-no LE tcust
    AND (IF lselected THEN CAN-FIND(FIRST ttCustList WHERE ttCustList.cust-no EQ cust.cust-no
    AND ttCustList.log-fld NO-LOCK) ELSE TRUE)
    NO-LOCK:
        
    FOR EACH ar-inv
        WHERE ar-inv.company  EQ cocode
        AND ar-inv.posted   EQ YES
        AND ar-inv.cust-no  EQ cust.cust-no
        AND ar-inv.inv-date GE fdate
        AND ar-inv.inv-date LE tdate       
        AND (ar-inv.type    NE "FC" OR v-inc-fc)
        NO-LOCK:
        FIND FIRST invoiceLines NO-LOCK WHERE invoiceLines.company EQ ar-inv.company 
          AND invoiceLines.invoiceNumber EQ ar-inv.inv-no no-error.
        IF AVAIL invoiceLines THEN 
          NEXT.
		
        CREATE tt-report.
        ASSIGN 
            tt-report.term-id = ""
            tt-report.key-09  = cust.cust-no
            tt-report.key-10  = "ar-inv"
            tt-report.rec-id  = RECID(ar-inv).
        ttRecsCreated =  ttRecsCreated + 1.
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
    FIND ar-inv WHERE RECID(ar-inv) EQ tt-report.rec-id NO-LOCK.
	
    FIND FIRST invoiceLines NO-LOCK WHERE invoiceLines.company EQ ar-inv.company
      AND invoiceLines.invoiceNumber EQ ar-inv.inv-no no-error.
    IF AVAIL invoiceLines THEN 
        NEXT.
		
    FOR EACH ar-invl
        WHERE ar-invl.x-no    EQ ar-inv.x-no
        AND ar-invl.i-no    GE fitem
        AND ar-invl.i-no    LE titem
        AND ((tb_prep AND ar-invl.billable) OR NOT ar-invl.misc)
        NO-LOCK:
                
        
               
        FIND FIRST itemfg
            WHERE itemfg.company EQ cocode
            AND itemfg.i-no    EQ ar-invl.i-no
            AND itemfg.procat  GE fpcat
            AND itemfg.procat  LE tpcat
            NO-LOCK NO-ERROR.
              
        IF ("" LT fpcat OR "" GT tpcat) AND (NOT AVAILABLE itemfg) THEN NEXT.
        CREATE xtt-report.
        ASSIGN
            xtt-report.term-id = ""
            xtt-report.rec-id  = RECID(ar-invl)
            xtt-report.key-01  = IF sort-by-cust THEN tt-report.key-09
        ELSE IF AVAILABLE itemfg THEN itemfg.procat ELSE ""
            xtt-report.key-02  = ar-invl.i-no
            xtt-report.key-03  = STRING(ar-invl.inv-no,"999999")
            xtt-report.key-09  = tt-report.key-09
            xtt-report.key-10  = "ar-invl".
        ttLineRecsCreated = ttLineRecsCreated + 1.
    END.
END.

     DEFINE VARIABLE cnt3 AS INTEGER.
       DEFINE VARIABLE cnt4 AS INTEGER.
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
       
    FIND FIRST itemfg
        WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ w-data.i-no
        NO-LOCK NO-ERROR.
          
    /* # of colors */
	if avail itemfg then do:
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
       
        ASSIGN
            v-cust-part-no = itemfg.part-no.
 
        ASSIGN
            v-cust-part-no2 = w-data.i-no.
       
  
        ASSIGN
            v-style = itemfg.style
            v-len   = itemfg.l-score[50]
            v-wid   = itemfg.w-score[50]
            v-dep   = itemfg.d-score[50].
    END.
    
    IF AVAILABLE itemfg then do:
        FOR EACH eb FIELDS(test flute est-no) WHERE eb.company  EQ cocode
            AND eb.est-no   EQ itemfg.est-no
            AND eb.stock-no EQ itemfg.i-no NO-LOCK:
            ASSIGN 
                v-test  = eb.test
                v-flute = eb.flute.
            LEAVE.
        END.
    END.
            cnt3 = cnt3 + 1.
    IF tt-report.key-10 EQ "ar-invl" THEN 
    DO:
        FIND FIRST ar-invl WHERE RECID(ar-invl) EQ w-data.rec-id NO-LOCK.
        FIND ar-inv WHERE ar-inv.x-no EQ ar-invl.x-no NO-LOCK.
        ASSIGN
            v-cust-no  = ar-inv.cust-name
            v-date     = STRING(ar-inv.inv-date)
            v-pric     = ar-invl.unit-pr
            v-uom      = ar-invl.pr-uom
            v-job-no   = ar-invl.job-no
            v-job-no2  = ar-invl.job-no2
            v-po-no-po = ar-invl.po-no-po
            v-qty[1]   = IF ar-invl.ship-qty GT 0 THEN ar-invl.ship-qty ELSE ar-invl.inv-qty
            v-amt[1]   = ar-invl.amt
            v-msf[1]   = ar-invl.amt-msf.
        
        find first period NO-LOCK
            where period.company eq cocode
            and period.pst     LE ar-inv.inv-date
            and period.pend    ge ar-inv.inv-date
            and period.pstat
			NO-ERROR.
  
        if available period then       
            v-period = period.pnum.
     


        FIND FIRST oe-ord WHERE oe-ord.company = cocode 
            AND oe-ord.ord-no  = ar-inv.ord-no NO-LOCK NO-ERROR.
        IF AVAILABLE oe-ord THEN ASSIGN v-order-date = STRING(oe-ord.ord-date) .
        
        FIND FIRST job-hdr WHERE job-hdr.company = cocode
            AND job-hdr.job-no  = ar-invl.job-no
            AND job-hdr.job-no2 = ar-invl.job-no2
            AND job-hdr.i-no    = w-data.i-no NO-LOCK NO-ERROR.
        IF AVAILABLE(job-hdr) AND job-hdr.est-no <> "" THEN
            v-est-no = TRIM(job-hdr.est-no).
        ELSE
            v-est-no = TRIM(ar-invl.est-no).
        IF v-est-no = "" THEN 
        DO:
            IF AVAILABLE itemfg THEN
                v-est-no = TRIM(itemfg.est-no) .
        END.
        FIND FIRST oe-boll NO-LOCK 
            WHERE oe-boll.company EQ ar-invl.company
              AND oe-boll.b-no EQ ar-invl.b-no
            NO-ERROR. 
        IF AVAILABLE oe-boll THEN DO:
            FIND FIRST oe-bolh NO-LOCK 
               WHERE oe-bolh.b-no EQ oe-boll.b-no
               NO-ERROR.
            IF AVAILABLE oe-bolh THEN 
              FIND FIRST shipto NO-LOCK 
                 WHERE shipto.company EQ cocode 
                   AND shipto.cust-no EQ oe-bolh.cust-no 
                   AND shipto.ship-id EQ oe-bolh.ship-id
                 NO-ERROR. 
              IF AVAILABLE shipto THEN DO:
                  ASSIGN 
                    v-shiptoName    = shipto.ship-name
                    v-shiptoStreet  = shipto.ship-addr[1]
                    v-shiptoCity    = shipto.ship-city
                    v-shiptoState   = shipto.ship-state
                    v-shiptoZip     = shipto.ship-zip
                    . 
              END.              
        END.
           
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
            OUTPUT v-cst2[1]).
            
        if v-msf[1] eq 0 and avail itemfg then
            v-msf[1] = (v-qty[1] * itemfg.t-sqft / 1000).

        if v-msf[1] eq 0 and avail itemfg then
            v-msf[1] = (v-qty[1] * itemfg.t-sqft / 1000).

        /*   if v-unit eq "E" then
             assign
              v-brdc = v-cst[1]
              v-marg = v-amt[1] - v-cst[1].
              
           else do:*/
        v-brdc = v-cst[1] / (v-qty[1] / 1000).
        v-ordc = v-cst1[1] / (v-qty[1] / 1000).
        v-invc = v-cst2[1] / (v-qty[1] / 1000).
        
        /*    if v-unit eq "U" then
              v-marg = (v-amt[1] - v-cst[1]) / (v-qty[1] / 1000).
            else*/
        v-marg = v-cst[1] / v-msf[1].
        /*   end.*/

        /*   if v-cost3 eq "C" then
             v-brdp = v-cst[1] / v-amt[1] * 100.
           else*/
        v-brdp = (v-amt[1] - v-cst[1]) / v-amt[1] * 100.
       
        v-$msf = v-amt[1] / v-msf[1].

        if v-brdc eq ? then v-brdc = 0.
        if v-ordc eq ? then v-ordc = 0.
        if v-invc eq ? then v-invc = 0.
        if v-marg eq ? then v-marg = 0.
        if v-brdp eq ? then v-brdp = 0.
        if v-$msf eq ? then v-$msf = 0.

        /* IF tl_color = YES THEN */
        x-v-color = string(v-color).
        /*            
        DISPLAY 
            v-cust-no
            w-data.inv-no
            v-shiptoName   
            v-shiptoStreet 
            v-shiptoCity   
            v-shiptoState  
            v-shiptoZip                
            w-data.i-no       
            v-cust-part-no    
           
            
            itemfg.procat     
            WHEN AVAILABLE itemfg
            
            v-qty[1]                                        /* qty-ship */
            
            itemfg.t-sqft     
            WHEN AVAILABLE itemfg                           /* i-sq */
            
            v-msf[1]                                        /* total msf */
            v-$msf                                          /* $ msf     */
            
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
          cnt4 = cnt4 + 1.
        CREATE InvoiceLines.    
        ASSIGN
            InvoiceLines.AdhesiveCost           = 0
            InvoiceLines.BoardCost              = v-brdc
            InvoiceLines.BoardGrade             = ""
            InvoiceLines.CoatingsCost           = 0
            InvoiceLines.company                = ar-invl.company
            InvoiceLines.ConsolCustName         = ""
            InvoiceLines.CustomerName           = v-cust-no
            InvoiceLines.CustomerPONumber       = ar-inv.po-no
            InvoiceLines.CuttingDieNumbers      = ""
            InvoiceLines.DeliveryCost           = ar-invl.t-freight
            InvoiceLines.DeliveryType           = ""
            InvoiceLines.DesignNumber           = ""
            InvoiceLines.DirectCosts            = 0
            InvoiceLines.DirectOverheadCost     = 0
            InvoiceLines.EquipmentRecovery      = 0
            InvoiceLines.FixedCosts             = (if avail itemfg then fExtCost(itemfg.std-fix-cost, itemfg.prod-uom, v-qty[1]) else 0)
            InvoiceLines.FixedOverHeadCosts     = (if avail itemfg then fExtCost(itemfg.std-fix-cost , itemfg.prod-uom, v-qty[1]) else 0)
            InvoiceLines.VariableOverheadcost   = (if avail itemfg then fExtCost(itemfg.std-var-cost  , itemfg.prod-uom, v-qty[1]) else 0)
            InvoiceLines.GlueCost               = 0
            InvoiceLines.InkCost                = 0
            .
            assign
            InvoiceLines.InvoiceDate            = ar-invl.inv-date
            InvoiceLines.InvoiceLineNumber      = ar-invl.line
            InvoiceLines.InvoiceMonthChar       = ""
            InvoiceLines.InvoiceNumber          = w-data.inv-no
            InvoiceLines.InvoiceType            = ar-inv.type
            InvoiceLines.LabelsCost             = 0
            InvoiceLines.LabourCost             = (if avail itemfg then fExtCost(itemfg.std-lab-cost  , itemfg.prod-uom, v-qty[1])  else 0)
            InvoiceLines.LabourDirectCost       = 0 
            InvoiceLines.LabourIndirectCost     = 0
            InvoiceLines.MachineRouting         = ""
            InvoiceLines.MaintenancePartscost   = 0
            InvoiceLines.MaterialsCost          = (if avail itemfg then fExtCost(itemfg.std-mat-cost  , itemfg.prod-uom, v-qty[1])  else 0)
            InvoiceLines.MSFPerInvoice          = v-msf[1]
            InvoiceLines.OrderNumber            = ar-invl.ord-no
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
            InvoiceLines.SalesDollsPerInvoice   = ar-inv.t-sales
            InvoiceLines.SGACosts               = (if avail itemfg then fExtCost(itemfg.std-var-cost , itemfg.prod-uom, v-qty[1])  else 0)
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
            InvoiceLines.TermsDiscount          = 0
            InvoiceLines.ToolingCost            = 0
            InvoiceLines.TotalCost              = (if avail itemfg then fExtCost(itemfg.total-std-cost  , itemfg.prod-uom, v-qty[1])  else 0) /* v-invc */
            InvoiceLines.UnitPrice              = (if v-unit EQ "E" then v-amt[1] else v-pric)
            InvoiceLines.UOM                    = (if v-unit EQ "E" then "" else v-uom)
            InvoiceLines.WarehouseBin           = ""
            InvoiceLines.WarehouseCode          = ""
            InvoiceLines.WarehouseCost          = 0
            InvoiceLines.WeightLbs              = 0
            .                                
    END.
END.
OUTPUT STREAM sDebug CLOSE.
MESSAGE "cnt1" ttRecsCreated "cnt2" ttLineRecsCreated SKIP "cnt3" cnt3 SKIP "cnt4" cnt4
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
                 .
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
    DEFINE INPUT PARAMETER  v-bol-no   LIKE oe-boll.bol-no.
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

/* ************************  Function Implementations ***************** */


FUNCTION fExtCost RETURNS DECIMAL 
	(INPUT dCost AS DECIMAL, INPUT cUom AS CHARACTER , INPUT dQty AS decimal):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/	

		DEFINE VARIABLE dResult AS DECIMAL NO-UNDO.
        dResult = dCost * dQty / 
            (IF cUom EQ "C"  THEN 100               ELSE
            IF cUom EQ "M"  THEN 1000              ELSE
            IF cUom EQ "CS" AND AVAILABLE itemfg AND
            itemfg.case-count NE 0 THEN itemfg.case-count ELSE 1).
        
        IF dResult = ? THEN dResult = 0.
PUT STREAM sDebug UNFORMATTED  ar-invl.inv-no " cost " dcost " qty " dqty " cuom " cuom " dres " dResult
        SKIP. 

		RETURN dResult.


		
END FUNCTION.
