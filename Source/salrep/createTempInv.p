/*------------------------------------------------------------------------
    File        : createTempInv.p
    Purpose     : Export AR Invoices to table InvoiceLines

    Syntax      :

    Description : 

    Author(s)   : Wade Kaldawl (update by Ron Stark 8.7.2019)
    Created     : Mon Jan 08 11:33:40 EST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE INPUT PARAMETER lPurgeRecs AS LOGICAL          NO-UNDO.
DEFINE INPUT PARAMETER fCompany   AS CHARACTER        NO-UNDO.
DEFINE INPUT PARAMETER tCompany   AS CHARACTER        NO-UNDO.
DEFINE INPUT PARAMETER fcust      AS CHARACTER        NO-UNDO.
DEFINE INPUT PARAMETER tcust    LIKE fcust            NO-UNDO.
DEFINE INPUT PARAMETER fitem    LIKE itemfg.i-no      NO-UNDO.
DEFINE INPUT PARAMETER titem    LIKE fitem            NO-UNDO.
DEFINE INPUT PARAMETER fpcat    LIKE itemfg.procat    NO-UNDO.
DEFINE INPUT PARAMETER tpcat    LIKE fpcat            NO-UNDO.
DEFINE INPUT PARAMETER fdate      AS DATE             NO-UNDO.
DEFINE INPUT PARAMETER tdate    LIKE fdate            NO-UNDO.

DEFINE VARIABLE v-date            AS CHARACTER        NO-UNDO.
DEFINE VARIABLE v-pric          LIKE ar-invl.unit-pr  NO-UNDO.
DEFINE VARIABLE v-uom           LIKE ar-invl.pr-uom   NO-UNDO.
DEFINE VARIABLE v-brdc            AS DECIMAL          NO-UNDO.
DEFINE VARIABLE v-ordc            AS DECIMAL          NO-UNDO.
DEFINE VARIABLE v-invc            AS DECIMAL          NO-UNDO.
DEFINE VARIABLE v-marg            AS DECIMAL          NO-UNDO.
DEFINE VARIABLE v-brdp            AS DECIMAL          NO-UNDO.
DEFINE VARIABLE v-$msf            AS DECIMAL          NO-UNDO.
DEFINE VARIABLE v-qty             AS INTEGER EXTENT 4 NO-UNDO.
DEFINE VARIABLE v-msf             AS DECIMAL EXTENT 4 NO-UNDO.
DEFINE VARIABLE v-cst             AS DECIMAL EXTENT 4 NO-UNDO.
DEFINE VARIABLE v-cst1            AS DECIMAL EXTENT 4 NO-UNDO.
DEFINE VARIABLE v-cst2            AS DECIMAL EXTENT 4 NO-UNDO.
DEFINE VARIABLE v-amt             AS DECIMAL EXTENT 4 NO-UNDO.
DEFINE VARIABLE v-cust-no       LIKE cust.cust-no     NO-UNDO.
DEFINE VARIABLE v-order-date      AS CHARACTER        NO-UNDO.
DEFINE VARIABLE v-job-no        LIKE job.job-no       NO-UNDO.
DEFINE VARIABLE v-job-no2       LIKE job.job-no2      NO-UNDO.
DEFINE VARIABLE v-po-no-po      LIKE ar-invl.po-no-po NO-UNDO.
DEFINE VARIABLE v-fac             AS INTEGER          NO-UNDO.
DEFINE VARIABLE v-shiptoName      AS CHARACTER        NO-UNDO.
DEFINE VARIABLE v-shiptoStreet    AS CHARACTER        NO-UNDO.
DEFINE VARIABLE v-shiptoCity      AS CHARACTER        NO-UNDO.
DEFINE VARIABLE v-shiptoState     AS CHARACTER        NO-UNDO.
DEFINE VARIABLE v-shiptoZip       AS CHARACTER        NO-UNDO.
DEFINE VARIABLE v-color           AS INTEGER          NO-UNDO.
DEFINE VARIABLE x-v-color         AS CHARACTER        NO-UNDO.
DEFINE VARIABLE v-style           AS CHARACTER        NO-UNDO.
DEFINE VARIABLE v-flute           AS CHARACTER        NO-UNDO.
DEFINE VARIABLE v-est-no          AS CHARACTER        NO-UNDO.
DEFINE VARIABLE v-test            AS CHARACTER        NO-UNDO.
DEFINE VARIABLE v-len             AS DECIMAL          NO-UNDO.
DEFINE VARIABLE v-wid             AS DECIMAL          NO-UNDO.
DEFINE VARIABLE v-dep             AS DECIMAL          NO-UNDO.
DEFINE VARIABLE v-cust-part-no    AS CHARACTER        NO-UNDO.
DEFINE VARIABLE v-cust-part-no2   AS CHARACTER        NO-UNDO.
DEFINE VARIABLE v-period          AS INTEGER          NO-UNDO.

DEFINE TEMP-TABLE tt-report LIKE report.

DEFINE BUFFER btt-report FOR tt-report.

DEFINE TEMP-TABLE w-data NO-UNDO
    FIELD sorter   AS CHARACTER
    FIELD i-no   LIKE ar-invl.i-no
    FIELD inv-no LIKE ar-invl.inv-no
    FIELD rec-id   AS RECID
    .
{sys/inc/var.i "NEW SHARED"}
{sys/form/s-top.f}
{sys/ref/CustList.i NEW}

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */

FUNCTION fExtCost RETURNS DECIMAL 
    (dCost AS DECIMAL, cUom AS CHARACTER, dQty AS DECIMAL) FORWARD.

/* ***************************  Main Block  *************************** */

IF lPurgeRecs THEN DO:
    FOR EACH invoiceLines.
        DELETE invoiceLines.
    END. 
END.

FOR EACH company NO-LOCK
    WHERE company.company GE fCompany
      AND company.company LE tCompany,
    EACH cust NO-LOCK
    WHERE cust.company EQ company.company
      AND cust.cust-no GE fcust
      AND cust.cust-no LE tcust
    :
    FOR EACH ar-inv NO-LOCK
        WHERE ar-inv.company  EQ company.company
          AND ar-inv.posted   EQ YES
          AND ar-inv.cust-no  EQ cust.cust-no
          AND ar-inv.inv-date GE fdate
          AND ar-inv.inv-date LE tdate       
          AND ar-inv.type     NE "FC"
        :
        FIND FIRST invoiceLines NO-LOCK
             WHERE invoiceLines.company       EQ ar-inv.company 
               AND invoiceLines.invoiceNumber EQ ar-inv.inv-no
             NO-ERROR.
        IF AVAILABLE invoiceLines THEN NEXT.
        CREATE tt-report.
        ASSIGN 
            tt-report.term-id = ""
            tt-report.key-08  = ar-inv.company
            tt-report.key-09  = ar-inv.cust-no
            tt-report.key-10  = "ar-inv"
            tt-report.rec-id  = RECID(ar-inv)
            .
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
    :
    FIND ar-inv NO-LOCK 
         WHERE RECID(ar-inv) EQ tt-report.rec-id.
    FOR EACH ar-invl NO-LOCK
        WHERE ar-invl.x-no EQ ar-inv.x-no
          AND ar-invl.i-no GE fitem
          AND ar-invl.i-no LE titem
          AND ar-invl.misc EQ NO
        :
        FIND FIRST itemfg NO-LOCK
             WHERE itemfg.company EQ ar-invl.company
               AND itemfg.i-no    EQ ar-invl.i-no
               AND itemfg.procat  GE fpcat
               AND itemfg.procat  LE tpcat
             NO-ERROR.
        IF NOT AVAILABLE itemfg THEN NEXT.
        CREATE btt-report.
        ASSIGN
            btt-report.term-id = ""
            btt-report.rec-id  = RECID(ar-invl)
            btt-report.key-01  = tt-report.key-09
            btt-report.key-02  = ar-invl.i-no
            btt-report.key-03  = STRING(ar-invl.inv-no,"999999")
            btt-report.key-08  = tt-report.key-08
            btt-report.key-09  = tt-report.key-09
            btt-report.key-10  = "ar-invl"
            .
    END.
END.

FOR EACH tt-report
    WHERE tt-report.term-id EQ ""
    BREAK BY tt-report.key-01
          BY tt-report.key-02
          BY tt-report.key-03
    TRANSACTION:

    CREATE w-data.
    ASSIGN
        w-data.i-no   = tt-report.key-02
        w-data.inv-no = INTEGER(tt-report.key-03)
        w-data.rec-id = tt-report.rec-id
        .       
    FIND FIRST itemfg NO-LOCK
         WHERE itemfg.company EQ tt-report.key-08
           AND itemfg.i-no    EQ w-data.i-no
        NO-ERROR.          
    /* # of colors */
    IF AVAILABLE itemfg THEN DO:
        FOR EACH itemfg-ink OF itemfg NO-LOCK
            WHERE itemfg-ink.i-no EQ itemfg.i-no, 
            EACH item NO-LOCK
            WHERE item.company EQ itemfg-ink.company
              AND item.i-no EQ itemfg-ink.rm-i-no 
            :
            IF AVAILABLE itemfg-ink THEN
            v-color = v-color + itemfg-ink.occurs.
        END.
        ASSIGN
            v-cust-part-no  = itemfg.part-no
            v-cust-part-no2 = w-data.i-no
            v-style         = itemfg.style
            v-len           = itemfg.l-score[50]
            v-wid           = itemfg.w-score[50]
            v-dep           = itemfg.d-score[50]
            .
        FOR EACH eb FIELDS(test flute est-no) NO-LOCK
            WHERE eb.company  EQ itemfg.company
              AND eb.est-no   EQ itemfg.est-no
              AND eb.stock-no EQ itemfg.i-no
            :
            ASSIGN 
                v-test  = eb.test
                v-flute = eb.flute
                .
            LEAVE.
        END.
    END.    
    
    IF tt-report.key-10 EQ "ar-invl" THEN DO:
        FIND FIRST ar-invl NO-LOCK WHERE RECID(ar-invl) EQ w-data.rec-id.
        FIND ar-inv NO-LOCK WHERE ar-inv.x-no EQ ar-invl.x-no.
        ASSIGN
            v-cust-no  = ar-inv.cust-name
            v-date     = STRING(ar-inv.inv-date)
            v-pric     = ar-invl.unit-pr
            v-uom      = ar-invl.pr-uom
            v-job-no   = ar-invl.job-no
            v-job-no2  = ar-invl.job-no2
            v-po-no-po = ar-invl.po-no-po
            v-qty[1]   = IF ar-invl.ship-qty GT 0 THEN ar-invl.ship-qty
                       ELSE ar-invl.inv-qty
            v-amt[1]   = ar-invl.amt
            v-msf[1]   = ar-invl.amt-msf
            .        
        FIND FIRST period NO-LOCK
            WHERE period.company EQ ar-inv.company
              AND period.pst     LE ar-inv.inv-date
              AND period.pend    GE ar-inv.inv-date
              AND period.pstat
            NO-ERROR.  
        IF AVAILABLE period THEN
        v-period = period.pnum.
     
        FIND FIRST oe-ord NO-LOCK
             WHERE oe-ord.company EQ ar-inv.company 
               AND oe-ord.ord-no  EQ ar-inv.ord-no  
             NO-ERROR.
        IF AVAILABLE oe-ord THEN
        v-order-date = STRING(oe-ord.ord-date).
        
        FIND FIRST job-hdr NO-LOCK
             WHERE job-hdr.company EQ ar-invl.company
               AND job-hdr.job-no  EQ ar-invl.job-no
               AND job-hdr.job-no2 EQ ar-invl.job-no2
               AND job-hdr.i-no    EQ w-data.i-no
             NO-ERROR.            
        IF AVAILABLE(job-hdr) AND job-hdr.est-no NE "" THEN
            v-est-no = TRIM(job-hdr.est-no).
        ELSE
            v-est-no = TRIM(ar-invl.est-no).
        IF v-est-no EQ "" AND AVAILABLE itemfg THEN
        v-est-no = TRIM(itemfg.est-no).
        
        FIND FIRST oe-boll NO-LOCK 
             WHERE oe-boll.company EQ ar-invl.company
               AND oe-boll.b-no    EQ ar-invl.b-no
             NO-ERROR. 
        IF AVAILABLE oe-boll THEN DO:
            FIND FIRST oe-bolh NO-LOCK 
                 WHERE oe-bolh.b-no EQ oe-boll.b-no
                 NO-ERROR.
            IF AVAILABLE oe-bolh THEN 
            FIND FIRST shipto NO-LOCK 
                 WHERE shipto.company EQ oe-bolh.company 
                   AND shipto.cust-no EQ oe-bolh.cust-no 
                   AND shipto.ship-id EQ oe-bolh.ship-id
                NO-ERROR. 
            IF AVAILABLE shipto THEN 
            ASSIGN 
                v-shiptoName   = shipto.ship-name
                v-shiptoStreet = shipto.ship-addr[1]
                v-shiptoCity   = shipto.ship-city
                v-shiptoState  = shipto.ship-state
                v-shiptoZip    = shipto.ship-zip
                . 
        END.
           
        RUN salecost (
            "4",
            ROWID(ar-invl),
            v-job-no,
            v-job-no2,
            v-qty[1],
            OUTPUT v-cst[1]
            ).
        RUN salecost (
            "2",
            ROWID(ar-invl),
            v-job-no,
            v-job-no2,
            v-qty[1],
            OUTPUT v-cst1[1]
            ).
        RUN salecost (
            "3",
            ROWID(ar-invl),
            v-job-no,
            v-job-no2,
            v-qty[1],
            OUTPUT v-cst2[1]
            ).
            
        IF v-msf[1] EQ 0 AND AVAILABLE itemfg THEN
            v-msf[1] = (v-qty[1] * itemfg.t-sqft / 1000).

        IF v-msf[1] EQ 0 AND AVAILABLE itemfg THEN
            v-msf[1] = (v-qty[1] * itemfg.t-sqft / 1000).

        ASSIGN
            v-brdc = v-cst[1] / (v-qty[1] / 1000)
            v-ordc = v-cst1[1] / (v-qty[1] / 1000)
            v-invc = v-cst2[1] / (v-qty[1] / 1000)
            v-marg = v-cst[1] / v-msf[1]
            v-brdp = (v-amt[1] - v-cst[1]) / v-amt[1] * 100
            v-$msf = v-amt[1] / v-msf[1]
            x-v-color = STRING(v-color)
            .
        IF v-brdc EQ ? THEN v-brdc = 0.
        IF v-ordc EQ ? THEN v-ordc = 0.
        IF v-invc EQ ? THEN v-invc = 0.
        IF v-marg EQ ? THEN v-marg = 0.
        IF v-brdp EQ ? THEN v-brdp = 0.
        IF v-$msf EQ ? THEN v-$msf = 0.

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
            InvoiceLines.FixedCosts             = (IF AVAILABLE itemfg THEN fExtCost(itemfg.std-fix-cost, itemfg.prod-uom, v-qty[1]) ELSE 0)
            InvoiceLines.FixedOverHeadCosts     = (IF AVAILABLE itemfg THEN fExtCost(itemfg.std-fix-cost, itemfg.prod-uom, v-qty[1]) ELSE 0)
            InvoiceLines.VariableOverheadcost   = (IF AVAILABLE itemfg THEN fExtCost(itemfg.std-var-cost, itemfg.prod-uom, v-qty[1]) ELSE 0)
            InvoiceLines.GlueCost               = 0
            InvoiceLines.InkCost                = 0
            InvoiceLines.InvoiceDate            = ar-invl.inv-date
            InvoiceLines.InvoiceLineNumber      = ar-invl.line
            InvoiceLines.InvoiceMonthChar       = ""
            InvoiceLines.InvoiceNumber          = w-data.inv-no
            InvoiceLines.InvoiceType            = ar-inv.type
            InvoiceLines.LabelsCost             = 0
            InvoiceLines.LabourCost             = (IF AVAILABLE itemfg THEN fExtCost(itemfg.std-lab-cost, itemfg.prod-uom, v-qty[1]) ELSE 0)
            InvoiceLines.LabourDirectCost       = 0 
            InvoiceLines.LabourIndirectCost     = 0
            InvoiceLines.MachineRouting         = ""
            InvoiceLines.MaintenancePartscost   = 0
            InvoiceLines.MaterialsCost          = (IF AVAILABLE itemfg THEN fExtCost(itemfg.std-mat-cost, itemfg.prod-uom, v-qty[1]) ELSE 0)
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
            InvoiceLines.SGACosts               = (IF AVAILABLE itemfg THEN fExtCost(itemfg.std-var-cost, itemfg.prod-uom, v-qty[1]) ELSE 0)
            InvoiceLines.ShipToCity             = v-shiptoCity
            InvoiceLines.ShipToCountry          = ""
            InvoiceLines.ShipToName             = v-shiptoName
            InvoiceLines.ShipToState            = v-shiptoState
            InvoiceLines.ShipToStreet           = v-shiptoStreet
            InvoiceLines.ShipToZip              = v-ShiptoZip
            InvoiceLines.StrappingBundlingCost  = 0
            InvoiceLines.StretchWrapCost        = 0
            InvoiceLines.TermsDiscount          = 0
            InvoiceLines.ToolingCost            = 0
            InvoiceLines.TotalCost              = (IF AVAILABLE itemfg THEN fExtCost(itemfg.total-std-cost, itemfg.prod-uom, v-qty[1]) ELSE 0)
            InvoiceLines.UnitPrice              = v-pric
            InvoiceLines.UOM                    = v-uom
            InvoiceLines.WarehouseBin           = ""
            InvoiceLines.WarehouseCode          = ""
            InvoiceLines.WarehouseCost          = 0
            InvoiceLines.WeightLbs              = 0
            .                                
    END.
END.
  
PROCEDURE boardCost:
/* -------------------------------------------- sys/inc/boardCost.p 01/02 JLF */
/* Calculate the total cost of the board received for a job/item              */
/* -------------------------------------------------------------------------- */
    DEFINE INPUT  PARAMETER ipcCompany  AS CHARACTER      NO-UNDO.
    DEFINE INPUT  PARAMETER v-job-no  LIKE job.job-no     NO-UNDO.
    DEFINE INPUT  PARAMETER v-job-no2 LIKE job.job-no2    NO-UNDO.
    DEFINE INPUT  PARAMETER v-i-no    LIKE itemfg.i-no    NO-UNDO.
    DEFINE INPUT  PARAMETER v-bol-no  LIKE oe-boll.bol-no NO-UNDO.
    DEFINE INPUT  PARAMETER v-qty       AS INTEGER        NO-UNDO.
    DEFINE INPUT  PARAMETER v-act-cost  AS LOGICAL        NO-UNDO.
    DEFINE OUTPUT PARAMETER v-tot-cost  AS DECIMAL        NO-UNDO.

    DEFINE VARIABLE v-job-qty   LIKE job-hdr.qty  NO-UNDO.
    DEFINE VARIABLE v-cost        AS DECIMAL      NO-UNDO.
    DEFINE VARIABLE lv-est-type LIKE est.est-type NO-UNDO.

    IF v-job-no EQ "" THEN DO:
        /* Find BOL for item. */
        FIND FIRST oe-boll NO-LOCK
             WHERE oe-boll.company EQ ipcCompany
               AND oe-boll.bol-no  EQ v-bol-no
               AND oe-boll.i-no    EQ v-i-no
             NO-ERROR.
        IF AVAILABLE oe-boll THEN
        ASSIGN
            v-job-no  = oe-boll.job-no
            v-job-no2 = oe-boll.job-no2
            .
    END. /* if v-job-no eq "" */

    FIND FIRST job NO-LOCK
         WHERE job.company EQ ipcCompany
           AND job.job-no  EQ v-job-no
           AND job.job-no2 EQ v-job-no2
         NO-ERROR.            
    IF AVAILABLE job THEN DO:
        FIND FIRST job-hdr NO-LOCK
             WHERE job-hdr.company EQ job.company
               AND job-hdr.job     EQ job.job
               AND job-hdr.job-no  EQ job.job-no
               AND job-hdr.job-no2 EQ job.job-no2
               AND job-hdr.i-no    EQ v-i-no
             NO-ERROR.
        IF AVAILABLE job-hdr THEN DO:
            IF job.est-no NE "" THEN
                FIND FIRST est NO-LOCK
                     WHERE est.company EQ job.company
                       AND est.est-no  EQ job.est-no
                     NO-ERROR.
            lv-est-type = IF AVAILABLE est THEN est.est-type ELSE 1.
            IF lv-est-type GT 4 THEN
            lv-est-type = lv-est-type - 4.
    
            FOR EACH fg-rcpth NO-LOCK
                WHERE fg-rcpth.company   EQ job-hdr.company
                  AND fg-rcpth.job-no    EQ job-hdr.job-no
                  AND fg-rcpth.job-no2   EQ job-hdr.job-no2
                  AND fg-rcpth.i-no      EQ job-hdr.i-no
                  AND fg-rcpth.rita-code EQ "R",
                EACH fg-rdtlh NO-LOCK
                WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no
                :
                v-job-qty = v-job-qty + fg-rdtlh.qty.
            END.
    
            IF v-act-cost THEN DO:
                FOR EACH mat-act NO-LOCK
                    WHERE mat-act.company EQ job-hdr.company
                      AND mat-act.job     EQ job-hdr.job
                      AND mat-act.job-no  EQ job-hdr.job-no
                      AND mat-act.job-no2 EQ job-hdr.job-no2
                      AND (mat-act.s-num  EQ job-hdr.frm
                       OR lv-est-type EQ 2),
                    FIRST job-mat NO-LOCK
                    WHERE job-mat.company  EQ mat-act.company
                      AND job-mat.job      EQ mat-act.job
                      AND job-mat.frm      EQ mat-act.s-num
                      AND job-mat.blank-no EQ mat-act.b-num
                      AND job-mat.i-no     EQ mat-act.i-no,
                    FIRST item NO-LOCK
                    WHERE item.company EQ mat-act.company
                      AND item.i-no    EQ mat-act.i-no
                      AND INDEX("BA",item.mat-type) GT 0
                    :      
                    IF item.r-wid EQ 0 THEN
                        RUN sys/ref/convcuom.p (
                            job-mat.sc-uom,
                            mat-act.qty-uom,
                           (IF job-mat.basis-w  NE 0 THEN
                            job-mat.basis-w ELSE item.basis-w),
                           (IF job-mat.len      NE 0 THEN
                            job-mat.len ELSE item.s-len),
                           (IF job-mat.wid      NE 0 THEN
                            job-mat.wid ELSE item.s-wid),
                            item.s-dep,   
                            mat-act.cost,
                            OUTPUT v-cost
                            ).
                    ELSE
                        RUN sys/ref/convcuom.p (
                            job-mat.sc-uom,
                            mat-act.qty-uom,
                           (IF job-mat.basis-w  NE 0 THEN
                            job-mat.basis-w ELSE item.basis-w),
                            job-mat.len,
                           (IF job-mat.wid      NE 0 THEN
                            job-mat.wid ELSE item.r-wid),
                            item.s-dep,   
                            mat-act.cost,
                            OUTPUT v-cost
                            ).
                    IF mat-act.ext-cost EQ 0 OR mat-act.ext-cost EQ ? THEN
                        v-tot-cost = v-tot-cost + (v-cost * mat-act.qty).
                    ELSE
                        v-tot-cost = v-tot-cost + mat-act.ext-cost.
                END.
        
                ASSIGN
                    v-tot-cost = v-tot-cost * (job-hdr.qty / v-job-qty)
                    v-tot-cost = v-tot-cost / job-hdr.qty * v-qty
                    .
            END. /* if v-act-cost */
      
            ELSE
                FOR EACH job-mat NO-LOCK
                    WHERE job-mat.company EQ job-hdr.company
                      AND job-mat.job     EQ job-hdr.job
                      AND job-mat.job-no  EQ job-hdr.job-no
                      AND job-mat.job-no2 EQ job-hdr.job-no2
                      AND (job-mat.frm    EQ job-hdr.frm
                       OR lv-est-type EQ 2),
                    FIRST item NO-LOCK
                    WHERE item.company EQ job-mat.company
                      AND item.i-no    EQ job-mat.i-no
                      AND index("BA",item.mat-type) GT 0
                    BREAK BY job-mat.frm 
                          BY item.mat-type
                    :      
                    IF item.mat-type NE "B" OR FIRST-OF(item.mat-type) THEN
                    v-tot-cost = v-tot-cost + (job-mat.cost-m * v-qty / 1000).      
                END.
        END.
    END.
END PROCEDURE.

PROCEDURE boardCostM:
/* -------------------------------------------- sys/inc/boardCost.p 01/02 JLF */
/* Calculate the total cost of the board received for a job/item              */
/* -------------------------------------------------------------------------- */
    DEFINE INPUT  PARAMETER ipcCompany  AS CHARACTER      NO-UNDO.
    DEFINE INPUT  PARAMETER v-job-no  LIKE job.job-no     NO-UNDO.
    DEFINE INPUT  PARAMETER v-job-no2 LIKE job.job-no2    NO-UNDO.
    DEFINE INPUT  PARAMETER v-i-no    LIKE itemfg.i-no    NO-UNDO.
    DEFINE INPUT  PARAMETER v-bol-no  LIKE oe-boll.bol-no NO-UNDO.
    DEFINE INPUT  PARAMETER v-qty       AS INTEGER        NO-UNDO.
    DEFINE INPUT  PARAMETER v-act-cost  AS LOGICAL        NO-UNDO.
    DEFINE OUTPUT PARAMETER v-tot-cost  AS DECIMAL        NO-UNDO.

    DEFINE VARIABLE v-fgrec-qty LIKE job-hdr.qty  NO-UNDO.
    DEFINE VARIABLE v-cost        AS DECIMAL      NO-UNDO.
    DEFINE VARIABLE lv-est-type LIKE est.est-type NO-UNDO.
    DEFINE VARIABLE v-po-cost     AS DECIMAL      NO-UNDO.
    DEFINE VARIABLE v-issued-qty  AS INTEGER      NO-UNDO.
    DEFINE VARIABLE v-rec-qty     AS INTEGER      NO-UNDO.
    DEFINE VARIABLE v-sub-tot     AS DECIMAL      NO-UNDO.
    DEFINE VARIABLE v-sub-qty     AS DECIMAL      NO-UNDO.
    DEFINE VARIABLE v-req-qty     AS INTEGER      NO-UNDO.

    /* Find BOL for item. */
    FOR EACH oe-boll NO-LOCK
        WHERE oe-boll.company EQ ipcCompany
          AND oe-boll.bol-no  EQ v-bol-no
          AND oe-boll.i-no    EQ v-i-no
        :
        ASSIGN 
            v-job-no    = oe-boll.job-no
            v-job-no2   = oe-boll.job-no2
            v-fgrec-qty = 0
            v-sub-tot   = 0 
            v-req-qty   = 0
            v-sub-qty   = 0
            .
        FIND FIRST job NO-LOCK
             WHERE job.company EQ oe-boll.company
               AND job.job-no  EQ oe-boll.job-no
               AND job.job-no2 EQ oe-boll.job-no2
             NO-ERROR.    
        IF AVAILABLE job THEN DO:
            FIND FIRST job-hdr NO-LOCK
                WHERE job-hdr.company EQ job.company
                  AND job-hdr.job     EQ job.job
                  AND job-hdr.job-no  EQ job.job-no
                  AND job-hdr.job-no2 EQ job.job-no2
                  AND job-hdr.i-no    EQ v-i-no
                NO-ERROR.
            IF AVAILABLE job-hdr THEN DO:
                IF job.est-no NE "" THEN
                    FIND FIRST est NO-LOCK
                         WHERE est.company EQ job.company
                           AND est.est-no  EQ job.est-no
                         NO-ERROR.
                lv-est-type = IF AVAILABLE est THEN est.est-type ELSE 1.
                IF lv-est-type GT 4 THEN lv-est-type = lv-est-type - 4.
    
                FOR EACH fg-rcpth NO-LOCK
                    WHERE fg-rcpth.company   EQ job-hdr.company
                      AND fg-rcpth.job-no    EQ job-hdr.job-no
                      AND fg-rcpth.job-no2   EQ job-hdr.job-no2
                      AND fg-rcpth.i-no      EQ job-hdr.i-no
                      AND fg-rcpth.rita-code EQ "R",
                    EACH fg-rdtlh NO-LOCK
                    WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no
                    :
                    v-fgrec-qty = v-fgrec-qty + fg-rdtlh.qty.
                END.
         
                FOR EACH mat-act NO-LOCK
                    WHERE mat-act.company EQ job-hdr.company
                      AND mat-act.job     EQ job-hdr.job
                      AND mat-act.job-no  EQ job-hdr.job-no
                      AND mat-act.job-no2 EQ job-hdr.job-no2
                      AND (mat-act.s-num  EQ job-hdr.frm
                       OR lv-est-type EQ 2),
                    FIRST job-mat NO-LOCK
                    WHERE job-mat.company  EQ mat-act.company
                      AND job-mat.job      EQ mat-act.job
                      AND job-mat.frm      EQ mat-act.s-num
                      AND job-mat.blank-no EQ mat-act.b-num
                      AND job-mat.i-no     EQ mat-act.i-no,
                    FIRST item NO-LOCK
                    WHERE item.company EQ mat-act.company
                      AND item.i-no    EQ mat-act.i-no
                      AND index("BA",item.mat-type) GT 0
                    BREAK BY job-mat.i-no
                    :
                    FIND FIRST job-mch NO-LOCK
                         WHERE job-mch.company EQ job.company 
                           AND job-mch.job     EQ job.job 
                           AND job-mch.job-no  EQ job.job-no 
                           AND job-mch.job-no2 EQ job.job-no2 
                         USE-INDEX line-idx NO-ERROR.
                    IF AVAILABLE job-mch THEN
                    v-req-qty = job-mch.run-qty.
              
                    FOR EACH po-ordl NO-LOCK
                        WHERE po-ordl.company   EQ job-mat.company
                          AND po-ordl.i-no      EQ job-mat.i-no
                          AND po-ordl.job-no    EQ v-job-no
                          AND po-ordl.job-no2   EQ v-job-no2
                        USE-INDEX item 
                        BY po-ordl.po-no DESCENDING
                        :
                        LEAVE.
                    END.        
                    IF AVAILABLE po-ordl THEN DO:
                    ASSIGN
                        v-po-cost    = po-ordl.cons-cost * 1000  /* po-ordl.t-cost */
                        v-issued-qty = po-ordl.t-rec-qty
                        .
                    ASSIGN
                        v-sub-tot = v-sub-tot + (v-issued-qty / 1000 * v-po-cost )
                        v-sub-qty = ( v-sub-tot / ( MIN( v-issued-qty / v-req-qty , 1) * job-hdr.qty ) * v-fgrec-qty )
                        v-tot-cost = v-tot-cost + ( v-sub-qty / 1000 * oe-boll.qty )
                        .
                    END.
                    ELSE DO:
                        IF FIRST-OF(job-mat.i-no) THEN
                        v-tot-cost = v-tot-cost + oe-boll.qty / 1000 * job-mat.cost-m.
                    END.
                END.  /* job-mat */
            END. /* avail job-hdr */
        END. /* avail job */
    END.  /* for each oe-boll */
END PROCEDURE.

PROCEDURE salecost:
    DEFINE INPUT  PARAMETER ip-type    AS   INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER ip-rowid   AS   ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER ip-job-no  LIKE job.job-no  NO-UNDO.
    DEFINE INPUT  PARAMETER ip-job-no2 LIKE job.job-no2 NO-UNDO.
    DEFINE INPUT  PARAMETER ip-qty     AS   INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER op-cost    AS   DECIMAL     NO-UNDO.

    DEFINE VARIABLE lv-uom   LIKE itemfg.prod-uom NO-UNDO.
    DEFINE VARIABLE v-bol-no LIKE oe-boll.bol-no  NO-UNDO.

    v-bol-no = 0.
    FIND ar-invl NO-LOCK
         WHERE ROWID(ar-invl) EQ ip-rowid
         NO-ERROR.
    IF AVAILABLE ar-invl THEN DO:
        v-bol-no = ar-invl.bol-no.
        FIND fg-ctrl NO-LOCK
             WHERE fg-ctrl.company EQ ar-invl.company
             NO-ERROR.
        FIND FIRST itemfg NO-LOCK
             WHERE itemfg.company EQ ar-invl.company
               AND itemfg.i-no    EQ ar-invl.i-no
             NO-ERROR.
        IF ip-type EQ 3 AND ar-invl.t-cost NE ? THEN DO:
            IF ar-invl.dscr[1] EQ "M" OR ar-invl.dscr[1] EQ "" THEN
                op-cost = ip-qty / 1000 * ar-invl.cost.
            ELSE
                op-cost = ip-qty * ar-invl.cost.
        END.

        IF ip-type EQ 2 THEN DO:
            FIND FIRST oe-ordl NO-LOCK
                 WHERE oe-ordl.company EQ ar-invl.company
                   AND oe-ordl.ord-no  EQ ar-invl.ord-no
                   AND oe-ordl.i-no    EQ ar-invl.i-no
                 NO-ERROR.
            IF AVAILABLE oe-ordl THEN
            op-cost = oe-ordl.cost * ip-qty / 1000.
        END.

        IF ip-type EQ 1 THEN
            RUN boardCost (
                ar-invl.company,
                ip-job-no,
                ip-job-no2,                            
                ar-invl.i-no,
                v-bol-no,
                ip-qty,
                YES,
                OUTPUT op-cost
                ).
        IF ip-type EQ 4 THEN
            RUN boardCostM (
                ar-invl.company,
                ip-job-no,
                ip-job-no2,                            
                ar-invl.i-no,
                v-bol-no,
                ip-qty,
                YES,
                OUTPUT op-cost
                ).

        IF op-cost EQ ? THEN
        op-cost = 0.

        IF op-cost EQ 0 THEN DO:
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
                lv-uom  = po-ordl.cons-uom
                .
            IF op-cost EQ 0 AND AVAILABLE itemfg THEN
            ASSIGN
                op-cost = IF itemfg.i-code EQ "C" AND ip-type EQ 2 THEN
                          itemfg.std-mat-cost ELSE
                          IF fg-ctrl.inv-meth EQ "A" THEN itemfg.avg-cost
                          ELSE itemfg.last-cost
                lv-uom  = itemfg.prod-uom
                .
            op-cost = op-cost * ip-qty /
                     (IF lv-uom EQ "C"  THEN 100  ELSE
                      IF lv-uom EQ "M"  THEN 1000 ELSE
                      IF lv-uom EQ "CS" AND AVAILABLE itemfg AND
                      itemfg.case-count NE 0 THEN itemfg.case-count ELSE 1).
        END.

        IF op-cost EQ ? THEN
        op-cost = 0.
    END.
END PROCEDURE.

/* ************************  Function Implementations ***************** */

FUNCTION fExtCost RETURNS DECIMAL 
    (INPUT dCost AS DECIMAL, INPUT cUom AS CHARACTER , INPUT dQty AS DECIMAL):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/	
    DEFINE VARIABLE dResult AS DECIMAL NO-UNDO.
    
    dResult = dCost * dQty / 
             (IF cUom EQ "C"  THEN 100  ELSE
              IF cUom EQ "M"  THEN 1000 ELSE
              IF cUom EQ "CS" AND AVAILABLE itemfg AND
              itemfg.case-count NE 0 THEN itemfg.case-count ELSE 1).
    IF dResult = ? THEN
    dResult = 0.

    RETURN dResult.
END FUNCTION.
