&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : aoaAppSrv/aoaAR.p
    Purpose     : AppServer Functions and Procedures

    Syntax      : 

    Description : AppServer Functions and Procedures

    Author(s)   : Ron Stark
    Created     : 3.23.2016
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Customer.rpa */
DEFINE TEMP-TABLE ttCust NO-UNDO RCODE-INFORMATION
    FIELD company  LIKE cust.company  
    FIELD custNo   LIKE cust.cust-no
    FIELD custName LIKE cust.name
        INDEX ttCust company custNo
        .
/* Customer.rpa */

/* Cash Receipts By SalesRep.rpa */
DEFINE TEMP-TABLE ttCashReceiptBySalesRep NO-UNDO
    FIELD salesRep    AS CHARACTER LABEL "Sales Rep"
    FIELD salesName   AS CHARACTER LABEL "Sales Name"
    FIELD custNo      AS CHARACTER LABEL "Customer"
    FIELD custName    AS CHARACTER LABEL "Customer Name"
    FIELD terms       AS CHARACTER LABEL "Terms"
    FIELD invoiceNo   AS INTEGER   LABEL "Invoice"
    FIELD invDate     AS DATE      LABEL "Inv Date"
    FIELD chkDate     AS DATE      LABEL "Check Date"
    FIELD Aging       AS DECIMAL   LABEL "Aging"
    FIELD invAmt      AS DECIMAL   LABEL "Inv Amount"
    FIELD amtPaid     AS DECIMAL   LABEL "Amt Paid"
    FIELD discount    AS DECIMAL   LABEL "Discount"
    FIELD balAftPay   AS DECIMAL   LABEL "Bal Aft Pymt"
    FIELD commAmt     AS DECIMAL   LABEL "Comm Amt"
    FIELD commPct     AS DECIMAL   LABEL "Comm Pct"
    FIELD rec-id      AS RECID
    FIELD row-id      AS ROWID
    .
DEFINE TEMP-TABLE tt-report NO-UNDO LIKE report
    FIELD inv-no AS INT 
    FIELD chk-inv AS LOG INIT YES.
DEFINE TEMP-TABLE tt-report-inv NO-UNDO LIKE report 
    FIELD inv-no AS INT  .
/* Cash Receipts By SalesRep.rpa */

/* Commision Cash Receipt.rpa */
DEFINE TEMP-TABLE ttCommissionCashReceipt NO-UNDO
    FIELD salesRep    AS CHARACTER LABEL "Sales Rep"
    FIELD custNo      AS CHARACTER LABEL "Customer"
    FIELD invoiceNo   AS INTEGER   LABEL "Invoice"
    FIELD invDate     AS DATE      LABEL "Inv Date"
    FIELD custPart    AS CHARACTER LABEL "Cust Part"
    FIELD orderNo     AS INTEGER   LABEL "Order"
    FIELD invQty      AS DECIMAL   LABEL "Quantity"
    FIELD invAmt      AS DECIMAL   LABEL "Inv Amt"
    FIELD amtPaid     AS DECIMAL   LABEL "Amt Paid"
    FIELD amtD        AS DECIMAL   LABEL "Amt D"
    FIELD delta       AS DECIMAL   LABEL "Delta"
    FIELD grossProfit AS DECIMAL   LABEL "Gross Profit"
    FIELD cost        AS DECIMAL   LABEL "Cost"
    FIELD commission  AS DECIMAL   LABEL "Commission"
    FIELD commAmt     AS DECIMAL   LABEL "Comm Amt"
    FIELD commPct     AS DECIMAL   LABEL "Comm Pct"
    FIELD cashDate    AS DATE      LABEL "Cash Date"
    FIELD basis       AS CHARACTER LABEL "Basis"
    FIELD totalCost   AS DECIMAL   LABEL "Total Cost"
    FIELD rec-id      AS RECID
    FIELD row-id      AS ROWID
    .
{sys/ref/CustList.i NEW}
/* Commision Cash Receipt.rpa */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */


&IF DEFINED(EXCLUDE-fCashReceiptBySalesRep) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fCashReceiptBySalesRep Procedure 
FUNCTION fCashReceiptBySalesRep RETURNS HANDLE
  ( ipcCompany AS CHARACTER,
    ipiBatch   AS INTEGER,
    ipcUserID  AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fCommissionCashReceipt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fCommissionCashReceipt Procedure 
FUNCTION fCommissionCashReceipt RETURNS HANDLE
  ( ipcCompany AS CHARACTER,
    ipiBatch   AS INTEGER,
    ipcUserID  AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fCustomers) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fCustomers Procedure 
FUNCTION fCustomers RETURNS HANDLE
  ( ipcCompany AS CHARACTER,
    ipiBatch   AS INTEGER,
    ipcUserID  AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fGetTableHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetTableHandle Procedure 
FUNCTION fGetTableHandle RETURNS HANDLE
  ( ipcProgramID AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pBuildCustList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBuildCustList Procedure 
PROCEDURE pBuildCustList :
/*------------------------------------------------------------------------------
  Purpose:     Commission Cash Receipt.rpa
  Parameters:  Company, Use List?, Start Cust, End Cust, ID
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplList      AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcStartCust AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEndCust   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcID        AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bCust FOR cust.
    
    DEFINE VARIABLE lActive AS LOGICAL NO-UNDO.
    
    EMPTY TEMP-TABLE ttCustList.

    IF iplList THEN
    RUN sys/ref/CustList.p (ipcCompany, ipcID, YES, OUTPUT lActive).
    ELSE DO:
        FOR EACH bCust NO-LOCK
            WHERE bCust.company EQ ipcCompany
              AND bCust.cust-no GE ipcStartCust
              AND bCust.cust-no LE ipcEndCust
            :
            CREATE ttCustList.
            ASSIGN 
                ttCustList.cust-no = bCust.cust-no
                ttCustList.log-fld = YES
                .
        END. /* each bcust */
    END. /* else */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCashReceiptBySalesRep) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCashReceiptBySalesRep Procedure 
PROCEDURE pCashReceiptBySalesRep :
/*------------------------------------------------------------------------------
  Purpose:     Commission Cash Receipt.rpa
  Parameters:  Company, Batch Seq, User ID
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiBatch   AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcUserID  AS CHARACTER NO-UNDO.
    
    /* parameter values loaded into these variables */
    DEFINE VARIABLE dtStartReceiptDate      AS DATE      NO-UNDO.
    DEFINE VARIABLE cStartReceiptDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtEndReceiptDate        AS DATE      NO-UNDO.
    DEFINE VARIABLE cEndReceiptDateOption   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllSalesReps           AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cStartSalesRep          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndSalesRep            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cShowInvoice            AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lDiscount               AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lPrep                   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE iOldDay                   AS INT   NO-UNDO.
    DEFINE VARIABLE cSelectedColumns        AS CHARACTER NO-UNDO.

    /* local variables */
    def var v-sman as   CHAR NO-UNDO.
    def var v-amt  like ar-cashl.amt-paid extent 2 NO-UNDO.
    def var v-paid like v-amt NO-UNDO.
    def var v-dsc  like v-amt NO-UNDO.
    def var v-com  like ar-cashl.amt-paid NO-UNDO.
    def var v-com-2 like v-com NO-UNDO.
    def var v-c-%  as   DEC NO-UNDO.
    def var v-tax  as   DEC NO-UNDO.
    def var v-pct  as   DEC NO-UNDO.
    DEF VAR v-misc AS   DEC NO-UNDO.
    DEF VAR v-basis LIKE sman.commbasis INIT "" NO-UNDO.
    DEF VAR v-term LIKE tt-report.term-id NO-UNDO.
    def var v-cust   as   log format "Customer/Invoice" init no no-undo.
    def var v-tot-amt as   dec extent 4 NO-UNDO.
    def var v-tot-rem as   dec extent 4 NO-UNDO.
    def var v-tot-com like v-tot-amt NO-UNDO.
    def var v-tot-dsc like v-tot-amt NO-UNDO.
    def var v-tot-paid like v-tot-amt NO-UNDO.
    DEF VAR vcustno AS CHAR NO-UNDO.
    DEF VAR v-aging AS INTE NO-UNDO.
    DEF VAR v-check-date AS DATE NO-UNDO.
    DEF VAR v-inv-date AS DATE NO-UNDO.
    DEF VAR v-disc-meth AS logi NO-UNDO INIT YES.
    DEF VAR v-amt-full AS DECI NO-UNDO FORM "->>>,>>>,>>9.99".
    DEF VAR v-inv-full AS DECI NO-UNDO FORM "->>>,>>>,>>9.99".
    DEF VAR v-rem-bal AS DECI NO-UNDO FORM "->>>,>>>,>>9.99".
    DEF VAR v-dsc2 AS DECI NO-UNDO.
    DEF VAR v-tmp-amt-1 AS DEC NO-UNDO.
    DEF VAR v-inv-found AS LOG NO-UNDO.

    def var i as INT NO-UNDO.
    def var j as INT NO-UNDO.
    

    DEFINE BUFFER bARInvl FOR ar-invl.
    
    /* locate parameter values record */
    RUN pGetParamValues (ipcCompany, "r-cashs2.", ipcUserID, ipiBatch).
    
    /* load parameter values from above record into variables */
    ASSIGN
        dtStartReceiptDate      = DATE(DYNAMIC-FUNCTION("fGetParamValue","svStartReceiptDate"))
        cStartReceiptDateOption = DYNAMIC-FUNCTION("fGetParamValue","svStartReceiptDateOption")
        dtEndReceiptDate        = DATE(DYNAMIC-FUNCTION("fGetParamValue","svEndReceiptDate"))
        cEndReceiptDateOption   = DYNAMIC-FUNCTION("fGetParamValue","svEndReceiptDateOption")
        lAllSalesReps           = DYNAMIC-FUNCTION("fGetParamValue","svAllSalesReps") EQ "yes"
        cStartSalesRep          = DYNAMIC-FUNCTION("fGetParamValue","svStartSalesRep")
        cEndSalesRep            = DYNAMIC-FUNCTION("fGetParamValue","svEndSalesRep")         
        cShowInvoice            = DYNAMIC-FUNCTION("fGetParamValue","svShowInvoice") EQ "yes"
        lDiscount               = DYNAMIC-FUNCTION("fGetParamValue","svTdisc") EQ "yes"
        lPrep                   = DYNAMIC-FUNCTION("fGetParamValue","svPrep") EQ "yes"
        iOldDay                   = DYNAMIC-FUNCTION("fGetParamValue","svDayOld") 
        dtStartReceiptDate      = DYNAMIC-FUNCTION("fDateOptionDate",cStartReceiptDateOption,dtStartReceiptDate)
        dtEndReceiptDate        = DYNAMIC-FUNCTION("fDateOptionDate",cEndReceiptDateOption,dtEndReceiptDate)
        cSelectedColumns        = DYNAMIC-FUNCTION("fGetParamValue","svSelectedColumns")
        .

    IF lAllSalesReps THEN
    ASSIGN
        cStartSalesRep = CHR(32)
        cEndSalesRep   = CHR(127)
        .
    for each cust where cust.company eq ipcCompany no-lock:
      
    if iOldDay eq 0 then
    for each ar-inv
        where ar-inv.company  eq ipcCompany
          and ar-inv.posted   eq yes
          and ar-inv.cust-no  eq cust.cust-no
          and ar-inv.inv-date ge dtStartReceiptDate
          and ar-inv.inv-date le dtEndReceiptDate
          and ar-inv.terms    eq "CASH"
        no-lock,

        each ar-invl
        where ar-invl.x-no eq ar-inv.x-no
        no-lock
        
        transaction:
         
      do i = 1 to 3:
        v-sman = if ar-invl.sman[i] eq "" and i eq 1 then cust.sman
                 else ar-invl.sman[i].

        if v-sman   lt cStartSalesRep                         or
           v-sman   gt cEndSalesRep                         or
           (i ne 1 and
            (v-sman eq "" or ar-invl.s-pct[i] eq 0)) then next.

        IF NOT CAN-FIND(first tt-report
            where tt-report.term-id eq v-term
              and tt-report.key-01  eq v-sman
              and tt-report.inv-no  eq ar-invl.inv-no
              and tt-report.rec-id  eq recid(ar-invl)) THEN do:
          create tt-report.
          assign
           tt-report.term-id = v-term
           tt-report.key-01  = v-sman
           tt-report.key-09  = cust.cust-no
           tt-report.key-08  = cust.terms
           tt-report.key-10  = cust.NAME
           tt-report.rec-id  = recid(ar-invl)
           tt-report.inv-no  = ar-invl.inv-no.

          IF cShowInvoice THEN
             ASSIGN
                tt-report.key-02  = cust.cust-no
                tt-report.key-03  = string(ar-invl.inv-no,"9999999999").
          ELSE
             ASSIGN
                tt-report.key-02  = string(ar-invl.inv-no,"9999999999")
                tt-report.key-03  = cust.cust-no.

          RELEASE tt-report.
        end.
      end.
    end.      

    for each ar-cash
        where ar-cash.company    eq ipcCompany
          and ar-cash.cust-no    eq cust.cust-no
          and ar-cash.check-date ge dtStartReceiptDate
          and ar-cash.check-date le dtEndReceiptDate
          and ar-cash.posted     eq yes
          and ar-cash.check-no   ne 0
        no-lock,

        each ar-cashl
        where ar-cashl.c-no   eq ar-cash.c-no
          and ar-cashl.posted eq yes
          and ar-cashl.memo   eq no
        no-lock
      
        transaction:

      IF NOT (iOldDay eq 0 or
         (ar-cash.check-date - ar-cashl.inv-date gt iOldDay and
          ar-cashl.inv-no ne 0)) THEN NEXT.

      if ar-cashl.inv-no ne 0 then
      for each ar-invl
          where ar-invl.company eq ipcCompany
            and ar-invl.cust-no eq ar-cash.cust-no
            and ar-invl.inv-no  eq ar-cashl.inv-no
          no-lock:

         do i = 1 to 3:
            v-sman = if ar-invl.sman[i] eq "" and i eq 1 then cust.sman
                     else ar-invl.sman[i].
           
            if v-sman  lt cStartSalesRep                         or
               v-sman  gt cEndSalesRep                           or
               (i ne 1 and
                (v-sman eq "" or ar-invl.s-pct[i] eq 0)) then next.
           
            IF NOT CAN-FIND(first tt-report
                where tt-report.term-id eq v-term
                  and tt-report.key-01  eq v-sman
                  and tt-report.inv-no  eq ar-invl.inv-no
                  and tt-report.rec-id  eq recid(ar-cashl)) then do:
               create tt-report.
               assign
                tt-report.term-id = v-term
                tt-report.key-01  = v-sman
                tt-report.key-09  = cust.cust-no
                tt-report.key-08  = cust.terms
                tt-report.key-10  = cust.NAME
                tt-report.rec-id  = recid(ar-cashl)
                tt-report.inv-no  = ar-invl.inv-no.

               IF cShowInvoice THEN
                  ASSIGN
                     tt-report.key-02  = cust.cust-no
                     tt-report.key-03  = string(ar-invl.inv-no,"9999999999").
               ELSE
                  ASSIGN
                     tt-report.key-02  = string(ar-invl.inv-no,"9999999999")
                     tt-report.key-03  = cust.cust-no.

               RELEASE tt-report.
            end.
         end.
      end.
      
      else
      if cust.sman ge cStartSalesRep  and
         cust.sman le cEndSalesRep    then do:
         v-sman = cust.sman.
        
         IF NOT CAN-FIND(first tt-report
             where tt-report.term-id eq v-term
               and tt-report.key-01  eq v-sman
               and tt-report.inv-no  eq ar-cashl.inv-no
               and tt-report.rec-id  eq recid(ar-cashl)) then do:
            create tt-report.
           
            assign
             tt-report.term-id = v-term
             tt-report.key-01  = v-sman
             tt-report.key-09  = cust.cust-no
             tt-report.key-08  = cust.terms
             tt-report.key-10  = cust.NAME
             tt-report.rec-id  = recid(ar-cashl)
             tt-report.inv-no  = ar-cashl.inv-no.
           
            IF cShowInvoice THEN
              ASSIGN
                 tt-report.key-02  = cust.cust-no
                 tt-report.key-03  = string(ar-cashl.inv-no,"9999999999").
            ELSE
              ASSIGN
                 tt-report.key-02  = string(ar-cashl.inv-no,"9999999999")
                 tt-report.key-03  = cust.cust-no.
           
            RELEASE tt-report.
         end.
      end.
    end.
  end.

  FOR EACH tt-report NO-LOCK:
    FIND FIRST tt-report-inv WHERE tt-report-inv.inv-no = tt-report.inv-no 
                                    AND  tt-report-inv.key-01 NE tt-report.key-01  NO-LOCK NO-ERROR.
      IF NOT AVAIL tt-report-inv THEN do:
      CREATE  tt-report-inv.
      ASSIGN
          tt-report-inv.key-01  =    tt-report.key-01
          tt-report-inv.key-02  =    tt-report.key-02
          tt-report-inv.key-03  =    tt-report.key-03
          tt-report-inv.inv-no  =    tt-report.inv-no  .
         
      END. 
      ELSE do:
          tt-report.chk-inv = FALSE .
      END.
          
  END.
  FOR EACH tt-report-inv NO-LOCK:
      DELETE tt-report-inv .
  END.

  for each tt-report where tt-report.term-id eq v-term,
  
      first cust
      where cust.company eq ipcCompany
        and cust.cust-no eq tt-report.key-09
      no-lock
          
      break by tt-report.key-01
            by tt-report.key-02
            by tt-report.key-03
            BY tt-report.inv-no
      
      transaction:
     
    find first sman
        where sman.company eq ipcCompany
          and sman.sman    eq tt-report.key-01
        no-lock no-error.
   
    release ar-inv.
    release ar-cash.

    ASSIGN v-check-date = ?
           v-inv-date = ?
           v-amt-full = 0
           v-inv-full = 0
           v-dsc2 = 0
           v-misc = 0.
    
    find ar-cashl where recid(ar-cashl) eq tt-report.rec-id no-lock no-error.    
        
    if avail ar-cashl then do:
       find first ar-cash where ar-cash.c-no eq ar-cashl.c-no no-lock.
       assign
          v-dsc[1] = if lDiscount then ar-cashl.amt-disc else 0
          v-check-date = ar-cash.check-date
          v-amt-full = ar-cashl.amt-paid
          v-inv-found = NO.
      
       if ar-cashl.inv-no ne 0 then
       for each ar-invl WHERE
           ar-invl.company eq ipcCompany AND
           ar-invl.cust-no eq ar-cash.cust-no AND
           ar-invl.inv-no  eq ar-cashl.inv-no
           no-lock,
           first ar-inv where
                 ar-inv.x-no eq ar-invl.x-no
                 no-lock
           break by ar-invl.inv-no:
      
           v-inv-found = YES.

           FIND FIRST itemfg
               WHERE itemfg.company EQ ipcCompany
                 AND itemfg.i-no    EQ ar-invl.i-no
               NO-LOCK NO-ERROR.
           
           RUN custom/combasis.p (ipcCompany, tt-report.key-01, cust.type,
                                  (IF AVAIL itemfg THEN itemfg.procat ELSE ""), 0,
                                  cust.cust-no,
                                  OUTPUT v-basis).
           
           if FIRST-OF(tt-report.inv-no) then
           DO:
              IF FIRST(ar-invl.inv-no) THEN
                 assign
                    v-amt    = 0
                    v-amt[1] = ar-inv.tax-amt +
                               (if ar-inv.f-bill then ar-inv.freight else 0)
                    v-inv-full = v-amt[1]
                    v-com    = 0
                    v-com-2  = 0
                    v-inv-date = ar-inv.inv-date.
                
   
              ASSIGN
                 v-inv-full = v-inv-full + ar-invl.amt.

              IF NOT lPrep AND ar-invl.misc THEN v-misc = v-misc + ar-invl.amt.
              ELSE DO:
                ASSIGN
                      v-amt[1] = v-amt[1] + ar-invl.amt
                      v-tmp-amt-1 = v-amt[1].
    
                  if ar-invl.sman[1] ne "" then
                  do i = 1 to 3:
                     if tt-report.key-01 eq ar-invl.sman[i] then do:
                        ASSIGN
                           v-amt[2] = v-amt[2] + (ar-invl.amt * ar-invl.s-pct[i] / 100)
                           v-com    = v-com +
                                      (((ar-invl.amt - if v-basis EQ "G" then ar-invl.t-cost else 0) *
                                      ar-invl.s-pct[i] / 100) * ar-invl.s-comm[i] / 100).
                        leave.
                     end.
                  end.
               
                  else
                     assign
                        v-amt[2] = v-amt[2] + ar-invl.amt
                        v-com    = v-com +
                                   ((ar-invl.amt - if v-basis EQ "G" then ar-invl.t-cost else 0) *
                                   (if avail sman then (sman.scomm / 100) else 0)).
                    
              END.
           end. /*end FIRST-OF(tt-report.inv-no)*/
         
       END. /*end each ar-invl*/

       IF v-inv-found = NO THEN
          ASSIGN
             v-amt[1] = ar-cashl.amt-paid + v-dsc[1] - v-misc
             v-tmp-amt-1 = v-amt[1]
             v-amt[2] = v-amt[1]
             v-com    = v-amt[1] * (if avail sman then (sman.scomm / 100) else 0).

       assign
          v-pct    = v-amt[2] / v-tmp-amt-1
          v-amt[1] = (ar-cashl.amt-paid + v-dsc[1] - v-misc) * v-pct    /* task 02261403 */
          v-pct    = v-amt[1] / v-amt[2]
          v-com-2  = v-com * v-pct.

       release ar-inv.
    end.
    
    else do:
      find ar-invl where recid(ar-invl) eq tt-report.rec-id no-lock.
      find first ar-inv where ar-inv.x-no eq ar-invl.x-no no-lock.

      FIND FIRST itemfg
            WHERE itemfg.company EQ ipcCompany
              AND itemfg.i-no    EQ ar-invl.i-no
            NO-LOCK NO-ERROR.

      RUN custom/combasis.p (ipcCompany, tt-report.key-01, cust.type,
                             (IF AVAIL itemfg THEN itemfg.procat ELSE ""), 0,
                             cust.cust-no,
                             OUTPUT v-basis).
     ASSIGN
         v-inv-full = ar-invl.amt .

      IF NOT lPrep AND ar-invl.misc THEN NEXT.

    ASSIGN
       v-amt[1] = ar-invl.amt 
       v-tmp-amt-1 = v-amt[1]
       v-com    = (ar-invl.amt - if v-basis EQ "G" then ar-invl.t-cost else 0) *
                  (if avail sman then (sman.scomm / 100) else 0)
       v-com-2  = v-com.
        
    end.
    
    if v-com-2  eq ? then v-com-2 = 0.
    if v-amt[1] eq ? then
       ASSIGN v-amt[1] = 0
              v-tmp-amt-1 = 0.
    
    ASSIGN
       v-c-% = v-com-2 / v-amt[1] * 100
       v-amt[1] = v-tmp-amt-1. /*multiple payments against an invoice, reset v-amt[1] value*/
    
    if v-c-% eq ? then v-c-% = 0.
    
    v-paid[1] = v-amt-full.

  

    v-aging = (IF v-check-date <> ? AND v-inv-date <> ? THEN v-check-date - v-inv-date ELSE 0).

    IF FIRST-OF(tt-report.inv-no) THEN
       v-rem-bal  = v-inv-full - v-amt-full - v-dsc[1].
    ELSE
       v-rem-bal  = v-rem-bal - v-amt-full - v-dsc[1].

       PUT UNFORMATTED "ttCashReceiptBySalesRep" SKIP.

       CREATE ttCashReceiptBySalesRep .
       ASSIGN
           ttCashReceiptBySalesRep.salesRep      = string(tt-report.key-01) 
           ttCashReceiptBySalesRep.salesName     =  IF AVAIL sman THEN string(sman.sname) ELSE ""
           ttCashReceiptBySalesRep.custNo        = STRING(tt-report.key-09)
           ttCashReceiptBySalesRep.custName      = tt-report.key-10
           ttCashReceiptBySalesRep.terms         = tt-report.key-08
           ttCashReceiptBySalesRep.invoiceNo     = IF AVAIL ar-cashl THEN  ar-cashl.inv-no ELSE IF AVAIL ar-inv THEN  ar-inv.inv-no ELSE 0
           ttCashReceiptBySalesRep.invDate       = v-inv-date   
           ttCashReceiptBySalesRep.chkDate       = v-check-date 
           ttCashReceiptBySalesRep.Aging         = v-aging
           ttCashReceiptBySalesRep.invAmt        = v-inv-full
           ttCashReceiptBySalesRep.amtPaid       = v-amt-full
           ttCashReceiptBySalesRep.discount      = v-dsc[1]
           ttCashReceiptBySalesRep.balAftPay     = v-rem-bal
           ttCashReceiptBySalesRep.commAmt       = v-com-2
           ttCashReceiptBySalesRep.commPct       = v-c-%  .
          
    assign
     v-tot-paid[1] = v-tot-paid[1] + v-paid[1]
     v-tot-dsc[1] = v-tot-dsc[1] + v-dsc[1]
     v-tot-amt[1] = v-tot-amt[1] + v-inv-full
     v-tot-com[1] = v-tot-com[1] + v-com-2.

    IF LAST-OF(tt-report.inv-no) THEN
       v-tot-rem[1] = v-tot-rem[1] + v-rem-bal.
     
    if last-of(tt-report.key-02) then do:
      if cShowInvoice then do:
       
       v-c-% = v-tot-com[1] / v-tot-amt[1] * 100.
        
        if v-c-% eq ? then v-c-% = 0.
       
      end.

      ASSIGN
       v-tot-paid[1] = 0
       v-tot-dsc[1] = 0
       v-tot-amt[1] = 0
       v-tot-rem[1] = 0
       v-tot-com[1] = 0.

    end. /* last of key-02 */
 

    delete tt-report.
  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCommissionCashReceipt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCommissionCashReceipt Procedure 
PROCEDURE pCommissionCashReceipt :
/*------------------------------------------------------------------------------
  Purpose:     Commission Cash Receipt.rpa
  Parameters:  Company, Batch Seq, User ID
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiBatch   AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcUserID  AS CHARACTER NO-UNDO.

    /* parameter values loaded into these variables */
    DEFINE VARIABLE dtStartReceiptDate      AS DATE      NO-UNDO.
    DEFINE VARIABLE cStartReceiptDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtEndReceiptDate        AS DATE      NO-UNDO.
    DEFINE VARIABLE cEndReceiptDateOption   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtStartInvoiceDate      AS DATE      NO-UNDO.
    DEFINE VARIABLE cStartInvoiceDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtEndInvoiceDate        AS DATE      NO-UNDO.
    DEFINE VARIABLE cEndInvoiceDateOption   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllSalesReps           AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cStartSalesRep          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndSalesRep            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lCustList               AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lAllCustomers           AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cStartCustNo            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndCustNo              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cShowInvoice            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lDetailed               AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lPrep                   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lCalc                   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cSelectedColumns        AS CHARACTER NO-UNDO.

    /* local variables */
    DEFINE VARIABLE dtDate                  AS DATE      NO-UNDO.
    DEFINE VARIABLE dAmt                    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dAmtPaid                AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dAmtD                   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dInvPct                 AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lComplete               AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE idx                     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cSalesRep               AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dSalesPct               AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cCommBasis              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dProfit                 AS DECIMAL   NO-UNDO.

    DEFINE BUFFER bARInvl FOR ar-invl.
    
    /* locate parameter values record */
    RUN pGetParamValues (ipcCompany, "r-commcr.", ipcUserID, ipiBatch).
    
    /* load parameter values from above record into variables */
    ASSIGN
        dtStartReceiptDate      = DATE(DYNAMIC-FUNCTION("fGetParamValue","svStartReceiptDate"))
        cStartReceiptDateOption = DYNAMIC-FUNCTION("fGetParamValue","svStartReceiptDateOption")
        dtEndReceiptDate        = DATE(DYNAMIC-FUNCTION("fGetParamValue","svEndReceiptDate"))
        cEndReceiptDateOption   = DYNAMIC-FUNCTION("fGetParamValue","svEndReceiptDateOption")
        dtStartInvoiceDate      = DATE(DYNAMIC-FUNCTION("fGetParamValue","svStartInvoiceDate"))
        cStartInvoiceDateOption = DYNAMIC-FUNCTION("fGetParamValue","svStartInvoiceDateOption")
        dtEndInvoiceDate        = DATE(DYNAMIC-FUNCTION("fGetParamValue","svEndInvoiceDate"))
        cEndInvoiceDateOption   = DYNAMIC-FUNCTION("fGetParamValue","svEndInvoiceDateOption")
        lAllSalesReps           = DYNAMIC-FUNCTION("fGetParamValue","svAllSalesReps") EQ "yes"
        cStartSalesRep          = DYNAMIC-FUNCTION("fGetParamValue","svStartSalesRep")
        cEndSalesRep            = DYNAMIC-FUNCTION("fGetParamValue","svEndSalesRep")
        lCustList               = DYNAMIC-FUNCTION("fGetParamValue","svCustList") EQ "yes"
        lAllCustomers           = DYNAMIC-FUNCTION("fGetParamValue","svAllCustomers") EQ "yes"
        cStartCustNo            = DYNAMIC-FUNCTION("fGetParamValue","svStartCustNo")
        cEndCustNo              = DYNAMIC-FUNCTION("fGetParamValue","svEndCustNo")
        cShowInvoice            = DYNAMIC-FUNCTION("fGetParamValue","svShowInvoice")
        lDetailed               = DYNAMIC-FUNCTION("fGetParamValue","svDetailed") EQ "yes"
        lPrep                   = DYNAMIC-FUNCTION("fGetParamValue","svPrep") EQ "yes"
        lCalc                   = DYNAMIC-FUNCTION("fGetParamValue","svCalc") EQ "yes"
        dtStartReceiptDate      = DYNAMIC-FUNCTION("fDateOptionDate",cStartReceiptDateOption,dtStartReceiptDate)
        dtEndReceiptDate        = DYNAMIC-FUNCTION("fDateOptionDate",cEndReceiptDateOption,dtEndReceiptDate)
        dtStartInvoiceDate      = DYNAMIC-FUNCTION("fDateOptionDate",cStartInvoiceDateOption,dtStartInvoiceDate)
        dtEndInvoiceDate        = DYNAMIC-FUNCTION("fDateOptionDate",cEndInvoiceDateOption,dtEndInvoiceDate)
        cSelectedColumns        = DYNAMIC-FUNCTION("fGetParamValue","svSelectedColumns")
        .

    IF lAllSalesReps THEN
    ASSIGN
        cStartSalesRep = CHR(32)
        cEndSalesRep   = CHR(127)
        .

    IF lAllCustomers THEN
    ASSIGN
        cStartCustNo = CHR(32)
        cEndCustNo   = CHR(127)
        .

    RUN pBuildCustList (ipcCompany, lCustList, cStartCustNo, cEndCustNo, "AR15").

    FIND FIRST oe-ctrl NO-LOCK WHERE oe-ctrl.company EQ ipcCompany NO-ERROR.
    IF NOT AVAILABLE oe-ctrl THEN RETURN.

    FOR EACH ttCustList NO-LOCK
        WHERE ttCustList.log-fld,
        EACH ar-inv NO-LOCK
        WHERE ar-inv.company  EQ ipcCompany
          AND ar-inv.posted   EQ YES
          AND ar-inv.cust-no  EQ ttCustList.cust-no
          AND ar-inv.inv-date GE dtStartInvoiceDate
          AND ar-inv.inv-date LE dtEndInvoiceDate,
        FIRST cust NO-LOCK
        WHERE cust.company EQ ar-inv.company
          AND cust.cust-no EQ ar-inv.cust-no
        :

      dtDate = ?.
      FOR EACH ar-cashl NO-LOCK
          WHERE ar-cashl.company  EQ ar-inv.company
            AND ar-cashl.posted   EQ YES
            AND ar-cashl.cust-no  EQ ar-inv.cust-no
            AND ar-cashl.inv-no   EQ ar-inv.inv-no
          USE-INDEX inv-no,
          EACH ar-cash NO-LOCK
          WHERE ar-cash.c-no EQ ar-cashl.c-no
            AND ar-cash.memo EQ NO
            AND ar-cash.check-date GE dtStartReceiptDate
            AND ar-cash.check-date LE dtEndReceiptDate
          USE-INDEX c-no            
          BREAK BY ar-cash.check-date
          :
        /* Duplicate loop below just to get correct dtDate */
        IF NOT ar-cashl.memo THEN
        ASSIGN dtDate = ar-cash.check-date.
      END. /* each ar-cashl */

      dAmt = IF ar-inv.net EQ ar-inv.gross + ar-inv.freight + ar-inv.tax-amt THEN ar-inv.net
             ELSE ar-inv.gross.

      IF dtDate NE ? AND dtDate LE ar-inv.inv-date - ar-inv.disc-days THEN
      ASSIGN dAmt = dAmt - ROUND(dAmt * (ar-inv.disc-% / 100),2).

      IF dAmt EQ ? THEN dAmt = 0.

      ASSIGN 
          dAmtD    = dAmt
          dAmtPaid = dAmt
          dtDate   = ?
          .

      FOR EACH ar-cashl NO-LOCK
          WHERE ar-cashl.company  EQ ar-inv.company
            AND ar-cashl.posted   EQ YES
            AND ar-cashl.cust-no  EQ ar-inv.cust-no
            AND ar-cashl.inv-no   EQ ar-inv.inv-no
          USE-INDEX inv-no,
          EACH ar-cash NO-LOCK
          WHERE ar-cash.c-no EQ ar-cashl.c-no
            AND ar-cash.memo EQ NO
          USE-INDEX c-no
          BREAK BY ar-cash.check-date
          :

        IF ar-cashl.memo THEN    /* Just in case we change FOR EACH */
          IF ar-cashl.amt-disc NE 0 AND ar-cashl.amt-paid EQ 0 THEN
          ASSIGN
              dAmtD    = dAmtD    - ar-cashl.amt-disc
              dAmtPaid = dAmtPaid - ar-cashl.amt-disc
              .
          ELSE 
          IF ar-cashl.amt-paid + ar-cashl.amt-disc GT 0 THEN
          ASSIGN
              dAmtD    = dAmtD    + (ar-cashl.amt-paid + ar-cashl.amt-disc)
              dAmtPaid = dAmtPaid + (ar-cashl.amt-paid + ar-cashl.amt-disc)
              .
          ELSE
          ASSIGN
              dAmtD    = dAmtD    + (ar-cashl.amt-paid + (- (ar-cashl.amt-disc)))
              dAmtPaid = dAmtPaid + (ar-cashl.amt-paid + (- (ar-cashl.amt-disc)))
              .
        ELSE
        ASSIGN
            dAmtD     = dAmtD    + ((ar-cashl.amt-paid * -1) + (ar-cashl.amt-disc * -1))
            dAmtPaid  = dAmtPaid + (ar-cashl.amt-paid * -1)
            dtDate    = ar-cash.check-date
            .
      END. /* each ar-cashl */

      ASSIGN
          dAmtD    = dAmt - dAmtD
          dAmtPaid = dAmt - dAmtPaid
          .

      IF cShowInvoice EQ "All"                        OR
         (dAmtD LT dAmt AND cShowInvoice EQ "Unpaid") OR
         (dAmtD GE dAmt AND cShowInvoice EQ "Paid")   THEN DO:

        dInvPct = 0.
        FOR EACH bARInvl WHERE bARInvl.x-no EQ ar-inv.x-no NO-LOCK:
          dInvPct = dInvPct + bARInvl.amt.
        END. /* each barinvl */
        dInvPct = dInvPct / dAmt.
        IF dInvPct EQ ? THEN dInvPct = 0.

        ASSIGN
            dAmt     = dAmt     * dInvPct
            dAmtD    = dAmtD    * dInvPct
            dAmtPaid = dAmtPaid * dInvPct
            .

        FOR EACH ar-invl NO-LOCK
            WHERE ar-invl.x-no EQ ar-inv.x-no
              AND ((lPrep AND ar-invl.billable) OR NOT ar-invl.misc)
            :

          RUN oe/invlcomp.p (ROWID(ar-invl), OUTPUT lComplete).
          IF lComplete THEN NEXT.

          dInvPct = 0.
          FOR EACH bARInvl WHERE bARInvl.x-no EQ ar-invl.x-no NO-LOCK:
            dInvPct = dInvPct + bARInvl.amt.
            ACCUMULATE 1 (TOTAL).
          END. /* each barinvl */

          ASSIGN dInvPct = IF dInvPct EQ 0 THEN
              (1 / IF (ACCUM TOTAL 1) EQ 0 THEN 1
                   ELSE (ACCUM TOTAL 1))
                   ELSE (ar-invl.amt / dInvPct).

          IF dInvPct EQ ? THEN dInvPct = 0.

          DO idx = 1 TO 3:
            cSalesRep = IF ar-invl.sman[idx] EQ "" AND idx EQ 1 AND NOT ar-invl.misc THEN cust.sman
                        ELSE ar-invl.sman[idx].

            IF cSalesRep   LT cStartSalesRep OR
               cSalesRep   GT cEndSalesRep   OR
             ((idx NE 1 OR ar-invl.misc)     AND
              (cSalesRep EQ "" OR ar-invl.s-pct[idx] EQ 0)) THEN NEXT.

            dSalesPct = IF ar-invl.sman[idx] EQ "" OR
                          (ar-invl.s-pct[idx] EQ 0 AND idx EQ 1) THEN 100
                        ELSE ar-invl.s-pct[idx].

            FIND FIRST sman NO-LOCK
                WHERE sman.company EQ ipcCompany
                  AND sman.sman    EQ cSalesRep
                NO-ERROR.

            PUT UNFORMATTED "ttCommissionCashReceipt" SKIP.

            CREATE ttCommissionCashReceipt.
            ASSIGN
                ttCommissionCashReceipt.salesRep   = cSalesRep
                ttCommissionCashReceipt.custNo     = ar-inv.cust-no
                ttCommissionCashReceipt.invoiceNo  = ar-inv.inv-no
                ttCommissionCashReceipt.invQty     = ar-invl.inv-qty * dSalesPct / 100
                ttCommissionCashReceipt.invAmt     = dAmt * dInvPct * dSalesPct / 100
                ttCommissionCashReceipt.amtD       = dAmtD * dInvPct * dSalesPct / 100
                ttCommissionCashReceipt.amtPaid    = dAmtPaid * dInvPct * dSalesPct / 100
                ttCommissionCashReceipt.delta      = ttCommissionCashReceipt.invAmt - ttCommissionCashReceipt.amtD
                ttCommissionCashReceipt.cost       = ar-invl.t-cost * dSalesPct / 100
                ttCommissionCashReceipt.commission = IF ar-invl.sman[idx] EQ "" AND AVAIL sman
                                                     THEN sman.scomm ELSE ar-invl.s-comm[idx]
                ttCommissionCashReceipt.cashDate   = dtDate
                ttCommissionCashReceipt.rec-id     = RECID(ar-invl)
                ttCommissionCashReceipt.row-id     = ROWID(ar-invl)
                .
          END. /* do idx */
        END. /* each ar-invl */
      END. /* show all invoice */
    END. /* each ttCustList */

    FOR EACH ttCommissionCashReceipt,
        FIRST ar-invl NO-LOCK WHERE RECID(ar-invl) EQ ttCommissionCashReceipt.rec-id,
        FIRST ar-inv NO-LOCK WHERE ar-inv.x-no EQ ar-invl.x-no,
        FIRST cust NO-LOCK
        WHERE cust.company EQ ar-inv.company
          AND cust.cust-no EQ ar-inv.cust-no
        BREAK BY ttCommissionCashReceipt.salesRep
              BY ttCommissionCashReceipt.custNo
              BY ttCommissionCashReceipt.invoiceNo
              BY ttCommissionCashReceipt.row-id
              BY ttCommissionCashReceipt.rec-id
              BY ROWID(ttCommissionCashReceipt)
        :

      FIND FIRST sman NO-LOCK
          WHERE sman.company EQ ipcCompany
            AND sman.sman    EQ ttCommissionCashReceipt.salesRep
          NO-ERROR.

      RUN custom/combasis.p (
          ipcCompany,
          ttCommissionCashReceipt.salesRep,
          cust.type,
          "",
          0,
          cust.cust-no,
          OUTPUT cCommBasis
          ).

      RELEASE prep.
      RELEASE itemfg.

      IF ar-invl.misc THEN
      FIND FIRST prep NO-LOCK
           WHERE prep.company EQ ipcCompany
             AND prep.code    EQ ar-invl.i-name
           NO-ERROR.
      ELSE
      FIND FIRST itemfg NO-LOCK
           WHERE itemfg.company EQ ipcCompany
             AND itemfg.i-no    EQ ar-invl.i-no
           NO-ERROR.

      RUN custom/combasis.p (
          ipcCompany,
          ttCommissionCashReceipt.salesRep,
          cust.type,
         (IF AVAILABLE itemfg THEN itemfg.procat ELSE ""),
          0,
          cust.cust-no,
          OUTPUT cCommBasis
          ).

      IF ttCommissionCashReceipt.cost EQ ? THEN ttCommissionCashReceipt.cost = 0.
      IF ttCommissionCashReceipt.commission EQ ? THEN ttCommissionCashReceipt.commission = 0.
      
      ttCommissionCashReceipt.commPct = 0.
      
      ASSIGN
          dProfit = ttCommissionCashReceipt.invAmt - ttCommissionCashReceipt.cost
          ttCommissionCashReceipt.commAmt     = IF cCommBasis EQ "G" THEN (dProfit * ttCommissionCashReceipt.commission / 100)
                                                ELSE (ttCommissionCashReceipt.invAmt * (ttCommissionCashReceipt.commission / 100))
          ttCommissionCashReceipt.commPct     = ttCommissionCashReceipt.commission
          ttCommissionCashReceipt.grossProfit = ROUND(dProfit / ttCommissionCashReceipt.invAmt * 100,2)
          ttCommissionCashReceipt.commAmt     = ROUND(ttCommissionCashReceipt.commAmt,2)
          ttCommissionCashReceipt.invDate     = ar-inv.inv-date
          ttCommissionCashReceipt.custPart    = ar-invl.part-no
          ttCommissionCashReceipt.orderNo     = ar-invl.ord-no
          ttCommissionCashReceipt.basis       = IF AVAILABLE sman THEN sman.commbasis ELSE ""
          ttCommissionCashReceipt.totalCost   = IF AVAILABLE ar-invl THEN ar-invl.cost ELSE 0
          .

      IF ttCommissionCashReceipt.delta LE .99 THEN ttCommissionCashReceipt.delta = 0.

      IF lCalc AND ttCommissionCashReceipt.delta GT 0 THEN ttCommissionCashReceipt.commAmt = 0.

      IF ttCommissionCashReceipt.commPct EQ ? THEN ttCommissionCashReceipt.commPct = 0.
      IF ttCommissionCashReceipt.grossProfit EQ ? THEN ttCommissionCashReceipt.grossProfit = 0.

      {sys/inc/roundup.i ttCommissionCashReceipt.invQty}

    END. /* each ttCommissionCashReceipt */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */


&IF DEFINED(EXCLUDE-fCashReceiptBySalesRep) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fCashReceiptBySalesRep Procedure 
FUNCTION fCashReceiptBySalesRep RETURNS HANDLE
  ( ipcCompany AS CHARACTER,
    ipiBatch   AS INTEGER,
    ipcUserID  AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  Cash Receipts by Sales Rep.rpa
    Notes:  
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttCashReceiptBySalesRep.

    RUN pCashReceiptBySalesRep (ipcCompany, ipiBatch, ipcUserID).
    
    RETURN TEMP-TABLE ttCashReceiptBySalesRep:HANDLE .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fCommissionCashReceipt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fCommissionCashReceipt Procedure 
FUNCTION fCommissionCashReceipt RETURNS HANDLE
  ( ipcCompany AS CHARACTER,
    ipiBatch   AS INTEGER,
    ipcUserID  AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  Commission Cash Receipt.rpa
    Notes:  
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttCommissionCashReceipt.

    RUN pCommissionCashReceipt (ipcCompany, ipiBatch, ipcUserID).
    
    RETURN TEMP-TABLE ttCommissionCashReceipt:HANDLE .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fCustomers) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fCustomers Procedure 
FUNCTION fCustomers RETURNS HANDLE
  ( ipcCompany AS CHARACTER,
    ipiBatch   AS INTEGER,
    ipcUserID  AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  Customer.rpa
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cStartCustNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndCustNo   AS CHARACTER NO-UNDO.
    
    EMPTY TEMP-TABLE ttCust .
    
    RUN getParamValues (ipcCompany, "custaoa.", ipcUserID, ipiBatch).

    ASSIGN
        cStartCustNo = DYNAMIC-FUNCTION("fGetParamValue","svStartCustNo")
        cEndCustNo   = DYNAMIC-FUNCTION("fGetParamValue","svEndCustNo")
        .

    FOR EACH cust NO-LOCK 
        WHERE cust.company EQ ipcCompany
          AND cust.cust-no GT cStartCustNo
          AND cust.cust-no LE cEndCustNo
        :
        CREATE ttCust .
        ASSIGN 
            ttCust.company  = cust.company
            ttCust.custNo   = cust.cust-no
            ttCust.custName = cust.name
            .
    END .

    RETURN TEMP-TABLE ttCust:HANDLE .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fGetTableHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetTableHandle Procedure 
FUNCTION fGetTableHandle RETURNS HANDLE
  ( ipcProgramID AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    CASE ipcProgramID:
        /* Customers.rpa */
        WHEN "custaoa." THEN
        RETURN TEMP-TABLE ttCust:HANDLE.
        /* Cash Receipts By SalesRep.rpa */
        WHEN "r-cashs2." THEN
        RETURN TEMP-TABLE ttCashReceiptBySalesRep:HANDLE.
        /* Commision Cash Receipt.rpa */
        WHEN "r-commcr." THEN
        RETURN TEMP-TABLE ttCommissionCashReceipt:HANDLE.
    END CASE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

