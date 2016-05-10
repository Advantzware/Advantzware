&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : aoaAppSrv/aoaOE.p
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
/*************  Shared Varable  *******************/
DEF NEW SHARED VAR cocode AS CHAR NO-UNDO .

/*    Orders Booked.rpa           */
    DEFINE TEMP-TABLE ttOrdersBooked NO-UNDO
        FIELD rowType      AS CHARACTER INITIAL "Data" FORMAT "X(500)"
        FIELD dueDate      AS DATE      LABEL "DUE DATE" FORMAT 99/99/9999
        FIELD orderNo      AS INTEGER   LABEL "ORDER NO" FORMAT ">>>>>>>"
        FIELD custNo       AS CHARACTER LABEL "CUST NO" FORMAT "X(8)"
        FIELD custName     AS CHARACTER LABEL "CUSTOMER NAME"  FORMAT "X(30)"
        FIELD salesRep     AS CHARACTER LABEL "SALES REP"  FORMAT "X(3)"
        FIELD salesRepName AS CHARACTER LABEL "SALES NAME"  FORMAT "X(30)"
        FIELD commPer      AS DECIMAL   LABEL "COMM "  FORMAT ">>>>>9.99"
        FIELD prodCode     AS CHARACTER LABEL "PROD CODE" FORMAT "x(8)"
        FIELD fgItemNo     AS CHARACTER LABEL "FG ITEM"  FORMAT "X(15)"
        FIELD fgItemName   AS CHARACTER LABEL "FG ITEM NAME"  FORMAT "X(30)"
        FIELD qtyOrdEa     AS INTEGER   LABEL "QTY ORDERED EA" FORMAT ">,>>>,>>>"
        FIELD sqFit        AS DECIMAL   LABEL "SQ FT"    FORMAT ">>,>>>.999"
        FIELD totalSqfit   AS DECIMAL   LABEL "TOTAL Sq Ft M" format "->,>>>.999"
        FIELD msfPrice     AS DECIMAL   LABEL "MSF" FORMAT "->>,>>9.99"
        FIELD price        AS DECIMAL   LABEL "PRICE" format ">>>,>>9.99<<<<"
        FIELD orderAmount  AS DECIMAL   LABEL "ORDER AMOUNT" FORMAT "->,>>>,>>9.99"
        FIELD profitPer    AS DECIMAL   LABEL " PROFIT" FORMAT "->>,>>9.9"
        FIELD totalTons    AS DECIMAL   LABEL "TOTAL TONS" FORMAT "->,>>>.9"
        FIELD ton          AS DECIMAL   LABEL  "TON" FORMAT "->>,>>9.99"
        FIELD vUserID      AS CHARACTER LABEL  "ID" FORMAT "x(8)"
        FIELD custPartNo   AS CHARACTER LABEL  "CUST PART" FORMAT "x(15)" 
        .

    DEF TEMP-TABLE wkrecap no-undo    /* recap by product category */
        FIELD procat like itemfg.procat column-label "Cat"
        FIELD t-sqft like itemfg.t-sqft  extent 2 column-label "Sq Ft" format ">>,>>>.999"
        FIELD t-tons as dec column-label "Tons" extent 2 format "->,>>>.9"
        FIELD revenue like oe-ordl.t-price   extent 2 column-label "Amount"
        FIELD price-per-m  as dec column-label "$/MSF" extent 2
        FIELD price-per-t  as dec column-label "$/TON" extent 2
        FIELD num-of-ord as int column-label "#Orders"
        .

    DEF TEMP-TABLE w-data no-undo
        field ord-no like oe-ord.ord-no
        field line   like oe-ordl.line
        field sman   as char format "x(3)"
        field item-n like itemfg.i-name column-label "Item Description" format "x(27)"
        field procat like itemfg.procat column-label "Prod!Code"
        field qty like oe-ordl.qty column-label "Quantity!Ordered/EA" format ">,>>>,>>>"
        field sqft like itemfg.t-sqft column-label "Sq Ft" format ">>,>>>.999"
        field t-sqft like itemfg.t-sqft column-label "Total!Sq Ft/M" format "->,>>>.999"
        field t-tons as dec column-label "Total!  Tons" format "->,>>>.9"
        field price like oe-ordl.price format ">>>,>>9.99<<<<"
        field revenue like oe-ordl.t-price column-label "Order!Amount"
        field misc as log
        field cost as dec
        field comm as dec label "Comm %"
        FIELD margin AS DEC
        . 

    DEFINE TEMP-TABLE tt-report NO-UNDO LIKE report
        FIELD inv-no AS INT 
        FIELD chk-inv AS LOG INIT YES
        FIELD q-onh    LIKE itemfg.q-onh
        FIELD q-shp    LIKE itemfg.q-onh
        FIELD q-rel    LIKE itemfg.q-onh
        FIELD q-wip    LIKE itemfg.q-onh
        FIELD q-avl    LIKE itemfg.q-onh
        FIELD po-no    LIKE oe-ord.po-no
        FIELD inv      AS   LOG
        FIELD cad-no   LIKE itemfg.cad-no
        FIELD row-id   AS ROWID 
        FIELD due-date LIKE oe-ordl.req-date
        FIELD unit-count LIKE eb.cas-cnt
        FIELD units-pallet LIKE eb.cas-pal
        .
    
/* Orders Booked     */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-fGetTableHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetTableHandle Procedure 
FUNCTION fGetTableHandle RETURNS HANDLE
  ( ipcProgramID AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fOrdersBooked) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fOrdersBooked Procedure 
FUNCTION fOrdersBooked RETURNS HANDLE
  ( ipcCompany AS CHARACTER,
    ipiBatch   AS INTEGER,
    ipcUserID  AS CHARACTER )  FORWARD.

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

&IF DEFINED(EXCLUDE-pOrdersBooked) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pOrdersBooked Procedure 
PROCEDURE pOrdersBooked :
    /*------------------------------------------------------------------------------
    Purpose:     OrdersBooked.rpa
    Parameters:  Company, Batch Seq, User ID
    Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiBatch   AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcUserID  AS CHARACTER NO-UNDO.

    /* parameter values loaded into these variables */
    DEFINE VARIABLE lCustList             AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lAllCustomers         AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cStartCustNo          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndCustNo            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtStartOrderDate      AS DATE      NO-UNDO.
    DEFINE VARIABLE cStartOrderDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtEndOrderDate        AS DATE      NO-UNDO.
    DEFINE VARIABLE cEndOrderDateOption   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllSalesReps         AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cStartSalesRep        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndSalesRep          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllProdCategory      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cStartProdCategory    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndProdCategory      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lMiscChg              AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lPageRep              AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lSetCom               AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lRepTot               AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lRelOrd               AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lUnder                AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE iUnderValue           AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lOver                 AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE iOverValue            AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cAvailableColumns     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSelectedColumns      AS CHARACTER NO-UNDO.
   
    /* local variables */
    DEF BUFFER b-itemfg FOR itemfg.
    DEF VAR i AS INT NO-UNDO .
    DEF VAR J AS INT NO-UNDO .
    DEF VAR K AS INT NO-UNDO .
    def var ii like i no-undo.
    DEF VAR v-code AS CHAR NO-UNDO.
    def var prt-sqft as log init yes format "SqFt/PartNo" no-undo.
    def var mdate as date no-undo.
    def var v-per-days as int extent 2 no-undo init 0.
    def var v-n-lines  as int no-undo.
    def var v-sman AS CHAR no-undo.
    def var v-exclude as log no-undo.
    def var v-misc as LOG NO-UNDO.
    def var v-amt  like oe-ord.t-revenue NO-UNDO.
    def var v-pct as dec format "99.99" NO-UNDO.
    def var v-sqft like itemfg.t-sqft  format ">,>>9.999" NO-UNDO.
    def var v-tons as DEC NO-UNDO.
    def var v-qty like oe-ordl.qty format "->>>,>>9.99" NO-UNDO.
    def var v-price-per-m as dec column-label "$/MSF" no-undo.
    def var v-price-per-t as dec column-label "$/TON" no-undo.
    def var v-revenue like oe-ordl.t-price format "->,>>>,>>9.99" no-undo
      column-label "Order!Amount".
    def var v-profit as dec format "->>,>>9.9" no-undo
      column-label "% Profit".
    DEF VAR v-margin AS DEC FORMAT "->>,>>9.9" NO-UNDO COLUMN-LABEL "% Margin".
    def var v-sname like sman.sname.
    def var v as INT NO-UNDO.
    def var qm as DEC NO-UNDO.
    DEFINE VARIABLE tb_sortby AS LOGICAL INITIAL NO NO-UNDO .
    
    def var lo_trandate like dtStartOrderDate no-undo.

    find first w-data no-error.
    
     /* locate parameter values record */
    RUN pGetParamValues (ipcCompany, "r-booked.", ipcUserID, ipiBatch).
     /* load parameter values from above record into variables */
    ASSIGN
        cStartCustNo           = DYNAMIC-FUNCTION("fGetParamValue","svStartCustNo") 
        cEndCustNo             = DYNAMIC-FUNCTION("fGetParamValue","svEndCustNo")
        dtStartOrderDate       = DATE(DYNAMIC-FUNCTION("fGetParamValue","svStartOrdDate")) 
        cStartOrderDateOption  = DYNAMIC-FUNCTION("fGetParamValue","svStartOrdDateOption") 
        dtEndOrderDate         = DATE(DYNAMIC-FUNCTION("fGetParamValue","svEndOrdDate"))       
        cEndOrderDateOption    = DYNAMIC-FUNCTION("fGetParamValue","svEndOrdDateOption") 
        cStartSalesRep         = DYNAMIC-FUNCTION("fGetParamValue","svStartRep")
        cEndSalesRep           = DYNAMIC-FUNCTION("fGetParamValue","svEndRep")
        cStartProdCategory     = DYNAMIC-FUNCTION("fGetParamValue","svStartCat")
        cEndProdCategory       = DYNAMIC-FUNCTION("fGetParamValue","svEndCat")
        lMiscChg               = DYNAMIC-FUNCTION("fGetParamValue","svMiscChg") EQ "yes"
        lPageRep               = DYNAMIC-FUNCTION("fGetParamValue","svPageRep") EQ "yes"
        lSetCom                = DYNAMIC-FUNCTION("fGetParamValue","svSetCom") EQ "yes"
        lRepTot                = DYNAMIC-FUNCTION("fGetParamValue","svRepTot") EQ "yes"
        lRelOrd                = DYNAMIC-FUNCTION("fGetParamValue","svRelOrd") EQ "yes"
        lUnder                 = DYNAMIC-FUNCTION("fGetParamValue","svUnder") EQ "yes"
        iUnderValue            = decimal(DYNAMIC-FUNCTION("fGetParamValue","svTxtUnder"))
        lOver                  = DYNAMIC-FUNCTION("fGetParamValue","svOver") EQ "yes"
        iOverValue             = decimal(DYNAMIC-FUNCTION("fGetParamValue","svTxtOver"))
        cAvailableColumns      = DYNAMIC-FUNCTION("fGetParamValue","svAvailableColumns")
        cSelectedColumns       = DYNAMIC-FUNCTION("fGetParamValue","svSelectedColumns")            
        .

    RUN pGetColumns (TEMP-TABLE ttOrdersBooked:HANDLE,
                     cAvailableColumns,
                     cSelectedColumns
                     ).

        /*IF lAllCustomers THEN
            ASSIGN
            cStartCustNo = CHR(32)
            cEndCustNo   = CHR(255)
            .
        IF lAllSalesReps THEN
            ASSIGN
            cStartSalesRep = CHR(32)
            cEndSalesRep   = CHR(255)
            .
        IF lAllProdCategory THEN
            ASSIGN
            cStartProdCategory = CHR(32)
            cEndProdCategory   = CHR(255)
            .

        RUN pBuildCustList (ipcCompany, lCustList, cStartCustNo, cEndCustNo, "OR5").*/
        ASSIGN cocode = ipcCompany .
        
   find first ce-ctrl where ce-ctrl.company = ipcCompany  no-lock.

   find first period 
       where period.company eq ipcCompany
       and period.pst     le dtEndOrderDate
       and period.pend    ge dtEndOrderDate no-lock no-error.

   lo_trandate = if avail period then minimum(dtStartOrderDate,period.pst) else dtStartOrderDate.
        
   for each oe-ord no-lock
       where oe-ord.company  eq ipcCompany
       AND oe-ord.cust-no  GE cStartCustNo
       AND oe-ord.cust-no  LE cEndCustNo
       and oe-ord.ord-date ge lo_trandate
       and oe-ord.ord-date le dtEndOrderDate
       and oe-ord.type     ne "T"
       AND oe-ord.stat     NE "D"
       by oe-ord.company by oe-ord.ord-date by oe-ord.ord-no:

       IF  lRelOrd  THEN DO:
           IF oe-ord.TYPE EQ "T" THEN NEXT.
           v-code = "".

           FOR EACH oe-rel FIELDS(r-no) NO-LOCK 
               WHERE oe-rel.company = oe-ord.company 
               AND oe-rel.ord-no  = oe-ord.ord-no,
               FIRST reftable NO-LOCK 
               WHERE reftable.reftable EQ "oe-rel.s-code" 
               AND reftable.company  EQ STRING(oe-rel.r-no,"9999999999") 
               AND reftable.CODE EQ "T":
               
               v-code = "T".
               LEAVE.
           END. /*FOR EACH oe-rel*/

           IF v-code = "T" THEN NEXT.
       END. /*IF  lRelOrd  THEN DO*/

       for each oe-ordl no-lock
           where oe-ordl.company eq ipcCompany
           and oe-ordl.ord-no  eq oe-ord.ord-no
           AND (oe-ordl.is-a-component EQ NO OR  lSetCom = NO),

           first itemfg no-lock
           where itemfg.company eq ipcCompany
           and itemfg.i-no    eq oe-ordl.i-no
           and itemfg.procat  ge  cStartProdCategory
           and itemfg.procat  le cEndProdCategory
           break by oe-ordl.line:

           v-exclude = yes.
           
           do i = 1 to 3:
               if v-exclude 
                   and oe-ordl.s-man[i] ge cStartSalesRep 
                   and oe-ordl.s-man[i] le cEndSalesRep 
                   then v-exclude = no.
           end. /* do i.. */

           if v-exclude then next.

           v-misc = false.
           do i = 1 to 3:
               if v-misc then leave.
               if oe-ordl.s-man[i] lt cStartSalesRep 
                   or oe-ordl.s-man[i] gt cEndSalesRep then next.

               /* if no salesman number then assign to misc, ie, blank no */
               if i eq 1 
                   and oe-ordl.s-man[1] eq "" 
                   and oe-ordl.s-man[2] eq "" 
                   and oe-ordl.s-man[3] eq "" then v-sman = "MISC".

               else   /* if blank salesman # then ignore */
                   if oe-ordl.s-man[i] eq "" then next.
                   /* There must be at least 1 salesman in either pos'n 1, 2 or 3 */
                   else v-sman = oe-ordl.s-man[i].

               if oe-ord.ord-date ge  dtStartOrderDate 
                   and oe-ord.ord-date le dtEndOrderDate then do:
                   create tt-report.
                   assign
                       tt-report.term-id  = ""
                       tt-report.key-01   = v-sman
                       tt-report.key-02   = IF tb_sortby THEN STRING(oe-ord.ord-no,">>>>>>>>>>") ELSE ""
                       tt-report.key-03   = string(i,"9")
                       tt-report.rec-id   = recid(oe-ordl).           
               end.    /* date in selected period */


               assign
                   v-pct  = oe-ordl.s-pct[i] / 100
                   v-qty  = oe-ordl.qty * v-pct
                   v-sqft = itemfg.t-sqft * v-qty / 1000
                   v-tons = itemfg.weight-100 * v-qty / 100 / 2000
                   v-amt  = oe-ordl.t-price * v-pct.

               find first wkrecap
                   where wkrecap.procat eq if avail itemfg then itemfg.procat 
                       else ? no-error.
               if not avail wkrecap then do:
                   create wkrecap.
                   assign
                       wkrecap.procat     = if avail itemfg then itemfg.procat else ?
                       wkrecap.num-of-ord = wkrecap.num-of-ord + 1.
               end.  /*if not avail wkrecap*/

               else wkrecap.num-of-ord = wkrecap.num-of-ord + 1.

               j = if oe-ord.ord-date ge  dtStartOrderDate 
                   and oe-ord.ord-date le  dtEndOrderDate then 1 else 2.

               k = if AVAIL period AND oe-ord.ord-date ge period.pst  
                   and oe-ord.ord-date le period.pend then 2 else 1.

               if j le k then do ii = j to k:
                   assign
                       wkrecap.t-sqft[ii]  = wkrecap.t-sqft[ii] + v-sqft
                       wkrecap.t-tons[ii]  = wkrecap.t-tons[ii] + v-tons
                       wkrecap.revenue[ii] = wkrecap.revenue[ii] + v-amt.
               end. /*if j le k then*/
           end. /* do i = 1 to 3... */


           if oe-ord.ord-date ne mdate then do:
               mdate = oe-ord.ord-date.

               if oe-ord.ord-date ge  dtStartOrderDate 
                   and oe-ord.ord-date le  dtEndOrderDate then
                   v-per-days[1] = v-per-days[1] + 1.
               if AVAIL period AND oe-ord.ord-date ge period.pst  
                   and oe-ord.ord-date le period.pend then
                   v-per-days[2] = v-per-days[2] + 1.
           end. /*if oe-ord.ord-date ne mdate then do:*/
       end. /*for each oe-ordl no-lock*/ 

        if lMiscChg then 
        for each oe-ordm no-lock
            where oe-ordm.company eq ipcCompany
            and oe-ordm.ord-no  eq oe-ord.ord-no:

            v-exclude = yes.
            do i = 1 to 3:
                if v-exclude and
                    oe-ordm.s-man[i] ge cStartSalesRep and
                    oe-ordm.s-man[i] le cEndSalesRep then v-exclude = no.
            end. /* do i.. */

            if v-exclude then next.
    
            /* At this point we have either 1, 2 or 3 valid salesman, in any  */
            /* combination of the array. */
    
            v-misc = false.
            do i = 1 to 3:
                if v-misc then leave.
    
                if oe-ordm.s-man[i] lt cStartSalesRep or
                    oe-ordm.s-man[i] gt cEndSalesRep then next.
                
                /* if no salesman number then assign to misc, ie, blank no */
                if i eq 1 and
                    oe-ordm.s-man[1] eq "" and
                    oe-ordm.s-man[2] eq "" and
                    oe-ordm.s-man[3] eq "" then v-sman = "MISC".
                else   /* if blank salesman # then ignore */
                    if oe-ordm.s-man[i] eq "" then next.
    
                    /* There must be at least 1 salesman in either pos'n 1, 2 or 3 */
                    else v-sman = oe-ordm.s-man[i].
    
                    if oe-ord.ord-date ge  dtStartOrderDate 
                        and oe-ord.ord-date le  dtEndOrderDate then do:
                        
                        create tt-report.
                        assign
                            tt-report.term-id = ""
                            tt-report.key-01  = v-sman
                            tt-report.key-02  = IF tb_sortby THEN STRING(oe-ord.ord-no,">>>>>>>>>>") ELSE ""
                            tt-report.key-03  = string(i,"9")
                            tt-report.rec-id  = recid(oe-ordm).
                    end.
    
                    assign
                        v-pct = oe-ordm.s-pct[i] / 100
                        v-amt = oe-ordm.amt * v-pct.
    
                    find first wkrecap where wkrecap.procat eq "P/M" no-error.
                    if not avail wkrecap then do:
                        create wkrecap.
                        assign
                            wkrecap.procat     = "P/M"
                            wkrecap.num-of-ord = wkrecap.num-of-ord + 1.
                    end.
    
                    else wkrecap.num-of-ord = wkrecap.num-of-ord + 1.
                     
                    j = if oe-ord.ord-date ge  dtStartOrderDate 
                        and oe-ord.ord-date le  dtEndOrderDate then 1 else 2.
    
                    k = if AVAIL period AND oe-ord.ord-date ge period.pst  
                        and oe-ord.ord-date le period.pend then 2 else 1.
    
                    /* We cannot disturb loop variable i from within loop,so use ii: */
                    if j le k then do ii = j to k:
                        wkrecap.revenue[ii] = wkrecap.revenue[ii] + v-amt.
                    end.
            end.  /*do i = 1 to 3:*/
        end. /*for each oe-ordm no-lock*/   
   end.  /* for each oe-ord */

   for each tt-report where tt-report.term-id eq ""
      break by tt-report.key-01 by tt-report.key-02:

       find first oe-ordm
           where recid(oe-ordm) eq tt-report.rec-id no-lock no-error.
       if avail oe-ordm then do:
          find first oe-ord of oe-ordm no-lock.
          assign
              i     = int(tt-report.key-03)
              v-pct = oe-ordm.s-pct[i] / 100
              v-amt = oe-ordm.amt * v-pct.

          assign
              v-revenue     = v-amt
              v-profit      = (v-revenue - (oe-ordm.cost * v-pct)) / v-revenue * 100
              .
       END.  /*if avail oe-ordm then do:*/
       ELSE DO:
           find first oe-ordl
               where recid(oe-ordl) eq tt-report.rec-id no-lock no-error.
           assign
               i      = int(tt-report.key-03)
               v-pct  = oe-ordl.s-pct[i] / 100
               v-qty  = oe-ordl.qty * v-pct
               v-amt  = oe-ordl.t-price * v-pct .
           qm              = oe-ordl.qty / 1000 .
           
           assign
               v-revenue     = v-amt
               v-profit      = (v-revenue - (oe-ordl.cost * qm)) / v-revenue * 100
               .
       END.  /*ELSE DO:*/
       
       if v-profit      eq ? then v-profit  = 0.
       if prt-sqft then do:       
           /*==== new with selectable columns ====*/
           IF lUnder AND lOver THEN DO:
               IF v-profit >= iUnderValue AND v-profit <= iOverValue THEN 
                   DELETE tt-report .
           END.  /*IF lUnder AND lOver THEN DO:*/
           ELSE IF lUnder AND NOT lOver THEN DO:
               IF v-profit >= iUnderValue THEN DELETE tt-report.
           END. /*ELSE IF lUnder AND NOT lOver THEN DO:*/
           
           ELSE IF lOver AND NOT lUnder THEN DO:
               IF v-profit <= iOverValue THEN DELETE tt-report.
           END.  /*ELSE IF lOver AND NOT lUnder THEN DO:*/
       END.  /* if prt-sqft then do */

      ELSE  DO:
          IF lUnder AND v-profit > iUnderValue THEN DELETE tt-report.
          IF lOver AND v-profit < iOverValue THEN DELETE tt-report.

      END. /* end of else do prt-sqft */
      
   END. /* for each tt-report */

  for each tt-report where tt-report.term-id eq ""
      break by tt-report.key-01 by tt-report.key-02:
      
    {oe/rep/oe-sman2.i}

    find first oe-ordl where recid(oe-ordl) eq tt-report.rec-id no-lock no-error.

    IF FIRST-OF(tt-report.key-01) THEN DO:
        FIND FIRST sman
            WHERE sman.company eq ipcCompany
            AND sman.sman    eq w-data.sman
            NO-LOCK NO-ERROR.

        v-sname = IF AVAIL sman THEN sman.sname
            ELSE "* NOT IN SALES REP FILE *".
    END. /*IF FIRST-OF(tt-report.key-01) THEN DO:*/

    find first oe-ord
        where oe-ord.company eq ipcCompany
          and oe-ord.ord-no  eq w-data.ord-no
        no-lock no-error.
    find cust of oe-ord no-lock no-error.

    assign
     v-revenue     = w-data.revenue
     v-price-per-m = v-revenue / w-data.t-sqft
     v-price-per-t = v-revenue / w-data.t-tons
     v-profit      = (v-revenue - w-data.cost) / v-revenue * 100
     v-margin      = w-data.margin.

    if v-price-per-m eq ? then v-price-per-m = 0.
    if v-price-per-t eq ? then v-price-per-t = 0.
    if v-profit      eq ? then v-profit      = 0.
    IF v-margin      EQ ? THEN v-margin      = 0.
    
    accumulate
     w-data.t-sqft (total by tt-report.key-01)
     w-data.t-tons (total by tt-report.key-01)
     v-revenue     (total by tt-report.key-01)
     w-data.cost   (total by tt-report.key-01).
        
     /*==== new with selectable columns ====*/
     IF lUnder AND lOver THEN DO:
         IF v-profit >= iUnderValue AND v-profit <= iOverValue THEN NEXT.
     END.  /*IF lUnder AND lOver THEN DO:*/
     ELSE IF lUnder AND NOT lOver THEN DO:
         IF v-profit >= iUnderValue THEN NEXT.
     END. /*ELSE IF lUnder AND NOT lOver THEN DO:*/
     ELSE IF lOver AND NOT lUnder THEN DO:
         IF v-profit <= iOverValue THEN NEXT.
     END. /*ELSE IF lOver AND NOT lUnder THEN DO:*/

     PUT UNFORMATTED "ttOrdersBooked" SKIP.
     
     create ttOrdersBooked.

     assign 
         /*ttOrdersBooked.rowtype    =   oe-ord.due-date*/              
         ttOrdersBooked.dueDate      =   oe-ord.due-date                    
         ttOrdersBooked.orderNo      =   w-data.ord-no                      
         ttOrdersBooked.custName     =   cust.name                   
         ttOrdersBooked.custNo       =   cust.cust-no
         ttOrdersBooked.salesRep     =   IF AVAIL sman THEN sman.sman ELSE ""
         ttOrdersBooked.salesRepName =   IF AVAIL sman THEN sman.sname ELSE "" 
         ttOrdersBooked.commPer      =   w-data.comm                      
         ttOrdersBooked.prodCode     =   w-data.procat 
         ttOrdersBooked.fgItemNo     =   oe-ordl.i-no                       
         ttOrdersBooked.fgItemName   =   w-data.item-n                      
         ttOrdersBooked.qtyOrdEa     =   w-data.qty                         
         ttOrdersBooked.sqFit        =   w-data.sqft                     
         ttOrdersBooked.totalSqfit   =   w-data.t-sqft                          
         ttOrdersBooked.msfPrice     =   v-price-per-m                   
         ttOrdersBooked.price        =   w-data.price                     
         ttOrdersBooked.orderAmount  =   v-revenue                       
         ttOrdersBooked.profitPer    =   v-profit                       
         ttOrdersBooked.totalTons    =   w-data.t-tons                          
         ttOrdersBooked.ton          =   v-price-per-t  
         ttOrdersBooked.vUserID      =   oe-ord.user-id                  
         ttOrdersBooked.custPartNo   =   oe-ordl.part-no
             . 

        delete w-data.
        delete tt-report.
    
     end.
  end. /* for each tt-report */

&ANALYZE-RESUME

&ENDIF


/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-fOrdersBooked) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fOrdersBooked Procedure 
FUNCTION fOrdersBooked RETURNS HANDLE
  ( ipcCompany AS CHARACTER,
    ipiBatch   AS INTEGER,
    ipcUserID  AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  Order Booked.rpa
    Notes:  
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttOrdersBooked.

    RUN pOrdersBooked (ipcCompany, ipiBatch, ipcUserID).
    
    RETURN TEMP-TABLE ttOrdersBooked:HANDLE .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fGetTableHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetTableHandle Procedure 

FUNCTION fGetTableHandle RETURNS HANDLE (ipcProgramID AS CHARACTER ):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    CASE ipcProgramID:
         /* Order Booked.rpa */
        WHEN "r-booked." THEN do:
            RETURN TEMP-TABLE ttOrdersBooked:HANDLE.
        END.
    END CASE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


