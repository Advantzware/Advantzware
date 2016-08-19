&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : aoaAppSrv/aoaFG.p
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

DEFINE NEW SHARED VARIABLE lShowCost   AS LOG       NO-UNDO.

DEFINE NEW SHARED VARIABLE v-prt-c     AS LOG       FORMAT "Y/N" INIT YES NO-UNDO.
DEFINE NEW SHARED VARIABLE v-prt-p     AS LOG       FORMAT "Y/N" INIT YES NO-UNDO.
DEFINE NEW SHARED VARIABLE v-prt-cpn   AS LOG       FORMAT "Y/N" INIT NO NO-UNDO.
DEFINE NEW SHARED VARIABLE v-prt-po    AS LOG       FORMAT "Y/N" INIT NO NO-UNDO.
DEFINE NEW SHARED VARIABLE v-fg-lot    AS LOG       FORMAT "Y/N" INIT NO NO-UNDO.
DEFINE NEW SHARED VARIABLE v-prt-arqty AS LOG       FORMAT "Y/N" INIT NO NO-UNDO.
DEFINE NEW SHARED VARIABLE v-po-type   AS CHARACTER FORMAT "!" INIT "L" NO-UNDO.
DEFINE NEW SHARED VARIABLE v-prt-msf   AS LOG       FORMAT "Y/N" INIT NO NO-UNDO.
DEFINE NEW SHARED VARIABLE v-fgprice   AS LOG       NO-UNDO.
DEFINE NEW SHARED VARIABLE v-rct-date  AS LOG       FORMAT "Y/N" INIT NO NO-UNDO.
DEFINE NEW SHARED VARIABLE v-file      AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE v-tot-qty         AS DECIMAL   FORMAT "->>>,>>>,>>9" EXTENT 3 NO-UNDO.
DEFINE NEW SHARED VARIABLE v-tot-msf         AS DECIMAL   FORMAT "->>>,>>>,>>9" EXTENT 3 NO-UNDO.
DEFINE NEW SHARED VARIABLE v-tot-cst         AS DECIMAL   FORMAT "->>>>>,>>9.99<<" EXTENT 3 NO-UNDO.
DEFINE NEW SHARED VARIABLE v-tot-ext         AS DECIMAL   FORMAT "->>>>>,>>9.99" EXTENT 3 NO-UNDO.
DEFINE NEW SHARED VARIABLE v-tot-gsl         AS DECIMAL   FORMAT "->>>>>,>>9.99" EXTENT 3 NO-UNDO.
DEFINE NEW SHARED VARIABLE v-tot-gsm         AS DECIMAL   FORMAT "->>>>>,>>9.99" EXTENT 3 NO-UNDO.
DEFINE NEW SHARED VARIABLE v-po-no           LIKE oe-ordl.po-no NO-UNDO.
DEFINE NEW SHARED VARIABLE v-qoh-f           AS CHARACTER NO-UNDO.

DEFINE            VARIABLE v-cost            AS DECIMAL   FORMAT "->>>,>>9.99<<" NO-UNDO.
DEFINE            VARIABLE v-cost1           AS DECIMAL   FORMAT "->>>>9.9<<<<" NO-UNDO.
DEFINE            VARIABLE v-costl           LIKE fg-bin.std-lab-Cost NO-UNDO.
DEFINE            VARIABLE v-costm           LIKE fg-bin.std-mat-Cost NO-UNDO.
DEFINE            VARIABLE v-ext             AS DECIMAL   FORMAT "->>>,>>9.99" NO-UNDO.
DEFINE            VARIABLE ftyp              LIKE itemfg.mat-type INIT " " NO-UNDO.
DEFINE            VARIABLE ttyp              LIKE itemfg.mat-type INIT ">" NO-UNDO.
DEFINE            VARIABLE jobNo             AS CHARACTER FORMAT "x(9)" NO-UNDO.
DEFINE            VARIABLE v-first           AS LOG       INIT NO EXTENT 2 NO-UNDO.
DEFINE            VARIABLE v-prnt            AS LOG       INIT NO EXTENT 2 NO-UNDO.
DEFINE            VARIABLE v-qoh             LIKE fg-bin.qty NO-UNDO.
DEFINE            VARIABLE v-arq             LIKE fg-bin.qty FORMAT "->,>>>,>>9" NO-UNDO.
DEFINE            VARIABLE v-bin-arq         LIKE fg-bin.qty FORMAT "->,>>>,>>9" NO-UNDO.
DEFINE            VARIABLE v-qoh-s           AS CHARACTER NO-UNDO.
DEFINE            VARIABLE v-tot-sum         AS DECIMAL   FORMAT "->>>,>>9.99<<" NO-UNDO.
DEFINE            VARIABLE v-ext-sum         AS DECIMAL   FORMAT "->>>,>>9.99<<" NO-UNDO.
DEFINE            VARIABLE v-gsl-sum         AS DECIMAL   FORMAT "->>>,>>9.99<<" NO-UNDO.
DEFINE            VARIABLE v-gsm-sum         AS DECIMAL   FORMAT "->>>,>>9.99<<" NO-UNDO.
DEFINE            VARIABLE v-all-sum         AS DECIMAL   FORMAT "->>>,>>9.99<<" NO-UNDO.
DEFINE            VARIABLE v-label1          AS CHARACTER FORMAT "x(8)" EXTENT 4 NO-UNDO.
DEFINE            VARIABLE v-label2          AS CHARACTER FORMAT "x(11)" EXTENT 6 NO-UNDO.
DEFINE            VARIABLE v-label3          AS CHARACTER FORMAT "x(10)" EXTENT 3 NO-UNDO.
DEFINE            VARIABLE v-label4          AS CHARACTER FORMAT "x(15)" EXTENT 3 NO-UNDO.
DEFINE            VARIABLE v-procat          LIKE itemfg.procat NO-UNDO.
DEFINE            VARIABLE v-password        AS CHARACTER FORMAT "x(16)" NO-UNDO.
DEFINE            VARIABLE v-bin             AS LOG       NO-UNDO.
DEFINE            VARIABLE lv-sellPrice      LIKE itemfg.sell-Price NO-UNDO.
DEFINE            VARIABLE lv-sellPrice-fg   LIKE itemfg.sell-Price NO-UNDO.
DEFINE            VARIABLE lv-sellPrice-ord  LIKE oe-ordl.price NO-UNDO.
DEFINE            VARIABLE lv-sellValueFg    AS DECIMAL   FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE            VARIABLE lv-sellValueOrd   AS DECIMAL   FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE            VARIABLE lv-sellValueFg-s  AS DECIMAL   FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE            VARIABLE lv-sellValueOrd-s AS DECIMAL   FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE            VARIABLE lv-sell-uom       LIKE itemfg.sell-uom NO-UNDO.
DEFINE            VARIABLE lv-sell-uom-fg    LIKE itemfg.sell-uom NO-UNDO.
DEFINE            VARIABLE lv-sell-uom-ord   LIKE oe-ordl.pr-uom NO-UNDO.
DEFINE            VARIABLE lv-case-count     LIKE itemfg.case-count NO-UNDO.
DEFINE            VARIABLE lv-rct-date       AS DATE      FORMAT "99/99/99" NO-UNDO.

DEFINE            VARIABLE v-binqty          AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE v-qty             AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE v-found-job       AS LOG       NO-UNDO.

DEFINE            VARIABLE v-tot-bin-sum     AS DECIMAL   FORMAT "->>>>9.9<<<<" NO-UNDO.
DEFINE            VARIABLE v-ext-bin-sum     AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE v-bin-qoh         AS INTEGER   NO-UNDO.
DEFINE            VARIABLE fgLotVal          AS CHARACTER NO-UNDO.
DEFINE            VARIABLE iCnt              AS INTEGER   NO-UNDO.

{fg/rep/tt-fgbin.i NEW SHARED}

DEFINE TEMP-TABLE tt-rdtlh NO-UNDO LIKE fg-rdtlh
    
    INDEX tt-rdtlh job-No job-No2 loc loc-Bin trans-date r-no rec_key.

&SCOPED-DEFINE itemfg-index i-No job-No job-No2 loc loc-Bin tag binCustNo
DEFINE NEW SHARED TEMP-TABLE tt-itemfg NO-UNDO
    FIELD row-id      AS ROWID
    FIELD i-No        LIKE itemfg.i-No
    FIELD cust-No     LIKE itemfg.cust-No
    FIELD part-No     LIKE itemfg.part-No
    FIELD part-cust   AS CHARACTER
    FIELD procat      LIKE itemfg.procat
    FIELD job-No      LIKE fg-rcpth.job-no
    FIELD job-No2     LIKE fg-rcpth.job-no2
    FIELD loc         LIKE fg-rdtlh.loc
    FIELD loc-Bin     LIKE fg-rdtlh.loc-bin
    FIELD tag         LIKE fg-rdtlh.tag
    FIELD binCustNo   LIKE fg-rdtlh.cust-no
    FIELD loc-Bin-Tag AS CHARACTER
    INDEX i-No        {&itemfg-index}
    INDEX cust-No     cust-No         {&itemfg-index}
    INDEX partNo      part-cust       {&itemfg-index}
    INDEX procat      procat          {&itemfg-index}
    INDEX loc-Bin-Tag loc-Bin-Tag     {&itemfg-index}.


DEFINE VARIABLE ll-secure          AS LOG       NO-UNDO.
DEFINE VARIABLE is-xprint-form     AS LOG       NO-UNDO.
DEFINE VARIABLE ls-fax-file        AS cha       NO-UNDO.
DEFINE VARIABLE excel-header-var-1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE excel-header-var-2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE excel-header-var-3 AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-msf-oh           AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-ordPrice         AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-po-ord           AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-last-inv         AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-tot-mat          AS DECIMAL   FORMAT "->>,>>>,>>9.99" EXTENT 3 NO-UNDO.
DEFINE VARIABLE v-tot-lab          AS DECIMAL   FORMAT "->>,>>>,>>9.99" EXTENT 3 NO-UNDO.
DEFINE VARIABLE v-bin-msf          AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-po-rel           AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-sales-rep        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCustName          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cItemtype          AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldummy             AS LOG       NO-UNDO.
DEFINE VARIABLE cTextListToSelect  AS cha       NO-UNDO.
DEFINE VARIABLE cFieldListToSelect AS cha       NO-UNDO.
DEFINE VARIABLE cFieldLength       AS cha       NO-UNDO.
DEFINE VARIABLE cFieldType         AS cha       NO-UNDO.
DEFINE VARIABLE iColumnLength      AS INTEGER   NO-UNDO.
DEFINE BUFFER b-itemfg FOR itemfg .
DEFINE VARIABLE cTextListToDefault AS cha NO-UNDO.
                                
ASSIGN 
    cTextListToSelect  = "CUSTOMER,CUST NAME,ITEM #,TAG #,FULL TAG #,FG LOT#,CUST PART#,DESCRIPTION,JOB#,REC DATE," +
                           "WHSE,BIN,MSF OH,C-UOM,REL QTY,QTY ON HAND,LAST SALE,FG CAT," +
                           "VIEW PO,LINE PO#,REL PO#,FG PRICE,ORDER PRICE,UOM COST,TOTAL COST,MAT COST,LABOR COST,REP," +
                           "SELL VAL(FG),SELL VAL(ORD),DAYS OLD,CUST OWN,SET HEADER,QTY PER SET"
    cFieldListToSelect = "itemfg.cust-no,custName,itemfg.i-no,tag-No,tag,fgLotVal,itemfg.part-no,itemfg.i-name,jobNo,recDate," +
                                "loc,bin,msfOnHand,costUom,relQty,qtyOnHand,lastSale,itemfg.procat," +
                                "viewPo,linePo,relPo,sellPrice,ordPr,uomCost,totCost,matCost,labCost,salesRep," + 
                                "sellValueFg,sellValueOrd,daysOld,custno,setHeader,qtyPerSet"
    cFieldLength       = "8,30,15,6,24,20,15,30,10,8," + "5,8,8,5,11,11,9,7," + "11,11,11,11,11,11,11,11,11,3," + "14,14,8,8,15,11"
    cFieldType         = "c,c,c,c,c,c,c,c,c,c,c," + "c,c,i,c,i,i,c,c," + "c,c,c,i,i,i,i,i,i,c," + "i,i,i,c,c,i"
    .

	
/* Inventory Value.rpa */
DEFINE TEMP-TABLE ttInventoryValue NO-UNDO
    {aoaAppSrv/ttFields.i}
    FIELD custNo       LIKE itemfg.cust-no LABEL "CUSTOMER"
    FIELD custName     AS CHARACTER LABEL "CUST NAME" FORMAT "x(30)"
    FIELD salesRep     AS CHARACTER LABEL "REP" FORMAT "X(3)"
    FIELD iNo          LIKE itemfg.i-no LABEL "ITEM #" FORMAT "x(15)"
    FIELD iName        LIKE itemfg.i-name LABEL "DESCRIPTION" FORMAT "X(30)"
    FIELD tagNo        AS CHARACTER LABEL "TAG #" FORMAT "x(6)"
    FIELD tag          AS CHARACTER LABEL "FULL TAG #" FORMAT "x(24)"    
    FIELD fgLotVal     AS CHARACTER LABEL "FG LOT#" FORMAT "x(20)"
    FIELD partNo       LIKE itemfg.part-no LABEL "CUST PART#" FORMAT "X(15)"    
    FIELD procat       AS CHARACTER LABEL "FG CAT" FORMAT "X(5)"         
    FIELD loc          AS CHARACTER LABEL "WHSE" FORMAT "X(5)"
    FIELD bin          AS CHARACTER LABEL "BIN" FORMAT "X(8)" 
    FIELD jobNo        AS CHARACTER LABEL "JOB #" FORMAT "x(10)"
    FIELD msfOnHand    AS DECIMAL   LABEL "MSF OH" FORMAT "->>9.999" DECIMALS 3
    FIELD qtyOnHand    AS INTEGER   LABEL "QTY ON HAND" FORMAT "->>,>>>,>>9" 
    FIELD relQty       AS INTEGER   LABEL "REL QTY" FORMAT "->>,>>>,>>9"     
    FIELD sellPrice    AS DECIMAL   LABEL "FG PRICE" FORMAT "->>>,>>9.99" 
    FIELD ordPr        AS DECIMAL   LABEL "ORDER PRICE" FORMAT "->>>,>>9.99"     
    FIELD uomCost      AS DECIMAL   LABEL "UOM COST" FORMAT "->>>>>9.999" DECIMALS 3
    FIELD totCost      AS DECIMAL   LABEL "TOTAL COST" FORMAT "->>>,>>9.99" 
    FIELD matCost      AS DECIMAL   LABEL "MAT COST" FORMAT "->>>,>>9.99" 
    FIELD labCost      AS DECIMAL   LABEL "LABOR COST" FORMAT "->>>,>>9.99" 
    FIELD costUom      AS CHARACTER LABEL "C-UOM" FORMAT "x(5)" 
    FIELD sellValueFg  AS DECIMAL   LABEL "SELL VAL(FG)" FORMAT "->>,>>>,>>9.99" 
    FIELD sellValueOrd AS DECIMAL   LABEL "SELL VAL(ORD)" FORMAT "->>,>>>,>>9.99"  
    FIELD lastSale     AS CHARACTER LABEL "LAST SALE" FORMAT "x(10)"     
    FIELD viewPo       AS CHARACTER LABEL "VIEW PO" FORMAT "x(11)" 
    FIELD linePo       AS CHARACTER LABEL "Line PO" FORMAT "x(10)" 
    FIELD relPo        AS CHARACTER LABEL "Rel PO" FORMAT "x(11)"      
    FIELD daysOld      AS INTEGER   LABEL "DAYS OLD" FORMAT "->>>>>>9" 
    FIELD custNoOwned  AS CHARACTER LABEL "CUST OWN" FORMAT "X(8)" 
    FIELD setHeader    AS CHARACTER LABEL "SET HEADER" FORMAT "X(15)" 
    FIELD qtyPerSet    AS INTEGER   LABEL "QTY PER SET" FORMAT "->>,>>>,>>9" 
    FIELD recDate      AS CHARACTER LABEL "REC DATE" FORMAT "x(8)"	
    FIELD xxSort AS CHARACTER LABEL "Sort" FORMAT "x(500)"
        INDEX sortBy IS PRIMARY rowType xxSort
        .
/* Inventory Value.rpa */

{sys/ref/CustList.i NEW}
{sys/inc/ttRptSel.i}
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

&IF DEFINED(EXCLUDE-fInventoryValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fInventoryValue Procedure 
FUNCTION fInventoryValue RETURNS HANDLE
  ( ipcCompany AS CHARACTER,
    ipiBatch   AS INTEGER,
    ipcUserID  AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GEtFieldValue C-Win 
FUNCTION GEtFieldValue RETURNS CHARACTER
    ( hipField AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pBuildCustList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBuildCustList Procedure 
PROCEDURE pBuildCustList :
/*------------------------------------------------------------------------------
  Purpose:     Customer Inventory.rpa
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

&IF DEFINED(EXCLUDE-pInventoryValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInventoryValue Procedure 
PROCEDURE pInventoryValue :
/*------------------------------------------------------------------------------
  Purpose:     Inventory Value.rpa
  Parameters:  Company, Batch Seq, User ID
  Notes:       
------------------------------------------------------------------------------*/
    {aoaAppSrv/pInventoryValue.i}

    /* local variables */

	
    /* subject business logic */	
    {aoaAppSrv/pInventoryValueLogic.i}
	
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-fCustomerInventory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fCustomerInventory Procedure 
FUNCTION fCustomerInventory RETURNS HANDLE
  ( ipcCompany AS CHARACTER,
    ipiBatch   AS INTEGER,
    ipcUserID  AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  Customer Inventory.rpa
    Notes:  
------------------------------------------------------------------------------*/

    EMPTY TEMP-TABLE ttInventoryValue.
    RUN pCustomerInventory (ipcCompany, ipiBatch, ipcUserID).

    RETURN TEMP-TABLE ttInventoryValue:HANDLE .

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
        /* Customer Inventory.rpa */
        WHEN "r-cusinv." THEN
        RETURN TEMP-TABLE ttInventoryValue:HANDLE.
        /* Inventory Value.rpa */
        WHEN "r-fgohbb." THEN
        RETURN TEMP-TABLE ttInventoryValue:HANDLE.
    END CASE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fInventoryValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fInventoryValue Procedure 
FUNCTION fInventoryValue RETURNS HANDLE
  ( ipcCompany AS CHARACTER,
    ipiBatch   AS INTEGER,
    ipcUserID  AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  Inventory Value.rpa
    Notes:  
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttInventoryValue.

    RUN pInventoryValue (ipcCompany, ipiBatch, ipcUserID).

    RETURN TEMP-TABLE ttInventoryValue:HANDLE .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GEtFieldValue C-Win 
FUNCTION GEtFieldValue RETURNS CHARACTER
    ( hipField AS HANDLE ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    /*RETURN string(hField:BUFFER-VALUE, hField:FORMAT) */
    RETURN STRING(hipField:BUFFER-VALUE).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME