
/*------------------------------------------------------------------------
    File        : AutoPOProcs.p
    Purpose     : Replaces doPo.p

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Thu Nov 12 15:19:43 EST 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{system/VendorCostProcs.i}

DEFINE TEMP-TABLE ttAutoPoMaster NO-UNDO 
    FIELD useNetSheets                    AS LOGICAL
    FIELD doAutoPOForRM                   AS LOGICAL 
    FIELD doAutoPOForRMRule               AS CHARACTER 
    FIELD doAutoPOForFG                   AS LOGICAL 
    FIELD doAutoPOForFGRule               AS CHARACTER 
    FIELD doAutoPOForPrep                 AS LOGICAL 
    FIELD doAutoPOForPrepRule             AS CHARACTER 
    FIELD userCanDoAutoPOForRM            AS LOGICAL 
    FIELD userCanDoAutoPOForFG            AS LOGICAL 
    FIELD userCanDoAutoPOForPrep          AS LOGICAL
    FIELD apGLFromCostType                AS LOGICAL
    FIELD apGLSource                      AS CHARACTER 
    FIELD apGLAccount                     AS CHARACTER  
    FIELD poCostSource                    AS CHARACTER
    FIELD poCostHoldOverprices            AS LOGICAL
    FIELD defaultPOQtyUOMSource           AS CHARACTER
    FIELD defaultPOConsQtyUOMToTonIfPaper AS LOGICAL  
    .
    
DEFINE TEMP-TABLE ttItemToPurchase NO-UNDO 
    FIELD itemID             AS CHARACTER
    FIELD itemType           AS CHARACTER 
    FIELD validToPurchase    AS LOGICAL 
    FIELD quantityToPurchase AS DECIMAL 
    FIELD quantityUOM        AS CHARACTER 
    .

/*                                                                                                                                                                                            */
/*DEFINE INPUT PARAMETER ipriOrderOrJob AS ROWID NO-UNDO.                                                                                                                                     */
/*DEFINE INPUT  PARAMETER iplPromptRM AS LOGICAL     NO-UNDO.                                                                                                                                 */
/*                                                                                                                                                                                            */
/*{sys/inc/var.i shared}                                                                                                                                                                      */
/*{system/VendorCostProcs.i}                                                                                                                                                                  */
/*{po/ttItemPurchased.i}                                                                                                                                                                      */
/*{fg/fullset.i NEW}                                                                                                                                                                          */
/*{ce/msfcalc.i}                                                                                                                                                                              */
/*{sys/inc/ap-gl#.i}                                                                                                                                                                          */
/*                                                                                                                                                                                            */
/*DO TRANSACTION:                                                                                                                                                                             */
/*    {sys/inc/oeautofg.i}                                                                                                                                                                    */
/*    {sys/inc/pouom.i}                                                                                                                                                                       */
/*    {sys/inc/aptax.i}                                                                                                                                                                       */
/*END.                                                                                                                                                                                        */
/*                                                                                                                                                                                            */
/*                                                                                                                                                                                            */
/*DEFINE VARIABLE gvlDebug AS LOG INIT NO NO-UNDO.                                                                                                                                            */
/*DEFINE STREAM sDebug.                                                                                                                                                                       */
/*                                                                                                                                                                                            */
/*DEFINE BUFFER tmp-po-ord FOR po-ord.                                                                                                                                                        */
/*DEFINE BUFFER xjob-mat   FOR job-mat.                                                                                                                                                       */
/*DEFINE BUFFER bpo-ordl   FOR po-ordl.                                                                                                                                                       */
/*DEFINE BUFFER b-item     FOR ITEM.                                                                                                                                                          */
/*DEFINE BUFFER b-oe-ordl  FOR oe-ordl.                                                                                                                                                       */
/*DEFINE BUFFER bf-ordl    FOR oe-ordl.                                                                                                                                                       */
/*DEFINE BUFFER bf-ord     FOR oe-ord.                                                                                                                                                        */
/*DEFINE BUFFER b-jc-calc  FOR reftable.                                                                                                                                                      */
/*DEFINE BUFFER b-ref1     FOR reftable.                                                                                                                                                      */
/*DEFINE BUFFER b-ref2     FOR reftable.                                                                                                                                                      */
/*DEFINE BUFFER b-job-mat  FOR job-mat.                                                                                                                                                       */
/*DEFINE BUFFER b-job-hdr  FOR job-hdr.                                                                                                                                                       */
/*DEFINE BUFFER xest       FOR est.                                                                                                                                                           */
/*DEFINE BUFFER xeb        FOR eb.                                                                                                                                                            */
/*DEFINE BUFFER b-po-ordl FOR po-ordl.                                                                                                                                                        */
/*DEFINE BUFFER bf-itemfg FOR itemfg.                                                                                                                                                         */
/*                                                                                                                                                                                            */
/*DEFINE            VARIABLE v-ord-no               LIKE oe-ordl.ord-no NO-UNDO.                                                                                                              */
/*DEFINE            VARIABLE po-found               AS LOGICAL   NO-UNDO.                                                                                                                     */
/*DEFINE            VARIABLE ll-drop           AS LOGICAL   NO-UNDO.                                                                                                                          */
/*                                                                                                                                                                                            */
/*DEFINE            VARIABLE cFilIdSource           AS CHARACTER NO-UNDO.                                                                                                                     */
/*DEFINE            VARIABLE lv-recid               AS RECID     NO-UNDO.                                                                                                                     */
/*DEFINE            VARIABLE nufile                 AS LOGICAL   NO-UNDO.                                                                                                                     */
/*DEFINE            VARIABLE sel                    AS INTEGER   NO-UNDO.                                                                                                                     */
/*DEFINE            VARIABLE call_id                AS RECID     NO-UNDO.                                                                                                                     */
/*DEFINE            VARIABLE gvlChoice              AS LOGICAL   NO-UNDO.                                                                                                                     */
/*DEFINE            VARIABLE v-tot-cost             AS DECIMAL   DECIMALS 4 NO-UNDO.                                                                                                          */
/*DEFINE            VARIABLE v-access-close         AS LOGICAL   NO-UNDO.                                                                                                                     */
/*DEFINE            VARIABLE v-access-list          AS CHARACTER NO-UNDO.                                                                                                                     */
/*DEFINE            VARIABLE v-autopo-sec           AS LOGICAL   NO-UNDO.                                                                                                                     */
/*DEFINE            VARIABLE v-autofg-sec           AS LOGICAL   NO-UNDO.                                                                                                                     */
/*DEFINE            VARIABLE v-autoprep-sec         AS LOGICAL   NO-UNDO.                                                                                                                     */
/*DEFINE            VARIABLE nk1-oeautopo-int       AS INTEGER   NO-UNDO.                                                                                                                     */
/*DEFINE            VARIABLE nk1-oeautopo-log  LIKE sys-ctrl.log-fld INIT NO NO-UNDO.                                                                                                         */
/*DEFINE            VARIABLE nk1-oeautopo-char AS CHARACTER FORMAT "x(8)" NO-UNDO.                                                                                                            */
/*DEFINE            VARIABLE v-job             AS CHARACTER NO-UNDO.                                                                                                                          */
/*DEFINE VARIABLE cRtnChar  AS CHARACTER NO-UNDO.                                                                                                                                             */
/*DEFINE VARIABLE lRecFound AS LOGICAL   NO-UNDO.                                                                                                                                             */
/*DEFINE VARIABLE dOeAutoFg AS DECIMAL   NO-UNDO.                                                                                                                                             */
/*                                                                                                                                                                                            */
/*DEFINE            VARIABLE gvcVendNo              LIKE vend.vend-no INIT "" NO-UNDO.                                                                                                        */
/*DEFINE VARIABLE gvrPoOrdl       AS ROWID     NO-UNDO.                                                                                                                                       */
/*DEFINE VARIABLE gvrItem         AS ROWID     NO-UNDO.                                                                                                                                       */
/*DEFINE VARIABLE gvrVend         AS ROWID     NO-UNDO.                                                                                                                                       */
/*                                                                                                                                                                                            */
/*DEFINE VARIABLE gvrOeOrdl       AS ROWID     NO-UNDO.                                                                                                                                       */
/*DEFINE VARIABLE gvrOeOrd        AS ROWID     NO-UNDO.                                                                                                                                       */
/*DEFINE VARIABLE gvrJob          AS ROWID     NO-UNDO.                                                                                                                                       */
/*DEFINE VARIABLE gvrPoOrd        AS ROWID     NO-UNDO.                                                                                                                                       */
/*DEFINE VARIABLE gvrTT-eiv       AS ROWID     NO-UNDO.                                                                                                                                       */
/*DEFINE VARIABLE gvrTT-ei        AS ROWID     NO-UNDO.                                                                                                                                       */
/*DEFINE VARIABLE gvrItemfg       AS ROWID     NO-UNDO.                                                                                                                                       */
/*DEFINE VARIABLE gvcFilIdSource  AS CHARACTER NO-UNDO.                                                                                                                                       */
/*DEFINE VARIABLE gvrWJobMat      AS ROWID     NO-UNDO.                                                                                                                                       */
/*DEFINE VARIABLE lNextOuters     AS LOGICAL   NO-UNDO.                                                                                                                                       */
/*DEFINE            VARIABLE v-new-avail       AS LOGICAL   INIT NO NO-UNDO.                                                                                                                  */
/*DEFINE VARIABLE llFirstJobFrm   AS LOGICAL   NO-UNDO.                                                                                                                                       */
/*DEFINE VARIABLE llFirstOfJobFrm AS LOGICAL   NO-UNDO.                                                                                                                                       */
/*DEFINE VARIABLE lPoExists       AS LOGICAL   NO-UNDO.                                                                                                                                       */
/*DEFINE            VARIABLE v-exp-limit       AS INTEGER   NO-UNDO INIT 10.                                                                                                                  */
/*DEFINE            VARIABLE gvdPoDate         AS DATE      NO-UNDO.                                                                                                                          */
/*DEFINE            VARIABLE gvdDueDate        AS DATE      NO-UNDO.                                                                                                                          */
/*DEFINE            VARIABLE gvcDropCustNo     AS cha       NO-UNDO.                                                                                                                          */
/*DEFINE            VARIABLE gvcShipChoice     AS cha       NO-UNDO.                                                                                                                          */
/*DEFINE TEMP-TABLE tt-itemfg NO-UNDO                                                                                                                                                         */
/*    FIELD isaset       LIKE itemfg.isaset                                                                                                                                                   */
/*    FIELD isacomponent AS LOG                                                                                                                                                               */
/*    FIELD pur-man      LIKE itemfg.pur-man                                                                                                                                                  */
/*    FIELD form-no      LIKE eb.form-no                                                                                                                                                      */
/*    FIELD blank-no     LIKE eb.blank-no                                                                                                                                                     */
/*    FIELD qty          LIKE oe-ordl.qty                                                                                                                                                     */
/*    FIELD pur-uom      LIKE itemfg.pur-uom                                                                                                                                                  */
/*    FIELD row-id       AS ROWID.                                                                                                                                                            */
/*                                                                                                                                                                                            */
/*DEFINE BUFFER b-tt-itemfg FOR tt-itemfg.                                                                                                                                                    */
/*DEFINE VARIABLE v-from-po-entry AS LOGICAL   NO-UNDO.                                                                                                                                       */
/*DEFINE VARIABLE v-po-best       AS LOGICAL   NO-UNDO.                                                                                                                                       */
/*      /* dopo.p called from  b-po-inq.w (procedure add-po-best) then v-po-best = yes else no */                                                                                             */
/*                                                                                                                                                                                            */
/*DEFINE            VARIABLE oeautoprep-log    LIKE sys-ctrl.log-fld NO-UNDO.                                                                                                                 */
/*                                                                                                                                                                                            */
/* ********************  Preprocessor Definitions  ******************** */
/*                                                                                                                                                                                            */
/*                                                                                                                                                                                            */
/* ***************************  Main Block  *************************** */


/*/* Check if authorized to create PO's */                                                                                                                                                    */
/*IF oeautofg-log THEN                                                                                                                                                                        */
/*    RUN methods/prgsecur.p                                                                                                                                                                  */
/*        (INPUT "OEAutoFG", /* Program master program name */                                                                                                                                */
/*        INPUT "ALL",      /* Security based on */                                                                                                                                           */
/*        INPUT NO,                                                                                                                                                                           */
/*        INPUT NO,                                                                                                                                                                           */
/*        INPUT NO,                                                                                                                                                                           */
/*        OUTPUT v-autofg-sec,                                                                                                                                                                */
/*        OUTPUT v-access-close,                                                                                                                                                              */
/*        OUTPUT v-access-list).                                                                                                                                                              */
/*                                                                                                                                                                                            */
/*/* only check security from order entry */                                                                                                                                                  */
/*IF v-from-po-entry THEN v-autofg-sec = TRUE.                                                                                                                                                */
/*                                                                                                                                                                                            */
/*/* Code to fetch sys-ctrl configuration "OEAUTOFG" decimal field */                                                                                                                         */
/*RUN sys/ref/nk1look.p (                                                                                                                                                                     */
/*    INPUT  cocode,     /* Company       */                                                                                                                                                  */
/*    INPUT  "OEAUTOFG", /* Sys-Ctrl Name */                                                                                                                                                  */
/*    INPUT  "D",        /* Logical       */                                                                                                                                                  */
/*    INPUT  NO,         /* Check by cust */                                                                                                                                                  */
/*    INPUT  YES,        /* Use Cust      */                                                                                                                                                  */
/*    INPUT  "",         /* Customer      */                                                                                                                                                  */
/*    INPUT  "",         /* Ship-to       */                                                                                                                                                  */
/*    OUTPUT cRtnChar,                                                                                                                                                                        */
/*    OUTPUT lRecFound                                                                                                                                                                        */
/*    ).                                                                                                                                                                                      */
/*IF lRecFound THEN                                                                                                                                                                           */
/*    dOeAutoFG = DECIMAL(cRtnChar).                                                                                                                                                          */
/*                                                                                                                                                                                            */
/*/*IF gvlDebug THEN                                        */                                                                                                                                */
/*/*    OUTPUT STREAM sDebug TO c:\tmp\doPoDebug.txt.       */                                                                                                                                */
/*/*                                                        */                                                                                                                                */
/*/*IF gvlDebug THEN                                        */                                                                                                                                */
/*/*    PUT STREAM sDebug UNFORMATTED "Start Program " SKIP.*/                                                                                                                                */
/*                                                                                                                                                                                            */
/*/* ========= main procedures ======== */                                                                                                                                                    */
/*RUN doPOBuild.  /* build temp-table ttItemPurchased */                                                                                                                                      */
/*RUN doPoUI.     /* handle all prompting as UI */                                                                                                                                            */
/*RUN ValidatePO.                                                                                                                                                                             */
/*RUN doPOProcess. /* Process all not canceled temp-table from prompt UI */                                                                                                                   */
/*RUN PostPO.      /* Update all relative tables after PO created */                                                                                                                          */
/*RUN DisplayTempTable.  /* monitor/debug purpose */                                                                                                                                          */
/*                                                                                                                                                                                            */
/*                                                                                                                                                                                            */
/* **********************  Internal Procedures  *********************** */

PROCEDURE AutoPOFromOrderLine:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriOeOrdl AS ROWID NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-oe-ordl        FOR oe-ordl.
    DEFINE BUFFER bf-ttAutoPoMaster FOR ttAutoPOMaster.
    
    DEFINE VARIABLE lAbort AS LOGICAL NO-UNDO.
    
    FIND bf-oe-ordl NO-LOCK 
        WHERE ROWID(bf-oe-ordl) EQ ipriOeOrdl
        NO-ERROR.
    IF NOT AVAILABLE bf-oe-ordl THEN 
    DO:
        ASSIGN 
            oplError   = YES
            opcMessage = "Invalid Order Line RowID"
            .
        RETURN.
    END.
    
    RUN pSetAutoPOMaster(bf-oe-ordl.company, BUFFER bf-ttAutoPoMaster).
    
    RUN pCheckAbort(BUFFER bf-ttAutoPOMaster, OUTPUT lAbort, OUTPUT opcMessage).
    IF lAbort THEN RETURN.
    RUN pBuild
    RUN pAddFGItemsToPurchase(bf-oe-ordl.company, bf-oe-ordl.i-no, OUTPUT oplError, OUTPUT opcMessage).
    RUN pAddRMItemsToPurchase(bf-oe-ordl.company, bf-oe-ordl.i-no, bf-oe-ordl.job-no, bf-oe-ordl.job-no2, OUTPUT oplError, OUTPUT opcMessage).
    
    
    RUN pProcessItemsToPurchase.
    
END PROCEDURE.

PROCEDURE pAddFGItemsToPurchase PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-itemfg FOR itemfg.
    
    FIND bf-itemfg NO-LOCK 
        WHERE bf-itemfg.company EQ ipcCompany
        AND bf-itemfg.i-no EQ ipcItemID
        NO-ERROR.
    IF AVAILABLE bf-itemfg THEN 
    DO:
        
        RUN pAddFGItemToPurchase(BUFFER bf-itemfg, OUTPUT oplError, OUTPUT opcMessage).
        IF bf-itemfg.isaset THEN 
        DO:
        END.
        
    END.
    ELSE 
        ASSIGN 
            oplError   = YES
            opcMessage = "Invalid FG Item (" + ipcItemID + ") for company " + ipcCompany
            .
    
END PROCEDURE.

PROCEDURE pAddFGItemToPurchase PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-itemfg FOR itemfg.
    DEFINE INPUT PARAMETER ipdQuantity AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcQuantityUOM AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE dQuantity AS DECIMAL NO-UNDO.
    
    FIND FIRST ttItemToPurchase EXCLUSIVE-LOCK
        WHERE ttItemToPurchase.itemID EQ ipbf-itemfg.i-no
        AND ttItemToPurchase.itemType EQ "FG"
        NO-ERROR.
    IF NOT AVAILABLE ttItemToPurchase THEN 
    DO:
        CREATE ttItemToPurchase.
        ASSIGN 
            ttItemToPurchase.itemID             = ipbf-itemfg.i-no
            ttItemToPurchase.itemType           = "FG"
            ttItemToPurchase.quantityToPurchase = ipdQuantity
            ttItemToPurchase.quantityUOM        = ipcQuantityUOM.
    END.
    ELSE 
    DO:
        IF ipcQuantityUOM NE ttItemToPurchase.quantityUOM THEN 
            RUN Conv_ValueFromUOMToUOMForItem(ROWID(ipbf-itemfg), ipdQuantity, ipcQuantityUOM, ttItemToPurchase.quantityUOM, 
                OUTPUT dQuantity, OUTPUT oplError, OUTPUT opcMessage).
        ELSE dQuantity = ipdQuantity.
        ttItemToPurchase.quantityToPurchase = ttItemToPurchase.quantityToPurchase + dQuantity. 
    END.    

END PROCEDURE.

PROCEDURE pBuildItemsToPurchase PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:  Builds the items to purchase
 Notes:
------------------------------------------------------------------------------*/
    

END PROCEDURE.

PROCEDURE pCheckAbort PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Checks to see if the process should abort since there is no 
        access for user or 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttAutoPoMaster FOR ttAutoPOMaster.
    DEFINE OUTPUT PARAMETER oplAbort AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    IF NOT (ipbf-ttAutoPOMaster.userCanDoAutoPOForFG 
        OR ipbf-ttAutoPOMaster.userCanDoAutoPOForRM
        OR ipbf-ttAutoPOMaster.userCanDoAutoPOForPrep) THEN 
        ASSIGN 
            oplAbort   = YES  
            opcMessage = "User has no access to any OEAutoPO function."
            .
    IF NOT oplAbort THEN 
    DO: 
        IF NOT (ipbf-ttAutoPOMaster.doAutoPOForFG 
            OR ipbf-ttAutoPOMaster.doAutoPOForRM
            OR ipbf-ttAutoPOMaster.doAutoPOForPrep) THEN 
            ASSIGN 
                oplAbort   = YES  
                opcMessage = "OEAutoPO configuration not activated."
                .
    END.
    
END PROCEDURE.

PROCEDURE pProcessItemsToPurchase PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a posting master record, process the ttItemToPurchase records
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttAutoPOMaster FOR ttAutoPOMaster.

    FOR EACH ttItemToPurchase:
        
    END.    
    FOR EACH w-job-mat    
    WHERE (IF iplPromptRM THEN TRUE ELSE w-job-mat.this-is-a-rm EQ FALSE)
        BREAK BY w-job-mat.this-is-a-rm
        BY w-job-mat.frm
        BY w-job-mat.blank-no
        BY w-job-mat.i-no:

        ASSIGN 
            gvcVendNo      = ""
            gvrPoOrdl      = ?
            gvrItem        = ?
            gvrVend        = ?
            gvrPoOrd       = ?
            gvrTT-eiv      = ?
            gvrTT-ei       = ?
            gvrItemfg      = ?
            gvcFilIdSource = ? 
            gvrWJobMat     = ?.

        IF NOT w-job-mat.this-is-a-rm THEN 
        DO:
            FIND itemfg NO-LOCK WHERE itemfg.company EQ cocode
                AND itemfg.i-no EQ w-job-mat.i-no 
                NO-ERROR.
            IF AVAILABLE itemfg THEN
                gvrItemfg = ROWID(itemfg).
        END.
        FIND oe-ord NO-LOCK WHERE ROWID(oe-ord) EQ gvrOeOrd NO-ERROR.
  
        IF gvlDebug THEN
            PUT STREAM sDebug UNFORMATTED "Process Item " + w-job-mat.i-no SKIP.
        lNextOuters = NO.

        EMPTY TEMP-TABLE tt-ei.
        EMPTY TEMP-TABLE tt-eiv.

        v-vendor-chosen-report = ?.
        gvrWJobMat = ROWID(w-job-mat).

        llFirstOfJobFrm = FIRST-OF(w-job-mat.frm).
        llFirstJobFrm = FIRST(w-job-mat.frm).        

        IF w-job-mat.this-is-a-rm THEN 
        DO:
  
            /* Create tt-ei and tt-eiv for e-itemvend of an item */
            IF lNewVendorItemCost THEN RUN RevCreateTtEivVend (INPUT cocode, INPUT ROWID(w-job-mat), INPUT v-po-best, OUTPUT gvrItem).
            ELSE RUN createTtEivVend (INPUT cocode, INPUT ROWID(w-job-mat), INPUT v-po-best, OUTPUT gvrItem).
        END.
        ELSE 
        DO:            
      
            FIND itemfg NO-LOCK WHERE itemfg.company = cocode
                AND itemfg.i-no = w-job-mat.rm-i-no NO-ERROR.
            IF NOT AVAIL itemfg THEN RETURN.
            /* Create tt-eiv for a w-job-mat and itemfg */
            IF lNewVendorItemCost THEN RUN RevCreateTtEiv (INPUT rowid(itemfg), INPUT  ROWID(w-job-mat)).
            ELSE RUN createTtEivItemfg (INPUT  cocode, INPUT  ROWID(w-job-mat)).
        END.

        /* Just a prompt to create a line */
        RUN promptCreatePoLine.        

        /* User choose not to create, so don't continue with this item */
        IF NOT gvlChoice THEN
            NEXT outers.

        /* Sets gvrB-orderpo, initialize global variables and create a b-orderpo */
        RUN initJobVals (INPUT cocode,
            INPUT fil_id,
            INPUT gvrJob,
            INPUT ROWID(w-job-mat),
            INPUT gvrOeOrdl,
            OUTPUT v-job,
            OUTPUT gvcVendNo).

        /* Get v-len, v-wid, v-dep, v-job-mat-qty, v-qty-comp, v-uom-comp */         
        RUN initRptRecs (INPUT cocode,
            INPUT ROWID(w-job-mat),
            OUTPUT gvrTT-ei) .
  

  
  
        /* Creates a report record for each tt-eiv, sets fil_id */
   
        RUN buildRptRecs (INPUT cocode, 
            INPUT llFirstJobFrm,
            INPUT ROWID(w-job-mat),
            INPUT gvrOeOrdl,
            INPUT gvrTT-ei,
            INPUT gvrItemfg,
            OUTPUT gvrVend).

        /* Warning message that vendor matrix does not exist */
        IF gvcVendNo EQ "" AND gvlChoice AND NOT ll-canceled THEN 
            RUN cancelMessage.        
         
        IF gvcVendNo EQ "" OR ll-canceled THEN 
        DO:
            IF gvlDebug THEN             
                PUT STREAM sDebug UNFORMATTED "Skip Item for canceled or gvcVendNo " w-job-mat.i-no " gvcVendNo " gvcVendNo SKIP.
            NEXT.
        END.
    
        /* Set po dates from oe-ord or job */
        RUN setPoDates (INPUT gvrVend, INPUT gvrOeOrd, INPUT gvrJob).

        /* Set GV ll-drop */
        RUN promptDropShip.

        /* prompt for updating PO for given vendor and date */
        RUN promptUpdPoNum (INPUT cocode, 
            INPUT w-job-mat.po-no,
            OUTPUT gvrPoOrd,
            OUTPUT gvrTT-eiv,
            OUTPUT lNextOuters). /* set choice */

        IF lNextOuters THEN
            NEXT outers.

        FIND po-ord NO-LOCK WHERE ROWID(po-ord) EQ gvrPoOrd NO-ERROR.
        FIND oe-ord NO-LOCK WHERE ROWID(oe-ord) EQ gvrOeOrd NO-ERROR.
  
        IF NOT gvlChoice THEN 
        DO:
            IF NOT AVAILABLE po-ord THEN 
            DO:
                /* SEts globals gvdDueDate and gvdPoDate */
                RUN calcDueDate (INPUT ROWID(w-job-mat)).
            END.
        END.

        /* If they chose not to update existing then return */
        IF NOT gvlChoice AND AVAIL(po-ord) THEN 
        DO:
            IF gvlDebug THEN             
                PUT STREAM sDebug UNFORMATTED "Return since choose not to update existing " w-job-mat.i-no  SKIP.
            /* RETURN. WFK - taken out so that prompts for remaining RMs */
            NEXT outers.
        END.
    
        /* Check gvlChoice and update oe-ordl.po-no-po and vend-no */
        RUN ProcessExisting (INPUT cocode,
            INPUT gvrOeOrdl,
            INPUT llFirstOfJobFrm,
            INPUT ROWID(w-job-mat),
            INPUT gvrPoOrd).
  
        /* Find existing PO for a due date and vendor. */
        RUN findExistingPo (INPUT w-job-mat.po-no, OUTPUT lPoExists, OUTPUT gvrPoOrd).

        FIND po-ord NO-LOCK WHERE ROWID(po-ord) EQ gvrPoOrd NO-ERROR.
        FIND oe-ord NO-LOCK WHERE ROWID(oe-ord) EQ gvrOeOrd NO-ERROR.
  
        IF NOT lPoExists THEN 
        DO:
            IF AVAILABLE po-ord THEN 
            DO:
                /* Po exists for given vendor and date, locks record if say yes */
                RUN PromptExistingPo. /* release current po-ord buffer if they say no */      
            END.
            IF NOT AVAILABLE po-ord THEN 
            DO:
                RUN createPoOrd (INPUT gvrOeOrd, OUTPUT gvrPoOrd, OUTPUT lNextOuters).
                IF lNextOuters THEN 
                DO:
                    IF gvlDebug THEN             
                        PUT STREAM sDebug UNFORMATTED "Skip do to createPoOrd " w-job-mat.i-no  SKIP.
                    NEXT outers.
                END.
                FIND po-ord NO-LOCK WHERE ROWID(po-ord) EQ gvrPoOrd NO-ERROR.
   
            END. /* Not avail po-ord then add it */
        END.
        ELSE
            gvlChoice = YES.

        /* Assign po values to oe-ordl and assign vend values to po-ord */
        RUN setPoValues (INPUT llFirstOfJobFrm, 
            INPUT gvrPoOrd, 
            INPUT gvrOeOrdl,
            INPUT ROWID(w-job-mat)).
                  
        FIND po-ord NO-LOCK WHERE ROWID(po-ord) EQ gvrpoOrd NO-ERROR.

        IF NOT AVAILABLE po-ord THEN 
        DO:
            MESSAGE "Error: No Po found (after assign po values)"
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
            NEXT.
        END.

        FIND b-item NO-LOCK WHERE ROWID(b-item) EQ gvrItem NO-ERROR.
        FIND vend NO-LOCK WHERE ROWID(vend) EQ gvrVend NO-ERROR.

        IF gvlDebug THEN             
            PUT STREAM sDebug UNFORMATTED "Create PO Line " w-job-mat.i-no  SKIP.
        FIND bf-ordl NO-LOCK WHERE ROWID(bf-ordl) EQ gvrOeOrdl NO-ERROR.

        /* creates po-ordl based on gvlChoice */
        RUN createPoOrdl (INPUT gvrPoOrd,
            INPUT gvrOeOrdl,
            INPUT ROWID(w-job-mat),
            INPUT gvrItem,
            OUTPUT gvrItemfg).

        FIND itemfg NO-LOCK WHERE ROWID(itemfg) EQ gvrItemfg NO-ERROR.

        FIND po-ord NO-LOCK WHERE ROWID(po-ord) EQ gvrpoOrd NO-ERROR.
        IF NOT AVAILABLE po-ord THEN 
        DO:
            MESSAGE "Error: No Po found (after create po-ordl)"
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
            NEXT.
        END.

        FIND po-ordl NO-LOCK WHERE ROWID(po-ordl) EQ gvrPoOrdl NO-ERROR.
        IF NOT AVAILABLE po-ordl THEN 
        DO:
            MESSAGE "PO Line not available, skipping it"
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
            NEXT outers.
        END.

        /* set values from item and w-job-mat */
        IF po-ordl.item-type THEN
            RUN setPoOrdRm (INPUT gvrPoOrd,
                INPUT gvrPoOrdl,
                INPUT gvrItem,
                INPUT ROWID(w-job-mat),
                INPUT gvrOeOrdl,
                INPUT gvrJob).
        ELSE
            /* Set values from itemfg */
            RUN setPoOrdlFg (INPUT gvrPoOrd,
                INPUT gvrPoOrdl,
                INPUT gvrItemfg,
                INPUT ROWID(w-job-mat),
                INPUT gvrOeOrdl,
                INPUT gvrJob).


        FIND po-ord NO-LOCK WHERE ROWID(po-ord) EQ gvrpoOrd NO-ERROR.
        IF NOT AVAILABLE po-ord THEN 
        DO:
            MESSAGE "Error: No Po found (after assign po-ordl)"
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
            NEXT.
        END.

        FIND po-ordl NO-LOCK WHERE ROWID(po-ordl) EQ gvrPoOrdl NO-ERROR.

        IF NOT AVAILABLE po-ordl THEN 
        DO:
            MESSAGE 'Error: No Po Line available (main block)' 
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
            NEXT.
        END.


        IF po-ordl.item-type THEN 
        DO:
  
            IF NOT AVAILABLE tt-ei THEN
                FIND FIRST tt-ei
                    WHERE tt-ei.company EQ cocode
                    AND tt-ei.i-no    EQ w-job-mat.rm-i-no
                    NO-LOCK NO-ERROR.

            IF AVAILABLE tt-ei THEN
                gvrTT-ei = ROWID(tt-ei).
            ELSE 
                gvrTT-ei = ?.

            /* get v-new-i-no, v-new-len, v-new-wid, set po-ordl.cons-uom and pr-uom */
            RUN poOrdlAddVals (INPUT gvrPoOrdl,
                INPUT gvrItem,
                INPUT ROWID(w-job-mat),
                INPUT gvrOeOrdl,
                INPUT gvrTT-ei).
        END. /* run poOrdlAddVals */   

        /* Get len, wid, depth from item. Set po-ordl.cust-no */
        IF lNewVendorItemCost THEN RUN calcLenWidN (INPUT gvrPoOrd,
                INPUT gvrPoOrdl,
                INPUT gvrItem).
        ELSE RUN calcLenWid (INPUT gvrPoOrd,
                INPUT gvrPoOrdl,
                INPUT gvrItem).

        /* get po-ordl.ord-qty from job-hdr, w-job-mat or oe-ordl */
        IF po-ordl.item-type THEN 
            RUN calcEstValues (INPUT gvrPoOrdl,
                INPUT gvrJob,
                INPUT ROWID(w-job-mat),
                INPUT gvrOeOrdl,
                INPUT gvrItem).
        /* Set po-ordl.s-num and b-num. UOM Conversion on po-ordl.ord-qty */
        RUN calcOrdQty    (INPUT gvrPoOrdl, INPUT ROWID(w-job-mat)).

        /* get po-ordl.cost from w-job-mat or v-item-cost */
        RUN calcCostSetup (INPUT gvrTt-ei, INPUT ROWID(w-job-mat),
            INPUT gvrPoOrdl, INPUT gvcVendNo,
            INPUT v-vend-item).

        /*  Calculate v-tot-msf. Set po-ordl.s-len and s-wid */
        RUN calcMSF       (INPUT gvrPoOrdl).

        /* runs po/po-adder.p and po/po-vendc.i */
        RUN processAdders (INPUT ROWID(w-job-mat), INPUT gvrPoOrdl, INPUT gvrPoOrd).

        /* UOM conversion for po-ordl.cost */
        RUN calcCost      (INPUT gvrTT-ei, INPUT gvrPoOrdl).

        RELEASE b-item.
  
        /* Warns user of zero length or width in Job */
        RUN zeroLenWarning (INPUT gvrPoOrdl, OUTPUT gvrItem).

        FIND b-item WHERE ROWID(b-item) EQ gvrItem NO-LOCK NO-ERROR.
        FIND po-ordl WHERE ROWID(po-ordl) EQ gvrPoOrdl NO-LOCK NO-ERROR.

        IF (AVAILABLE b-item AND index("1234BPR",b-item.mat-type) GT 0) OR
            NOT po-ordl.item-type                                   THEN 
        DO:
    
            /* Validate than Length was entered */
            RUN brdLenCheck (INPUT gvrPoOrdl).

            /* UOM conversions to handle zere line qty or cost. Calculate oe-ordl.cost  */
            RUN checkZeroQty (INPUT gvrPoOrdl).

            /* Calculate po-ordl.t-cost and cons-cost */
            RUN calcExtCost (INPUT gvrPoOrdl).

            /* Find po-ordl and set v-old-i-no, v-tot-ord and po-ordl.dscr */
            RUN addHeaderTot (INPUT gvrPoOrdl).

            /* If v-new-i-no is a valid item, set w-job-mat.rm-i-no to it */
            RUN autoRm (INPUT gvrPoOrdl,
                INPUT ROWID(w-job-mat),
                OUTPUT gvrItem) /* needs additional buffers */.

        END. /* if avail b-item ... */

        /* Update Farm recs, deal with changes to scoring allowances */
        RUN PoOrdlFinal (INPUT gvrPoOrdl).
        FIND po-ord NO-LOCK WHERE ROWID(po-ord) EQ gvrPoOrd NO-ERROR.
        IF NOT AVAILABLE po-ord THEN 
        DO:
            MESSAGE "Internal Error - PO not found." 
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
            NEXT.
        END.

        /* Calculate PO Header Totals */
        RUN po/po-total.p (RECID(po-ord)).

        /* CHECK for exceeding vendor's max PO Cost and worn for it (set v-hold) */
        RUN validMaxCost (gvrPoOrd).

        FIND po-ordl WHERE ROWID(po-ordl) EQ gvrPoOrdl NO-LOCK NO-ERROR.
        IF NOT AVAILABLE po-ordl THEN 
        DO:
            MESSAGE "Error - Po Line not found (22)" 
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
            NEXT.
        END.

        /* Update item inventory totals */
        RUN po/poordlup.p (RECID(po-ordl), 1, v-cont-upd).

    END. /* each w-job-mat */

END PROCEDURE.

PROCEDURE pSetAutoPOMaster PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Given a company, create the ttAutoPOMaster record
     Notes:  RUN pSetAutoPOMaster(ipcCompany, BUFFER bf-ttAutoPOMaster).
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE PARAMETER BUFFER opbf-ttAutoPOMaster FOR ttAutoPOMaster.

    DEFINE VARIABLE lFound       AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cResult      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAccessClose AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cAccessList  AS CHARACTER NO-UNDO.
    
    EMPTY TEMP-TABLE ttAutoPoMaster.
    
    CREATE opbf-ttAutoPOMaster.
    
    RUN sys/ref/nk1look.p (ipcCompany,"POQTY", "C", NO, NO, "", "", OUTPUT cResult, OUTPUT lFound).
    opbf-ttAutoPOMaster.useNetSheets = lFound AND cResult EQ "Net Shts".
    
    RUN sys/ref/nk1look.p (ipcCompany,"POCOST", "C", NO, NO, "", "", OUTPUT opbf-ttAutoPOMaster.poCostSource, OUTPUT lFound).
    RUN sys/ref/nk1look.p (ipcCompany,"POCOST", "L", NO, NO, "", "", OUTPUT cResult, OUTPUT lFound).
    opbf-ttAutoPOMaster.poCostHoldOverprices = lFound AND cResult EQ "YES".
    
    RUN sys/ref/nk1look.p (ipcCompany,"OEAUTOFG", "C", NO, NO, "", "", OUTPUT opbf-ttAutoPOMaster.doAutoPOForFGRule, OUTPUT lFound).
    RUN sys/ref/nk1look.p (ipcCompany,"OEAUTOFG", "L", NO, NO, "", "", OUTPUT cResult, OUTPUT lFound).
    opbf-ttAutoPOMaster.doAutoPOForFG = lFound AND cResult EQ "YES".

    RUN sys/ref/nk1look.p (ipcCompany,"OEAUTOPO", "C", NO, NO, "", "", OUTPUT opbf-ttAutoPOMaster.doAutoPOForRMRule, OUTPUT lFound).    
    RUN sys/ref/nk1look.p (ipcCompany,"OEAUTOPO", "L", NO, NO, "", "", OUTPUT cResult, OUTPUT lFound).
    opbf-ttAutoPOMaster.doAutoPOForRM = lFound AND cResult EQ "YES".
    
    RUN sys/ref/nk1look.p (ipcCompany,"OEAUTOPREP", "C", NO, NO, "", "", OUTPUT opbf-ttAutoPOMaster.doAutoPOForPrepRule, OUTPUT lFound).
    RUN sys/ref/nk1look.p (ipcCompany,"OEAUTOPREP", "L", NO, NO, "", "", OUTPUT cResult, OUTPUT lFound).
    opbf-ttAutoPOMaster.doAutoPOForPrep = lFound AND cResult EQ "YES".
    
    RUN sys/ref/nk1look.p (ipcCompany,"AP GL#", "C", NO, NO, "", "", OUTPUT opbf-ttAutoPOMaster.apGLSource, OUTPUT lFound).
    RUN sys/ref/nk1look.p (ipcCompany,"AP GL#", "L", NO, NO, "", "", OUTPUT cResult, OUTPUT lFound).
    opbf-ttAutoPOMaster.apGLFromCostType = lFound AND cResult EQ "YES".
    
    RUN sys/ref/nk1look.p (ipcCompany,"POUOM", "C", NO, NO, "", "", OUTPUT opbf-ttAutoPOMaster.defaultPOQtyUOMSource, OUTPUT lFound).        
    RUN sys/ref/nk1look.p (ipcCompany,"POUOM", "I", NO, NO, "", "", OUTPUT cResult, OUTPUT lFound).
    opbf-ttAutoPOMaster.defaultPOConsQtyUOMToTonIfPaper = lFound AND TRIM(cResult) EQ "1".
    
    RUN methods/prgsecur.p ("OEAutoPO", "ALL", NO, NO, NO, OUTPUT opbf-ttAutoPOMaster.userCanDoAutoPOForRM, OUTPUT lAccessClose, OUTPUT cAccessList).
    RUN methods/prgsecur.p ("OEAutoFG", "ALL", NO, NO, NO, OUTPUT opbf-ttAutoPOMaster.userCanDoAutoPOForFG, OUTPUT lAccessClose, OUTPUT cAccessList).
    RUN methods/prgsecur.p ("OEAutoPrep", "ALL", NO, NO, NO, OUTPUT opbf-ttAutoPOMaster.userCanDoAutoPOForPrep, OUTPUT lAccessClose, OUTPUT cAccessList).
    
    
END PROCEDURE.

/*                                                                                                                                                                                            */
/*PROCEDURE BuildItemPurchased:                                                                                                                                                               */
/*/*------------------------------------------------------------------------------                                                                                                            */
/*     Purpose:                                                                                                                                                                               */
/*     Notes:  Old ttItemPurchased table -> ttItemPurchased                                                                                                                                   */
/*------------------------------------------------------------------------------*/                                                                                                            */
/*                                                                                                                                                                                            */
/*    DEFINE BUFFER bf-job FOR job.                                                                                                                                                           */
/*    IF gvlDebug THEN                                                                                                                                                                        */
/*        PUT STREAM sDebug UNFORMATTED "Start buildJobMat" SKIP.                                                                                                                             */
/*                                                                                                                                                                                            */
/*    FIND oe-ord WHERE ROWID(oe-ord) EQ gvrOeOrd NO-LOCK NO-ERROR.                                                                                                                           */
/*    FIND oe-ordl WHERE ROWID(oe-ordl) EQ gvrOeOrdl NO-LOCK NO-ERROR.                                                                                                                        */
/*    FIND bf-job WHERE ROWID(bf-job) EQ gvrJob NO-LOCK NO-ERROR.                                                                                                                             */
/*    FIND bf-ordl WHERE ROWID(bf-ordl) EQ gvrOeOrdl NO-LOCK NO-ERROR.                                                                                                                        */
/*                                                                                                                                                                                            */
/*    IF AVAIL(oe-ordl) AND NOT AVAIL(oe-ord) THEN                                                                                                                                            */
/*        FIND oe-ord WHERE oe-ord.company EQ oe-ordl.company                                                                                                                                 */
/*            AND oe-ord.ord-no EQ oe-ordl.ord-no                                                                                                                                             */
/*            NO-LOCK NO-ERROR.                                                                                                                                                               */
/*                                                                                                                                                                                            */
/*    /* Create ttItemPurchased from itemfg */                                                                                                                                                */
/*    IF oeautofg-log AND v-autofg-sec AND AVAILABLE oe-ord AND AVAILABLE oe-ordl THEN                                                                                                        */
/*        RUN BuildItemPurchasedDetailFGItem                                                                                                                                                  */
/*            (INPUT "ttItemFG",                                                                                                                                                              */
/*            INPUT  oeautofg-chr,                                                                                                                                                            */
/*            INPUT  gvrOeOrd,                                                                                                                                                                */
/*            INPUT gvrOeOrdl).                                                                                                                                                               */
/*                                                                                                                                                                                            */
/*    /* Create ttItemPurchased from job-mat */                                                                                                                                               */
/*    IF AVAILABLE bf-job AND ((nk1-oeautopo-char NE "Manual" AND v-autopo-sec AND v-po-best EQ NO) OR v-po-best) THEN                                                                        */
/*        RUN BuildItemPurchasedDetailJobMat (v-po-best, INPUT ROWID(bf-job), INPUT gvrOeOrdl).                                                                                               */
/*                                                                                                                                                                                            */
/*    IF AVAILABLE bf-job                                                                                                                                                                     */
/*        AND oeautoprep-log AND v-autoprep-sec                                                                                                                                               */
/*        AND AVAILABLE bf-ordl AND v-po-best EQ NO THEN                                                                                                                                      */
/*    DO:                                                                                                                                                                                     */
/*                                                                                                                                                                                            */
/*        /* create ttItemPurchased from job-prep */                                                                                                                                          */
/*        RUN BuildItemPurchasedDetailJobPrep (INPUT ROWID(bf-job), INPUT oe-ordl.i-no).                                                                                                      */
/*        /* create ttItemPurchased from b-job-mat */                                                                                                                                         */
/*        RUN BuildItemPurchasedDetailJobMat2 (INPUT ROWID(bf-job)).                                                                                                                          */
/*                                                                                                                                                                                            */
/*    END.                                                                                                                                                                                    */
/*                                                                                                                                                                                            */
/*END PROCEDURE.                                                                                                                                                                              */
/*                                                                                                                                                                                            */
/*PROCEDURE BuildItemPurchasedDetail:                                                                                                                                                         */
/*   /*------------------------------------------------------------------------------                                                                                                         */
/*     Purpose:                                                                                                                                                                               */
/*     Notes:                                                                                                                                                                                 */
/*    ------------------------------------------------------------------------------*/                                                                                                        */
/*                                                                                                                                                                                            */
/*END PROCEDURE.                                                                                                                                                                              */
/*                                                                                                                                                                                            */
/*PROCEDURE BuildItemPurchasedDetailFGItem:                                                                                                                                                   */
/*/*------------------------------------------------------------------------------                                                                                                            */
/*  Purpose:                                                                                                                                                                                  */
/*  Parameters:  <none>                                                                                                                                                                       */
/*  Notes:                                                                                                                                                                                    */
/*  Inputs                                                                                                                                                                                    */
/*            oeautofg-log v-autofg-sec bf-ord bf-ordl oeautofg-chr                                                                                                                           */
/*------------------------------------------------------------------------------*/                                                                                                            */
/*    /* wfk - Create ttItemPurchased based on tt-itemfg created above */                                                                                                                     */
/*    /*       ttItemPurchased handling both fg and rm                 */                                                                                                                     */
/*                                                                                                                                                                                            */
/*    DEFINE INPUT  PARAMETER ipcOeAutoFg AS CHARACTER   NO-UNDO.                                                                                                                             */
/*    DEFINE INPUT  PARAMETER iprOeOrd     AS ROWID       NO-UNDO.                                                                                                                            */
/*    DEFINE INPUT  PARAMETER iprOeOrdl    AS ROWID       NO-UNDO.                                                                                                                            */
/*                                                                                                                                                                                            */
/*    DEFINE BUFFER bf-ord  FOR oe-ord.                                                                                                                                                       */
/*    DEFINE BUFFER bf-ordl FOR oe-ordl.                                                                                                                                                      */
/*                                                                                                                                                                                            */
/*    IF gvlDebug THEN                                                                                                                                                                        */
/*        PUT STREAM sDebug UNFORMATTED "Start create-w-job-from-tt-itemfg"  SKIP.                                                                                                            */
/*                                                                                                                                                                                            */
/*                                                                                                                                                                                            */
/*    FIND bf-ord NO-LOCK WHERE ROWID(bf-ord) EQ iprOeOrd NO-ERROR.                                                                                                                           */
/*    FIND bf-ordl NO-LOCK WHERE ROWID(bf-ordl) EQ iprOeOrdl NO-ERROR.                                                                                                                        */
/*                                                                                                                                                                                            */
/*    FOR EACH tt-itemfg WHERE tt-itemfg.pur-man,                                                                                                                                             */
/*        FIRST itemfg NO-LOCK WHERE ROWID(itemfg) EQ tt-itemfg.row-id :                                                                                                                      */
/*                                                                                                                                                                                            */
/*        IF gvlDebug THEN                                                                                                                                                                    */
/*            PUT STREAM sDebug UNFORMATTED "Create ttItemPurchased from tt-itemfg consider criteria " itemfg.i-no                                                                            */
/*                " oeautofg-chr " oeautofg-chr " ipcOeAutofg " ipcOeAutofg SKIP.                                                                                                             */
/*                                                                                                                                                                                            */
/*        IF ((oeautofg-chr EQ "NonStock" OR ipcOeAutoFg EQ "Any") AND                                                                                                                        */
/*            NOT itemfg.stocked)                                         OR                                                                                                                  */
/*            ((ipcOeAutoFg EQ "LotCntrl" OR ipcOeAutoFg EQ "Any") AND                                                                                                                        */
/*            NOT itemfg.ord-policy)                                      OR                                                                                                                  */
/*            ((ipcOeAutoFg EQ "Avail<0"  OR ipcOeAutoFg EQ "Any") AND                                                                                                                        */
/*            itemfg.q-avail LT 0)                                        THEN                                                                                                                */
/*        DO:                                                                                                                                                                                 */
/*                                                                                                                                                                                            */
/*            IF gvlDebug THEN                                                                                                                                                                */
/*                PUT STREAM sDebug UNFORMATTED "Create ttItemPurchased from purchased tt-itemfg " itemfg.i-no  SKIP.                                                                         */
/*            CREATE ttItemPurchased.                                                                                                                                                         */
/*            ASSIGN                                                                                                                                                                          */
/*                ttItemPurchased.purchaseType = "FGItem"                                                                                                                                     */
/*                ttItemPurchased.w-recid      = ?                                                                                                                                            */
/*                ttItemPurchased.rm-i-no      = itemfg.i-no                                                                                                                                  */
/*                ttItemPurchased.i-no         = itemfg.i-no                                                                                                                                  */
/*                ttItemPurchased.fg-i-no      = itemfg.i-no                                                                                                                                  */
/*                ttItemPurchased.fg-part-no   = bf-ordl.part-no                                                                                                                              */
/*                ttItemPurchased.est-no       = bf-ordl.est-no                                                                                                                               */
/*                ttItemPurchased.frm          = tt-itemfg.form-no                                                                                                                            */
/*                ttItemPurchased.blank-no     = tt-itemfg.blank-no                                                                                                                           */
/*                ttItemPurchased.isaset       = itemfg.isaset                                                                                                                                */
/*                ttItemPurchased.isacomponent = tt-itemfg.isacomponent                                                                                                                       */
/*                ttItemPurchased.this-is-a-rm = NO                                                                                                                                           */
/*                ttItemPurchased.basis-w      = itemfg.t-wid * itemfg.t-len * 100                                                                                                            */
/*                ttItemPurchased.basis-w      = itemfg.weight-100 /                                                                                                                          */
/*                                (IF v-corr THEN (ttItemPurchased.basis-w * .007)                                                                                                            */
/*                                           ELSE (ttItemPurchased.basis-w / 144) /                                                                                                           */
/*                                 1000)                                                                                                                                                      */
/*                ttItemPurchased.qty-uom      = "EA"                                                                                                                                         */
/*                ttItemPurchased.sc-uom       = itemfg.pur-uom                                                                                                                               */
/*                ttItemPurchased.qty          = tt-itemfg.qty.                                                                                                                               */
/*            RUN po/GetFGDimsForPO.p (ROWID(itemfg), OUTPUT ttItemPurchased.len, OUTPUT ttItemPurchased.wid, OUTPUT ttItemPurchased.dep).                                                    */
/*                                                                                                                                                                                            */
/*        END. /* if q-avail is OK */                                                                                                                                                         */
/*    END. /* each tt-itemfg */                                                                                                                                                               */
/*                                                                                                                                                                                            */
/*                                                                                                                                                                                            */
/*END PROCEDURE.                                                                                                                                                                              */
/*                                                                                                                                                                                            */
/*PROCEDURE BuildItemPurchasedDetailJobMat:                                                                                                                                                   */
/*    /*------------------------------------------------------------------------------                                                                                                        */
/*      Purpose:                                                                                                                                                                              */
/*      Parameters:  <none>                                                                                                                                                                   */
/*      Notes:                                                                                                                                                                                */
/*      Inputs:  "ttItemFg", "JobMat", "JobPrep", "BJobMat"                                                                                                                                   */
/*        job                                                                                                                                                                                 */
/*        v-po-best                                                                                                                                                                           */
/*        bf-ordl   must exist                                                                                                                                                                */
/*        b-jc-calc may exists                                                                                                                                                                */
/*      Outputs:                                                                                                                                                                              */
/*        creates ttItemPurchased                                                                                                                                                             */
/*    ------------------------------------------------------------------------------*/                                                                                                        */
/*    /* Create ttItemPurchased from job-mat */                                                                                                                                               */
/*/*    DEFINE INPUT  PARAMETER ipPurchaseType AS CHAR   NO-UNDO.*/                                                                                                                           */
/*    DEFINE INPUT  PARAMETER iplPoBest AS LOGICAL     NO-UNDO.                                                                                                                               */
/*    DEFINE INPUT  PARAMETER iprJob    AS ROWID       NO-UNDO.                                                                                                                               */
/*    DEFINE INPUT  PARAMETER iprOeOrdl AS ROWID       NO-UNDO.                                                                                                                               */
/*                                                                                                                                                                                            */
/*    DEFINE BUFFER bf-ordl FOR oe-ordl.                                                                                                                                                      */
/*    DEFINE BUFFER bf-job  FOR job.                                                                                                                                                          */
/*    FIND bf-job NO-LOCK WHERE ROWID(bf-job) EQ iprJob NO-ERROR.                                                                                                                             */
/*    FIND bf-ordl NO-LOCK WHERE ROWID(bf-ordl) EQ iprOeOrdl NO-ERROR.                                                                                                                        */
/*                                                                                                                                                                                            */
/*                                                                                                                                                                                            */
/*    FOR EACH job-mat NO-LOCK                                                                                                                                                                */
/*        WHERE job-mat.company EQ bf-job.company                                                                                                                                             */
/*        AND job-mat.job     EQ bf-job.job                                                                                                                                                   */
/*        AND job-mat.job-no  EQ bf-job.job-no                                                                                                                                                */
/*        AND job-mat.job-no2 EQ bf-job.job-no2                                                                                                                                               */
/*        AND NOT CAN-FIND(FIRST tt-itemfg                                                                                                                                                    */
/*        WHERE tt-itemfg.form-no EQ job-mat.frm                                                                                                                                              */
/*        AND tt-itemfg.pur-man EQ YES                                                                                                                                                        */
/*        AND tt-itemfg.isaset EQ NO)                                                                                                                                                         */
/*        ,                                                                                                                                                                                   */
/*                                                                                                                                                                                            */
/*        FIRST ITEM NO-LOCK                                                                                                                                                                  */
/*        WHERE item.company EQ job-mat.company                                                                                                                                               */
/*        AND item.i-no    EQ job-mat.rm-i-no                                                                                                                                                 */
/*        :                                                                                                                                                                                   */
/*        IF gvlDebug THEN                                                                                                                                                                    */
/*            PUT STREAM sDebug UNFORMATTED "Create ttItemPurchased from job-mat " job-mat.i-no " f " job-mat.frm " b " job-mat.blank-no " iplpobest "                                        */
/*                iplPoBest " type " item.mat-type SKIP.                                                                                                                                      */
/*                                                                                                                                                                                            */
/*        IF iplPoBest = NO AND NOT CAN-DO("1,2,3,4,B,P,R",item.mat-type) THEN NEXT.                                                                                                          */
/*        ELSE IF iplPoBest AND ITEM.mat-type NE "B" THEN NEXT.                                                                                                                               */
/*                                                                                                                                                                                            */
/*        FIND FIRST b-jc-calc NO-LOCK                                                                                                                                                        */
/*            WHERE b-jc-calc.reftable EQ "jc/jc-calc.p"                                                                                                                                      */
/*            AND b-jc-calc.company  EQ bf-job.company                                                                                                                                        */
/*            AND b-jc-calc.loc      EQ ""                                                                                                                                                    */
/*            AND b-jc-calc.code     EQ STRING(bf-job.job,"999999999")                                                                                                                        */
/*            AND b-jc-calc.val[12]  EQ job-mat.frm                                                                                                                                           */
/*            AND (b-jc-calc.val[13] EQ job-mat.blank-no OR job-mat.blank-no EQ 0)                                                                                                            */
/*            AND b-jc-calc.code2    NE bf-ordl.i-no                                                                                                                                          */
/*            NO-ERROR.                                                                                                                                                                       */
/*        IF gvlDebug THEN                                                                                                                                                                    */
/*            PUT STREAM sDebug UNFORMATTED "Create ttItemPurchased from job-mat " job-mat.i-no " f " job-mat.frm " b " job-mat.blank-no SKIP.                                                */
/*        CREATE ttItemPurchased.                                                                                                                                                             */
/*        BUFFER-COPY job-mat TO ttItemPurchased                                                                                                                                              */
/*            ASSIGN                                                                                                                                                                          */
/*            ttItemPurchased.purchaseType = "JobMat"                                                                                                                                         */
/*            ttItemPurchased.w-rowid      = ROWID(job-mat)                                                                                                                                   */
/*            ttItemPurchased.w-recid      = RECID(job-mat)                                                                                                                                   */
/*            ttItemPurchased.this-is-a-rm = YES                                                                                                                                              */
/*            ttItemPurchased.dep          = item.s-dep                                                                                                                                       */
/*            ttItemPurchased.basis-w      = item.basis-w                                                                                                                                     */
/*            ttItemPurchased.fg-i-no      = IF AVAILABLE b-jc-calc THEN b-jc-calc.code2                                                                                                      */
/*                                               ELSE bf-ordl.i-no.                                                                                                                           */
/*        IF ttItemPurchased.dep EQ 0 THEN ttItemPurchased.dep = job-mat.dep.                                                                                                                 */
/*    END. /* each job-mat */                                                                                                                                                                 */
/*                                                                                                                                                                                            */
/*                                                                                                                                                                                            */
/*END PROCEDURE.                                                                                                                                                                              */
/*                                                                                                                                                                                            */
/*PROCEDURE BuildItemPurchasedDetailJobMat2:                                                                                                                                                  */
/*    /*------------------------------------------------------------------------------                                                                                                        */
/*       Purpose:                                                                                                                                                                             */
/*       Parameters:  <none>                                                                                                                                                                  */
/*       Notes:                                                                                                                                                                               */
/*         Inputs:                                                                                                                                                                            */
/*           job                                                                                                                                                                              */
/*           updates ttItemPurchased                                                                                                                                                          */
/*                                                                                                                                                                                            */
/*     ------------------------------------------------------------------------------*/                                                                                                       */
/*    DEFINE INPUT PARAMETER iprJob AS ROWID NO-UNDO.                                                                                                                                         */
/*    DEFINE BUFFER bf-job FOR job.                                                                                                                                                           */
/*                                                                                                                                                                                            */
/*    FIND bf-job WHERE ROWID(bf-job) EQ iprJob NO-LOCK NO-ERROR.                                                                                                                             */
/*                                                                                                                                                                                            */
/*                                                                                                                                                                                            */
/*    /* Create ttItemPurchased from item */                                                                                                                                                  */
/*    FOR EACH b-job-mat NO-LOCK WHERE                                                                                                                                                        */
/*        b-job-mat.company EQ bf-job.company AND                                                                                                                                             */
/*        b-job-mat.job     EQ bf-job.job AND                                                                                                                                                 */
/*        b-job-mat.job-no  EQ bf-job.job-no AND                                                                                                                                              */
/*        b-job-mat.job-no2 EQ bf-job.job-no2 AND                                                                                                                                             */
/*        NOT CAN-FIND(FIRST tt-itemfg WHERE                                                                                                                                                  */
/*        tt-itemfg.form-no EQ b-job-mat.frm AND                                                                                                                                              */
/*        tt-itemfg.pur-man EQ YES)                                                                                                                                                           */
/*        ,                                                                                                                                                                                   */
/*        FIRST prep FIELDS(i-no number-up) NO-LOCK WHERE                                                                                                                                     */
/*        prep.company EQ b-job-mat.company AND                                                                                                                                               */
/*        prep.i-no EQ b-job-mat.i-no                                                                                                                                                         */
/*        ,                                                                                                                                                                                   */
/*        FIRST ITEM  NO-LOCK WHERE                                                                                                                                                           */
/*        item.company EQ b-job-mat.company AND                                                                                                                                               */
/*        item.i-no    EQ prep.i-no AND                                                                                                                                                       */
/*        CAN-DO("7,8,M,X,Y",item.mat-type)                                                                                                                                                   */
/*        :                                                                                                                                                                                   */
/*                                                                                                                                                                                            */
/*        FIND FIRST b-jc-calc NO-LOCK WHERE                                                                                                                                                  */
/*            b-jc-calc.reftable EQ "jc/jc-calc.p" AND                                                                                                                                        */
/*            b-jc-calc.company  EQ bf-job.company AND                                                                                                                                        */
/*            b-jc-calc.loc      EQ "" AND                                                                                                                                                    */
/*            b-jc-calc.code     EQ STRING(bf-job.job,"999999999") AND                                                                                                                        */
/*            b-jc-calc.val[12]  EQ b-job-mat.frm AND                                                                                                                                         */
/*            (b-jc-calc.val[13] EQ b-job-mat.blank-no OR b-job-mat.blank-no EQ 0) AND                                                                                                        */
/*            b-jc-calc.code2    NE bf-ordl.i-no                                                                                                                                              */
/*            NO-ERROR.                                                                                                                                                                       */
/*                                                                                                                                                                                            */
/*        IF gvlDebug THEN                                                                                                                                                                    */
/*            PUT STREAM sDebug UNFORMATTED "Create ttItemPurchased from b-job-mat " b-job-mat.i-no  SKIP.                                                                                    */
/*                                                                                                                                                                                            */
/*        CREATE ttItemPurchased.                                                                                                                                                             */
/*        BUFFER-COPY b-job-mat TO ttItemPurchased                                                                                                                                            */
/*            ASSIGN                                                                                                                                                                          */
/*            ttItemPurchased.purchaseType = "BufferJobMat"                                                                                                                                   */
/*            ttItemPurchased.w-rowid      = ROWID(b-job-mat)                                                                                                                                 */
/*            ttItemPurchased.w-recid      = RECID(b-job-mat)                                                                                                                                 */
/*            ttItemPurchased.this-is-a-rm = YES                                                                                                                                              */
/*            ttItemPurchased.dep          = item.s-dep                                                                                                                                       */
/*            ttItemPurchased.basis-w      = item.basis-w                                                                                                                                     */
/*            ttItemPurchased.fg-i-no      = IF AVAILABLE b-jc-calc THEN b-jc-calc.code2                                                                                                      */
/*                                                    ELSE bf-ordl.i-no                                                                                                                       */
/*            ttItemPurchased.prep         = YES                                                                                                                                              */
/*            ttItemPurchased.rm-i-no      = b-job-mat.i-no                                                                                                                                   */
/*            ttItemPurchased.i-no         = b-job-mat.i-no                                                                                                                                   */
/*            ttItemPurchased.qty-uom      = "EA"                                                                                                                                             */
/*            ttItemPurchased.n-up         = b-job-mat.n-up.                                                                                                                                  */
/*                                                                                                                                                                                            */
/*        IF ttItemPurchased.dep EQ 0 THEN ttItemPurchased.dep = b-job-mat.dep.                                                                                                               */
/*                                                                                                                                                                                            */
/*    END. /* for each b-job-mat */                                                                                                                                                           */
/*                                                                                                                                                                                            */
/*END PROCEDURE.                                                                                                                                                                              */
/*                                                                                                                                                                                            */
/*PROCEDURE BuildItemPurchasedDetailJobPrep:                                                                                                                                                  */
/*    /*------------------------------------------------------------------------------                                                                                                        */
/*        Purpose:                                                                                                                                                                            */
/*        Parameters:  <none>                                                                                                                                                                 */
/*        Notes:                                                                                                                                                                              */
/*        Inputs:                                                                                                                                                                             */
/*          job                                                                                                                                                                               */
/*          bf-ordl.i-no                                                                                                                                                                      */
/*        Outputs:                                                                                                                                                                            */
/*          creates ttItemPurchased                                                                                                                                                           */
/*                                                                                                                                                                                            */
/*      ------------------------------------------------------------------------------*/                                                                                                      */
/*    DEFINE INPUT  PARAMETER iprJob AS ROWID       NO-UNDO.                                                                                                                                  */
/*    DEFINE INPUT  PARAMETER ipcIno AS CHARACTER   NO-UNDO.                                                                                                                                  */
/*                                                                                                                                                                                            */
/*    FIND job NO-LOCK WHERE ROWID(job) EQ iprJob NO-ERROR.                                                                                                                                   */
/*                                                                                                                                                                                            */
/*    IF NOT AVAILABLE job THEN                                                                                                                                                               */
/*        RETURN.                                                                                                                                                                             */
/*                                                                                                                                                                                            */
/*    /* Create ttItemPurchased  from job-prep */                                                                                                                                             */
/*    FOR EACH job-prep NO-LOCK WHERE                                                                                                                                                         */
/*        job-prep.company EQ job.company AND                                                                                                                                                 */
/*        job-prep.job     EQ job.job AND                                                                                                                                                     */
/*        job-prep.job-no  EQ job.job-no AND                                                                                                                                                  */
/*        job-prep.job-no2 EQ job.job-no2 AND                                                                                                                                                 */
/*        NOT CAN-FIND(FIRST tt-itemfg WHERE                                                                                                                                                  */
/*        tt-itemfg.form-no EQ job-prep.frm AND                                                                                                                                               */
/*        tt-itemfg.pur-man EQ YES AND                                                                                                                                                        */
/*        tt-itemfg.isaset EQ NO)                                                                                                                                                             */
/*        ,                                                                                                                                                                                   */
/*        FIRST prep FIELDS(i-no number-up)  NO-LOCK WHERE                                                                                                                                    */
/*        prep.company EQ job-prep.company AND                                                                                                                                                */
/*        prep.CODE EQ job-prep.CODE AND                                                                                                                                                      */
/*        prep.i-no NE ""                                                                                                                                                                     */
/*        ,                                                                                                                                                                                   */
/*        FIRST ITEM NO-LOCK WHERE                                                                                                                                                            */
/*        item.company EQ job-prep.company AND                                                                                                                                                */
/*        item.i-no    EQ prep.i-no                                                                                                                                                           */
/*        :                                                                                                                                                                                   */
/*                                                                                                                                                                                            */
/*        FIND FIRST b-jc-calc WHERE                                                                                                                                                          */
/*            b-jc-calc.reftable EQ "jc/jc-calc.p" AND                                                                                                                                        */
/*            b-jc-calc.company  EQ job.company AND                                                                                                                                           */
/*            b-jc-calc.loc      EQ "" AND                                                                                                                                                    */
/*            b-jc-calc.code     EQ STRING(job.job,"999999999") AND                                                                                                                           */
/*            b-jc-calc.val[12]  EQ job-prep.frm AND                                                                                                                                          */
/*            (b-jc-calc.val[13] EQ job-prep.blank-no OR job-prep.blank-no EQ 0) AND                                                                                                          */
/*            b-jc-calc.code2    NE bf-ordl.i-no                                                                                                                                              */
/*            NO-LOCK NO-ERROR.                                                                                                                                                               */
/*/*        IF gvlDebug THEN                                                                    */                                                                                            */
/*/*            PUT STREAM sDebug UNFORMATTED "Create ttItemPurchased from job-prep " prep.i-no  SKIP.*/                                                                                      */
/*        CREATE ttItemPurchased.                                                                                                                                                             */
/*        BUFFER-COPY job-prep TO ttItemPurchased                                                                                                                                             */
/*            ASSIGN                                                                                                                                                                          */
/*            ttItemPurchased.purchaseType = "JobPrep"                                                                                                                                        */
/*            ttItemPurchased.w-rowid      = ROWID(job-prep)                                                                                                                                  */
/*            ttItemPurchased.w-recid      = RECID(job-prep)                                                                                                                                  */
/*            ttItemPurchased.this-is-a-rm = YES                                                                                                                                              */
/*            ttItemPurchased.dep          = item.s-dep                                                                                                                                       */
/*            ttItemPurchased.basis-w      = item.basis-w                                                                                                                                     */
/*            ttItemPurchased.fg-i-no      = IF AVAILABLE b-jc-calc THEN b-jc-calc.code2                                                                                                      */
/*                                                    ELSE ipcIno                                                                                                                             */
/*            ttItemPurchased.prep         = YES                                                                                                                                              */
/*            ttItemPurchased.rm-i-no      = prep.i-no                                                                                                                                        */
/*            ttItemPurchased.i-no         = prep.i-no                                                                                                                                        */
/*            ttItemPurchased.qty-uom      = "EA"                                                                                                                                             */
/*            ttItemPurchased.n-up         = prep.number-up.                                                                                                                                  */
/*    END.                                                                                                                                                                                    */
/*    RELEASE job.                                                                                                                                                                            */
/*                                                                                                                                                                                            */
/*END PROCEDURE.                                                                                                                                                                              */
/*                                                                                                                                                                                            */
/*PROCEDURE CreatePOOrd:                                                                                                                                                                      */
/*    /*------------------------------------------------------------------------------                                                                                                        */
/*       Purpose:                                                                                                                                                                             */
/*       Parameters:  <none>                                                                                                                                                                  */
/*       Notes:                                                                                                                                                                               */
/*         Requires company record                                                                                                                                                            */
/*         Creates PO-ord                                                                                                                                                                     */
/*     ------------------------------------------------------------------------------*/                                                                                                       */
/*    DEFINE INPUT PARAMETER iprOeOrd AS ROWID NO-UNDO.                                                                                                                                       */
/*    DEFINE OUTPUT PARAMETER oprPoOrd AS ROWID       NO-UNDO.                                                                                                                                */
/*    DEFINE OUTPUT PARAMETER oplNextOuters AS LOGICAL     NO-UNDO.                                                                                                                           */
/*                                                                                                                                                                                            */
/*    DEFINE BUFFER bf-ord FOR oe-ord.                                                                                                                                                        */
/*    FIND bf-ord WHERE ROWID(bf-ord) EQ iprOeOrd NO-LOCK NO-ERROR.                                                                                                                           */
/*    DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.                                                                                                                                                */
/*                                                                                                                                                                                            */
/*    oplNextOuters = NO.                                                                                                                                                                     */
/*    /* outers label is needed for po-ord.a                                                                                                                                                  */
/*       if a 'next' was done in po/po-ord.a then                                                                                                                                             */
/*       iCnt will become = 2 and routine will return */                                                                                                                                      */
/*    outers:                                                                                                                                                                                 */
/*    REPEAT iCnt = 1 TO 2:                                                                                                                                                                   */
/*        IF iCnt NE 1 THEN                                                                                                                                                                   */
/*        DO:                                                                                                                                                                                 */
/*            ASSIGN                                                                                                                                                                          */
/*                oplNextOuters = TRUE.                                                                                                                                                       */
/*            LEAVE.                                                                                                                                                                          */
/*        END.                                                                                                                                                                                */
/*        /* Requires cocode, sets fil_id to new po-ord */                                                                                                                                    */
/*        {po/po-ord.a}                                                                                                                                                                       */
/*        LEAVE.                                                                                                                                                                              */
/*    END.                                                                                                                                                                                    */
/*    IF oplNextOuters THEN                                                                                                                                                                   */
/*        RETURN.                                                                                                                                                                             */
/*                                                                                                                                                                                            */
/*    ASSIGN                                                                                                                                                                                  */
/*        po-ord.po-date        = gvdPoDate                                                                                                                                                   */
/*        po-ord.due-date       = gvdDueDate                                                                                                                                                  */
/*        po-ord.last-ship-date = po-ord.due-date                                                                                                                                             */
/*        po-ord.vend-no        = gvcVendNo.                                                                                                                                                  */
/*                                                                                                                                                                                            */
/*    IF AVAILABLE bf-ord THEN                                                                                                                                                                */
/*        ASSIGN                                                                                                                                                                              */
/*            gvcDropCustNo = bf-ord.cust-no                                                                                                                                                  */
/*            gvcShipChoice = "C". /* task# 09160518*/                                                                                                                                        */
/*                                                                                                                                                                                            */
/*    gvrPoOrd = ROWID(po-ord).                                                                                                                                                               */
/*                                                                                                                                                                                            */
/*    /* Prompt for drop ship shipto and assign po-ord ship fields */                                                                                                                         */
/*    IF ll-drop THEN                                                                                                                                                                         */
/*    DO:                                                                                                                                                                                     */
/*        RUN askDropShip (INPUT gvrPoOrd,                                                                                                                                                    */
/*            INPUT gvrWJobMat,                                                                                                                                                               */
/*            INPUT gvrOeOrdl) NO-ERROR.                                                                                                                                                      */
/*        IF ERROR-STATUS:ERROR THEN                                                                                                                                                          */
/*        DO:                                                                                                                                                                                 */
/*            MESSAGE "ASI Error message: Not able to run askDropShip"                                                                                                                        */
/*                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.                                                                                                                                   */
/*        END.                                                                                                                                                                                */
/*                                                                                                                                                                                            */
/*    END.                                                                                                                                                                                    */
/*    ELSE                                                                                                                                                                                    */
/*        IF AVAILABLE company THEN                                                                                                                                                           */
/*            ASSIGN                                                                                                                                                                          */
/*                po-ord.ship-id      = company.company                                                                                                                                       */
/*                po-ord.ship-name    = company.NAME                                                                                                                                          */
/*                po-ord.ship-addr[1] = company.addr[1]                                                                                                                                       */
/*                po-ord.ship-addr[2] = company.addr[2]                                                                                                                                       */
/*                po-ord.ship-city    = company.city                                                                                                                                          */
/*                po-ord.ship-state   = company.state                                                                                                                                         */
/*                po-ord.ship-zip     = company.zip.                                                                                                                                          */
/*                                                                                                                                                                                            */
/*    oprPoOrd = ROWID(po-ord).                                                                                                                                                               */
/*    FIND CURRENT po-ord NO-LOCK NO-ERROR.                                                                                                                                                   */
/*    RELEASE po-ord.                                                                                                                                                                         */
/*                                                                                                                                                                                            */
/*END PROCEDURE.                                                                                                                                                                              */
/*                                                                                                                                                                                            */
/*PROCEDURE createPoOrdl:                                                                                                                                                                     */
/*    /*------------------------------------------------------------------------------                                                                                                        */
/*       Purpose:                                                                                                                                                                             */
/*       Parameters:  <none>                                                                                                                                                                  */
/*       Notes:                                                                                                                                                                               */
/*       Requires:                                                                                                                                                                            */
/*         item                                                                                                                                                                               */
/*         bf-ordl                                                                                                                                                                            */
/*         ttItemPurchased                                                                                                                                                                    */
/*         cocode                                                                                                                                                                             */
/*         v-po-best                                                                                                                                                                          */
/*         po-ord (for po-ordl.a)                                                                                                                                                             */
/*         vend (found in po-ordl.a)                                                                                                                                                          */
/*                                                                                                                                                                                            */
/*     ------------------------------------------------------------------------------*/                                                                                                       */
/*    DEFINE INPUT  PARAMETER iprPoOrd AS ROWID       NO-UNDO.                                                                                                                                */
/*    DEFINE INPUT  PARAMETER iprOeOrdl AS ROWID       NO-UNDO.                                                                                                                               */
/*    DEFINE INPUT  PARAMETER iprWJobMat AS ROWID       NO-UNDO.                                                                                                                              */
/*    DEFINE INPUT  PARAMETER iprItem AS ROWID       NO-UNDO.                                                                                                                                 */
/*    DEFINE OUTPUT PARAMETER oprItemfg AS ROWID NO-UNDO.                                                                                                                                     */
/*                                                                                                                                                                                            */
/*    DEFINE BUFFER bf-ordl      FOR oe-ordl.                                                                                                                                                 */
/*    DEFINE BUFFER bf-ttItemPurchased FOR ttItemPurchased.                                                                                                                                   */
/*                                                                                                                                                                                            */
/*    FIND ITEM WHERE ROWID(ITEM) EQ iprItem NO-LOCK NO-ERROR.                                                                                                                                */
/*    FIND bf-ttItemPurchased WHERE ROWID(bf-ttItemPurchased) EQ iprWJobMat NO-LOCK NO-ERROR.                                                                                                 */
/*    FIND po-ord WHERE ROWID(po-ord) EQ iprPoOrd NO-LOCK NO-ERROR.                                                                                                                           */
/*    FIND bf-ordl WHERE ROWID(bf-ordl) EQ iprOeOrdl NO-LOCK NO-ERROR.                                                                                                                        */
/*                                                                                                                                                                                            */
/*    IF AVAILABLE item THEN                                                                                                                                                                  */
/*        FIND b-item WHERE RECID(b-item) EQ RECID(item) NO-LOCK.                                                                                                                             */
/*                                                                                                                                                                                            */
/*    v-new-avail = NO.                                                                                                                                                                       */
/*    /* If bf-itemfg found, then this is an FG item and don't join on frm */                                                                                                                 */
/*    /* 05281404 - added join to rm-i-no */                                                                                                                                                  */
/*    FIND FIRST bf-itemfg                                                                                                                                                                    */
/*        WHERE bf-itemfg.company EQ bf-ordl.company                                                                                                                                          */
/*        AND bf-itemfg.i-no    EQ /* wfk - 05281404 - bf-ordl.i-no */ bf-ttItemPurchased.rm-i-no                                                                                             */
/*        NO-LOCK NO-ERROR.                                                                                                                                                                   */
/*                                                                                                                                                                                            */
/*    IF gvlChoice THEN                                                                                                                                                                       */
/*        FIND FIRST po-ordl                                                                                                                                                                  */
/*            WHERE po-ordl.company    EQ cocode                                                                                                                                              */
/*            AND po-ordl.job-no     EQ bf-ttItemPurchased.job-no                                                                                                                             */
/*            AND po-ordl.job-no2    EQ bf-ttItemPurchased.job-no2                                                                                                                            */
/*            AND (po-ordl.s-num      EQ bf-ttItemPurchased.frm OR AVAIL(bf-itemfg))                                                                                                          */
/*            AND po-ordl.i-no       EQ bf-ttItemPurchased.rm-i-no                                                                                                                            */
/*            AND po-ordl.item-type  EQ bf-ttItemPurchased.this-is-a-rm                                                                                                                       */
/*            AND (bf-ttItemPurchased.job-no NE "" OR                                                                                                                                         */
/*            po-ordl.ord-no    EQ v-ord-no)                                                                                                                                                  */
/*            NO-ERROR.                                                                                                                                                                       */
/*                                                                                                                                                                                            */
/*    IF NOT AVAILABLE po-ordl AND nk1-oeautopo-char EQ "AutoRM" AND v-autopo-sec AND bf-ttItemPurchased.this-is-a-rm THEN                                                                    */
/*    DO:                                                                                                                                                                                     */
/*        FIND FIRST po-ordl EXCLUSIVE-LOCK                                                                                                                                                   */
/*            WHERE po-ordl.company EQ cocode                                                                                                                                                 */
/*            AND po-ordl.job-no  EQ bf-ttItemPurchased.job-no                                                                                                                                */
/*            AND po-ordl.job-no2 EQ bf-ttItemPurchased.job-no2                                                                                                                               */
/*            AND po-ordl.s-num   EQ bf-ttItemPurchased.frm                                                                                                                                   */
/*            AND po-ordl.i-no    EQ                                                                                                                                                          */
/*            IF LENGTH(bf-ttItemPurchased.i-no) LE 10 THEN bf-ttItemPurchased.i-no                                                                                                           */
/*            ELSE substr(bf-ttItemPurchased.i-no,LENGTH(bf-ttItemPurchased.i-no) - 9,10)                                                                                                     */
/*            NO-ERROR.                                                                                                                                                                       */
/*                                                                                                                                                                                            */
/*        IF AVAILABLE po-ordl THEN                                                                                                                                                           */
/*        DO:                                                                                                                                                                                 */
/*            FIND FIRST b-item                                                                                                                                                               */
/*                WHERE b-item.company EQ cocode                                                                                                                                              */
/*                AND b-item.i-no    EQ                                                                                                                                                       */
/*                IF LENGTH(bf-ttItemPurchased.i-no) LE 10 THEN bf-ttItemPurchased.i-no                                                                                                       */
/*                ELSE substr(bf-ttItemPurchased.i-no,LENGTH(bf-ttItemPurchased.i-no) - 9,10)                                                                                                 */
/*                NO-LOCK NO-ERROR.                                                                                                                                                           */
/*                                                                                                                                                                                            */
/*            IF AVAILABLE b-item THEN v-new-avail = YES.                                                                                                                                     */
/*            ELSE                                                                                                                                                                            */
/*            DO:                                                                                                                                                                             */
/*                IF bf-ttItemPurchased.prep EQ NO THEN                                                                                                                                       */
/*                DO:                                                                                                                                                                         */
/*                    IF v-po-best EQ NO THEN                                                                                                                                                 */
/*                        FIND FIRST b-item                                                                                                                                                   */
/*                            WHERE b-item.company  EQ cocode                                                                                                                                 */
/*                            AND b-item.i-no     EQ bf-ttItemPurchased.rm-i-no                                                                                                               */
/*                            AND index("1234BPR",b-item.mat-type) GT 0                                                                                                                       */
/*                            NO-LOCK NO-ERROR.                                                                                                                                               */
/*                    ELSE                                                                                                                                                                    */
/*                        FIND FIRST b-item                                                                                                                                                   */
/*                            WHERE b-item.company  EQ cocode                                                                                                                                 */
/*                            AND b-item.i-no     EQ bf-ttItemPurchased.rm-i-no                                                                                                               */
/*                            AND b-item.mat-type EQ "B"                                                                                                                                      */
/*                            NO-LOCK NO-ERROR.                                                                                                                                               */
/*                END. /* if bf-ttItemPurchased.prep eq no */                                                                                                                                 */
/*                ELSE                                                                                                                                                                        */
/*                    FIND FIRST b-item                                                                                                                                                       */
/*                        WHERE b-item.company  EQ cocode                                                                                                                                     */
/*                        AND b-item.i-no     EQ bf-ttItemPurchased.rm-i-no                                                                                                                   */
/*                        NO-LOCK NO-ERROR.                                                                                                                                                   */
/*            END. /* not avail b-item */                                                                                                                                                     */
/*        END. /* if avail po-ordl */                                                                                                                                                         */
/*    END. /* Not avail po-ordl and this is RM */                                                                                                                                             */
/*                                                                                                                                                                                            */
/*    IF NOT AVAILABLE po-ordl THEN                                                                                                                                                           */
/*    DO:                                                                                                                                                                                     */
/*    {po/po-ordl.a}                                                                                                                                                                          */
/*        ASSIGN                                                                                                                                                                              */
/*            po-ordl.tax       = po-ord.tax-gr NE "" AND                                                                                                                                     */
/*                         (aptax-chr EQ "Vendor" OR                                                                                                                                          */
/*                          (aptax-chr EQ "Item" AND                                                                                                                                          */
/*                           (AVAILABLE b-item AND b-item.tax-rcpt) OR                                                                                                                        */
/*                           (AVAILABLE itemfg AND itemfg.taxable)))                                                                                                                          */
/*            po-ordl.item-type = bf-ttItemPurchased.this-is-a-rm.                                                                                                                            */
/*    END. /* Not avail po-ordl then add it */                                                                                                                                                */
/*                                                                                                                                                                                            */
/*    IF AVAILABLE bf-itemfg THEN                                                                                                                                                             */
/*        oprItemfg = ROWID(bf-itemfg).                                                                                                                                                       */
/*                                                                                                                                                                                            */
/*    IF AVAILABLE po-ordl THEN                                                                                                                                                               */
/*        gvrPoOrdl = ROWID(po-ordl).                                                                                                                                                         */
/*    FIND CURRENT po-ordl NO-LOCK NO-ERROR.                                                                                                                                                  */
/*    RELEASE po-ordl.                                                                                                                                                                        */
/*                                                                                                                                                                                            */
/*END PROCEDURE.                                                                                                                                                                              */
/*                                                                                                                                                                                            */
/*PROCEDURE DisplayTempTable:                                                                                                                                                                 */
/*/*------------------------------------------------------------------------------                                                                                                            */
/* Purpose:                                                                                                                                                                                   */
/* Notes:                                                                                                                                                                                     */
/*------------------------------------------------------------------------------*/                                                                                                            */
/*   FOR EACH ttItemPurchased NO-LOCK:                                                                                                                                                        */
/*       DISPLAY ttItemPurchased.Canceled                                                                                                                                                     */
/*               ttItemPurchased.PurchaseType                                                                                                                                                 */
/*               ttItemPurchased.ReasonType                                                                                                                                                   */
/*               ttItemPurchased.fg-i-no                                                                                                                                                      */
/*               ttItemPurchased.est-no                                                                                                                                                       */
/*               ttItemPurchased.this-is-a-rm                                                                                                                                                 */
/*               ttItemPurchased.poDate                                                                                                                                                       */
/*               ttItemPurchased.DueDate                                                                                                                                                      */
/*               ttItemPurchased.CostPerUom                                                                                                                                                   */
/*               ttItemPurchased.CostSetup                                                                                                                                                    */
/*               ttItemPurchased.CostUOM                                                                                                                                                      */
/*               ttItemPurchased.CostTotal                                                                                                                                                    */
/*               .                                                                                                                                                                            */
/*   END.                                                                                                                                                                                     */
/*                                                                                                                                                                                            */
/*END PROCEDURE.                                                                                                                                                                              */
/*                                                                                                                                                                                            */
/*PROCEDURE doPOProcess:                                                                                                                                                                      */
/*/*------------------------------------------------------------------------------                                                                                                            */
/*     Purpose:                                                                                                                                                                               */
/*     Notes:                                                                                                                                                                                 */
/*                                                                                                                                                                                            */
/*      buildItemPurchased - will GET vendor cost too                                                                                                                                         */
/*      FindPO                                                                                                                                                                                */
/*      UpdateExistingPO                                                                                                                                                                      */
/*      CreatePO - findExistingPo,PromptExistingPo,createPoOrd - createPoOrdl                                                                                                                 */
/*                                                                                                                                                                                            */
/*       setPoValues, setPoOrdRm + setPoOrdlFg + poOrdlAddVals + PoOrdlFinal + calcLenWid + calcEstValues                                                                                     */
/*       calcOrdQty calcCostSetup calcMSF processAdders calcCost                                                                                                                              */
/*       zeroLenWarning brdLenCheck checkZeroQty calcExtCost addHeaderTot autoRm                                                                                                              */
/*                                                                                                                                                                                            */
/*------------------------------------------------------------------------------*/                                                                                                            */
/*                                                                                                                                                                                            */
/*                                                                                                                                                                                            */
/*    RUN ProcessItemPurchased /* processJobMat */ .                                                                                                                                          */
/*                                                                                                                                                                                            */
/*                                                                                                                                                                                            */
/*END PROCEDURE.                                                                                                                                                                              */
/*                                                                                                                                                                                            */
/*PROCEDURE DoPoBuild:                                                                                                                                                                        */
/*    /*------------------------------------------------------------------------------                                                                                                        */
/*         Purpose:                                                                                                                                                                           */
/*         Notes:                                                                                                                                                                             */
/*                                                                                                                                                                                            */
/*          buildItemPurchased - will GET vendor cost too                                                                                                                                     */
/*          FindPO                                                                                                                                                                            */
/*          UpdateExistingPO                                                                                                                                                                  */
/*          CreatePO - findExistingPo,PromptExistingPo,createPoOrd - createPoOrdl                                                                                                             */
/*                                                                                                                                                                                            */
/*           setPoValues, setPoOrdRm + setPoOrdlFg + poOrdlAddVals + PoOrdlFinal + calcLenWid + calcEstValues                                                                                 */
/*           calcOrdQty calcCostSetup calcMSF processAdders calcCost                                                                                                                          */
/*           zeroLenWarning brdLenCheck checkZeroQty calcExtCost addHeaderTot autoRm                                                                                                          */
/*                                                                                                                                                                                            */
/*    ------------------------------------------------------------------------------*/                                                                                                        */
/*                                                                                                                                                                                            */
/*    RUN findOrderFromRecid (INPUT fil_id,                                                                                                                                                   */
/*        OUTPUT gvrOeOrdl,                                                                                                                                                                   */
/*        OUTPUT gvrJob,                                                                                                                                                                      */
/*        OUTPUT gvcFilIdSource).                                                                                                                                                             */
/*                                                                                                                                                                                            */
/*    FIND job WHERE ROWID(job) EQ gvrJob NO-LOCK NO-ERROR.                                                                                                                                   */
/*    cFilIdSource = gvcFilIdSource.                                                                                                                                                          */
/*                                                                                                                                                                                            */
/*    IF gvcFilIdSource EQ "JOB" THEN                                                                                                                                                         */
/*        RUN ttItemfgFromJob (INPUT gvrJob, INPUT gvrOeOrdl, INPUT gvcFilIdSource).                                                                                                          */
/*                                                                                                                                                                                            */
/*    FIND oe-ordl WHERE ROWID(oe-ordl) EQ gvrOeOrdl NO-LOCK NO-ERROR.                                                                                                                        */
/*    FIND oe-ord WHERE ROWID(oe-ord) EQ gvrOeOrd NO-LOCK NO-ERROR.                                                                                                                           */
/*    FIND job WHERE ROWID(job) EQ gvrJob NO-LOCK NO-ERROR.                                                                                                                                   */
/*                                                                                                                                                                                            */
/*    /* run several procedures to build ttItemPurchased */                                                                                                                                   */
/*    RUN BuildItemPurchased /* buildJobmat*/ .                                                                                                                                               */
/*                                                                                                                                                                                            */
/*END PROCEDURE.                                                                                                                                                                              */
/*                                                                                                                                                                                            */
/*PROCEDURE doPOUi:                                                                                                                                                                           */
/*/*------------------------------------------------------------------------------                                                                                                            */
/* Purpose:                                                                                                                                                                                   */
/* Notes:                                                                                                                                                                                     */
/*                                                                                                                                                                                            */
/*  PromptCreatePOLine                                                                                                                                                                        */
/*  PromptDropShip                                                                                                                                                                            */
/*  PromptUpdPONUm                                                                                                                                                                            */
/*  PromptExistingPO                                                                                                                                                                          */
/*  SetPODates                                                                                                                                                                                */
/*  CalcProcs - LWD, EstValues,OrdQty,CostSetup,MSF Cost, ExtCost                                                                                                                             */
/*                                                                                                                                                                                            */
/*------------------------------------------------------------------------------*/                                                                                                            */
/*  FOR EACH ttItemPurchased BREAK BY ttItemPurchased.this-is-a-rm                                                                                                                            */
/*      BY ttItemPurchased.frm                                                                                                                                                                */
/*      BY ttItemPurchased.blank-no                                                                                                                                                           */
/*      BY ttItemPurchased.i-no:                                                                                                                                                              */
/*                                                                                                                                                                                            */
/*    RUN PromptCreatePOLine.                                                                                                                                                                 */
/*    RUN promptDropShip.                                                                                                                                                                     */
/*    /* prompt for updating PO for given vendor and date */                                                                                                                                  */
/*    RUN promptUpdPoNum (INPUT cocode,                                                                                                                                                       */
/*          INPUT ttItemPurchased.po-no,                                                                                                                                                      */
/*          OUTPUT gvrPoOrd,                                                                                                                                                                  */
/*          OUTPUT gvrTT-eiv,                                                                                                                                                                 */
/*          OUTPUT lNextOuters). /* set choice */                                                                                                                                             */
/*    IF lNextOuters THEN   /*  NEXT outers */                                                                                                                                                */
/*       ASSIGN ttItemPurchased.PurchaseType = " "                                                                                                                                            */
/*              ttItemPurchased.ReasonType = "PO Updated Canceled"                                                                                                                            */
/*              ttItemPurchased.canceled = yes.                                                                                                                                               */
/*                                                                                                                                                                                            */
/*      /* Check gvlChoice and update oe-ordl.po-no-po and vend-no */                                                                                                                         */
/*      RUN ProcessExisting (INPUT cocode,                                                                                                                                                    */
/*          INPUT gvrOeOrdl,                                                                                                                                                                  */
/*          INPUT llFirstOfJobFrm,                                                                                                                                                            */
/*          INPUT ROWID(ttItemPurchased),                                                                                                                                                     */
/*          INPUT gvrPoOrd).                                                                                                                                                                  */
/*                                                                                                                                                                                            */
/*      /* Find existing PO for a due date and vendor. */                                                                                                                                     */
/*      RUN findExistingPo (INPUT ttItemPurchased.po-no, OUTPUT lPoExists, OUTPUT gvrPoOrd).                                                                                                  */
/*                                                                                                                                                                                            */
/*      FIND po-ord NO-LOCK WHERE ROWID(po-ord) EQ gvrPoOrd NO-ERROR.                                                                                                                         */
/*      FIND oe-ord NO-LOCK WHERE ROWID(oe-ord) EQ gvrOeOrd NO-ERROR.                                                                                                                         */
/*      IF NOT lPoExists THEN                                                                                                                                                                 */
/*      DO:                                                                                                                                                                                   */
/*          IF AVAILABLE po-ord THEN                                                                                                                                                          */
/*          DO:                                                                                                                                                                               */
/*              /* Po exists for given vendor and date, locks record if say yes */                                                                                                            */
/*              RUN PromptExistingPo. /* release current po-ord buffer if they say no */                                                                                                      */
/*          END.                                                                                                                                                                              */
/*          IF NOT AVAILABLE po-ord THEN                                                                                                                                                      */
/*          DO:                                                                                                                                                                               */
/*              RUN createPoOrd (INPUT gvrOeOrd, OUTPUT gvrPoOrd, OUTPUT lNextOuters).                                                                                                        */
/*              IF lNextOuters THEN                                                                                                                                                           */
/*              DO:                                                                                                                                                                           */
/*/*                  IF gvlDebug THEN                                                                       */                                                                               */
/*/*                      PUT STREAM sDebug UNFORMATTED "Skip do to createPoOrd " ttItemPurchased.i-no  SKIP.*/                                                                               */
/*/*                  NEXT outers.                                                                           */                                                                               */
/*                  ASSIGN ttItemPurchased.canceled = YES                                                                                                                                     */
/*                         .                                                                                                                                                                  */
/*              END.                                                                                                                                                                          */
/*              FIND po-ord NO-LOCK WHERE ROWID(po-ord) EQ gvrPoOrd NO-ERROR.                                                                                                                 */
/*                                                                                                                                                                                            */
/*          END. /* Not avail po-ord then add it */                                                                                                                                           */
/*      END.                                                                                                                                                                                  */
/*                                                                                                                                                                                            */
/*                                                                                                                                                                                            */
/*                                                                                                                                                                                            */
/*  END.                                                                                                                                                                                      */
/*                                                                                                                                                                                            */
/*                                                                                                                                                                                            */
/*END PROCEDURE.                                                                                                                                                                              */
/*                                                                                                                                                                                            */
/*PROCEDURE FindOrderFromRecid:                                                                                                                                                               */
/*/*------------------------------------------------------------------------------                                                                                                            */
/*      Purpose:                                                                                                                                                                              */
/*      Parameters:  <none>                                                                                                                                                                   */
/*      Notes:                                                                                                                                                                                */
/*      Inputs:                                                                                                                                                                               */
/*        fil_id                                                                                                                                                                              */
/*        po-found - this is not set anywhere                                                                                                                                                 */
/*      Outputs:                                                                                                                                                                              */
/*        oe-ordl buffer                                                                                                                                                                      */
/*        sets v-ord-no                                                                                                                                                                       */
/*        creates tt-fg-set                                                                                                                                                                   */
/*        create tt-itemfg records                                                                                                                                                            */
/*------------------------------------------------------------------------------*/                                                                                                            */
/*                                                                                                                                                                                            */
/*    DEFINE INPUT  PARAMETER iprFilId  AS RECID       NO-UNDO.                                                                                                                               */
/*    DEFINE OUTPUT PARAMETER oprOeOrdl AS ROWID       NO-UNDO.                                                                                                                               */
/*    DEFINE OUTPUT PARAMETER oprJob AS ROWID       NO-UNDO.                                                                                                                                  */
/*    DEFINE OUTPUT PARAMETER opcFilIdSource AS CHARACTER   NO-UNDO.                                                                                                                          */
/*                                                                                                                                                                                            */
/*    FIND bf-ordl WHERE RECID(bf-ordl) EQ iprFilId NO-LOCK NO-ERROR.                                                                                                                         */
/*                                                                                                                                                                                            */
/*    /* wfk - Build tt-itemfg based on job */                                                                                                                                                */
/*    IF AVAILABLE bf-ordl THEN                                                                                                                                                               */
/*    DO:                                                                                                                                                                                     */
/*        /* bf-itemfg holds buffer for item of bf-ordl since itemfg is used for set components below */                                                                                      */
/*        opcFilIdSource = "oe-ordl".                                                                                                                                                         */
/*        FIND FIRST bf-itemfg NO-LOCK                                                                                                                                                        */
/*            WHERE bf-itemfg.company EQ bf-ordl.company                                                                                                                                      */
/*            AND bf-itemfg.i-no    EQ bf-ordl.i-no                                                                                                                                           */
/*            NO-ERROR.                                                                                                                                                                       */
/*        v-ord-no = bf-ordl.ord-no.                                                                                                                                                          */
/*                                                                                                                                                                                            */
/*        FIND FIRST bf-ord NO-LOCK                                                                                                                                                           */
/*            WHERE bf-ord.company EQ bf-ordl.company                                                                                                                                         */
/*            AND bf-ord.ord-no  EQ bf-ordl.ord-no                                                                                                                                            */
/*            AND bf-ord.opened  EQ YES                                                                                                                                                       */
/*            AND bf-ord.stat    NE "H"                                                                                                                                                       */
/*            NO-ERROR.                                                                                                                                                                       */
/*                                                                                                                                                                                            */
/*        IF AVAILABLE bf-ord THEN                                                                                                                                                            */
/*        DO:                                                                                                                                                                                 */
/*            IF TRIM(bf-ordl.job-no) NE "" THEN                                                                                                                                              */
/*                FIND FIRST job NO-LOCK                                                                                                                                                      */
/*                    WHERE job.company EQ cocode                                                                                                                                             */
/*                    AND job.job-no  EQ bf-ordl.job-no                                                                                                                                       */
/*                    AND job.job-no2 EQ bf-ordl.job-no2                                                                                                                                      */
/*                    NO-ERROR.                                                                                                                                                               */
/*            gvrOeOrd = ROWID(bf-ord).                                                                                                                                                       */
/*            /* wfk - po-found was leftover in error from task 05281404       */                                                                                                             */
/*            /* and should probably be removed. This needs more investigation */                                                                                                             */
/*            IF NOT AVAILABLE job OR NOT po-found THEN                                                                                                                                       */
/*            DO:                                                                                                                                                                             */
/*                                                                                                                                                                                            */
/*                FIND FIRST itemfg NO-LOCK                                                                                                                                                   */
/*                    WHERE itemfg.company EQ bf-ordl.company                                                                                                                                 */
/*                    AND itemfg.i-no    EQ bf-ordl.i-no                                                                                                                                      */
/*                    NO-ERROR.                                                                                                                                                               */
/*                IF AVAILABLE itemfg THEN                                                                                                                                                    */
/*                DO:                                                                                                                                                                         */
/*                    IF itemfg.isaset THEN                                                                                                                                                   */
/*                    DO:                                                                                                                                                                     */
/*                        RUN fg/fullset.p (ROWID(itemfg)).                                                                                                                                   */
/*                                                                                                                                                                                            */
/*                        RELEASE itemfg.                                                                                                                                                     */
/*                                                                                                                                                                                            */
/*                        FOR EACH tt-fg-set,                                                                                                                                                 */
/*                            FIRST itemfg NO-LOCK                                                                                                                                            */
/*                            WHERE itemfg.company EQ bf-ordl.company                                                                                                                         */
/*                            AND itemfg.i-no    EQ tt-fg-set.part-no:                                                                                                                        */
/*                            IF gvlDebug THEN                                                                                                                                                */
/*                                PUT STREAM sDebug UNFORMATTED "Create tt-itemfg for set part " tt-fg-set.part-no SKIP.                                                                      */
/*                            CREATE tt-itemfg.                                                                                                                                               */
/*                            BUFFER-COPY itemfg EXCEPT rec_key TO tt-itemfg                                                                                                                  */
/*                                ASSIGN                                                                                                                                                      */
/*                                tt-itemfg.isacomponent = TRUE                                                                                                                               */
/*                                tt-itemfg.form-no  = 0                                                                                                                                      */
/*                                tt-itemfg.blank-no = 0                                                                                                                                      */
/*                                tt-itemfg.qty      = bf-ordl.qty * tt-fg-set.part-qty-dec                                                                                                   */
/*                                tt-itemfg.pur-uom  = "EA"                                                                                                                                   */
/*                                tt-itemfg.row-id   = ROWID(itemfg).                                                                                                                         */
/*                                                                                                                                                                                            */
/*                            /* WFK - 06051407 */                                                                                                                                            */
/*                            FIND FIRST eb NO-LOCK WHERE                                                                                                                                     */
/*                                eb.company EQ bf-ordl.company AND                                                                                                                           */
/*                                eb.est-no EQ bf-ordl.est-no AND                                                                                                                             */
/*                                eb.stock-no EQ tt-fg-set.part-no AND                                                                                                                        */
/*                                eb.blank-no EQ tt-itemfg.blank-no AND                                                                                                                       */
/*                                eb.form-no EQ tt-itemfg.form-no                                                                                                                             */
/*                                NO-ERROR.                                                                                                                                                   */
/*                                                                                                                                                                                            */
/*                            IF AVAILABLE eb THEN                                                                                                                                            */
/*                                tt-itemfg.pur-man = eb.pur-man.                                                                                                                             */
/*                        END. /* Each tt-fg-set */                                                                                                                                           */
/*                    END. /* If itemfg.isaset */                                                                                                                                             */
/*                                                                                                                                                                                            */
/*                    ELSE                                                                                                                                                                    */
/*                    DO:                                                                                                                                                                     */
/*                        IF gvlDebug THEN                                                                                                                                                    */
/*                            PUT STREAM sDebug UNFORMATTED "Create tt-itemfg for FG " itemfg.i-no SKIP.                                                                                      */
/*                                                                                                                                                                                            */
/*                        CREATE tt-itemfg.                                                                                                                                                   */
/*                        BUFFER-COPY itemfg EXCEPT rec_key TO tt-itemfg                                                                                                                      */
/*                            ASSIGN                                                                                                                                                          */
/*                            tt-itemfg.form-no  = bf-ordl.form-no                                                                                                                            */
/*                            tt-itemfg.blank-no = bf-ordl.blank-no                                                                                                                           */
/*                            tt-itemfg.qty      = bf-ordl.qty                                                                                                                                */
/*                            tt-itemfg.pur-uom  = "EA"                                                                                                                                       */
/*                            tt-itemfg.row-id   = ROWID(itemfg).                                                                                                                             */
/*                        FIND FIRST eb NO-LOCK WHERE                                                                                                                                         */
/*                            eb.company EQ bf-ordl.company AND                                                                                                                               */
/*                            eb.est-no EQ bf-ordl.est-no AND                                                                                                                                 */
/*                            eb.stock-no EQ bf-ordl.i-no AND                                                                                                                                 */
/*                            eb.blank-no EQ bf-ordl.blank-no AND                                                                                                                             */
/*                            eb.form-no EQ bf-ordl.form-no                                                                                                                                   */
/*                            NO-ERROR.                                                                                                                                                       */
/*                                                                                                                                                                                            */
/*                        IF AVAILABLE eb THEN                                                                                                                                                */
/*                            tt-itemfg.pur-man = eb.pur-man.                                                                                                                                 */
/*                    END. /* If Not Itemfg.isaset */                                                                                                                                         */
/*                END. /* If avail itemfg */                                                                                                                                                  */
/*            END. /* If not avail job ... */                                                                                                                                                 */
/*        END. /* If avail bf-ord */                                                                                                                                                          */
/*    END. /* If avail bf-ordl */                                                                                                                                                             */
/*                                                                                                                                                                                            */
/*    ELSE                                                                                                                                                                                    */
/*    DO:                                                                                                                                                                                     */
/*        /* Oe-ordl not available */                                                                                                                                                         */
/*        FIND job WHERE RECID(job) EQ iprFilId NO-LOCK NO-ERROR.                                                                                                                             */
/*        opcFilIdSource = "JOB".                                                                                                                                                             */
/*        FOR EACH job-hdr NO-LOCK                                                                                                                                                            */
/*            WHERE job-hdr.company EQ job.company                                                                                                                                            */
/*            AND job-hdr.job     EQ job.job                                                                                                                                                  */
/*            AND job-hdr.job-no  EQ job.job-no                                                                                                                                               */
/*            AND job-hdr.job-no2 EQ job.job-no2                                                                                                                                              */
/*            AND job-hdr.ord-no  NE 0:                                                                                                                                                       */
/*            FIND FIRST bf-ord NO-LOCK                                                                                                                                                       */
/*                WHERE bf-ord.company EQ cocode                                                                                                                                              */
/*                AND bf-ord.ord-no  EQ job-hdr.ord-no                                                                                                                                        */
/*                AND bf-ord.opened  EQ YES                                                                                                                                                   */
/*                AND bf-ord.stat    NE "H"                                                                                                                                                   */
/*                NO-ERROR.                                                                                                                                                                   */
/*            IF NOT AVAILABLE bf-ord THEN                                                                                                                                                    */
/*            DO:                                                                                                                                                                             */
/*                RELEASE job.                                                                                                                                                                */
/*                LEAVE.                                                                                                                                                                      */
/*            END. /* not avail bf-ord */                                                                                                                                                     */
/*                                                                                                                                                                                            */
/*            gvrOeOrd = ROWID(bf-ord).                                                                                                                                                       */
/*                                                                                                                                                                                            */
/*            FIND FIRST bf-ordl NO-LOCK                                                                                                                                                      */
/*                WHERE bf-ordl.company EQ job-hdr.company                                                                                                                                    */
/*                AND bf-ordl.ord-no  EQ job-hdr.ord-no                                                                                                                                       */
/*                AND bf-ordl.i-no    EQ job-hdr.i-no                                                                                                                                         */
/*                NO-ERROR.                                                                                                                                                                   */
/*            IF AVAILABLE bf-ordl THEN LEAVE.                                                                                                                                                */
/*        END. /* each job-hdr */                                                                                                                                                             */
/*                                                                                                                                                                                            */
/*    END. /* bf-ordl not avail */                                                                                                                                                            */
/*                                                                                                                                                                                            */
/*    IF AVAILABLE bf-ordl THEN                                                                                                                                                               */
/*    DO:                                                                                                                                                                                     */
/*                                                                                                                                                                                            */
/*        oprOeOrdl = ROWID(bf-ordl).                                                                                                                                                         */
/*        FIND FIRST oe-ord NO-LOCK                                                                                                                                                           */
/*            WHERE oe-ord.company EQ bf-ordl.company                                                                                                                                         */
/*            AND oe-ord.ord-no EQ bf-ordl.ord-no                                                                                                                                             */
/*            NO-ERROR.                                                                                                                                                                       */
/*        IF AVAILABLE oe-ord THEN                                                                                                                                                            */
/*            gvrOeOrd = ROWID(oe-ord).                                                                                                                                                       */
/*                                                                                                                                                                                            */
/*    END. /* avail bf-ordl */                                                                                                                                                                */
/*                                                                                                                                                                                            */
/*    IF AVAILABLE job THEN                                                                                                                                                                   */
/*        oprJob = ROWID(job).                                                                                                                                                                */
/*                                                                                                                                                                                            */
/*                                                                                                                                                                                            */
/*END PROCEDURE.                                                                                                                                                                              */
/*                                                                                                                                                                                            */
/*PROCEDURE PostPO:                                                                                                                                                                           */
/*/*------------------------------------------------------------------------------                                                                                                            */
/* Purpose:                                                                                                                                                                                   */
/* Notes:                                                                                                                                                                                     */
/*------------------------------------------------------------------------------*/                                                                                                            */
/*  /*                                                                                                                                                                                        */
/*  setpovalues                                                                                                                                                                               */
/*  POOrdlFinal                                                                                                                                                                               */
/*  POTotal                                                                                                                                                                                   */
/*  */                                                                                                                                                                                        */
/*END PROCEDURE.                                                                                                                                                                              */
/*                                                                                                                                                                                            */
/*PROCEDURE ProcessItemPurchased:                                                                                                                                                             */
/*/*------------------------------------------------------------------------------                                                                                                            */
/*         Purpose:                                                                                                                                                                           */
/*         Notes:                                                                                                                                                                             */
/*------------------------------------------------------------------------------*/                                                                                                            */
/*  DEF VAR vVendorID AS CHAR NO-UNDO.                                                                                                                                                        */
/*  DEF VAR vCustomerID AS CHAR NO-UNDO.                                                                                                                                                      */
/*/*  DEFINE VARIABLE dCostTotal  AS DECIMAL   NO-UNDO.*/                                                                                                                                     */
/*/*  DEFINE VARIABLE dCostPerUOM AS DECIMAL   NO-UNDO.*/                                                                                                                                     */
/*/*  DEFINE VARIABLE dCostSetup  AS DECIMAL   NO-UNDO.*/                                                                                                                                     */
/*/*  DEFINE VARIABLE cCostUOM    AS CHARACTER NO-UNDO.*/                                                                                                                                     */
/*  DEFINE VARIABLE lError   AS LOGICAL   NO-UNDO.                                                                                                                                            */
/*  DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.                                                                                                                                            */
/*                                                                                                                                                                                            */
/*  outers:                                                                                                                                                                                   */
/*  FOR EACH ttItemPurchased                                                                                                                                                                  */
/*     WHERE (IF iplPromptRM THEN TRUE ELSE ttItemPurchased.this-is-a-rm EQ FALSE)                                                                                                            */
/*        BREAK BY ttItemPurchased.this-is-a-rm                                                                                                                                               */
/*        BY ttItemPurchased.frm                                                                                                                                                              */
/*        BY ttItemPurchased.blank-no                                                                                                                                                         */
/*        BY ttItemPurchased.i-no:                                                                                                                                                            */
/*                                                                                                                                                                                            */
/*                                                                                                                                                                                            */
/*                                                                                                                                                                                            */
/*      /*     lNextOuters = NO.             */                                                                                                                                               */
/*      /*     v-vendor-chosen-report = ?.   */                                                                                                                                               */
/*      /*     gvrWJobMat = ROWID(ttItemPurchased).*/                                                                                                                                         */
/*                                                                                                                                                                                            */
/*      /*      llFirstOfJobFrm = FIRST-OF(ttItemPurchased.frm).*/                                                                                                                            */
/*      /*      llFirstJobFrm = FIRST(ttItemPurchased.frm).     */                                                                                                                            */
/*                                                                                                                                                                                            */
/*      IF NOT ttItemPurchased.this-is-a-rm THEN DO:                                                                                                                                          */
/*        FIND itemfg NO-LOCK WHERE itemfg.company = cocode                                                                                                                                   */
/*                 AND itemfg.i-no = ttItemPurchased.rm-i-no NO-ERROR.                                                                                                                        */
/*        IF NOT AVAIL itemfg THEN RETURN.                                                                                                                                                    */
/*      END.                                                                                                                                                                                  */
/*                                                                                                                                                                                            */
/*      /* get vendItemCost */                                                                                                                                                                */
/*      FIND FIRST po-ord NO-LOCK                                                                                                                                                             */
/*          WHERE po-ord.company   EQ cocode                                                                                                                                                  */
/*          AND po-ord.po-no     EQ INT(ttItemPurchased.po-no)                                                                                                                                */
/*          NO-ERROR.                                                                                                                                                                         */
/*                                                                                                                                                                                            */
/*      /* blank vendor process? if po-ord.vend-no = "" then next? */                                                                                                                         */
/*                                                                                                                                                                                            */
/*      ASSIGN vVendorID = IF AVAIL po-ord THEN po-ord.vend-no else ""                                                                                                                        */
/*             vCustomerID = ""                                                                                                                                                               */
/*             .                                                                                                                                                                              */
/*      RUN GetVendorCost(cocode,                                                                                                                                                             */
/*          ttItemPurchased.i-no,                                                                                                                                                             */
/*          IF ttItemPurchased.this-is-a-rm THEN "RM" else "FG",                                                                                                                              */
/*          vVendorID ,                                                                                                                                                                       */
/*          vCustomerID,                                                                                                                                                                      */
/*          "",                                                                                                                                                                               */
/*          0,                                                                                                                                                                                */
/*          0,                                                                                                                                                                                */
/*          ttItemPurchased.Qty,                                                                                                                                                              */
/*          ttItemPurchased.qty-UOM,                                                                                                                                                          */
/*          itemfg.t-len,                                                                                                                                                                     */
/*          itemfg.t-wid,                                                                                                                                                                     */
/*          0,                                                                                                                                                                                */
/*          "IN",                                                                                                                                                                             */
/*          itemfg.weight-100 / 100,                                                                                                                                                          */
/*          "LB/EA",                                                                                                                                                                          */
/*          NO,                                                                                                                                                                               */
/*          OUTPUT ttItemPurchased.CostPerUOM,                                                                                                                                                */
/*          OUTPUT ttItemPurchased.CostSetup,                                                                                                                                                 */
/*          OUTPUT ttItemPurchased.CostUOM,                                                                                                                                                   */
/*          OUTPUT ttItemPurchased.CostTotal,                                                                                                                                                 */
/*          OUTPUT lError,                                                                                                                                                                    */
/*          OUTPUT ttItemPurchased.ReasonType).                                                                                                                                               */
/*                                                                                                                                                                                            */
/*      /*          MESSAGE                                */                                                                                                                                 */
/*      /*              "Total Cost: " opdCostTotal SKIP   */                                                                                                                                 */
/*      /*              "Cost Per UOM: " opdCostPerUOM SKIP*/                                                                                                                                 */
/*      /*              "Cost UOM: " opcCostUOM SKIP       */                                                                                                                                 */
/*      /*              "Cost Setup: " opdCostSetup SKIP   */                                                                                                                                 */
/*      /*              "Error: " oplError SKIP            */                                                                                                                                 */
/*      /*              "Message: " opcMessage SKIP        */                                                                                                                                 */
/*      /*              VIEW-AS ALERT-BOX.                 */                                                                                                                                 */
/*                                                                                                                                                                                            */
/*      /* ========= getting ttvendItemcost list                                                                                                                                              */
/*      cValidScopes = DYNAMIC-FUNCTION("GetValidScopes").                                                                                                                                    */
/*      cScope = ENTRY(iScopeEntry, cValidScopes).                                                                                                                                            */
/*      RUN BuildVendItemCosts(ipcCompany, cItemID, cItemType, cScope, lIncludeBlankVendor,                                                                                                   */
/*          10000, "EA",                                                                                                                                                                      */
/*          dDimLength, dDimWidth, dDimDepth, "IN",                                                                                                                                           */
/*          dBasisWeight, "LBS/MSF",                                                                                                                                                          */
/*          OUTPUT TABLE ttVendItemCost,                                                                                                                                                      */
/*          OUTPUT lError, OUTPUT cMessage).                                                                                                                                                  */
/*      FOR EACH ttVendItemCost                                                                                                                                                               */
/*          BY ttVendItemCost.costTotal:                                                                                                                                                      */
/*          DISPLAY ttVendItemCost.quantityTargetInVendorUOM ttVendItemCost.isValid ttVendItemCost.vendorID ttVendItemCost.costTotal ttVendItemCost.costPerVendorUOM ttVendItemCost.vendorUOM.*/
/*      END.                                                                                                                                                                                  */
/*      ==========*/                                                                                                                                                                          */
/*                                                                                                                                                                                            */
/*      /* set podate & duedate */                                                                                                                                                            */
/*      IF AVAIL po-ord THEN FIND vend NO-LOCK WHERE vend.company = cocode                                                                                                                    */
/*                                         AND vend.vend-no = po-ord.vend-no NO-ERROR.                                                                                                        */
/*      IF AVAILABLE vend THEN                                                                                                                                                                */
/*         ASSIGN ttItemPurchased.PoDate = TODAY                                                                                                                                              */
/*                ttItemPurchased.DueDate = TODAY + vend.disc-days.                                                                                                                           */
/*      ELSE ASSIGN ttItemPurchased.poDate = IF AVAILABLE oe-ord THEN oe-ord.ord-date ELSE job.start-date                                                                                     */
/*                  ttItemPurchased.DueDate = IF AVAILABLE oe-ord THEN oe-ord.ord-date ELSE job.start-date.                                                                                   */
/*                                                                                                                                                                                            */
/*                                                                                                                                                                                            */
/*                                                                                                                                                                                            */
/*      RUN createPoOrd (INPUT gvrOeOrd, OUTPUT gvrPoOrd, OUTPUT lNextOuters).                                                                                                                */
/*      IF lNextOuters THEN                                                                                                                                                                   */
/*      DO:                                                                                                                                                                                   */
/*           NEXT outers.                                                                                                                                                                     */
/*      END.                                                                                                                                                                                  */
/*      FIND po-ord NO-LOCK WHERE ROWID(po-ord) EQ gvrPoOrd NO-ERROR.                                                                                                                         */
/*      /* creates po-ordl based on gvlChoice */                                                                                                                                              */
/*      RUN createPoOrdl (INPUT gvrPoOrd,                                                                                                                                                     */
/*          INPUT gvrOeOrdl,                                                                                                                                                                  */
/*          INPUT ROWID(ttItemPurchased),                                                                                                                                                     */
/*          INPUT gvrItem,                                                                                                                                                                    */
/*          OUTPUT gvrItemfg).                                                                                                                                                                */
/*                                                                                                                                                                                            */
/*  END. /* for each ttItemPurchased */                                                                                                                                                       */
/*                                                                                                                                                                                            */
/*END PROCEDURE.                                                                                                                                                                              */
/*                                                                                                                                                                                            */
/*PROCEDURE promptCreatePoLine:                                                                                                                                                               */
/*    /*------------------------------------------------------------------------------                                                                                                        */
/*     Purpose:                                                                                                                                                                               */
/*     Notes:                                                                                                                                                                                 */
/*    ------------------------------------------------------------------------------*/                                                                                                        */
/*                                                                                                                                                                                            */
/*    DEFINE VARIABLE lCheckFgItemPoStatus AS LOGICAL NO-UNDO .                                                                                                                               */
/*    IF gvlDebug THEN                                                                                                                                                                        */
/*        PUT STREAM sDebug UNFORMATTED "Prompt create PO line " ttItemPurchased.i-no  SKIP.                                                                                                  */
/*                                                                                                                                                                                            */
/*    IF NOT ttItemPurchased.this-is-a-rm THEN                                                                                                                                                */
/*        RUN fg/GetItemfgPoStatus.p (INPUT cocode,                                                                                                                                           */
/*            INPUT ttItemPurchased.rm-i-no,"",NO,                                                                                                                                            */
/*            OUTPUT lCheckFgItemPoStatus).                                                                                                                                                   */
/*    IF NOT ttItemPurchased.this-is-a-rm THEN                                                                                                                                                */
/*        RUN pCheckFGItemCustHold(cocode,ttItemPurchased.rm-i-no,INPUT-OUTPUT lCheckFgItemPoStatus) .                                                                                        */
/*                                                                                                                                                                                            */
/*    gvlChoice = NO.                                                                                                                                                                         */
/*    IF gvcVendNo EQ ""                                                                                                                                                                      */
/*        AND ((v-autopo-sec AND ttItemPurchased.this-is-a-rm) OR (v-autofg-sec AND NOT ttItemPurchased.this-is-a-rm AND lCheckFgItemPoStatus ))                                              */
/*        AND NOT ttItemPurchased.isaset THEN                                                                                                                                                 */
/*    DO ON ENDKEY UNDO, LEAVE:                                                                                                                                                               */
/*        IF dOeAutoFG EQ 1 THEN                                                                                                                                                              */
/*            ASSIGN gvlChoice = TRUE                                                                                                                                                         */
/*                   ttItemPurchased.ReasonType = "Auto PO Line Created"                                                                                                                      */
/*                   ttItemPurchased.PurchaseType = "OeAutoFG:1".                                                                                                                             */
/*        ELSE DO:                                                                                                                                                                            */
/*            MESSAGE "Do you wish to create a PO line for " +                                                                                                                                */
/*                (IF ttItemPurchased.this-is-a-rm                                                                                                                                            */
/*                THEN ("Job/Form/RM#: " + TRIM(v-job) + "/" +                                                                                                                                */
/*                TRIM(STRING(ttItemPurchased.frm,"99")))                                                                                                                                     */
/*                ELSE ("Order/FG#: " +                                                                                                                                                       */
/*                TRIM(STRING(v-ord-no,">>>>>>>>>>")))) +                                                                                                                                     */
/*                "/" + TRIM(ttItemPurchased.rm-i-no) + "?"                                                                                                                                   */
/*                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE gvlChoice.                                                                                                                  */
/*            IF gvlChoice THEN                                                                                                                                                               */
/*                ASSIGN ttItemPurchased.ReasonType = "PO Line Create prompted"                                                                                                               */
/*                       ttItemPurchased.PurchaseType = "Prompt".                                                                                                                             */
/*        END.                                                                                                                                                                                */
/*    END. /* Prompt to create po line */                                                                                                                                                     */
/*                                                                                                                                                                                            */
/*END PROCEDURE.                                                                                                                                                                              */
/*                                                                                                                                                                                            */
/*PROCEDURE promptDropShip:                                                                                                                                                                   */
/*/*------------------------------------------------------------------------------                                                                                                            */
/*     Purpose:                                                                                                                                                                               */
/*     Notes:                                                                                                                                                                                 */
/*------------------------------------------------------------------------------*/                                                                                                            */
/*                                                                                                                                                                                            */
/*    ll-drop = NO.                                                                                                                                                                           */
/*    IF nk1-oeautopo-int EQ 1 THEN                                                                                                                                                           */
/*    DO:                                                                                                                                                                                     */
/*        IF dOeAutoFG EQ 1 THEN                                                                                                                                                              */
/*            ASSIGN ll-drop = TRUE                                                                                                                                                           */
/*                   ttItemPurchased.reasonType = ttItemPurchased.reasonType + ", Auto Drop Ship".                                                                                            */
/*        ELSE DO:                                                                                                                                                                            */
/*            MESSAGE "Is this a Drop Shipment?"                                                                                                                                              */
/*                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO                                                                                                                                    */
/*                UPDATE ll-drop.                                                                                                                                                             */
/*            IF ll-drop THEN ASSIGN ttItemPurchased.reasonType = ttItemPurchased.reasonType + ", Prompt Drop Ship".                                                                          */
/*        END.                                                                                                                                                                                */
/*    END.                                                                                                                                                                                    */
/*                                                                                                                                                                                            */
/*END PROCEDURE.                                                                                                                                                                              */
/*                                                                                                                                                                                            */
/*PROCEDURE PromptExistionPO:                                                                                                                                                                 */
/*    /*------------------------------------------------------------------------------                                                                                                        */
/*        Purpose:                                                                                                                                                                            */
/*        Parameters:  <none>                                                                                                                                                                 */
/*        Notes:                                                                                                                                                                              */
/*          Finds po-ord with a share-lock                                                                                                                                                    */
/*          requires GV gvlChoice and                                                                                                                                                         */
/*          sets gvlChoice                                                                                                                                                                    */
/*          May release po-ord                                                                                                                                                                */
/*      ------------------------------------------------------------------------------*/                                                                                                      */
/*                                                                                                                                                                                            */
/*    IF NOT gvlChoice AND nk1-oeautopo-log THEN                                                                                                                                              */
/*    DO:                                                                                                                                                                                     */
/*        IF dOeAutoFG EQ 1 THEN                                                                                                                                                              */
/*            gvlChoice = TRUE.                                                                                                                                                               */
/*        ELSE                                                                                                                                                                                */
/*            MESSAGE "PO exists for given Vendor and Date." SKIP                                                                                                                             */
/*                "Do you want to update existing PO? "                                                                                                                                       */
/*                VIEW-AS ALERT-BOX BUTTON YES-NO UPDATE gvlChoice.                                                                                                                           */
/*    END.                                                                                                                                                                                    */
/*                                                                                                                                                                                            */
/*    IF  nk1-oeautopo-log = NO THEN                                                                                                                                                          */
/*        gvlChoice = NO.                                                                                                                                                                     */
/*                                                                                                                                                                                            */
/*    IF gvlChoice THEN FIND CURRENT po-ord NO-ERROR.                                                                                                                                         */
/*    ELSE RELEASE po-ord.                                                                                                                                                                    */
/*                                                                                                                                                                                            */
/*END PROCEDURE.                                                                                                                                                                              */
/*                                                                                                                                                                                            */
/*PROCEDURE promptUpdPoNum:                                                                                                                                                                   */
/*/*------------------------------------------------------------------------------                                                                                                            */
/*         Purpose:     If PO Already Exists, ask to Update it                                                                                                                                */
/*         Parameters:  <none>                                                                                                                                                                */
/*         Notes:                                                                                                                                                                             */
/*           Inputs:                                                                                                                                                                          */
/*             Needs b-orderpo                                                                                                                                                                */
/*             Finds po-ord                                                                                                                                                                   */
/*             cocode                                                                                                                                                                         */
/*             gvcVendNo                                                                                                                                                                      */
/*             ll-drop                                                                                                                                                                        */
/*             Sets gvlChoice                                                                                                                                                                 */
/*             finds tt-eiv                                                                                                                                                                   */
/*                                                                                                                                                                                            */
/*------------------------------------------------------------------------------*/                                                                                                            */
/*    DEFINE INPUT  PARAMETER ipcCocode AS CHARACTER   NO-UNDO.                                                                                                                               */
/*    DEFINE INPUT  PARAMETER ipiPoNo AS INT NO-UNDO.                                                                                                                                         */
/*    DEFINE OUTPUT PARAMETER oprPoOrd AS ROWID       NO-UNDO.                                                                                                                                */
/*    DEFINE OUTPUT PARAMETER oprTT-eiv AS ROWID       NO-UNDO.                                                                                                                               */
/*    DEFINE OUTPUT PARAMETER oplNextOuters AS LOGICAL     NO-UNDO.                                                                                                                           */
/*                                                                                                                                                                                            */
/*    gvlChoice = NO.                                                                                                                                                                         */
/*    oplNextOuters = FALSE.                                                                                                                                                                  */
/*    FIND LAST po-ord NO-LOCK                                                                                                                                                                */
/*        WHERE po-ord.company   EQ cocode                                                                                                                                                    */
/*        AND po-ord.po-no     EQ ipiPoNo                                                                                                                                                     */
/*        AND po-ord.vend-no   EQ gvcVendNo                                                                                                                                                   */
/*        AND (po-ord.type     EQ "D" OR NOT ll-drop)                                                                                                                                         */
/*        NO-ERROR.                                                                                                                                                                           */
/*                                                                                                                                                                                            */
/*    IF AVAILABLE po-ord AND NOT po-ord.opened THEN                                                                                                                                          */
/*    DO:                                                                                                                                                                                     */
/*        oplNextOuters = TRUE.                                                                                                                                                               */
/*    END. /* If PO was found but was not opened */                                                                                                                                           */
/*    ELSE                                                                                                                                                                                    */
/*    DO:                                                                                                                                                                                     */
/*        gvlChoice = AVAILABLE po-ord.                                                                                                                                                       */
/*                                                                                                                                                                                            */
/*        IF gvlChoice AND po-ord.opened AND v-autopo-sec THEN                                                                                                                                */
/*        DO:                                                                                                                                                                                 */
/*            MESSAGE "Do you wish to update PO #" +                                                                                                                                          */
/*                TRIM(STRING(po-ord.po-no,">>>>>>>>")) + " for " +                                                                                                                           */
/*                (IF AVAILABLE job THEN ("Job/Form/RM#: " + TRIM(v-job) + "/" +                                                                                                              */
/*                TRIM(STRING(ttItemPurchased.frm,"99")))                                                                                                                                     */
/*                ELSE ("Order/FG#: " +                                                                                                                                                       */
/*                TRIM(STRING(v-ord-no,">>>>>>>>>>")))) +                                                                                                                                     */
/*                "/" + TRIM(ttItemPurchased.rm-i-no) + "?"                                                                                                                                   */
/*                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE gvlChoice.                                                                                                                  */
/*                                                                                                                                                                                            */
/*        END.                                                                                                                                                                                */
/*                                                                                                                                                                                            */
/*        IF AVAILABLE po-ord THEN                                                                                                                                                            */
/*        DO:                                                                                                                                                                                 */
/*            oprPoOrd = ROWID(po-ord).                                                                                                                                                       */
/*        END.                                                                                                                                                                                */
/*                                                                                                                                                                                            */
/*    END. /* NOT If PO was found but was not opened */                                                                                                                                       */
/*                                                                                                                                                                                            */
/*END PROCEDURE.                                                                                                                                                                              */
/*                                                                                                                                                                                            */
/*PROCEDURE ttItemFgFromJob:                                                                                                                                                                  */
/*    /*------------------------------------------------------------------------------                                                                                                        */
/*          Purpose:                                                                                                                                                                          */
/*          Parameters:  <none>                                                                                                                                                               */
/*          Notes:                                                                                                                                                                            */
/*          Input:                                                                                                                                                                            */
/*            job buffer                                                                                                                                                                      */
/*            b-tt-itemfg?                                                                                                                                                                    */
/*            bf-ordl                                                                                                                                                                         */
/*          Output:                                                                                                                                                                           */
/*            creates tt-itemfg records                                                                                                                                                       */
/*            create tt-fg-set                                                                                                                                                                */
/*        ------------------------------------------------------------------------------*/                                                                                                    */
/*    DEFINE INPUT  PARAMETER iprJob AS ROWID       NO-UNDO.                                                                                                                                  */
/*    DEFINE INPUT  PARAMETER iprOeOrdl AS ROWID    NO-UNDO.                                                                                                                                  */
/*    DEFINE INPUT  PARAMETER ipcFilIdSource AS CHARACTER   NO-UNDO.                                                                                                                          */
/*    DEFINE BUFFER bf-ordl FOR oe-ordl.                                                                                                                                                      */
/*    /* Check that cFilIDSource is JOB since job record may be available here    */                                                                                                          */
/*    /* even though the fil_id is related to oe-ordl and could cause a duplicate */                                                                                                          */
/*    /* tt-itemfg                                                                */                                                                                                          */
/*    FIND job NO-LOCK WHERE ROWID(job) EQ iprJob NO-ERROR.                                                                                                                                   */
/*    FIND bf-ordl NO-LOCK WHERE ROWID(bf-ordl) EQ iprOeOrdl NO-ERROR.                                                                                                                        */
/*                                                                                                                                                                                            */
/*    IF gvlDebug THEN                                                                                                                                                                        */
/*        PUT STREAM sDebug UNFORMATTED "In ttItemfgFromJob " SKIP.                                                                                                                           */
/*                                                                                                                                                                                            */
/*    IF AVAILABLE job AND ipcFilIdSource = "JOB" THEN                                                                                                                                        */
/*    DO:                                                                                                                                                                                     */
/*        FOR EACH job-hdr NO-LOCK                                                                                                                                                            */
/*            WHERE job-hdr.company EQ job.company                                                                                                                                            */
/*            AND job-hdr.job     EQ job.job                                                                                                                                                  */
/*            AND job-hdr.job-no  EQ job.job-no                                                                                                                                               */
/*            AND job-hdr.job-no2 EQ job.job-no2,                                                                                                                                             */
/*            FIRST itemfg NO-LOCK                                                                                                                                                            */
/*            WHERE itemfg.company EQ job-hdr.company                                                                                                                                         */
/*            AND itemfg.i-no    EQ job-hdr.i-no:                                                                                                                                             */
/*                                                                                                                                                                                            */
/*            FIND FIRST eb NO-LOCK WHERE                                                                                                                                                     */
/*                eb.company EQ job-hdr.company AND                                                                                                                                           */
/*                eb.est-no EQ job-hdr.est-no AND                                                                                                                                             */
/*                eb.stock-no EQ job-hdr.i-no AND                                                                                                                                             */
/*                eb.blank-no EQ job-hdr.blank-no AND                                                                                                                                         */
/*                eb.form-no EQ job-hdr.frm                                                                                                                                                   */
/*                NO-ERROR.                                                                                                                                                                   */
/*            IF gvlDebug THEN                                                                                                                                                                */
/*                PUT STREAM sDebug UNFORMATTED "Create tt-itemfg from job-hdr " itemfg.i-no " frm " job-hdr.frm " blank " job-hdr.blank-no SKIP.                                             */
/*                                                                                                                                                                                            */
/*            CREATE tt-itemfg.                                                                                                                                                               */
/*            BUFFER-COPY itemfg EXCEPT rec_key pur-man TO tt-itemfg                                                                                                                          */
/*                ASSIGN                                                                                                                                                                      */
/*                tt-itemfg.form-no  = job-hdr.frm                                                                                                                                            */
/*                tt-itemfg.blank-no = job-hdr.blank-no                                                                                                                                       */
/*                tt-itemfg.qty      = IF AVAILABLE bf-ordl THEN bf-ordl.qty ELSE job-hdr.qty                                                                                                 */
/*                tt-itemfg.row-id   = ROWID(itemfg).                                                                                                                                         */
/*            tt-itemfg.pur-man = IF AVAILABLE eb THEN eb.pur-man ELSE itemfg.pur-man.                                                                                                        */
/*        END.                                                                                                                                                                                */
/*                                                                                                                                                                                            */
/*        /* WFK - needs more explanation, why would this be found? */                                                                                                                        */
/*        /* was in original versions of program                    */                                                                                                                        */
/*        FIND b-tt-itemfg EXCLUSIVE-LOCK NO-ERROR.                                                                                                                                           */
/*                                                                                                                                                                                            */
/*        IF AVAILABLE b-tt-itemfg THEN                                                                                                                                                       */
/*        DO:                                                                                                                                                                                 */
/*            IF b-tt-itemfg.isaset THEN                                                                                                                                                      */
/*            DO:                                                                                                                                                                             */
/*                b-tt-itemfg.pur-man = NO.                                                                                                                                                   */
/*                                                                                                                                                                                            */
/*                FOR EACH b-jc-calc NO-LOCK                                                                                                                                                  */
/*                    WHERE b-jc-calc.reftable EQ "jc/jc-calc.p"                                                                                                                              */
/*                    AND b-jc-calc.company  EQ job.company                                                                                                                                   */
/*                    AND b-jc-calc.loc      EQ ""                                                                                                                                            */
/*                    AND b-jc-calc.code     EQ STRING(job.job,"999999999"),                                                                                                                  */
/*                    FIRST itemfg NO-LOCK                                                                                                                                                    */
/*                    WHERE itemfg.company EQ b-jc-calc.company                                                                                                                               */
/*                    AND itemfg.i-no    EQ b-jc-calc.code2                                                                                                                                   */
/*                    AND itemfg.pur-man:                                                                                                                                                     */
/*                    IF gvlDebug THEN                                                                                                                                                        */
/*                        PUT STREAM sDebug UNFORMATTED "Create tt-itemfg from tt-fg-set (job) " itemfg.i-no                                                                                  */
/*                            " frm " b-jc-calc.val[12] " blank " b-jc-calc.val[13] SKIP.                                                                                                     */
/*                    CREATE tt-itemfg.                                                                                                                                                       */
/*                    BUFFER-COPY itemfg EXCEPT rec_key TO tt-itemfg                                                                                                                          */
/*                        ASSIGN                                                                                                                                                              */
/*                        tt-itemfg.form-no  = b-jc-calc.val[12]                                                                                                                              */
/*                        tt-itemfg.blank-no = b-jc-calc.val[13]                                                                                                                              */
/*                        tt-itemfg.row-id   = ROWID(itemfg).                                                                                                                                 */
/*                                                                                                                                                                                            */
/*                    IF AVAILABLE bf-ordl THEN                                                                                                                                               */
/*                        FIND FIRST bf-itemfg NO-LOCK                                                                                                                                        */
/*                            WHERE bf-itemfg.company EQ bf-ordl.company                                                                                                                      */
/*                            AND bf-itemfg.i-no    EQ bf-ordl.i-no                                                                                                                           */
/*                            NO-ERROR.                                                                                                                                                       */
/*                    IF AVAILABLE bf-itemfg THEN                                                                                                                                             */
/*                        RUN fg/fullset.p (ROWID(bf-itemfg)).                                                                                                                                */
/*                                                                                                                                                                                            */
/*                    FOR EACH tt-fg-set WHERE tt-fg-set.part-no EQ b-jc-calc.code2:                                                                                                          */
/*                        tt-itemfg.qty = tt-itemfg.qty + (b-tt-itemfg.qty * tt-fg-set.part-qty-dec).                                                                                         */
/*                    END. /* each tt-fg-set */                                                                                                                                               */
/*                END. /* each b-jc-calc */                                                                                                                                                   */
/*            END. /* if b-tt-itemfg.isaset */                                                                                                                                                */
/*                                                                                                                                                                                            */
/*            IF b-tt-itemfg.pur-man EQ NO THEN DELETE b-tt-itemfg.                                                                                                                           */
/*        END. /* if avail bb-tt-itemfg */                                                                                                                                                    */
/*    END. /* if avail job... */                                                                                                                                                              */
/*                                                                                                                                                                                            */
/*END PROCEDURE.                                                                                                                                                                              */
/*                                                                                                                                                                                            */
/*PROCEDURE ValidatePO:                                                                                                                                                                       */
/*/*------------------------------------------------------------------------------                                                                                                            */
/* Purpose:                                                                                                                                                                                   */
/* Notes:                                                                                                                                                                                     */
/*------------------------------------------------------------------------------*/                                                                                                            */
/*   /*                                                                                                                                                                                       */
/*   CheckZeroQty                                                                                                                                                                             */
/*   brdLenCheck                                                                                                                                                                              */
/*   ValidMaxCost                                                                                                                                                                             */
/*    */                                                                                                                                                                                      */
/*                                                                                                                                                                                            */
/*END PROCEDURE.                                                                                                                                                                              */
/*                                                                                                                                                                                            */
/*/* =========  main block ========== */                                                                                                                                                      */
/*RUN doPOUI.                                                                                                                                                                                 */
/*RUN ValidatePO.                                                                                                                                                                             */
/*RUN doPO.*/