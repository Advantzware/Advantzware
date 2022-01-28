/*------------------------------------------------------------------------
    File        : EstimateProcs.p
    Purpose     : Start moving some repetitive code into common procedures

    Syntax      :

    Description : Will houses common procedures for calculating estimates and jobs

    Author(s)   : BV
    Created     : Thu Jun 14 18:19:14 EDT 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{est/ttGoto.i}
{est/ttCalcLayoutSize.i}
{est/ttEstSysConfig.i}

DEFINE VARIABLE gcTypeSingle AS CHARACTER NO-UNDO INITIAL "Single".
DEFINE VARIABLE gcTypeSet    AS CHARACTER NO-UNDO INITIAL "Set".
DEFINE VARIABLE gcTypeCombo  AS CHARACTER NO-UNDO INITIAL "Combo/Tandem".
DEFINE VARIABLE gcTypeMisc   AS CHARACTER NO-UNDO INITIAL "Miscellaneous".
DEFINE VARIABLE gcTypeWood   AS CHARACTER NO-UNDO INITIAL "Wood".
DEFINE VARIABLE gcTypeList   AS CHARACTER NO-UNDO. 
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fEstimate_GetEstimateType RETURNS CHARACTER 
	(ipiEstimateStructureType AS INTEGER,
	 ipcEstimateTypeID AS CHARACTER) FORWARD.

FUNCTION fEstimate_GetQuantityPerSet RETURNS DECIMAL 
	(BUFFER ipbf-eb FOR eb) FORWARD.

FUNCTION fEstimate_IsDepartment RETURNS LOGICAL 
	(ipcDepartment AS CHARACTER,
	 ipcDepartmentList AS CHARACTER EXTENT 4) FORWARD.

FUNCTION fEstimate_IsComboType RETURNS LOGICAL 
    (ipcEstType AS CHARACTER) FORWARD.

FUNCTION fEstimate_IsInk RETURNS LOGICAL 
	(ipcMaterialType AS CHARACTER,
	 ipcInkType AS CHARACTER) FORWARD.

FUNCTION fEstimate_IsMiscType RETURNS LOGICAL 
    (ipcEstType AS CHARACTER) FORWARD.

FUNCTION fEstimate_IsSetType RETURNS LOGICAL 
    (ipcEstType AS CHARACTER) FORWARD.

FUNCTION fEstimate_IsSingleType RETURNS LOGICAL
    (ipcEstType AS CHARACTER) FORWARD.

FUNCTION fEstimate_IsWoodType RETURNS LOGICAL 
    (ipcEstType AS CHARACTER) FORWARD.

/* ***************************  Main Block  *************************** */

ASSIGN 
    /*Build mapping from estimate type # to descriptive type*/ 
    gcTypeList = gcTypeSingle + "," + gcTypeSet + ","  + gcTypeCombo + "," + gcTypeCombo + "," + gcTypeSingle + "," + gcTypeSet + ","  + gcTypeCombo + "," + gcTypeCombo
    .
    
/* **********************  Internal Procedures  *********************** */

PROCEDURE Estimate_GetSystemDataForEstimate:
/*------------------------------------------------------------------------------
     Purpose: Returns the system data in Temp-tables
     Notes: If No data is setup in user specific tables then use system tables 
    ------------------------------------------------------------------------------*/

    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttEstCostCategory. 
    DEFINE OUTPUT PARAMETER TABLE FOR ttEstCostGroup. 
    DEFINE OUTPUT PARAMETER TABLE FOR ttEstCostGroupLevel. 
    
    DEFINE BUFFER bf-estCostCategory         FOR estCostCategory.
    DEFINE BUFFER bf-estCostCategorySystem   FOR estCostCategorySystem.
    DEFINE BUFFER bf-estCostGroup            FOR estCostGroup.
    DEFINE BUFFER bf-estCostGroupSystem      FOR estCostGroupSystem.
    DEFINE BUFFER bf-estCostGroupLevel       FOR estCostGroupLevel.
    DEFINE BUFFER bf-estCostGroupLevelSystem FOR estCostGroupLevelSystem.
    
    EMPTY TEMP-TABLE ttEstCostCategory.
    EMPTY TEMP-TABLE ttEstCostGroup.
    EMPTY TEMP-TABLE ttEstCostGroupLevel.
    
    /* Load the estCostCategorySystem data. If category data is setup in estCostCategory then overwrite it */
    FOR EACH bf-estCostCategorySystem NO-LOCK:
        
        IF CAN-FIND(FIRST ttEstCostCategory WHERE ttEstCostCategory.estCostCategoryID = bf-estCostCategorySystem.estCostCategoryID ) THEN
            NEXT.
        
            
        CREATE ttEstCostCategory.
        
        FIND FIRST bf-estCostCategory NO-LOCK
            WHERE bf-estCostCategory.estCostCategoryID = bf-estCostCategorySystem.estCostCategoryID NO-ERROR.
        
        IF AVAILABLE bf-estCostCategory THEN
            BUFFER-COPY bf-estCostCategory TO ttEstCostCategory.
            
        ELSE 
            BUFFER-COPY bf-estCostCategorySystem TO ttEstCostCategory.
    END.
    
    /* Load the estCostGroupSystem data. If category data is setup in estCostGroup then overwrite it */
    FOR EACH bf-estCostGroupSystem NO-LOCK:
        
        IF CAN-FIND(FIRST ttEstCostGroup WHERE ttEstCostGroup.estCostGroupID = bf-estCostGroupSystem.estCostGroupID ) THEN
            NEXT.
                    
        CREATE ttEstCostGroup.
        
        FIND FIRST bf-estCostGroup NO-LOCK
            WHERE bf-estCostGroup.estCostGroupID = bf-estCostGroupSystem.estCostGroupID NO-ERROR.
        
        IF AVAILABLE bf-estCostGroup THEN
            BUFFER-COPY bf-estCostGroup TO ttEstCostGroup.
            
        ELSE 
            BUFFER-COPY bf-estCostGroupSystem TO ttEstCostGroup.
    END.
   
    /* Load the estCostGroupSystem data. If category data is setup in estCostGroup then overwrite it */
    FOR EACH bf-estCostGroupLevelSystem NO-LOCK:
        
        IF CAN-FIND(FIRST ttEstCostGroupLevel WHERE ttEstCostGroupLevel.estCostGroupLevelID = bf-estCostGroupLevelSystem.estCostGroupLevelID ) THEN
            NEXT.
            
        CREATE ttEstCostGroupLevel.
            
        FIND FIRST bf-estCostGroupLevel NO-LOCK
            WHERE bf-estCostGroupLevel.estCostGroupLevelID = bf-estCostGroupLevelSystem.estCostGroupLevelID NO-ERROR.
        
        IF AVAILABLE bf-estCostGroupLevel THEN
            BUFFER-COPY bf-estCostGroupLevel TO ttEstCostGroupLevel.
            
        ELSE 
            BUFFER-COPY bf-estCostGroupLevelSystem TO ttEstCostGroupLevel.
    END.

END PROCEDURE.

PROCEDURE Estimate_GetVersionSettings:
    /*------------------------------------------------------------------------------
     Purpose: Gets settings to use the new estimate calc and prompt, given est buffer
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimateTypeID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplUseNew AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplUseNewPrompt AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE cReturn    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE iCEVersion AS INTEGER   NO-UNDO.

    RUN sys/ref/nk1look.p (ipcCompany, "CEVersion", "C" /* Character */, NO /* check by cust */, 
        INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
        OUTPUT cReturn, OUTPUT lFound).
    oplUseNew = lFound AND cReturn EQ "New".
 
    RUN sys/ref/nk1look.p (ipcCompany, "CEVersion", "I" /* Character */, NO /* check by cust */, 
        INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
        OUTPUT cReturn, OUTPUT lFound).
    IF lFound THEN 
        iCEVersion = INTEGER(cReturn).
        
    IF oplUseNew THEN 
        CASE iCEVersion:
            WHEN 1 THEN 
                ASSIGN 
                    oplUseNewPrompt = oplUseNew.
            WHEN 2 THEN 
                DO:
                    IF NOT DYNAMIC-FUNCTION("sfIsUserSuperAdmin") THEN 
                        oplUseNew = NO.            
                    ASSIGN 
                        oplUseNewPrompt = oplUseNew.
                END.
            WHEN 3 THEN 
                DO:
                    IF DYNAMIC-FUNCTION("sfIsUserSuperAdmin") THEN 
                        oplUseNewPrompt = YES.
                    ELSE 
                        oplUseNewPrompt = NO.            
                END.
            WHEN 4 THEN 
                DO:
                    IF NOT ipcEstimateTypeID EQ "MISC" THEN 
                        ASSIGN 
                            oplUseNew       = NO
                            oplUseNewPrompt = NO.
                END.
        END CASE.

END PROCEDURE.

PROCEDURE Estimate_GetQuantities:
    /*------------------------------------------------------------------------------
     Purpose: Load the eb table data to ttGoto temp-table
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcEstNo     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcDelimiter AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcQtyList   AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE iQtyCount AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bf-est-qty  FOR est-qty.
    
    FIND FIRST bf-est-qty
        WHERE bf-est-qty.company EQ ipcCompany
        AND bf-est-qty.est-no  EQ ipcEstNo
        NO-LOCK NO-ERROR.
    
    RUN pBuildQuantityList(BUFFER bf-est-qty, 1, ipcDelimiter, OUTPUT opcQtyList). 

END.

PROCEDURE Estimate_GetQuantitiesForEstMaterial:
    /*------------------------------------------------------------------------------
     Purpose: Load the eb table data to ttGoto temp-table
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriEstMaterial AS ROWID NO-UNDO.
    DEFINE INPUT  PARAMETER ipcDelimiter AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcQtyList   AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE iQtyCount AS INTEGER NO-UNDO.
    DEFINE VARIABLE dMultiplier AS DECIMAL NO-UNDO.
    
    DEFINE BUFFER bf-est-qty FOR est-qty.
    DEFINE BUFFER bf-estMaterial FOR estMaterial.
    
    FIND bf-estMaterial NO-LOCK 
        WHERE ROWID(bf-estMaterial) EQ ipriEstMaterial
        NO-ERROR.
    IF NOT AVAILABLE bf-estMaterial THEN RETURN. 
    IF bf-estMaterial.quantityPer EQ "L" THEN 
        opcQtyList = STRING(bf-estMaterial.quantity).
    ELSE DO:
        FIND FIRST bf-est-qty NO-LOCK
            WHERE bf-est-qty.company EQ bf-estMaterial.company
            AND bf-est-qty.est-no  EQ bf-estMaterial.estimateNo
            NO-ERROR.
        IF bf-estMaterial.quantityPer EQ "E" THEN
            dMultiplier = bf-estMaterial.quantity * (1 + bf-estMaterial.wastePercent / 100). 
        ELSE 
            dMultiplier = 1.
        RUN pBuildQuantityList(BUFFER bf-est-qty, dMultiplier, ipcDelimiter, OUTPUT opcQtyList).
    END.
END.

PROCEDURE Estimate_LoadEstToTT:
/*------------------------------------------------------------------------------
 Purpose: Load the eb table data to ttGoto temp-table
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcEstNo   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttGoto. 
   
    DEFINE VARIABLE dReqQty  AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dPartQty AS DECIMAL NO-UNDO.

    DEFINE BUFFER bf-eb  FOR eb.
    DEFINE BUFFER bf-est FOR est.
    DEFINE BUFFER bf-ef  FOR ef.
    
    EMPTY TEMP-TABLE ttGoto.
    
    FIND FIRST bf-est NO-LOCK
         WHERE bf-est.company EQ ipcCompany
           AND bf-est.est-no  EQ ipcEstNo 
        NO-ERROR.
    
    FOR EACH bf-eb NO-LOCK 
        WHERE bf-eb.company EQ ipcCompany
          AND bf-eb.est-no  EQ ipcEstNo:
        /* Forms with zero values will be ignored */
        IF bf-eb.form-no EQ 0 THEN
            NEXT.
        
        CREATE ttGoto.
        ASSIGN
            ttGoto.company      = bf-eb.company
            ttGoto.location     = bf-eb.loc
            ttGoto.estNo        = bf-eb.est-no
            ttGoto.formNo       = bf-eb.form-no
            ttGoto.blankNo      = bf-eb.blank-no
            ttGoto.formNoOrig   = bf-eb.form-no
            ttGoto.blankNoOrig  = bf-eb.blank-no
            ttGoto.partNo       = bf-eb.part-no
            ttGoto.partDesc     = bf-eb.part-dscr1
            ttGoto.reqQty       = bf-eb.bl-qty
            ttGoto.reqQtyAdj    = bf-eb.reqQtyAdj
            ttGoto.numWid       = bf-eb.num-wid
            ttGoto.numLen       = bf-eb.num-len
            ttGoto.numUp        = bf-eb.num-up
            ttGoto.yieldRequest = bf-eb.yrprice
            ttGoto.yldQty       = bf-eb.yld-qty            
            ttGoto.eQty         = bf-eb.eqty
            ttGoto.ebRowid      = ROWID(bf-eb)
            .
        
        IF AVAILABLE bf-est THEN
            ttGoto.estType = bf-est.est-type.

        FIND FIRST bf-ef NO-LOCK
             WHERE bf-ef.company EQ bf-eb.company
               AND bf-ef.loc     EQ bf-eb.loc
               AND bf-ef.est-no  EQ bf-eb.est-no
               AND bf-ef.form-no EQ bf-eb.form-no
             NO-ERROR.
        IF AVAILABLE bf-ef THEN
            ASSIGN
                ttGoto.board     = bf-ef.board
                ttGoto.boardDesc = bf-ef.brd-dscr
                .
        
        IF ttGoto.estType GE 5 THEN
            ASSIGN
                ttGoto.numWid = bf-eb.num-len
                ttGoto.numLen = bf-eb.num-wid
                .

        IF ttGoto.estType EQ 2 OR ttGoto.estType EQ 6 THEN 
        DO:
            IF ttGoto.estType EQ 2 THEN
                ASSIGN
                    dReqQty  = bf-eb.bl-qty
                    dPartQty = bf-eb.cust-%
                    .
            ELSE
                ASSIGN
                    dReqQty  = bf-est.est-qty[1]
                    dPartQty = bf-eb.quantityPerSet
                    .
    
            dPartQty = IF dPartQty LT 0 THEN
                           (1 / (dPartQty * -1))
                       ELSE
                           dPartQty.
    
            ttGoto.reqQty = dReqQty * dPartQty.
        END.
    END.
END PROCEDURE.

PROCEDURE Estimate_UpdateEstDependencies:
/*------------------------------------------------------------------------------
 Purpose: This is replication of include file sys/inc/xeb-form.i 
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLocation   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcEstNo      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiFormNo     AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiBlankNo    AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdEQty       AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiEstType    AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiNewFormNo  AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiNewBlankNo AS INTEGER   NO-UNDO.    

    DEFINE BUFFER bf-est-flm         FOR est-flm.
    DEFINE BUFFER bf-ef              FOR ef.
    DEFINE BUFFER bf-est-prep        FOR est-prep.
    DEFINE BUFFER bf-est-op          FOR est-op.
    DEFINE BUFFER bf-est-inst        FOR est-inst.
    DEFINE BUFFER bf-box-design-line FOR box-design-line.
    DEFINE BUFFER bf-box-design-hdr  FOR box-design-hdr.
    DEFINE BUFFER bf-reftable        FOR reftable.
    DEFINE BUFFER bf-est             FOR est.
    DEFINE BUFFER bf-notes           FOR notes.
    DEFINE BUFFER bf-ef-nsh          FOR ef-nsh.
    DEFINE BUFFER bf-e-itemfg-vend   FOR e-itemfg-vend.
    
    DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.
    
    FOR EACH bf-est-flm
        WHERE bf-est-flm.company EQ ipcCompany
          AND bf-est-flm.est-no  EQ ipcEstNo
          AND bf-est-flm.snum    EQ ipiFormNo
          AND bf-est-flm.bnum    EQ ipiBlankNo:
        ASSIGN
            bf-est-flm.snum = ipiNewFormNo
            bf-est-flm.bnum = ipiNewBlankNo
            .
    END.

    FOR EACH bf-ef
        WHERE bf-ef.company EQ ipcCompany
          AND bf-ef.est-no  EQ ipcEstNo
          AND bf-ef.form-no EQ ipiFormNo:
        IF ipiNewBlankNo NE 0 THEN DO:
            DO iIndex = 1 TO EXTENT(bf-ef.leaf):
                IF bf-ef.leaf-bnum[iIndex] EQ ipiBlankNo THEN
                    bf-ef.leaf-bnum[iIndex] = ipiNewBlankNo.
            END.
            DO iIndex = 1 TO EXTENT(bf-ef.mis-cost):
                IF bf-ef.mis-bnum[iIndex] EQ ipiBlankNo THEN
                    bf-ef.mis-bnum[iIndex] = ipiNewBlankNo.
            END.
        END.
        DO iIndex = 1 TO EXTENT(bf-ef.leaf):
            IF bf-ef.leaf[iIndex] NE "" THEN
                bf-ef.leaf-snum[iIndex] = ipiNewFormNo.
        END.
        DO iIndex = 1 TO EXTENT(bf-ef.mis-cost):
            IF bf-ef.mis-cost[iIndex] NE "" THEN
                bf-ef.mis-snum[iIndex] = ipiNewFormNo.
        END.
    END.

    FOR EACH bf-est-prep
        WHERE bf-est-prep.company EQ ipcCompany
          AND bf-est-prep.est-no  EQ ipcEstNo
          AND bf-est-prep.s-num   EQ ipiFormNo
          AND bf-est-prep.b-num   EQ ipiBlankNo:
        ASSIGN
            bf-est-prep.s-num = ipiNewFormNo
            bf-est-prep.b-num = ipiNewBlankNo
            .
    END.

    FOR EACH bf-est-op
        WHERE bf-est-op.company EQ ipcCompany
          AND bf-est-op.est-no  EQ ipcEstNo
          AND (bf-est-op.qty    EQ ipdEQty OR ipiEstType GT 1)
          AND bf-est-op.s-num   EQ ipiFormNo
          AND bf-est-op.b-num   EQ ipiBlankNo:
        ASSIGN
            bf-est-op.s-num = ipiNewFormNo
            bf-est-op.b-num = ipiNewBlankNo
            .
    END.

    IF ipiNewBlankNo EQ 0 THEN DO:
        FOR EACH bf-est-inst
            WHERE bf-est-inst.company EQ ipcCompany
              AND bf-est-inst.est-no  EQ ipcEstNo
              AND bf-est-inst.line    EQ ipiFormNo:
            bf-est-inst.line = ipiNewFormNo.
        END.
    END.
    
    FOR EACH bf-box-design-line
        WHERE bf-box-design-line.design-no EQ 0
          AND bf-box-design-line.company   EQ ipcCompany
          AND bf-box-design-line.est-no    EQ ipcEstNo
          AND bf-box-design-line.form-no   EQ ipiFormNo
          AND bf-box-design-line.blank-no  EQ ipiBlankNo:
        ASSIGN
            bf-box-design-line.form-no  = ipiNewFormNo
            bf-box-design-line.blank-no = ipiNewBlankNo
            .
    END.

    FOR EACH bf-box-design-hdr
        WHERE bf-box-design-hdr.design-no EQ 0
          AND bf-box-design-hdr.company   EQ ipcCompany
          AND bf-box-design-hdr.est-no    EQ ipcEstNo
          AND bf-box-design-hdr.form-no   EQ ipiFormNo
          AND bf-box-design-hdr.blank-no  EQ ipiBlankNo:
        ASSIGN
            bf-box-design-hdr.form-no  = ipiNewFormNo
            bf-box-design-hdr.blank-no = ipiNewBlankNo
            .
    END.

    FOR EACH bf-reftable
        WHERE bf-reftable.reftable EQ "PLATE/FOUNTAIN"
          AND bf-reftable.company  EQ ipcCompany
          AND bf-reftable.loc      EQ ipcEstNo
          AND bf-reftable.code2    EQ STRING(ipiFormNo,"9999999999") + STRING(ipiBlankNo,"9999999999"):
        bf-reftable.code2 = STRING(ipiNewFormNo,"9999999999") + STRING(ipiNewBlankNo,"9999999999").
    END.

    IF ipiNewBlankNo EQ 0 THEN DO:
        FOR EACH bf-reftable
            WHERE bf-reftable.reftable EQ "bf-est-MISC"
              AND bf-reftable.company  EQ ipcCompany
              AND bf-reftable.loc      EQ ipcLocation
              AND bf-reftable.code     EQ TRIM(ipcEstNo) + STRING(ipiFormNo,"/99"):
            bf-reftable.code = TRIM(ipcEstNo) + STRING(ipiNewFormNo,"/99").
        END.
    END.

    FIND FIRST bf-est NO-LOCK
         WHERE bf-est.company   EQ ipcCompany
           AND bf-est.est-no EQ ipcEstNo
         NO-ERROR.

    IF AVAILABLE bf-est AND ipiNewBlankNo EQ 0 THEN DO:
        FOR EACH bf-notes
            WHERE bf-notes.rec_key      EQ bf-est.rec_key
              AND bf-notes.note_form_no EQ ipiFormNo
              AND bf-notes.note_form_no NE 0:
            bf-notes.note_form_no = ipiNewFormNo.
        END.
    END.

    FOR EACH bf-reftable
        WHERE bf-reftable.reftable EQ "cedepth"
          AND bf-reftable.company  EQ ipcCompany
          AND bf-reftable.loc      EQ ipcEstNo
          AND bf-reftable.code     EQ STRING(ipiFormNo,"9999999999")
          AND bf-reftable.code2    EQ STRING(ipiBlankNo,"9999999999"):
        ASSIGN
            bf-reftable.code  = STRING(ipiNewFormNo,"9999999999")
            bf-reftable.code2 = STRING(ipiNewBlankNo,"9999999999")
            .
    END.

    IF ipiNewBlankNo EQ 0 THEN DO:
        FOR EACH bf-ef-nsh
            WHERE bf-ef-nsh.company EQ ipcCompany
              AND bf-ef-nsh.est-no  EQ ipcEstNo
              AND bf-ef-nsh.form-no EQ ipiFormNo:
            bf-ef-nsh.form-no = ipiNewFormNo.
        END.
    END.

    FOR EACH bf-e-itemfg-vend
        WHERE bf-e-itemfg-vend.company  EQ ipcCompany
          AND bf-e-itemfg-vend.est-no   EQ ipcEstNo
          AND bf-e-itemfg-vend.form-no  EQ ipiFormNo
          AND bf-e-itemfg-vend.blank-no EQ ipiBlankNo:
        ASSIGN
            bf-e-itemfg-vend.form-no  = ipiNewFormNo
            bf-e-itemfg-vend.blank-no = ipiNewBlankNo
            .
    END.

    RELEASE bf-est-flm.
    RELEASE bf-ef.
    RELEASE bf-est-prep.
    RELEASE bf-est-op.
    RELEASE bf-est-inst.
    RELEASE bf-box-design-line.
    RELEASE bf-box-design-hdr.
    RELEASE bf-reftable.
    RELEASE bf-est.
    RELEASE bf-notes.
    RELEASE bf-ef-nsh.
    RELEASE bf-e-itemfg-vend.    
END PROCEDURE.

PROCEDURE Estimate_UpdateEstFromTT:
/*------------------------------------------------------------------------------
 Purpose: Updates the eb and it's dependent table records for an estimate from
          a given temp-table 
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER TABLE FOR ttGoto.
    
    DEFINE VARIABLE iOldNumUp         AS INTEGER NO-UNDO.
    DEFINE VARIABLE isFormBlankChange AS LOGICAL NO-UNDO.
    DEFINE VARIABLE iIndex            AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bf-eb     FOR eb.
    DEFINE BUFFER bf-ef     FOR ef.
    DEFINE BUFFER bf-new-ef FOR ef.
    DEFINE BUFFER bf-est    FOR est.
    DEFINE BUFFER bf-est-op FOR est-op.

    FOR EACH ttGoto
        BY ttGoto.formNo:
        FIND FIRST bf-eb EXCLUSIVE-LOCK
             WHERE ROWID(bf-eb) EQ ttGoto.ebRowID
             NO-ERROR.
        IF AVAILABLE bf-eb THEN DO:
            IF NOT isFormBlankChange AND (bf-eb.form-no NE ttGoto.formNo OR bf-eb.blank-no NE ttGoto.blankNo) THEN
                isFormBlankChange = TRUE.
                
            ASSIGN
                iOldNumUp       = bf-eb.num-up                
                bf-eb.num-wid   = ttGoto.numWid
                bf-eb.num-len   = ttGoto.numLen
                bf-eb.num-up    = ttGoto.numUp
                bf-eb.yrprice   = ttGoto.yieldRequest
                bf-eb.reqQtyAdj = ttGoto.reqQtyAdj
                bf-eb.part-no   = ttGoto.partNo
                .
            
            IF ttGoto.estType GE 5 THEN 
                ASSIGN
                    bf-eb.num-wid = ttGoto.numLen
                    bf-eb.num-len = ttGoto.numWid
                    .

            IF NOT (ttGoto.estType EQ 2 OR ttGoto.estType EQ 6) THEN
                ASSIGN
                    bf-eb.bl-qty  = ttGoto.reqQty
                    bf-eb.yld-qty = ttGoto.calcYldQty
                    .                                    

            IF iOldNumUp NE ttGoto.numUp THEN          
                ASSIGN 
                    bf-eb.die-in = bf-eb.die-in / iOldNumUp  
                    bf-eb.die-in = bf-eb.die-in * ttGoto.numUp
                    .
        END.
    END.

    IF isFormBlankChange THEN DO:
        /* Creates ef record and updates all the dependent tables of estimate */
        FOR EACH ttGoto
            BREAK BY ttGoto.formNo:
            IF FIRST-OF(ttGoto.formNo) THEN DO:
                FIND FIRST bf-eb NO-LOCK
                     WHERE ROWID(bf-eb) EQ ttGoto.ebRowID
                     NO-ERROR.
                IF NOT AVAILABLE bf-eb THEN
                    NEXT.

                IF CAN-FIND (FIRST ef
                             WHERE ef.company EQ ttGoto.company
                               AND ef.est-no  EQ ttGoto.estNo
                               AND ef.form-no EQ ttGoto.formNo) THEN
                    NEXT.

                FIND FIRST bf-ef NO-LOCK
                     WHERE bf-ef.company EQ ttGoto.company
                       AND bf-ef.est-no  EQ ttGoto.estNo
                       AND bf-ef.form-no EQ ttGoto.formNoOrig
                     NO-ERROR.
                IF NOT AVAILABLE bf-ef THEN
                    NEXT.

                CREATE bf-new-ef.
                BUFFER-COPY bf-ef EXCEPT rec_key TO bf-new-ef
                ASSIGN
                    bf-new-ef.form-no   = ttGoto.formNo
                    bf-new-ef.blank-qty = 1
                    .
                RUN Estimate_UpdateEstDependencies(
                    INPUT bf-eb.company,
                    INPUT bf-eb.loc,
                    INPUT bf-eb.est-no,
                    INPUT ttGoto.formNoOrig,
                    INPUT 0,
                    INPUT bf-eb.eqty,
                    INPUT bf-eb.est-type,
                    INPUT ttGoto.formNo,
                    INPUT 0
                    ).
            END.
        END.
        
        FOR EACH ttGoto
            BREAK BY ttGoto.formNo:
            
            IF ttGoto.formNo EQ ttGoto.formNoOrig AND 
               ttGoto.blankNo EQ ttGoto.blankNoOrig THEN
                NEXT.
                 
            FIND FIRST bf-eb EXCLUSIVE-LOCK
                 WHERE ROWID(bf-eb)EQ ttGoto.ebRowid
                 NO-ERROR.
            IF NOT AVAILABLE bf-eb THEN
                NEXT.
            
            IF bf-eb.form-no NE ttGoto.formNo THEN
                RUN Estimate_UpdateEstDependencies(
                    INPUT bf-eb.company,
                    INPUT bf-eb.loc,
                    INPUT bf-eb.est-no,
                    INPUT ttGoto.formNoOrig,
                    INPUT 0,
                    INPUT bf-eb.eqty,
                    INPUT bf-eb.est-type,
                    INPUT ttGoto.formNo,
                    INPUT 0
                    ).                

            ttGoto.blankNo = (ttGoto.blankNo * 1000) + (1 * (IF ttGoto.blankNo LT bf-eb.blank-no THEN -1 ELSE 1)).
            
            /* Assigns blank-no below, but this gets reset by est/resetf&b */
            /* Updates est-flm and ef */
            RUN Estimate_UpdateEstDependencies(
                INPUT bf-eb.company,
                INPUT bf-eb.loc,
                INPUT bf-eb.est-no,
                INPUT ttGoto.formNoOrig,
                INPUT ttGoto.blankNoOrig,
                INPUT bf-eb.eqty,
                INPUT bf-eb.est-type,
                INPUT ttGoto.formNo,
                INPUT ttGoto.blankNo * 1000
                ).          
           
            /* Assigning blank-no with a large value for now, as there will be conflict with unique
               index while assigning with updated blank value. This will assigned later with updated
               value in est/reset&fb.p  */
            ASSIGN
                bf-eb.form-no  = ttGoto.formNo
                bf-eb.blank-no = ttGoto.blankNo * 1000
                .

            RUN pUpdateFormBoard (
                INPUT bf-eb.company,    
                INPUT bf-eb.est-no,   
                INPUT bf-eb.eqty,   
                INPUT ttGoto.formNoOrig,
                INPUT ttGoto.board,
                INPUT ttGoto.boardDesc
                ).                                
        END. 
        
        FIND FIRST ttGoto NO-ERROR.
        IF NOT AVAILABLE ttGoto THEN
            LEAVE.

        FIND FIRST bf-est NO-LOCK
             WHERE bf-est.company EQ ttGoto.company
               AND bf-est.est-no  EQ ttGoto.estNo
             NO-ERROR.
        IF NOT AVAILABLE bf-est THEN
            LEAVE.

        RUN est/resetf&b.p (ROWID(bf-est), NO).
                
        iIndex = 0.
        FOR EACH bf-est-op EXCLUSIVE-LOCK
            WHERE bf-est-op.company EQ bf-est.company
              AND bf-est-op.est-no  EQ bf-est.est-no
              AND bf-est-op.line    LT 500
            BY bf-est-op.qty
            BY bf-est-op.s-num
            BY bf-est-op.b-num
            BY bf-est-op.d-seq
            BY bf-est-op.op-pass
            BY bf-est-op.rec_key:
            ASSIGN
                iIndex         = iIndex + 1
                bf-est-op.line = iIndex
                .
        END.                        
    END.
    
    FOR EACH ttGoto
        BREAK BY ttGoto.formNo:
        IF FIRST-OF(ttGoto.formNo) THEN
            RUN Estimate_UpdateEfFormQty (
                INPUT ttGoto.company,
                INPUT ttGoto.estNo,
                INPUT ttGoto.formNo
                ).
    END.

    RELEASE bf-eb.
    RELEASE bf-ef.
    RELEASE bf-new-ef.
    RELEASE bf-est.
    RELEASE bf-est-op.
        
END PROCEDURE.

PROCEDURE Estimate_GetEstimateDir:
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcBaseDir AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcSubDir  AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE dSubDir AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lFound  AS LOGICAL NO-UNDO.
    
    RUN sys/ref/nk1Look.p (
        ipcCompany,
        "CEBrowse",
        "C",
        NO,
        NO,
        "",
        "",
        OUTPUT opcBaseDir,
        OUTPUT lFound
        ).
    RUN sys/ref/nk1Look.p (
        ipcCompany,
        "CEBrowse",
        "D",
        NO,
        NO,
        "",
        "",
        OUTPUT opcSubDir,
        OUTPUT lFound
        ).    
    IF opcBaseDir EQ "" THEN
    opcBaseDir = "users\".
    
    ASSIGN 
          opcBaseDir = REPLACE(opcBaseDir,"/","\") /*replace slashes in wrong direction*/
          opcBaseDir = TRIM(opcBaseDir,"\") + "\"  /*ensure there is a slash on the end*/
          dSubDir    = DECIMAL(opcSubDir)
          .
    
    IF dSubDir EQ 0 THEN
    dSubDir = YEAR(TODAY) + MONTH(TODAY) / 100.

    ASSIGN
        opcSubDir = STRING(dSubDir,"9999.99")
        opcSubDir = opcBaseDir + opcSubDir + "\"
        .
    FILE-INFO:FILE-NAME = opcSubDir.
    IF FILE-INFO:FULL-PATHNAME EQ ? THEN
    OS-CREATE-DIR VALUE(opcSubDir).        

END PROCEDURE.

PROCEDURE Estimate_UpdateEfFormLayout:
    /*------------------------------------------------------------------------------
     Purpose: This procedure will update the ef record's dimension fields for a given
              estimate number. This code is to replace calc-dim.p programs,
              where it updates the EF and eb fields for an estimate.
              It calculates EF Gross, net, die size and other dimension fields
     Notes: This will be called across codebase to calculate layout fields
    ------------------------------------------------------------------------------*/

    DEFINE PARAMETER BUFFER ipbf-ef FOR ef.
    DEFINE PARAMETER BUFFER ipbf-eb FOR eb.
    
    IF NOT AVAILABLE ipbf-ef OR NOT AVAILABLE ipbf-eb THEN
        RETURN.

    RUN pUpdateEfFormLayout (BUFFER ipbf-ef, BUFFER ipbf-eb, INPUT NO).
    
    

END PROCEDURE.

PROCEDURE Estimate_UpdateEfFormLayoutSizeOnly:
    /*------------------------------------------------------------------------------
     Purpose: This procedure will update the ef record's dimension fields for a given
              estimate number. This code is to replace calc-dim1.p programs,
              where it updates the EF fields for an estimate.
              It calculates EF Gross, net, die size and other dimension fields
     Notes: It won't update Num On or Num Out fields
    ------------------------------------------------------------------------------*/

    DEFINE PARAMETER BUFFER ipbf-ef FOR ef.
    DEFINE PARAMETER BUFFER ipbf-eb FOR eb.
    
    
    RUN pUpdateEfFormLayout (BUFFER ipbf-ef, BUFFER ipbf-eb, INPUT YES).

END PROCEDURE.



PROCEDURE Estimate_UpdateEfFormQty PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: This procedure will update the ef record's blank-qty for a given
          estimate number. This code is a copy of a partial code in write.trg/ef.p,
          where it updates the blank-qty of ef records for a singe form.
          This code will update the form-qty for all the ef records of an estimate. 
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER  ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER  ipcEstNo   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER  ipiFormNo  AS INTEGER   NO-UNDO.
    
    DEFINE BUFFER bf-ef FOR ef.
    DEFINE BUFFER bf-eb FOR eb.
    
    DISABLE TRIGGERS FOR LOAD OF bf-ef.
    
    DEFINE VARIABLE iBlankCnt AS INTEGER NO-UNDO.
    
    FIND FIRST bf-ef EXCLUSIVE-LOCK
         WHERE bf-ef.company EQ ipcCompany
           AND bf-ef.est-no  EQ ipcEstNo
           AND bf-ef.form-no EQ ipiFOrmNo
         NO-ERROR.
    IF AVAILABLE bf-ef THEN DO:         
        FOR EACH bf-eb NO-LOCK
            WHERE bf-eb.company EQ ipcCompany
              AND bf-eb.est-no  EQ ipcEstNo
              AND bf-eb.form-no EQ ipiFormNo:
            iBlankCnt = iBlankCnt + 1.
        END.
        bf-ef.blank-qty = iBlankCnt.
    END.
    
    RELEASE bf-ef.
    RELEASE bf-eb.
END PROCEDURE.

PROCEDURE pBuildQuantityList PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:  Given an est-qty buffer, multiplier and delimiter, return a delimeter
    separated list of quantities
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-est-qty FOR est-qty.
    DEFINE INPUT PARAMETER ipdMultiplier AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcDelimiter AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcQtyList AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE iQtyCount AS INTEGER NO-UNDO.
    DEFINE VARIABLE iQty AS INTEGER NO-UNDO.
    
    IF AVAILABLE ipbf-est-qty THEN
    DO iQtyCount = 1 TO 20:
        IF ipbf-est-qty.qty[iQtyCount] NE 0 THEN DO:
            iQty = ipbf-est-qty.qty[iQtyCount].
            IF ipdMultiplier GT 0 THEN 
                iQty = INTEGER(ipdMultiplier * iQty).        
            opcQtyList = opcQtyList + ipcDelimiter + STRING(iQty).
        END.
    END.
    
    opcQtyList = TRIM(opcQtyList, ipcDelimiter).
    
END PROCEDURE.

PROCEDURE pUpdateEfFormLayout PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ef FOR ef.
    DEFINE PARAMETER BUFFER ipbf-eb FOR eb.
    DEFINE INPUT  PARAMETER iplCalcSizeOnly AS LOGICAL NO-UNDO.
    
    IF NOT AVAILABLE ipbf-ef OR NOT AVAILABLE ipbf-eb THEN
        RETURN.

    RUN est/CalcLayoutSize.p (INPUT ROWID(ipbf-ef),
        INPUT ROWID(ipbf-eb),
        OUTPUT TABLE ttLayoutSize).
    
        
    FOR FIRST ttLayoutSize:
        
        ASSIGN
            ipbf-ef.lsh-len  = ttLayoutSize.dLayoutSheetLength    
            ipbf-ef.lsh-wid  = ttLayoutSize.dLayoutSheetWidth     
            ipbf-ef.nsh-len  = ttLayoutSize.dNetSheetLength       
            ipbf-ef.nsh-wid  = ttLayoutSize.dNetSheetWidth        
            ipbf-ef.nsh-dep  = ttLayoutSize.dNetSheetDepth        
            ipbf-ef.gsh-len  = ttLayoutSize.dGrossSheetLength     
            ipbf-ef.gsh-wid  = ttLayoutSize.dGrossSheetWidth      
            ipbf-ef.gsh-dep  = ttLayoutSize.dGrossSheetDepth      
            ipbf-ef.trim-l   = ttLayoutSize.dDieSizeLength        
            ipbf-ef.trim-w   = ttLayoutSize.dDieSizeWidth         
            ipbf-ef.trim-d   = ttLayoutSize.dDieSizeDepth         
            ipbf-ef.roll-wid = ttLayoutSize.dRollWidth            
            ipbf-ef.die-in   = ttLayoutSize.dDieInchesRequired
            ipbf-eb.num-up   = ttLayoutSize.iBlankNumUp           
            . 
            
        /* Assign Num ON and Num Out fields only */    
        IF iplCalcSizeOnly = NO THEN 
            ASSIGN
                ipbf-ef.i-code  = ttLayoutSize.cBoardItemCode        
                ipbf-ef.weight  = ttLayoutSize.cBoardItemBasisWeight 
                ipbf-ef.cal     = ttLayoutSize.dBoardItemCaliper     
                ipbf-ef.roll    = ttLayoutSize.IsRollMaterial        
                ipbf-ef.n-out   = ttLayoutSize.iNumOutWidth          
                ipbf-ef.n-out-l = ttLayoutSize.iNumOutLength         
                ipbf-ef.n-out-d = ttLayoutSize.iNumOutDepth          
                ipbf-ef.n-cuts  = ttLayoutSize.iNumberCuts 
                ipbf-eb.num-wid = ttLayoutSize.iBlankNumOnWidth      
                ipbf-eb.num-len = ttLayoutSize.iBlankNumOnLength     
                ipbf-eb.num-dep = ttLayoutSize.iBlankNumOnDepth 
                .    
   
    END.     


END PROCEDURE.

PROCEDURE pUpdateFormBoard PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Updates the board and board description of the ef record
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcEstNo     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdEQty      AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiFormNo    AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcBoard     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcBoardDesc AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-ef FOR ef.
    
    DISABLE TRIGGERS FOR LOAD OF bf-ef.

    FIND FIRST bf-ef EXCLUSIVE-LOCK
         WHERE bf-ef.company EQ ipcCompany
           AND bf-ef.est-no  EQ ipcEstNo
           AND bf-ef.eqty    EQ ipdEQty 
           AND bf-ef.form-no EQ ipiFormNo
         NO-ERROR.
    IF AVAILABLE bf-ef THEN DO:
        ASSIGN
            bf-ef.board    = ipcBoard
            bf-ef.brd-dscr = ipcBoardDesc
            .
    END.
END PROCEDURE.


/* ************************  Function Implementations ***************** */

FUNCTION fEstimate_GetEstimateType RETURNS CHARACTER 
	(ipiEstimateStructureType AS INTEGER, ipcEstimateTypeID AS CHARACTER):
    /*------------------------------------------------------------------------------
    Purpose:  Given estimate qualifiers, return the Estimate Type
    Notes:
    ------------------------------------------------------------------------------*/	

    DEFINE VARIABLE cType AS CHARACTER NO-UNDO.
    
    cType = ENTRY(ipiEstimateStructureType, gcTypeList).
    CASE ipcEstimateTypeID:
        WHEN "Misc" THEN 
            cType = gcTypeMisc.
        WHEN "Wood" THEN
            cType = gcTypeWood.
    END CASE.	
    RETURN cType.
    
END FUNCTION.

FUNCTION fEstimate_GetQuantityPerSet RETURNS DECIMAL 
    (BUFFER ipbf-eb FOR eb):
    /*------------------------------------------------------------------------------
     Purpose: 
     Notes:
    ------------------------------------------------------------------------------*/    

    DEFINE VARIABLE dQuantityPerSet AS DECIMAL NO-UNDO.
       

    IF ipbf-eb.est-type LT 5 THEN
        dQuantityPerSet     = ipbf-eb.cust-%. 
    ELSE         
        dQuantityPerSet     = ipbf-eb.quantityPerSet.
        
    IF dQuantityPerSet LT 0 THEN 
        dQuantityPerSet     = ABSOLUTE(1 / dQuantityPerSet). 
    
    IF ipbf-eb.form-no EQ 0 OR dQuantityPerSet EQ 0 THEN
        dQuantityPerSet =  1.

    RETURN dQuantityPerSet.

END FUNCTION.

FUNCTION fEstimate_IsDepartment RETURNS LOGICAL 
    (ipcDepartment AS CHARACTER, ipcDepartmentList AS CHARACTER EXTENT 4):
    /*------------------------------------------------------------------------------
     Purpose: determine if provided department is in department list
     Notes:
    ------------------------------------------------------------------------------*/    
    DEFINE VARIABLE iIndex        AS INTEGER NO-UNDO.
    DEFINE VARIABLE lIsDepartment AS LOGICAL NO-UNDO. 
    
    DO iIndex = 1 TO 4:
        IF CAN-DO(ipcDepartment,ipcDepartmentList[iIndex]) THEN 
        DO:
            lIsDepartment = YES.
            LEAVE.
        END.
    END.
    RETURN lIsDepartment.
        
END FUNCTION.

FUNCTION fEstimate_IsComboType RETURNS LOGICAL 
    (ipcEstType AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose:  Returns the constant value for Combo Estimate Type
     Notes:
    ------------------------------------------------------------------------------*/    
    RETURN ipcEstType EQ gcTypeCombo.
    
END FUNCTION.

FUNCTION fEstimate_IsInk RETURNS LOGICAL 
	(ipcMaterialType AS CHARACTER, ipcInkType AS CHARACTER):
    /*------------------------------------------------------------------------------
    Purpose:  Given a material type and ink type, return if valid Ink
    Notes:
    ------------------------------------------------------------------------------*/	
    
    RETURN INDEX("IV",ipcMaterialType) GT 0 AND ipcInkType NE "A".
    		
END FUNCTION.

FUNCTION fEstimate_IsMiscType RETURNS LOGICAL 
    (ipcEstType AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose:  Returns the constant value for Combo Estimate Type
     Notes:
    ------------------------------------------------------------------------------*/    
    RETURN ipcEstType EQ gcTypeMisc.
        
END FUNCTION.

FUNCTION fEstimate_IsSetType RETURNS LOGICAL 
    (ipcEstType AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose:  Returns the constant value for Set Estimate Type
     Notes:
    ------------------------------------------------------------------------------*/    
    RETURN ipcEstType EQ gcTypeSet.
        
END FUNCTION.

FUNCTION fEstimate_IsSingleType RETURNS LOGICAL
    (ipcEstType AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose:  Returns the constant value for Single Estimate Type
     Notes:
    ------------------------------------------------------------------------------*/    
    RETURN ipcEstType EQ gcTypeSingle.
    
END FUNCTION.

FUNCTION fEstimate_IsWoodType RETURNS LOGICAL 
    (ipcEstType AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose:  Returns the constant value for Single Estimate Type
     Notes:
    ------------------------------------------------------------------------------*/    
    RETURN ipcEstType EQ gcTypeWood.
    
END FUNCTION.

FUNCTION fEstimate_UseNew RETURNS LOGICAL 
    (ipcCompany AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose: Returns the Setting to use new estimate calculation
     Notes:
    ------------------------------------------------------------------------------*/    
    DEFINE VARIABLE lFound  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
    
    RUN sys/ref/nk1look.p (ipcCompany, "CEVersion", "C" /* Character */, NO /* check by cust */, 
        INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
        OUTPUT cReturn, OUTPUT lFound).
    
    RETURN lFound AND cReturn EQ "New".
        
END FUNCTION.
