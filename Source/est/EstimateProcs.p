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
/* ********************  Preprocessor Definitions  ******************** */

/* ***************************  Main Block  *************************** */

/* **********************  Internal Procedures  *********************** */

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

        IF ttGoto.estType EQ 2 OR ttGoto.estType EQ 6 THEN DO:
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
