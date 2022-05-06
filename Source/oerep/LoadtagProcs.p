
/* **********************  Internal Procedures  *********************** */
DEFINE VARIABLE cRtnChar       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lBussFormModle AS LOGICAL   NO-UNDO.
DEFINE VARIABLE list-name      AS CHARACTER NO-UNDO.
DEFINE VARIABLE init-dir       AS CHARACTER NO-UNDO.
DEFINE VARIABLE ls-full-img1   AS CHARACTER NO-UNDO FORMAT "x(200)".
DEFINE VARIABLE ls-image1      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lFound         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iCountPallet   AS INTEGER   NO-UNDO.

DEFINE VARIABLE glUpdateSetWithMaxQuantity             AS LOGICAL   NO-UNDO.
DEFINE VARIABLE glUpdateLoadTagSSCC                    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE glUpdateLocBinFromItemFG               AS LOGICAL   NO-UNDO.
DEFINE VARIABLE glUpdateLocBinFromFGBin                AS LOGICAL   NO-UNDO.
DEFINE VARIABLE glCreateFGReceipts                     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE glCheckClosedStatus                    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE glCreateRFIDTag                        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE glCreateComponenetTagsForSetHeaderItem AS LOGICAL   NO-UNDO.
DEFINE VARIABLE glFGSetCreateAdjustment                AS LOGICAL   NO-UNDO.
DEFINE VARIABLE gcLoadtagPrintSoftware                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE glAutoPrint                            AS LOGICAL   NO-UNDO.
DEFINE VARIABLE giFGSetRec                             AS INTEGER   NO-UNDO.
DEFINE VARIABLE gcLoadTag                              AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcLoadTagOutputFile                    AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcLoadTagOutputPath                    AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcBOLLoadTagOutputFile                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcBOLLoadTagOutputPath                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcLoadtagLocationSource                AS CHARACTER NO-UNDO.
DEFINE VARIABLE glCreateTagsForEmptyBOLLineTags        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE glCreateTagForPartial                  AS LOGICAL   NO-UNDO.

{oerep/ttLoadTag.i}
{api/ttAPIOutboundEvent.i}
{fg/fullset.i NEW}
{oerep/r-loadtg.i }
{custom/xprint.i}

{sys/inc/var.i shared}
{sys/form/r-top3.f}

 DEFINE SHARED TEMP-TABLE tt-word-print LIKE w-ord 
   FIELD tag-no AS CHARACTER .
 
DEFINE TEMP-TABLE ttFGRctd NO-UNDO
    FIELD fgRctdROWID AS ROWID
    FIELD isComponent AS LOGICAL
    .
  
RUN sys/ref/nk1look.p (INPUT cocode, "BusinessFormModal", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    lBussFormModle = LOGICAL(cRtnChar) NO-ERROR.  
RUN sys/ref/nk1look.p (INPUT cocode,
                       INPUT "LoadTagXprintImage",
                       INPUT "C",
                       INPUT NO,
                       INPUT NO,
                       INPUT "",
                       INPUT "",
                       OUTPUT ls-image1,
                       OUTPUT lFound).

FILE-INFO:FILE-NAME = ls-image1.
ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".

/* ************************  Function Prototypes ********************** */
FUNCTION fGetNextTTLoadTagRecordID RETURNS INTEGER PRIVATE
    (  ) FORWARD.

FUNCTION fReplaceQuotes RETURNS CHARACTER PRIVATE
    ( INPUT ipcField AS CHARACTER ) FORWARD.

RUN pUpdateConfig.

PROCEDURE BuildLoadTagsFromBOL:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiBOLID   AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiCopies  AS INTEGER   NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttLoadTag.
    
    DEFINE VARIABLE lError   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
   
    RUN pBuildLoadTagsFromBOL (
        INPUT  ipcCompany,
        INPUT  ipiBOLID,
        INPUT  ipiCopies,
        OUTPUT lError,
        OUTPUT cMessage,
        INPUT-OUTPUT TABLE ttLoadTag
        ).    

END PROCEDURE.

PROCEDURE BuildLoadTagsFromRelease:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiReleaseID AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiCopies    AS INTEGER   NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttLoadTag.
    
    DEFINE VARIABLE lError   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
   
    RUN pBuildLoadTagsFromRelease (
        INPUT  ipcCompany,
        INPUT  ipiReleaseID,
        INPUT  ipiCopies,
        OUTPUT lError,
        OUTPUT cMessage,
        INPUT-OUTPUT TABLE ttLoadTag
        ).    

END PROCEDURE.

PROCEDURE CreateLoadTagFromTT:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLocation       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplPrint          AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER iplEmptyTTLoadtag AS LOGICAL   NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttLoadTag.

    RUN pCreateLoadTagFromTT (
        INPUT ipcCompany,
        INPUT ipcLocation,
        INPUT iplPrint,
        INPUT iplEmptyTTLoadtag,
        INPUT-OUTPUT TABLE ttLoadTag 
        ).
END PROCEDURE.

PROCEDURE pCreateLoadTagFromTT:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLocation       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplPrint          AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER iplEmptyTTLoadtag AS LOGICAL   NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttLoadTag.
    
    DEFINE VARIABLE lError                  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage                AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCopies                 AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lSuccess                AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lValidOutputPath        AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE iChildTTLoadTagRecordID AS INTEGER   NO-UNDO.
    
    DEFINE BUFFER bf-child-ttLoadTag FOR ttLoadTag.
    
    MAIN-BLOCK:
    DO ON ERROR UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
        ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:    
        RUN FileSys_ValidateDirectory(
            INPUT  gcLoadTagOutputPath,
            OUTPUT lValidOutputPath,
            OUTPUT cMessage
            ).
        IF NOT lValidOutputPath THEN DO:
            ASSIGN
                lError   = TRUE
                cMessage = "Invalid output path '" + gcLoadTagOutputPath + "'"
                .
            UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
        END.
        
        FOR EACH ttLoadTag
            WHERE ttLoadTag.tagStatus  EQ "Pending"
              AND ttLoadTag.isSelected EQ TRUE
              AND ttLoadTag.isChild    EQ FALSE:
            RUN pExplodeTTLoadTag (
                BUFFER ttLoadTag,
                OUTPUT lError,
                OUTPUT cMessage
                ).
        END.
        
        FOR EACH ttLoadTag
            WHERE ttLoadTag.tagStatus  EQ "Pending"
              AND ttLoadTag.isSelected EQ TRUE
              AND ttLoadTag.isChild    EQ TRUE:
            IF ttLoadTag.itemType EQ "FG" THEN
                RUN pCreateLoadTagFG (
                    BUFFER ttLoadTag,
                    OUTPUT ttLoadTag.isError,
                    OUTPUT ttLoadTag.errorMessage
                    ).
            ELSE IF ttLoadTag.itemType EQ "RM" THEN
                RUN pCreateLoadTagRM (
                    BUFFER ttLoadTag,
                    OUTPUT ttLoadTag.isError,
                    OUTPUT ttLoadTag.errorMessage
                    ).
        END.
       
        IF iplPrint THEN
            RUN pPrintTTLoadtags (
                INPUT ipcCompany,
                INPUT ipcLocation,
                INPUT-OUTPUT TABLE ttLoadTag
                ).
            
        IF iplEmptyTTLoadtag THEN 
            EMPTY TEMP-TABLE ttLoadTag.
    END.
END PROCEDURE.

PROCEDURE pBuildLoadTagsFromRelease PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiReleaseID AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiCopies    AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError     AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage   AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttLoadTag.
    
    DEFINE VARIABLE iTTLoadTagRecordID AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lSetsCreated       AS LOGICAL   NO-UNDO.
    
    DEFINE BUFFER bf-job       FOR job.
    DEFINE BUFFER bf-job-hdr   FOR job-hdr.
    DEFINE BUFFER bf-oe-relh   FOR oe-relh.
    DEFINE BUFFER bf-oe-rell   FOR oe-rell.
    DEFINE BUFFER bf-oe-ord    FOR oe-ord.
    DEFINE BUFFER bf-oe-ordl   FOR oe-ordl.
    DEFINE BUFFER bf-shipto    FOR shipto.
    DEFINE BUFFER bf-fg-bin    FOR fg-bin.   
    DEFINE BUFFER bf-ttLoadTag FOR ttLoadtag.
    
    MAIN-BLOCK:
    DO ON ERROR UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
        ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
                         
        FIND FIRST bf-oe-relh NO-LOCK
             WHERE bf-oe-relh.company  EQ ipcCompany
               AND bf-oe-relh.release# EQ ipiReleaseID
             NO-ERROR.             
        
        IF NOT AVAILABLE bf-oe-relh THEN DO:
            ASSIGN
                oplError   = TRUE
                opcMessage = "Invalid Release # '" + STRING(ipiReleaseID) + "'"
                .
            UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
        END.
        
        FOR EACH bf-oe-rell NO-LOCK
            WHERE bf-oe-rell.company EQ bf-oe-relh.company
              AND bf-oe-rell.r-no    EQ bf-oe-relh.r-no:
            FIND FIRST bf-oe-ord NO-LOCK
                 WHERE bf-oe-ord.company EQ bf-oe-rell.company
                   AND bf-oe-ord.ord-no  EQ bf-oe-rell.ord-no
                 NO-ERROR.
            IF AVAILABLE bf-oe-ord THEN
                FIND FIRST bf-oe-ordl NO-LOCK
                     WHERE bf-oe-ordl.company EQ bf-oe-ord.company
                       AND bf-oe-ordl.ord-no  EQ bf-oe-ord.ord-no
                       AND bf-oe-ordl.i-no    EQ bf-oe-rell.i-no
                     NO-ERROR.
            
            IF NOT AVAILABLE bf-oe-ordl THEN
                NEXT.
            
            IF CAN-FIND (FIRST oe-ordl
                         WHERE oe-ordl.company        EQ bf-oe-ordl.company
                           AND oe-ordl.ord-no         EQ bf-oe-ordl.ord-no
                           AND oe-ordl.is-a-component EQ YES
                           AND oe-ordl.set-hdr-line   EQ bf-oe-ordl.line) THEN
                NEXT.
                    
            RUN pCreateTTLoadTagFromItem (
                INPUT  bf-oe-rell.company,
                INPUT  bf-oe-rell.i-no,
                OUTPUT iTTLoadTagRecordID,
                OUTPUT oplError,
                OUTPUT opcMessage
                ).
            IF oplError THEN
                UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK. 

            FIND FIRST bf-ttLoadTag
                 WHERE bf-ttLoadTag.recordID EQ iTTLoadTagRecordID
                 NO-ERROR.
            IF NOT AVAILABLE bf-ttLoadTag THEN DO:
                ASSIGN
                    oplError   = TRUE
                    opcMessage = "Error while populating loadtag record'"
                    .            
                NEXT.
            END.

            RUN pUpdateTTLoadTagCustDetails (
                INPUT  bf-oe-rell.company,
                INPUT  bf-oe-rell.i-no,
                INPUT  bf-oe-ord.cust-no,
                INPUT  bf-ttLoadTag.recordID,
                OUTPUT oplError,
                OUTPUT opcMessage
                ).
            IF oplError THEN
                UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
  
            ASSIGN
                bf-ttLoadTag.orderID        = bf-oe-rell.ord-no
                bf-ttLoadTag.relID          = bf-oe-rell.rel-no
                bf-ttLoadTag.jobID          = bf-oe-rell.job-no
                bf-ttLoadTag.jobID2         = bf-oe-rell.job-no2
                bf-ttLoadTag.itemID         = bf-oe-rell.i-no
                bf-ttLoadTag.tag            = bf-oe-rell.tag
                bf-ttLoadTag.overPct        = 0
                bf-ttLoadTag.quantityTotal  = bf-oe-rell.qty
                bf-ttLoadTag.estID          = bf-oe-ordl.est-no
                bf-ttLoadTag.formNo         = bf-oe-ordl.form-no
                bf-ttLoadTag.tareWeight     = 10
                bf-ttLoadTag.uom            = "EA"
                bf-ttLoadTag.printCopies    = ipiCopies
                bf-ttLoadTag.ipReturn       = NO
                bf-ttLoadTag.isSelected     = TRUE
                bf-ttLoadTag.tagStatus      = IF TRIM(bf-ttLoadTag.tag) EQ "" THEN 
                                                  "Pending"
                                              ELSE
                                                  "Created"
                bf-ttLoadTag.recordSource   = "RELEASE"
                bf-ttLoadTag.exportFileType = "loadtag"
                bf-ttLoadTag.exportFile     = gcBOLLoadTagOutputPath + gcBOLLoadTagOutputFile
                .
    
            RUN pUpdateTTLoadTagOrderDetails (
                INPUT bf-oe-rell.company,
                INPUT bf-oe-rell.ord-no,
                INPUT bf-oe-rell.i-no,
                INPUT bf-ttLoadTag.recordID
                ).
            
            bf-ttLoadTag.lotID = bf-ttLoadTag.rellotID.            
            
            FIND FIRST bf-job-hdr NO-LOCK
                 WHERE bf-job-hdr.company = bf-oe-ordl.company 
                   AND bf-job-hdr.ord-no  = bf-oe-ordl.ord-no  
                   AND bf-job-hdr.i-no    = bf-oe-ordl.i-no
                 NO-ERROR.
            IF AVAILABLE bf-job-hdr THEN
                FIND FIRST bf-job NO-LOCK 
                     WHERE bf-job.company = bf-job-hdr.company
                       AND bf-job.job     = bf-job-hdr.job
                       AND bf-job.job-no  = bf-job-hdr.job-no
                       AND bf-job.job-no2 = bf-job-hdr.job-no2
                      NO-ERROR.
            
            IF AVAILABLE bf-job-hdr THEN
                ASSIGN
                    bf-ttLoadTag.jobQuantity   = bf-job-hdr.qty      
                    bf-ttLoadTag.dueDateJobHdr = IF bf-job-hdr.due-date NE ? THEN 
                                                     STRING(bf-job-hdr.due-date, "99/99/9999") 
                                                 ELSE
                                                     "".
            IF AVAILABLE bf-job THEN DO:
                bf-ttLoadTag.dueDateJob = IF bf-job.due-date NE ? THEN 
                                              STRING(bf-job.due-date, "99/99/9999") 
                                          ELSE 
                                              "".
    
                RUN pUpdateTTLoadTagEstimateDetails (
                    INPUT bf-job.company,
                    INPUT bf-job.est-no,
                    INPUT bf-ttLoadTag.recordID
                    ).
            END.
            
            FIND FIRST bf-fg-bin NO-LOCK 
                 WHERE bf-fg-bin.company EQ bf-oe-rell.company
                   AND bf-fg-bin.job-no  EQ bf-oe-rell.job-no
                   AND bf-fg-bin.job-no2 EQ bf-oe-rell.job-no2
                   AND bf-fg-bin.i-no    EQ bf-oe-rell.i-no
                   AND bf-fg-bin.loc     EQ bf-oe-rell.loc
                   AND bf-fg-bin.loc-bin EQ bf-oe-rell.loc-bin
                   AND bf-fg-bin.tag     EQ bf-oe-rell.tag
                 NO-ERROR.
            IF AVAILABLE bf-fg-bin THEN 
                bf-ttLoadTag.subUnitsPerUnit = bf-fg-bin.cases-unit.
            ELSE
                bf-ttLoadTag.subUnitsPerUnit = bf-oe-ordl.cases-unit.

            ASSIGN
                bf-ttLoadTag.quantityInSubUnit  = bf-oe-rell.qty-case
/*                bf-ttLoadTag.quantityOfSubUnits = bf-oe-rell.cases*/
/*                bf-ttLoadTag.partial            = bf-oe-rell.partial*/
/*                bf-ttLoadTag.quantityInUnit     = bf-ttLoadTag.quantityInSubUnit * bf-ttLoadTag.subUnitsPerUnit*/
/*                bf-ttLoadTag.quantityOfUnits    = bf-oe-rell.tot-pallets*/
/*                bf-ttLoadTag.partial            = bf-oe-rell.partial*/
                bf-ttLoadTag.ordQuantity        = bf-oe-ordl.qty
/*                bf-ttLoadTag.totalTags          = bf-oe-rell.tot-pallets*/
                bf-ttLoadTag.netWeight          = bf-ttLoadTag.sheetWeight * bf-ttLoadTag.quantity
                bf-ttLoadTag.grossWeight        = bf-ttLoadTag.sheetWeight * bf-ttLoadTag.quantity
                .
            
            RUN pUpdateTTLoadTagQuantites (
                INPUT bf-ttLoadTag.quantityTotal,
                INPUT bf-ttLoadTag.quantityInSubUnit,
                INPUT bf-ttLoadTag.subUnitsPerUnit,
                INPUT bf-ttLoadTag.recordID
                ).
                            
            RUN pGetExportTemplateFile(
                INPUT  bf-ttLoadTag.company,
                INPUT  "ReleaseTagFile",
                INPUT  bf-ttLoadTag.itemID,
                INPUT  bf-ttLoadTag.custID,
                INPUT  bf-ttLoadTag.shipID,
                OUTPUT bf-ttLoadTag.exportTemplateFile,
                OUTPUT bf-ttLoadTag.exportTemplate
                ).

            IF glCreateComponenetTagsForSetHeaderItem THEN DO:
                RUN pCreateSetComponentsForTTLoadTagItem (
                    INPUT  bf-ttLoadTag.recordID,
                    OUTPUT lSetsCreated
                    ).
        
                IF lSetsCreated THEN
                    DELETE bf-ttLoadTag.
            END.            
        END.
    END. 
END PROCEDURE.

PROCEDURE pBuildLoadTagsFromBOL PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiBOLID   AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiCopies  AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError   AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttLoadTag.
    
    DEFINE VARIABLE iTTLoadTagRecordID AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lSetsCreated       AS LOGICAL   NO-UNDO.
    
    DEFINE BUFFER bf-job       FOR job.
    DEFINE BUFFER bf-job-hdr   FOR job-hdr.
    DEFINE BUFFER bf-oe-bolh   FOR oe-bolh.
    DEFINE BUFFER bf-oe-boll   FOR oe-boll.
    DEFINE BUFFER bf-oe-ord    FOR oe-ord.
    DEFINE BUFFER bf-oe-ordl   FOR oe-ordl.
    DEFINE BUFFER bf-shipto    FOR shipto.
    DEFINE BUFFER bf-fg-bin    FOR fg-bin.   
    DEFINE BUFFER bf-ttLoadTag FOR ttLoadtag.
    
    MAIN-BLOCK:
    DO ON ERROR UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
        ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
                         
        FIND FIRST bf-oe-bolh NO-LOCK
             WHERE bf-oe-bolh.company EQ ipcCompany
               AND bf-oe-bolh.bol-no  EQ ipiBOLID
             NO-ERROR.             
        
        IF NOT AVAILABLE bf-oe-bolh THEN DO:
            ASSIGN
                oplError   = TRUE
                opcMessage = "Invalid BOL # '" + STRING(ipiBOLID) + "'"
                .
            UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
        END.
        
        FOR EACH bf-oe-boll NO-LOCK
            WHERE bf-oe-boll.company EQ bf-oe-bolh.company
              AND bf-oe-boll.b-no    EQ bf-oe-bolh.b-no:
            FIND FIRST bf-oe-ord NO-LOCK
                 WHERE bf-oe-ord.company EQ bf-oe-boll.company
                   AND bf-oe-ord.ord-no  EQ bf-oe-boll.ord-no
                 NO-ERROR.
            IF AVAILABLE bf-oe-ord THEN
                FIND FIRST bf-oe-ordl NO-LOCK
                     WHERE bf-oe-ordl.company EQ bf-oe-ord.company
                       AND bf-oe-ordl.ord-no  EQ bf-oe-ord.ord-no
                       AND bf-oe-ordl.i-no    EQ bf-oe-boll.i-no
                     NO-ERROR.
            
            IF NOT AVAILABLE bf-oe-ordl THEN
                NEXT.
            
            IF CAN-FIND (FIRST oe-ordl
                         WHERE oe-ordl.company        EQ bf-oe-ordl.company
                           AND oe-ordl.ord-no         EQ bf-oe-ordl.ord-no
                           AND oe-ordl.is-a-component EQ YES
                           AND oe-ordl.set-hdr-line   EQ bf-oe-ordl.line) THEN
                NEXT.
                    
            RUN pCreateTTLoadTagFromItem (
                INPUT  bf-oe-boll.company,
                INPUT  bf-oe-boll.i-no,
                OUTPUT iTTLoadTagRecordID,
                OUTPUT oplError,
                OUTPUT opcMessage
                ).
            IF oplError THEN
                UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK. 

            FIND FIRST bf-ttLoadTag
                 WHERE bf-ttLoadTag.recordID EQ iTTLoadTagRecordID
                 NO-ERROR.
            IF NOT AVAILABLE bf-ttLoadTag THEN DO:
                ASSIGN
                    oplError   = TRUE
                    opcMessage = "Error while populating loadtag record'"
                    .            
                NEXT.
            END.

            RUN pUpdateTTLoadTagCustDetails (
                INPUT  bf-oe-boll.company,
                INPUT  bf-oe-boll.i-no,
                INPUT  bf-oe-ord.cust-no,
                INPUT  bf-ttLoadTag.recordID,
                OUTPUT oplError,
                OUTPUT opcMessage
                ).
            IF oplError THEN
                UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
  
            ASSIGN
                bf-ttLoadTag.orderID        = bf-oe-boll.ord-no
                bf-ttLoadTag.bolID          = bf-oe-boll.bol-no
                bf-ttLoadTag.jobID          = bf-oe-boll.job-no
                bf-ttLoadTag.jobID2         = bf-oe-boll.job-no2
                bf-ttLoadTag.itemID         = bf-oe-boll.i-no
                bf-ttLoadTag.tag            = bf-oe-boll.tag
                bf-ttLoadTag.overPct        = 0
                bf-ttLoadTag.quantityTotal  = bf-oe-boll.qty
                bf-ttLoadTag.estID          = bf-oe-ordl.est-no
                bf-ttLoadTag.formNo         = bf-oe-ordl.form-no
                bf-ttLoadTag.tareWeight     = 10
                bf-ttLoadTag.uom            = "EA"
                bf-ttLoadTag.printCopies    = ipiCopies
                bf-ttLoadTag.ipReturn       = NO
                bf-ttLoadTag.isSelected     = TRUE
                bf-ttLoadTag.tagStatus      = IF TRIM(bf-ttLoadTag.tag) EQ "" THEN 
                                                  "Pending"
                                              ELSE
                                                  "Created"
                bf-ttLoadTag.recordSource   = "BOL"
                bf-ttLoadTag.exportFileType = "BOLTag"
                bf-ttLoadTag.exportFile     = gcBOLLoadTagOutputPath + gcBOLLoadTagOutputFile
                .
    
            RUN pUpdateTTLoadTagOrderDetails (
                INPUT bf-oe-boll.company,
                INPUT bf-oe-boll.ord-no,
                INPUT bf-oe-boll.i-no,
                INPUT bf-ttLoadTag.recordID
                ).
            
            bf-ttLoadTag.lotID = bf-ttLoadTag.rellotID.            
            
            FIND FIRST bf-job-hdr NO-LOCK
                 WHERE bf-job-hdr.company = bf-oe-ordl.company 
                   AND bf-job-hdr.ord-no  = bf-oe-ordl.ord-no  
                   AND bf-job-hdr.i-no    = bf-oe-ordl.i-no
                 NO-ERROR.
            IF AVAILABLE bf-job-hdr THEN
                FIND FIRST bf-job NO-LOCK 
                     WHERE bf-job.company = bf-job-hdr.company
                       AND bf-job.job     = bf-job-hdr.job
                       AND bf-job.job-no  = bf-job-hdr.job-no
                       AND bf-job.job-no2 = bf-job-hdr.job-no2
                      NO-ERROR.
            
            IF AVAILABLE bf-job-hdr THEN
                ASSIGN
                    bf-ttLoadTag.jobQuantity   = bf-job-hdr.qty      
                    bf-ttLoadTag.dueDateJobHdr = IF bf-job-hdr.due-date NE ? THEN 
                                                     STRING(bf-job-hdr.due-date, "99/99/9999") 
                                                 ELSE
                                                     "".
            IF AVAILABLE bf-job THEN DO:
                bf-ttLoadTag.dueDateJob = IF bf-job.due-date NE ? THEN 
                                              STRING(bf-job.due-date, "99/99/9999") 
                                          ELSE 
                                              "".
    
                RUN pUpdateTTLoadTagEstimateDetails (
                    INPUT bf-job.company,
                    INPUT bf-job.est-no,
                    INPUT bf-ttLoadTag.recordID
                    ).
            END.
            
            FIND FIRST bf-fg-bin NO-LOCK 
                 WHERE bf-fg-bin.company EQ bf-oe-boll.company
                   AND bf-fg-bin.job-no  EQ bf-oe-boll.job-no
                   AND bf-fg-bin.job-no2 EQ bf-oe-boll.job-no2
                   AND bf-fg-bin.i-no    EQ bf-oe-boll.i-no
                   AND bf-fg-bin.loc     EQ bf-oe-boll.loc
                   AND bf-fg-bin.loc-bin EQ bf-oe-boll.loc-bin
                   AND bf-fg-bin.tag     EQ bf-oe-boll.tag
                 NO-ERROR.
            IF AVAILABLE bf-fg-bin THEN 
                bf-ttLoadTag.subUnitsPerUnit = bf-fg-bin.cases-unit.
            ELSE
                bf-ttLoadTag.subUnitsPerUnit = bf-oe-ordl.cases-unit.

            ASSIGN
                bf-ttLoadTag.quantityInSubUnit  = bf-oe-boll.qty-case
/*                bf-ttLoadTag.quantityOfSubUnits = bf-oe-boll.cases*/
/*                bf-ttLoadTag.partial            = bf-oe-boll.partial*/
/*                bf-ttLoadTag.quantityInUnit     = bf-ttLoadTag.quantityInSubUnit * bf-ttLoadTag.subUnitsPerUnit*/
/*                bf-ttLoadTag.quantityOfUnits    = bf-oe-boll.tot-pallets*/
/*                bf-ttLoadTag.partial            = bf-oe-boll.partial*/
                bf-ttLoadTag.ordQuantity        = bf-oe-ordl.qty
/*                bf-ttLoadTag.totalTags          = bf-oe-boll.tot-pallets*/
                bf-ttLoadTag.netWeight          = bf-ttLoadTag.sheetWeight * bf-ttLoadTag.quantity
                bf-ttLoadTag.grossWeight        = bf-oe-boll.weight
                .
            
            RUN pUpdateTTLoadTagQuantites (
                INPUT bf-ttLoadTag.quantityTotal,
                INPUT bf-ttLoadTag.quantityInSubUnit,
                INPUT bf-ttLoadTag.subUnitsPerUnit,
                INPUT bf-ttLoadTag.recordID
                ).
                            
            RUN pGetExportTemplateFile(
                INPUT  bf-ttLoadTag.company,
                INPUT  "BOlTagFile",
                INPUT  bf-ttLoadTag.itemID,
                INPUT  bf-ttLoadTag.custID,
                INPUT  bf-ttLoadTag.shipID,
                OUTPUT bf-ttLoadTag.exportTemplateFile,
                OUTPUT bf-ttLoadTag.exportTemplate
                ).

            IF glCreateComponenetTagsForSetHeaderItem THEN DO:
                RUN pCreateSetComponentsForTTLoadTagItem (
                    INPUT  bf-ttLoadTag.recordID,
                    OUTPUT lSetsCreated
                    ).
        
                IF lSetsCreated THEN
                    DELETE bf-ttLoadTag.
            END.            
        END.
    END. 
END PROCEDURE.

PROCEDURE pExplodeTTLoadTag PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Explodes the given ttLoadTag record into multiple ttLoadTag records
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttLoadTag FOR ttLoadTag.
    DEFINE OUTPUT PARAMETER oplError   AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE VARIABLE iEndPalletID            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iStartPalletID          AS INTEGER   NO-UNDO.    
    DEFINE VARIABLE iTagCount               AS INTEGER   NO-UNDO. 
    DEFINE VARIABLE iTagCounter             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iPalletCounter          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iQuantityTotal          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iQuantity               AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iQuantityInSubUnit      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iSubUnitsPerUnit        AS INTEGER   NO-UNDO.

    DEFINE BUFFER bf-child-ttLoadTag FOR ttLoadTag.
    
    MAIN-BLOCK:
    DO ON ERROR UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
        ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
        IF NOT AVAILABLE ipbf-ttLoadTag THEN DO:
            ASSIGN
                oplError  = TRUE
                opcMessage = "Record not found for creating loadtag"
                .
            UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.    
        END.
        
        IF ipbf-ttLoadTag.tagStatus NE "Pending" THEN
            RETURN.
            
        IF ipbf-ttLoadTag.tag NE "" THEN DO:
            ipbf-ttLoadTag.tagStatus = "Created".
            RETURN.
        END.
            
        /* v-loadtag - additional code is required to calculate totaltags to print using LOADTAG NK1 */
        RUN pIncrementCustPalletID(
            INPUT  ipbf-ttLoadTag.company,
            INPUT  ipbf-ttLoadTag.custID,
            INPUT  ipbf-ttLoadTag.totalTags * ipbf-ttLoadTag.printCopies,
            OUTPUT iStartPalletID,
            OUTPUT iEndPalletID
            ).
            
        iPalletCounter = iStartPalletID.
        DO iTagCount = 1 TO ipbf-ttLoadTag.totalTags:
            iTagCounter = iTagCounter + 1.

            /* Create actual ttLoadTag record with updated loadtag fields  */
            CREATE bf-child-ttLoadTag.
            BUFFER-COPY ipbf-ttLoadTag EXCEPT ipbf-ttLoadTag.recordID TO bf-child-ttLoadTag.
            ASSIGN
                bf-child-ttLoadTag.recordID  = fGetNextTTLoadTagRecordID()
                bf-child-ttLoadTag.tagStatus = "Pending"
                bf-child-ttLoadTag.isChild   = TRUE
                .
            
            IF ipbf-ttLoadTag.itemType EQ "RM" THEN
                NEXT.
                
            IF ipbf-ttLoadTag.partial NE 0 AND glCreateTagForPartial THEN DO:
                ASSIGN
                    iQuantityTotal     = ipbf-ttLoadTag.partial
                    iQuantityInSubUnit = IF iQuantityTotal LT ipbf-ttLoadTag.quantityInSubUnit THEN
                                             iQuantityTotal
                                         ELSE
                                             ipbf-ttLoadTag.quantityInSubUnit
                    iSubUnitsPerUnit   = IF iQuantityTotal LT ipbf-ttLoadTag.quantityInSubUnit THEN
                                             1
                                         ELSE
                                             TRUNCATE(iQuantityTotal / ipbf-ttLoadTag.quantityInSubUnit, 0)
                    .
            END.               
            ELSE IF ipbf-ttLoadTag.partial NE 0 AND NOT glCreateTagForPartial THEN
                ASSIGN
                    iQuantity          = (ipbf-ttLoadTag.quantityInSubUnit * ipbf-ttLoadTag.subUnitsPerUnit) * INTEGER(ipbf-ttLoadTag.quantityTotal GE ipbf-ttLoadTag.quantityInSubUnit * ipbf-ttLoadTag.subUnitsPerUnit)
                    iQuantityTotal     = iQuantity + ipbf-ttLoadTag.partial
                    iQuantityInSubUnit = IF iQuantityTotal LT ipbf-ttLoadTag.quantityInSubUnit THEN
                                             iQuantityTotal
                                         ELSE
                                             ipbf-ttLoadTag.quantityInSubUnit
                    iSubUnitsPerUnit   = IF iQuantityTotal LT ipbf-ttLoadTag.quantityInSubUnit THEN
                                             1
                                         ELSE IF iQuantity GT 0 THEN
                                             TRUNCATE(iQuantity / ipbf-ttLoadTag.quantityInSubUnit, 0)
                                         ELSE
                                             TRUNCATE(ipbf-ttLoadTag.partial / ipbf-ttLoadTag.quantityInSubUnit, 0)
                    .
            ELSE
                ASSIGN
                    iQuantityTotal     = (ipbf-ttLoadTag.quantityInSubUnit * ipbf-ttLoadTag.subUnitsPerUnit) * INTEGER(ipbf-ttLoadTag.quantityTotal GE ipbf-ttLoadTag.quantityInSubUnit * ipbf-ttLoadTag.subUnitsPerUnit) 
                    iQuantityInSubUnit = IF ipbf-ttLoadTag.quantityTotal LT ipbf-ttLoadTag.quantityInSubUnit THEN
                                             ipbf-ttLoadTag.quantityTotal
                                         ELSE
                                             ipbf-ttLoadTag.quantityInSubUnit
                    iSubUnitsPerUnit   = IF ipbf-ttLoadTag.quantityTotal LT ipbf-ttLoadTag.quantityInSubUnit THEN
                                             1
                                         ELSE
                                             ipbf-ttLoadTag.subUnitsPerUnit
                    .
            
            ipbf-ttLoadTag.quantityTotal = ipbf-ttLoadTag.quantityTotal - iQuantityTotal.
                
            RUN pUpdateTTLoadTagQuantites (
                INPUT iQuantityTotal,
                INPUT iQuantityInSubUnit,
                INPUT iSubUnitsPerUnit,
                INPUT bf-child-ttLoadTag.recordID
                ).                    

           /* Set this value to zero, so that next tags that will be created won't have have partial */
            ipbf-ttLoadTag.partial = 0.
        
            ASSIGN
                bf-child-ttLoadTag.tagCounter         = iTagCounter
                bf-child-ttLoadTag.palletCounter      = iPalletCounter
                bf-child-ttLoadTag.netWeight          = bf-child-ttLoadTag.sheetWeight * (bf-child-ttLoadTag.quantity + bf-child-ttLoadTag.partial)
                bf-child-ttLoadTag.tareWeight         = 10
                bf-child-ttLoadTag.grossWeight        = bf-child-ttLoadTag.netWeight + bf-child-ttLoadTag.tareWeight
                .
        END.

        DELETE ipbf-ttLoadTag.       
    END.
END.

PROCEDURE pCreateLoadTagFG:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttLoadTag FOR ttLoadTag.
    DEFINE OUTPUT PARAMETER oplError   AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
        
    DEFINE VARIABLE cRfidTag              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iNextRNo              AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dCostPerUOM           AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cNextLoadtag          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lError                AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hdPOProcs             AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hdJobProcs            AS HANDLE    NO-UNDO. 
    DEFINE VARIABLE dMaxQty               AS DECIMAL   NO-UNDO.
    
    DEFINE BUFFER bf-loadtag  FOR loadtag.
    DEFINE BUFFER bf-po-ordl  FOR po-ordl.
    DEFINE BUFFER bf-itemfg   FOR itemfg.
    DEFINE BUFFER bf-fg-rctd  FOR fg-rctd.
    DEFINE BUFFER bf-fg-bin   FOR fg-bin.
    DEFINE BUFFER bf-fg-rcpth FOR fg-rcpth.
    DEFINE BUFFER bf-job      FOR job.
    DEFINE BUFFER bf-job-hdr  FOR job-hdr.
    DEFINE BUFFER bf-reftable FOR reftable.
    DEFINE BUFFER bf-rfidtag  FOR rfidtag.
    
    MAIN-BLOCK:
    DO ON ERROR UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
        ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
        IF NOT AVAILABLE ipbf-ttLoadTag THEN DO:
            ASSIGN
                oplError  = TRUE
                opcMessage = "Record not found for creating loadtag"
                .
            UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.    
        END.
        
        FIND FIRST bf-itemfg NO-LOCK
             WHERE bf-itemfg.company EQ ipbf-ttLoadTag.company
               AND bf-itemfg.i-no    EQ ipbf-ttLoadTag.itemID
             NO-ERROR.
        IF NOT AVAILABLE bf-itemfg THEN DO:
            ASSIGN
                oplError  = TRUE
                opcMessage = "Finished Good item # '" + ipbf-ttLoadTag.itemID + "' not found"
                .
            UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.    
        END.
        
        RUN loadtags\GetNextTag.p (
            INPUT  ipbf-ttLoadTag.company, 
            INPUT  ipbf-ttLoadTag.itemID,
            OUTPUT cNextLoadtag
            ).    
    
        IF glUpdateLoadTagSSCC THEN
            RUN oerep/ldtagSSCC.p (
                INPUT  ipbf-ttLoadTag.company,
                INPUT  ipbf-ttLoadTag.custID,
                OUTPUT ipbf-ttLoadTag.SSCC
                ).
    
        CREATE bf-loadtag.
        ASSIGN
            bf-loadtag.company      = ipbf-ttLoadTag.company
            bf-loadtag.tag-no       = IF ipbf-ttLoadTag.vendorTag NE "" THEN ipbf-ttLoadTag.vendorTag ELSE cNextLoadtag 
            bf-loadtag.item-type    = NO /*FGitem*/
            bf-loadtag.job-no       = ipbf-ttLoadTag.jobID
            bf-loadtag.job-no2      = ipbf-ttLoadTag.jobID2
            bf-loadtag.po-no        = ipbf-ttLoadTag.poID
            bf-loadtag.line         = ipbf-ttLoadTag.poLineID
            bf-loadtag.ord-no       = IF CAN-FIND(FIRST cust 
                                                  WHERE cust.company EQ bf-itemfg.company
                                                    AND cust.cust-no EQ bf-itemfg.cust-no
                                                    AND cust.active  EQ "X") THEN 
                                          0
                                      ELSE 
                                          ipbf-ttLoadTag.orderID
            bf-loadtag.i-no         = CAPS(ipbf-ttLoadTag.itemID)
            bf-loadtag.i-name       = ipbf-ttLoadTag.itemName
            bf-loadtag.sts          = "Printed"
            bf-loadtag.tag-date     = TODAY
            bf-loadtag.tag-time     = TIME
            bf-loadtag.misc-dec[1]  = ipbf-ttLoadTag.unitWeight 
            bf-loadtag.misc-dec[2]  = ipbf-ttLoadTag.palletWeight
            bf-loadtag.misc-char[2] = ipbf-ttLoadTag.lotID
            bf-loadtag.spare-char-1 = ipbf-ttLoadTag.SSCC
            bf-loadtag.pallet-no    = ipbf-ttLoadTag.itemPalletID
            bf-loadtag.misc-char[1] = ipbf-ttLoadTag.vendorTag
            .

        ASSIGN
            bf-loadtag.qty          = ipbf-ttLoadTag.quantity
            bf-loadtag.qty-case     = ipbf-ttLoadTag.quantityInSubUnit
            bf-loadtag.case-bundle  = ipbf-ttLoadTag.subUnitsPerUnit
            bf-loadtag.pallet-count = ipbf-ttLoadTag.quantityInUnit
            bf-loadtag.partial      = ipbf-ttLoadTag.partial
            .

        IF gcLoadtagLocationSource EQ "FGItem" THEN
            ASSIGN
                bf-loadtag.loc     = bf-itemfg.def-loc
                bf-loadtag.loc-bin = bf-itemfg.def-loc-bin
                .
        ELSE IF gcLoadtagLocationSource EQ "FGBin" THEN DO:
            FIND FIRST bf-fg-bin NO-LOCK
                 WHERE bf-fg-bin.company EQ bf-itemfg.company
                   AND bf-fg-bin.i-no    EQ bf-itemfg.i-no
                   AND bf-fg-bin.job-no  EQ ipbf-ttLoadTag.jobID
                   AND bf-fg-bin.tag     EQ bf-loadtag.tag-no
                 NO-ERROR.
            IF AVAILABLE bf-fg-bin THEN
                ASSIGN
                    bf-loadtag.loc     = bf-fg-bin.loc
                    bf-loadtag.loc-bin = bf-fg-bin.loc-bin
                    .

        END.
        ELSE
            RUN fg/autopost.p ( 
                INPUT  ROWID(bf-itemfg), 
                INPUT  ipbf-ttLoadTag.jobID,
                INPUT  ipbf-ttLoadTag.jobID2,
                OUTPUT bf-loadtag.loc, 
                OUTPUT bf-loadtag.loc-bin
                ).
    
        IF glCreateRFIDTag THEN DO:
            RUN GetNextRFIDTag (
                INPUT  ipbf-ttLoadTag.company, 
                OUTPUT cRfidTag
                ).
    
            CREATE bf-rfidtag.
            ASSIGN 
                bf-rfidtag.company   = bf-loadtag.company
                bf-rfidtag.item-type = bf-loadtag.item-type
                bf-rfidtag.tag-no    = bf-loadtag.tag-no
                bf-rfidtag.rfidtag   = cRfidTag
                .
        END.

        ASSIGN
            ipbf-ttLoadTag.tag          = bf-loadtag.tag-no
            ipbf-ttLoadTag.warehouseID  = bf-loadtag.loc
            ipbf-ttLoadTag.locationID   = bf-loadtag.loc-bin
            ipbf-ttLoadTag.rfidTag      = cRfidTag
            ipbf-ttLoadTag.itemPalletID = bf-loadtag.pallet-no
            ipbf-ttLoadTag.createdUser  = bf-loadtag.createUser
            ipbf-ttLoadTag.createdDate  = bf-loadtag.tag-date
            ipbf-ttLoadTag.createdTime  = bf-loadtag.tag-time
            ipbf-ttLoadTag.tagStatus    = "Created"
            .

        IF ipbf-ttLoadTag.recordSource EQ "BOL" OR ipbf-ttLoadTag.recordSource EQ "RELEASE" THEN
            RETURN.

        IF glCreateFGReceipts AND glCheckClosedStatus THEN DO:
            RUN po/POProcs.p PERSISTENT SET hdPOProcs.
            
            RUN CheckPOLineStatus IN hdPoProcs (
                INPUT ipbf-ttLoadTag.company,
                INPUT ipbf-ttLoadTag.poID,
                INPUT ipbf-ttLoadTag.poLineID
                ) NO-ERROR.
            
            DELETE PROCEDURE hdPOProcs.
            
            IF ERROR-STATUS:ERROR THEN 
                UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
     
            RUN jc/JobProcs.p PERSISTENT SET hdJobProcs.
            
            RUN CheckJobStatus IN hdJobProcs (
                INPUT ipbf-ttLoadTag.company,
                INPUT ipbf-ttLoadTag.jobID,
                INPUT ipbf-ttLoadTag.jobID2
                ) NO-ERROR.
                
            DELETE PROCEDURE hdJobProcs.
            
            IF ERROR-STATUS:ERROR THEN
                UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK. 
        END.
        
        
        IF glCreateFGReceipts THEN DO:
            iNextRNo = 0.
            FIND LAST bf-fg-rctd USE-INDEX fg-rctd NO-LOCK NO-ERROR.
            IF AVAILABLE bf-fg-rctd AND bf-fg-rctd.r-no GT iNextRNo THEN 
                iNextRNo = bf-fg-rctd.r-no.
    
            FIND LAST bf-fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
            IF AVAILABLE bf-fg-rcpth AND bf-fg-rcpth.r-no GT iNextRNo THEN
                iNextRNo = bf-fg-rcpth.r-no.
    
            DO WHILE TRUE:
                iNextRNo = iNextRNo + 1.
                FIND FIRST bf-fg-rcpth NO-LOCK
                     WHERE bf-fg-rcpth.r-no EQ iNextRNo 
                     USE-INDEX r-no NO-ERROR.
                IF AVAILABLE bf-fg-rcpth THEN
                    NEXT.
                    
                FIND FIRST bf-fg-rctd NO-LOCK
                     WHERE bf-fg-rctd.r-no EQ iNextRNo 
                     USE-INDEX fg-rctd NO-ERROR.
                IF AVAILABLE bf-fg-rctd THEN
                    NEXT.
                    
                LEAVE.
            END.
    
            CREATE bf-fg-rctd.
            ASSIGN
                bf-fg-rctd.r-no       = iNextRNo + 1
                bf-fg-rctd.rct-date   = TODAY
                bf-fg-rctd.trans-time = TIME
                bf-fg-rctd.company    = ipbf-ttLoadTag.company
                bf-fg-rctd.rita-code  = "R"
                bf-fg-rctd.i-name     = bf-itemfg.i-name
                bf-fg-rctd.i-no       = bf-loadtag.i-no
                bf-fg-rctd.job-no     = bf-loadtag.job-no
                bf-fg-rctd.job-no2    = bf-loadtag.job-no2
                bf-fg-rctd.t-qty      = bf-loadtag.pallet-count
                bf-fg-rctd.pur-uom    = bf-itemfg.prod-uom
                bf-fg-rctd.cost-uom   = bf-itemfg.prod-uom
                bf-fg-rctd.ext-cost   = (bf-fg-rctd.t-qty / 1000) * bf-fg-rctd.std-cost
                bf-fg-rctd.qty-case   = bf-loadtag.qty-case
                bf-fg-rctd.partial    = bf-loadtag.partial
                bf-fg-rctd.cases      = IF bf-loadtag.qty-case NE 0 THEN 
                                            TRUNCATE(bf-fg-rctd.t-qty / bf-loadtag.qty-case,0) 
                                        ELSE
                                            0
                bf-fg-rctd.cases-unit = bf-loadtag.case-bundle
                bf-fg-rctd.loc        = bf-loadtag.loc
                bf-fg-rctd.loc-bin    = bf-loadtag.loc-bin
                bf-fg-rctd.tag        = bf-loadtag.tag-no
                bf-fg-rctd.stack-code = bf-loadtag.misc-char[2]
                bf-fg-rctd.tot-wt     = bf-loadtag.misc-dec[1]
                .
    
            IF bf-fg-rctd.cases EQ 0 AND bf-fg-rctd.partial NE 0 THEN
                ASSIGN 
                    bf-fg-rctd.cases    = (IF bf-fg-rctd.partial LT 0 THEN -1 ELSE 1)
                    bf-fg-rctd.qty-case = (IF bf-fg-rctd.partial LT 0 THEN - bf-fg-rctd.partial ELSE bf-fg-rctd.partial)
                    bf-fg-rctd.partial  = 0
                    .
    
            CREATE ttFGRctd.
            ASSIGN 
                ttFGRctd.fgRctdROWID = ROWID(bf-fg-rctd)
                ttFGRctd.isComponent = ipbf-ttLoadTag.isComponent
                .
    
            IF ipbf-ttLoadTag.recordSource EQ "PO" THEN DO:
                ASSIGN
                    bf-fg-rctd.po-no   = TRIM(STRING(bf-loadtag.po-no,">>>>>>>>>>"))
                    bf-fg-rctd.po-line = bf-loadtag.line
                    .
    
                IF bf-loadtag.po-no GT 0 THEN
                    ASSIGN
                        bf-fg-rctd.job-no  = ""
                        bf-fg-rctd.job-no2 = 0
                        .
            END.
    
            IF TRIM(bf-fg-rctd.job-no) NE "" THEN
                FIND FIRST bf-job NO-LOCK
                     WHERE bf-job.company EQ bf-fg-rctd.company
                       AND bf-job.job-no  EQ bf-fg-rctd.job-no
                       AND bf-job.job-no2 EQ bf-fg-rctd.job-no2
                     USE-INDEX job 
                     NO-ERROR.
    
            IF AVAILABLE bf-job THEN DO:
                FIND FIRST bf-job-hdr NO-LOCK
                     WHERE bf-job-hdr.company EQ bf-job.company
                       AND bf-job-hdr.job-no  EQ bf-loadtag.job-no
                       AND bf-job-hdr.job-no2 EQ bf-loadtag.job-no2
                       AND bf-job-hdr.i-no    EQ bf-itemfg.i-no
                     NO-ERROR.
                IF AVAILABLE bf-job-hdr THEN 
                    bf-fg-rctd.std-cost = bf-job-hdr.std-tot-cost.
                ELSE
                    FIND FIRST bf-reftable NO-LOCK 
                         WHERE bf-reftable.reftable EQ "jc/jc-calc.p"
                           AND bf-reftable.company  EQ bf-job.company
                           AND bf-reftable.loc      EQ ""
                           AND bf-reftable.code     EQ STRING(bf-job.job,"999999999")
                           AND bf-reftable.code2    EQ bf-fg-rctd.i-no
                           USE-INDEX reftable
                         NO-ERROR.
    
                IF AVAILABLE bf-reftable AND bf-reftable.val[5] NE 0 THEN
                    bf-fg-rctd.std-cost = bf-reftable.val[5].
            END.
    
            IF NOT AVAILABLE bf-job-hdr AND NOT AVAILABLE bf-reftable THEN DO:
                bf-fg-rctd.std-cost = bf-itemfg.std-tot-cost.
                
                FIND FIRST bf-fg-bin NO-LOCK
                     WHERE bf-fg-bin.company EQ bf-itemfg.company
                       AND bf-fg-bin.i-no    EQ bf-itemfg.i-no
                       AND bf-fg-bin.job-no  EQ bf-loadtag.job-no
                       AND bf-fg-bin.job-no2 EQ bf-loadtag.job-no2 
                    NO-ERROR.
                IF AVAILABLE bf-fg-bin THEN 
                    bf-fg-rctd.std-cost = bf-fg-bin.std-tot-cost.
            END.
    
            IF glFGSetCreateAdjustment THEN DO:
                IF bf-itemfg.isaset AND
                  (bf-itemfg.alloc EQ NO  OR
                  (bf-itemfg.alloc EQ YES AND /* fgrecpt-char NE "Manual" AND*/ TRIM(bf-fg-rctd.job-no) NE "")) THEN DO:
                    ASSIGN
                        bf-fg-rctd.t-qty = bf-fg-rctd.cases * bf-fg-rctd.qty-case + bf-fg-rctd.partial
                        dMaxQty          = bf-fg-rctd.t-qty
                        .
        
                    RUN fg/checksetb.p (
                        INPUT        ROWID(bf-itemfg),
                        INPUT        ROWID(bf-fg-rctd),
                        INPUT        bf-fg-rctd.job-no,
                        INPUT        INT(bf-fg-rctd.job-no2),
                        INPUT        bf-fg-rctd.loc,
                        INPUT-OUTPUT dMaxQty
                        ).
        
                    IF dMaxQty LT bf-fg-rctd.t-qty THEN DO:
                        IF dMaxQty GT 0 AND glUpdateSetWithMaxQuantity THEN DO:
                            ASSIGN
                                bf-fg-rctd.t-qty   = dMaxQty
                                bf-fg-rctd.cases   = TRUNC((dMaxQty - DECIMAL(bf-fg-rctd.partial)) / DECIMAL(bf-fg-rctd.qty-case),0)
                                bf-fg-rctd.partial = dMaxQty - (DECIMAL(bf-fg-rctd.cases) * DECIMAL(bf-fg-rctd.qty-case))
                                .
        
                            IF bf-fg-rctd.cases EQ 0 AND bf-fg-rctd.partial NE 0 THEN
                                ASSIGN 
                                    bf-fg-rctd.cases    = (IF bf-fg-rctd.partial LT 0 THEN -1 ELSE 1)
                                    bf-fg-rctd.qty-case = (IF bf-fg-rctd.partial LT 0 THEN - bf-fg-rctd.partial ELSE bf-fg-rctd.partial)
                                    bf-fg-rctd.partial  = 0
                                    .
                        END.
                        
                        IF NOT glUpdateSetWithMaxQuantity OR dMaxQty EQ 0 THEN DO:                   
                            ASSIGN
                                oplError   = TRUE
                                opcMessage = "The finished goods receipt was not created "
                                           + "for item # " + bf-itemfg.i-no + ", Job # " 
                                           + bf-fg-rctd.job-no + "-" + STRING(bf-fg-rctd.job-no2)
                                           + " due to insufficient component inventory"
                                .
                            UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
                        END.
                    END.
                END.
            END.
            
            IF ipbf-ttLoadTag.poID NE 0 AND (ipbf-ttLoadTag.recordSource EQ "ORDER" OR ipbf-ttLoadTag.recordSource EQ "PO" OR ipbf-ttLoadTag.recordSource EQ "JOB") THEN DO:
                ASSIGN
                    bf-fg-rctd.po-no   = TRIM(STRING(ipbf-ttLoadTag.poID,">>>>>>>>>>"))
                    bf-fg-rctd.po-line = ipbf-ttLoadTag.poLineID
                    .
    
                FIND FIRST bf-po-ordl NO-LOCK
                     WHERE bf-po-ordl.company   EQ ipbf-ttLoadTag.company 
                       AND bf-po-ordl.po-no     EQ ipbf-ttLoadTag.poID
                       AND bf-po-ordl.item-type EQ NO
                       AND bf-po-ordl.i-no      EQ ipbf-ttLoadtag.itemID
                       AND (bf-po-ordl.line     EQ ipbf-ttLoadTag.poLineID OR ipbf-ttLoadTag.poLineID EQ 0)
                     NO-ERROR.
    
                IF AVAILABLE bf-po-ordl THEN DO:
                    IF bf-fg-rctd.po-line EQ 0 THEN 
                        bf-fg-rctd.po-line = 1.
                        
                    RUN pGetCostFromPO (
                        INPUT  bf-po-ordl.company, 
                        INPUT  bf-po-ordl.po-no, 
                        INPUT  bf-po-ordl.line, 
                        INPUT  bf-po-ordl.i-no, 
                        INPUT  bf-fg-rctd.t-qty,
                        OUTPUT bf-fg-rctd.std-cost, 
                        OUTPUT bf-fg-rctd.cost-uom, 
                        OUTPUT bf-fg-rctd.ext-cost, 
                        OUTPUT bf-fg-rctd.frt-cost
                        ).
                        
                    RUN Conv_ValueFromUOMtoUOM (
                        INPUT  ipbf-ttLoadTag.company, 
                        INPUT  bf-po-ordl.i-no, 
                        INPUT  "FG", 
                        INPUT  bf-fg-rctd.std-cost, 
                        INPUT  bf-fg-rctd.cost-uom, 
                        INPUT  bf-po-ordl.cons-uom, 
                        INPUT  0, 
                        INPUT  bf-po-ordl.s-len,
                        INPUT  bf-po-ordl.s-wid,
                        INPUT  bf-po-ordl.s-dep, 0, 
                        OUTPUT bf-fg-rctd.std-cost, 
                        OUTPUT lError, 
                        OUTPUT cMessage
                        ).
                        
                    ASSIGN 
                        bf-fg-rctd.pur-uom  = bf-po-ordl.cons-uom 
                        bf-fg-rctd.cost-uom = bf-po-ordl.cons-uom
                        .
                END.
            END.
            ELSE DO:
                /* Development in progress */
/*                RUN calc-ext-cost.*/
            END.
    
            IF ipbf-ttLoadTag.estID NE "" THEN DO:
                IF bf-itemfg.pur-man THEN 
                DO:
                    IF TRIM(bf-fg-rctd.job-no) NE "" AND TRIM(bf-fg-rctd.po-no) NE "" AND ipbf-ttLoadTag.recordSource EQ "PO" THEN
                        ASSIGN 
                            bf-fg-rctd.job-no  = "" 
                            bf-fg-rctd.job-no2 = 0
                            bf-fg-rctd.po-no   = TRIM(STRING(ipbf-ttLoadTag.poID,">>>>>>>>>>"))
                            bf-fg-rctd.po-line = ipbf-ttLoadTag.poLineID
                            .
                    ELSE IF TRIM(bf-fg-rctd.job-no) NE "" AND TRIM(bf-fg-rctd.po-no) NE "" AND (ipbf-ttLoadTag.recordSource EQ "ORDER" OR ipbf-ttLoadTag.recordSource EQ "JOB") THEN
                        ASSIGN
                            bf-fg-rctd.po-no   = ""
                            bf-fg-rctd.job-no  = bf-loadtag.job-no
                            bf-fg-rctd.job-no2 = bf-loadtag.job-no2
                            .
                END.
                ELSE IF NOT bf-itemfg.pur-man THEN DO:
                    IF TRIM(bf-fg-rctd.po-no) NE "" AND TRIM(bf-loadtag.job-no) NE "" AND (ipbf-ttLoadTag.recordSource EQ "ORDER" OR ipbf-ttLoadTag.recordSource EQ "JOB") THEN
                        ASSIGN 
                            bf-fg-rctd.po-no   = ""
                            bf-fg-rctd.job-no  = bf-loadtag.job-no
                            bf-fg-rctd.job-no2 = bf-loadtag.job-no2
                            .
                    ELSE IF TRIM(bf-fg-rctd.po-no) NE "" AND TRIM(bf-loadtag.job-no) NE "" AND ipbf-ttLoadTag.recordSource EQ "PO" THEN
                        ASSIGN 
                            bf-fg-rctd.job-no  = "" 
                            bf-fg-rctd.job-no2 = 0
                            bf-fg-rctd.po-no   = TRIM(STRING(ipbf-ttLoadTag.poID,">>>>>>>>>>"))
                            bf-fg-rctd.po-line = ipbf-ttLoadTag.poLineID
                            .
                END.
            END.

            IF NOT ( NOT glFGSetCreateAdjustment AND bf-itemfg.alloc NE YES) THEN
                RUN fg/comprcpt.p (
                    INPUT ROWID(bf-fg-rctd)
                    ).
        END.
       
        /* Update the other tags with this new quantity */
/*        IF AVAILABLE bf-fg-rctd AND lv-use-full-qty THEN*/
/*            RUN get-set-full-qty (                        */
/*                INPUT  fg-rctd.job-no,                    */
/*                INPUT  fg-rctd.job-no2,                   */
/*                INPUT  fg-rctd.i-no,                      */
/*                INPUT  0 /* new qty */,                   */
/*                INPUT  fg-rctd.std-cost /* cost to set */,*/
/*                OUTPUT lv-full-qty                        */
/*                ).                                        */
    END.
END PROCEDURE.

PROCEDURE pCreateLoadTagRM:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttLoadTag FOR ttLoadTag.
    DEFINE OUTPUT PARAMETER oplError   AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
        
    DEFINE VARIABLE cRfidTag              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iNextRNo              AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dCostPerUOM           AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cNextLoadtag          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lError                AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hdPOProcs             AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hdJobProcs            AS HANDLE    NO-UNDO. 
    DEFINE VARIABLE dMaxQty               AS DECIMAL   NO-UNDO.
    
    DEFINE BUFFER bf-loadtag  FOR loadtag.
    DEFINE BUFFER bf-po-ordl  FOR po-ordl.
    DEFINE BUFFER bf-item     FOR item.
    DEFINE BUFFER bf-job      FOR job.
    DEFINE BUFFER bf-job-hdr  FOR job-hdr.
    
    MAIN-BLOCK:
    DO ON ERROR UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
        ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
        IF NOT AVAILABLE ipbf-ttLoadTag THEN DO:
            ASSIGN
                oplError  = TRUE
                opcMessage = "Record not found for creating loadtag"
                .
            UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.    
        END.
        
        FIND FIRST bf-item NO-LOCK
             WHERE bf-item.company EQ ipbf-ttLoadTag.company
               AND bf-item.i-no    EQ ipbf-ttLoadTag.itemID
             NO-ERROR.
        IF NOT AVAILABLE bf-item THEN DO:
            ASSIGN
                oplError  = TRUE
                opcMessage = "Raw Material item # '" + ipbf-ttLoadTag.itemID + "' not found"
                .
            UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.    
        END.
        
        RUN loadtags\GetNextTag.p (
            INPUT  ipbf-ttLoadTag.company, 
            INPUT  ipbf-ttLoadTag.itemID,
            OUTPUT cNextLoadtag
            ).    
    
        CREATE bf-loadtag.
        ASSIGN
            bf-loadtag.company      = ipbf-ttLoadTag.company
            bf-loadtag.tag-no       = IF ipbf-ttLoadTag.vendorTag NE "" THEN ipbf-ttLoadTag.vendorTag ELSE cNextLoadtag 
            bf-loadtag.i-no         = CAPS(ipbf-ttLoadTag.itemID)
            bf-loadtag.i-name       = ipbf-ttLoadTag.itemName
            bf-loadtag.item-type    = TRUE /* RM Item*/
            bf-loadtag.job-no       = ipbf-ttLoadTag.jobID
            bf-loadtag.job-no2      = ipbf-ttLoadTag.jobID2
            bf-loadtag.po-no        = ipbf-ttLoadTag.poID
            bf-loadtag.line         = ipbf-ttLoadTag.poLineID
            bf-loadtag.form-no      = ipbf-ttLoadTag.formNo
            bf-loadtag.blank-no     = ipbf-ttLoadTag.blankNo
            bf-loadtag.ord-no       = ipbf-ttLoadTag.orderID
            bf-loadtag.sts          = "Printed"
            bf-loadtag.tag-date     = TODAY
            bf-loadtag.tag-time     = TIME
            bf-loadtag.misc-dec[1]  = ipbf-ttLoadTag.unitWeight 
            bf-loadtag.misc-dec[2]  = ipbf-ttLoadTag.palletWeight
            bf-loadtag.misc-char[2] = ipbf-ttLoadTag.lotID
            bf-loadtag.spare-char-1 = ipbf-ttLoadTag.SSCC
            bf-loadtag.pallet-no    = ipbf-ttLoadTag.itemPalletID
            bf-loadtag.misc-char[1] = ipbf-ttLoadTag.vendorTag
            .

        ASSIGN
            bf-loadtag.qty          = ipbf-ttLoadTag.ordQuantity
            bf-loadtag.qty-case     = ipbf-ttLoadTag.quantityInSubUnit
            bf-loadtag.case-bundle  = ipbf-ttLoadTag.subUnitsPerUnit
            bf-loadtag.pallet-count = ipbf-ttLoadTag.quantityInUnit
            bf-loadtag.loc          = ipbf-ttLoadTag.warehouseID
            bf-loadtag.loc-bin      = ipbf-ttLoadTag.locationID
            .

        ASSIGN
            ipbf-ttLoadTag.tag          = bf-loadtag.tag-no
            ipbf-ttLoadTag.warehouseID  = bf-loadtag.loc
            ipbf-ttLoadTag.locationID   = bf-loadtag.loc-bin
            ipbf-ttLoadTag.rfidTag      = cRfidTag
            ipbf-ttLoadTag.itemPalletID = bf-loadtag.pallet-no
            ipbf-ttLoadTag.createdUser  = bf-loadtag.createUser
            ipbf-ttLoadTag.createdDate  = bf-loadtag.tag-date
            ipbf-ttLoadTag.createdTime  = bf-loadtag.tag-time
            ipbf-ttLoadTag.tagStatus    = "Created"
            .
    END.
END PROCEDURE.

PROCEDURE Loadtag_CreateLoadTagFromVendorTag:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcVendorTag AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError     AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage   AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE iPOID        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iPOLine      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dQuantity    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cAddInfo     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cQuantityUOM AS CHARACTER NO-UNDO.
        
    DEFINE BUFFER bf-po-ord  FOR po-ord.
    DEFINE BUFFER bf-po-ordl FOR po-ordl.
    DEFINE BUFFER bf-item    FOR item.
    DEFINE BUFFER bf-itemfg  FOR itemfg.
    DEFINE BUFFER bf-loadtag FOR loadtag.
    
    RUN addon/rm/vendorTagParse.p(
        INPUT  ipcVendorTag,
        OUTPUT iPOID,
        OUTPUT iPOLine,
        OUTPUT dQuantity,
        OUTPUT cAddInfo,
        OUTPUT opcMessage,
        OUTPUT oplError
        ).
    IF oplError THEN
        RETURN.
    
    IF dQuantity EQ 0 THEN DO:
        ASSIGN
            oplError   = TRUE
            opcMessage = "Quantity cannot be 0"
            .
        RETURN.     
    END.
    
    FIND FIRST bf-po-ord NO-LOCK
         WHERE bf-po-ord.company EQ ipcCompany
           AND bf-po-ord.po-no   EQ iPOID
         NO-ERROR.
    IF NOT AVAILABLE bf-po-ord THEN DO:
        ASSIGN
            oplError   = TRUE
            opcMessage = "Invalid PO # '" + STRING(iPOID) + "' scanned in vendor tag"
            .
        RETURN. 
    END.    
    
    FIND FIRST bf-po-ordl NO-LOCK
         WHERE bf-po-ordl.company EQ bf-po-ord.company
           AND bf-po-ordl.po-no   EQ bf-po-ord.po-no
           AND bf-po-ordl.line    EQ iPOLine
         NO-ERROR.
    IF NOT AVAILABLE bf-po-ordl THEN DO:
        ASSIGN
            oplError   = TRUE
            opcMessage = "Invalid PO # Line '" + STRING(iPOID) + "-" + STRING(iPOLine) + "' scanned in vendor tag"
            .
        RETURN. 
    END.    

    IF bf-po-ordl.item-type THEN DO:
        FIND FIRST bf-item NO-LOCK
             WHERE bf-item.company EQ bf-po-ordl.company
               AND bf-item.i-no    EQ bf-po-ordl.i-no
             NO-ERROR. 
        IF NOT AVAILABLE bf-item THEN DO:
            ASSIGN
                oplError   = TRUE
                opcMessage = "Invalid Item # '" + bf-po-ordl.i-no + "'"
                .
            RETURN.
        END.
        
        cQuantityUOM = bf-item.cons-uom.
    END.
    ELSE DO:
        FIND FIRST bf-itemfg NO-LOCK
             WHERE bf-itemfg.company EQ bf-po-ordl.company
               AND bf-itemfg.i-no    EQ bf-po-ordl.i-no
             NO-ERROR.
        IF NOT AVAILABLE bf-itemfg THEN DO:
            ASSIGN
                oplError   = TRUE
                opcMessage = "Invalid Item # '" + bf-po-ordl.i-no + "'"
                .
            RETURN.
        END.
        
        cQuantityUOM = bf-po-ordl.pr-qty-uom.
    END.
    
    FIND FIRST bf-loadtag NO-LOCK
         WHERE bf-loadtag.company      EQ bf-po-ordl.company
           AND bf-loadtag.i-no         EQ bf-po-ordl.i-no
           AND bf-loadtag.item-type    EQ bf-po-ordl.item-type
           AND bf-loadtag.po-no        EQ bf-po-ordl.po-no
           AND bf-loadtag.line         EQ bf-po-ordl.line
           AND bf-loadtag.is-case-tag  EQ NO
           AND bf-loadtag.misc-char[1] EQ ipcVendorTag
         NO-ERROR.
    IF AVAILABLE bf-loadtag THEN
        RETURN.
    
    EMPTY TEMP-TABLE ttLoadTag.
    
    RUN pBuildLoadTagsFromPO (
        INPUT  bf-po-ordl.company,
        INPUT  bf-po-ordl.po-no,
        INPUT  bf-po-ordl.line,
        INPUT  dQuantity,
        INPUT  dQuantity,
        INPUT  1,    
        INPUT  cQuantityUOM,
        INPUT  1,
        INPUT  ipcVendorTag,
        OUTPUT oplError,
        OUTPUT opcMessage,
        INPUT-OUTPUT TABLE ttLoadTag
        ).
    IF oplError THEN
        RETURN.
        
    RUN pCreateLoadTagFromTT (
        INPUT  bf-po-ordl.company, 
        INPUT  bf-po-ord.loc, 
        INPUT  FALSE, 
        INPUT  TRUE, 
        INPUT-OUTPUT TABLE ttLoadTag
        ).        
END PROCEDURE.

PROCEDURE GetNextRFIDTag:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcRfidTag AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE dRFIDTag AS DECIMAL NO-UNDO.
    
    DEFINE BUFFER bf-oe-ctrl FOR oe-ctrl.
    
    FIND FIRST bf-oe-ctrl EXCLUSIVE-LOCK
         WHERE bf-oe-ctrl.company EQ ipcCompany
         NO-ERROR.
         
    dRFIDTag = IF AVAILABLE bf-oe-ctrl AND bf-oe-ctrl.spare-char-1 <> "" THEN
                   DECIMAL(bf-oe-ctrl.spare-char-1)
               ELSE 
                   111110000000000000000001. 
    
    bf-oe-ctrl.spare-char-1 = STRING(dRFIDTag + 1).

    opcRfidTag = STRING(dRFIDTag).

END PROCEDURE.

PROCEDURE pCreateSetComponentsForTTLoadTagItem PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipiTTLoadTagRecordID AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSetsCreated       AS LOGICAL NO-UNDO.
    
    DEFINE BUFFER bf-ttLoadTag     FOR ttLoadTag.
    DEFINE BUFFER bf-set-ttLoadTag FOR ttLoadTag.
    DEFINE BUFFER bf-itemfg        FOR itemfg.
    DEFINE BUFFER bf-set-itemfg    FOR itemfg.
    
    FOR FIRST bf-ttLoadTag
        WHERE bf-ttLoadTag.recordID   EQ ipiTTLoadTagRecordID
          AND bf-ttLoadTag.dontRunSet EQ NO:
        
        FIND FIRST bf-itemfg NO-LOCK
             WHERE bf-itemfg.company EQ bf-ttLoadTag.company
               AND bf-itemfg.i-no    EQ bf-ttLoadTag.itemID
               AND bf-itemfg.isaset
             NO-ERROR.
        IF NOT AVAILABLE bf-itemfg THEN
            RETURN.

        /* Populates temp-table tt-fg-set with set companents for a given itemfg */
        RUN fg/fullset.p (
            ROWID(bf-itemfg)
            ).
        
        FOR EACH tt-fg-set 
            WHERE tt-fg-set.part-no NE bf-ttLoadTag.itemID,
            FIRST bf-set-itemfg NO-LOCK
            WHERE bf-set-itemfg.company EQ tt-fg-set.company
              AND bf-set-itemfg.i-no    EQ tt-fg-set.part-no:
            CREATE bf-set-ttLoadTag.
            BUFFER-COPY bf-ttLoadTag EXCEPT bf-ttLoadTag.recordID TO bf-set-ttLoadTag.      
            ASSIGN
                bf-set-ttLoadTag.recordID    = fGetNextTTLoadTagRecordID()
                bf-set-ttLoadTag.itemID      = tt-fg-set.part-no
                bf-set-ttLoadTag.itemName    = bf-set-itemfg.i-name
                bf-set-ttLoadTag.isComponent = YES
                bf-set-ttLoadTag.ordQuantity = bf-ttLoadTag.ordQuantity * tt-fg-set.part-qty-dec
                bf-set-ttLoadTag.boxLen      = bf-set-itemfg.l-score[50]
                bf-set-ttLoadTag.boxWid      = bf-set-itemfg.w-score[50]
                bf-set-ttLoadTag.boxDep      = bf-set-itemfg.d-score[50].
                oplSetsCreated               = YES
                .
   
            RUN pUpdateTTLoadTagEstimateDetails (
                INPUT bf-set-ttLoadTag.company,
                INPUT bf-set-ttLoadTag.estID,
                INPUT bf-set-ttLoadTag.recordID
                ).
        END.

        oplSetsCreated = oplSetsCreated AND ( IF bf-itemfg.alloc EQ ? THEN FALSE ELSE bf-itemfg.alloc).
    END.

END PROCEDURE.

PROCEDURE pGetCostFromPO PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany          AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPONumber         AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPOLine           AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFGItemID         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdQty              AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOM       AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCostUOM          AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostTotal        AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostTotalFreight AS DECIMAL   NO-UNDO.

    DEFINE VARIABLE dCostPerEA        AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dCostFreight      AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dCostFreightPerEA AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lFound            AS LOGICAL NO-UNDO.
    DEFINE VARIABLE hdCostProcs       AS HANDLE  NO-UNDO.
    
    RUN system/CostProcs.p PERSISTENT SET hdCostProcs.
    
    RUN GetCostForPOLine IN hdCostProcs (
        INPUT  ipcCompany, 
        INPUT  ipiPONumber, 
        INPUT  ipiPOLine, 
        INPUT  ipcFGItemID, 
        OUTPUT opdCostPerUOM, 
        OUTPUT opcCostUOM, 
        OUTPUT dCostFreight, 
        OUTPUT lFound
        ).
    
    dCostPerEA = DYNAMIC-FUNCTION('fConvertCostForItem':U IN hdCostProcs,
                                  INPUT ipcCompany, 
                                  INPUT ipcFGItemID, 
                                  INPUT "FG", 
                                  INPUT opdCostPerUOM, 
                                  INPUT opcCostUOM, 
                                  INPUT "EA", 
                                  INPUT 0, /*BasisWeight*/
                                  INPUT 0, /*Length override - leave as 0 if not in UI or on Order/PO*/
                                  INPUT 0, /*Width override - leave as 0 if not in UI or on Order/PO*/
                                  INPUT 0, /*Depth override - leave as 0 if not in UI or on Order/PO*/
                                  INPUT 0, /*Case Count override - leave as 0 if not in UI or on Order/PO*/
                                  INPUT ipdQty, /*Lot Quantity - leave as 0 if not in UI or on Order/PO*/
                                  INPUT "EA" /*Lot Quantity UOM - leave as "" if not in UI or on PO*/
                                  ).
    
    dCostFreightPerEA = DYNAMIC-FUNCTION('fConvertCostForItem':U IN hdCostProcs,
                                         INPUT ipcCompany, 
                                         INPUT ipcFGItemID, 
                                         INPUT "FG", 
                                         INPUT dCostFreight, 
                                         INPUT opcCostUOM, 
                                         INPUT "EA", 
                                         INPUT 0, /*BasisWeight*/
                                         INPUT 0, /*Length override - leave as 0 if not in UI or on Order/PO*/
                                         INPUT 0, /*Width override - leave as 0 if not in UI or on Order/PO*/
                                         INPUT 0, /*Depth override - leave as 0 if not in UI or on Order/PO*/
                                         INPUT 0, /*Case Count override - leave as 0 if not in UI or on Order/PO*/
                                         INPUT ipdQty, /*Lot Quantity - leave as 0 if not in UI or on Order/PO*/
                                         INPUT "EA" /*Lot Quantity UOM - leave as "" if not in UI or on PO*/
                                         ).
    
    ASSIGN 
        opdCostTotal        = ipdQty * dCostPerEA
        opdCostTotalFreight = ipdQty * dCostFreightPerEA
        .
    
/*    IF fgpofrt-log THEN*/
        opdCostTotal = opdCostTotal + opdCostTotalFreight.
    
    DELETE PROCEDURE hdCostProcs.
END PROCEDURE.


PROCEDURE pGetExportTemplateFile PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcSysCtrlName  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemID       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCustomerID   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcShipID       AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcPathTemplate AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcTemplateType AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lFound  AS LOGICAL   NO-UNDO.
    
    DEFINE BUFFER bf-cust-part FOR cust-part.
    
    FIND FIRST bf-cust-part NO-LOCK
         WHERE bf-cust-part.company EQ ipcCompany
           AND bf-cust-part.cust-no EQ ipcCustomerID
           AND bf-cust-part.i-no    EQ ipcItemID
         NO-ERROR.
    IF AVAILABLE bf-cust-part THEN
        opcPathTemplate = bf-cust-part.labelPallet.
    
    IF opcPathTemplate EQ "" THEN DO:
        RUN sys/ref/nk1look.p (
            INPUT  ipcCompany, 
            INPUT  ipcSysCtrlName, 
            INPUT  "C", 
            INPUT  YES, 
            INPUT  YES, 
            INPUT  ipcCustomerID, 
            INPUT  ipcShipID, 
            OUTPUT opcPathTemplate, 
            OUTPUT lFound
            ).
    END.
    
    IF opcPathTemplate NE "" THEN
        opcTemplateType = IF opcPathTemplate MATCHES "*.xpr*" THEN 
                              "XPrint" 
                          ELSE IF opcPathTemplate MATCHES "*.lwl" THEN 
                              "Loftware" 
                          ELSE IF opcPathTemplate MATCHES "*.qdf" THEN
                              "LabelMatrix"
                          ELSE
                              "".
END PROCEDURE.

PROCEDURE pGetJobMaterialDimensions PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcJobNo        AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipiJobNo2       AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipiFormNo       AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipcItemID       AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopdLength      AS DECIMAL   NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopdWidth       AS DECIMAL   NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopdDepth       AS DECIMAL   NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopdBasisWeight AS DECIMAL   NO-UNDO.

    DEFINE BUFFER bf-job     FOR job.
    DEFINE BUFFER bf-job-mat FOR job-mat.
    DEFINE BUFFER bf-item    FOR item.
    
    FIND FIRST bf-item NO-LOCK
         WHERE bf-item.company EQ ipcCompany
           AND bf-item.i-no    EQ ipcItemID
         NO-ERROR.
    IF NOT AVAILABLE bf-item THEN 
        RETURN.
        
    IF (iopdLength EQ 0 OR iopdWidth EQ 0 OR iopdBasisWeight EQ 0) THEN DO:
        FIND FIRST bf-job NO-LOCK
             WHERE bf-job.company EQ ipcCompany
               AND bf-job.job-no  EQ ipcJobNo
               AND bf-job.job-no2 EQ ipiJobNo2
             NO-ERROR.

        IF AVAILABLE bf-job THEN DO :
            FOR EACH bf-job-mat NO-LOCK
                WHERE bf-job-mat.company EQ bf-job.company
                  AND bf-job-mat.job     EQ bf-job.job
                  AND bf-job-mat.job-no  EQ bf-job.job-no
                  AND bf-job-mat.job-no2 EQ bf-job.job-no2
                  AND bf-job-mat.i-no    EQ ipcItemID
                BY bf-job-mat.frm DESCENDING:
                  
                IF bf-job-mat.frm EQ ipiFormNo THEN 
                    LEAVE.
            END.
              
            IF AVAILABLE bf-job-mat THEN
                ASSIGN
                    iopdLength      = IF iopdLength EQ 0 THEN bf-job-mat.len ELSE iopdLength
                    iopdWidth       = IF iopdWidth EQ 0 THEN bf-job-mat.wid ELSE iopdWidth
                    iopdBasisWeight = IF iopdBasisWeight EQ 0 THEN bf-job-mat.basis-w ELSE iopdBasisWeight
                    .
        END.

        IF iopdLength EQ 0 THEN iopdLength = bf-item.s-len.

        IF iopdWidth EQ 0 THEN
            iopdWidth = IF bf-item.r-wid NE 0 THEN bf-item.r-wid ELSE bf-item.s-wid.

        IF iopdBasisWeight EQ 0 THEN iopdBasisWeight = bf-item.basis-w.
    END.

END PROCEDURE.

PROCEDURE pIncrementCustPalletID PRIVATE:
/*------------------------------------------------------------------------------
  Purpose:     Increment the pallet number for a given customer and return the
                new value
  Parameters:  INPUT: cust buffer OUTPUT: next pallet #
  Notes:       Defaults value if not set for given cust
               Returns error code of -1  
               Copied from r-loadtg.w 
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCustID        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiTags          AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opiStartPalletID AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opiEndPalletID   AS INTEGER   NO-UNDO.
    
    DEFINE VARIABLE iPalletID AS INTEGER NO-UNDO.
    DEFINE VARIABLE lTagCount AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bf-cust FOR cust.
    
    FIND FIRST bf-cust EXCLUSIVE-LOCK
         WHERE bf-cust.company EQ ipcCompany
           AND bf-cust.cust-no EQ ipcCustID
           NO-ERROR.
    IF NOT AVAILABLE bf-cust THEN DO:
        opiEndPalletID = 0.
        RETURN.
    END.
    
    IF bf-cust.spare-int-1 EQ 0 THEN DO:
        opiEndPalletID = 0.
        RETURN.
    END.
    
    iPalletID = bf-cust.spare-int-1.
    
    IF iPalletID MOD 1000000 = 999999 THEN DO:
        opiEndPalletID = -1.
        RETURN.
    END.
    
    opiStartPalletID = iPalletID + 1.
    DO lTagCount = 1 TO ipiTags:
        iPalletID = iPalletID + 1.
        IF iPalletID MOD 1000000 = 999999 THEN DO:
            /*protection code*/
            opiEndPalletID = -1.
            RETURN.
        END.
    END.
    
    ASSIGN 
        opiEndPalletID      = iPalletID
        bf-cust.spare-int-1 = iPalletID
        .

END PROCEDURE.

PROCEDURE pPrintLabelMatrix PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcPathTemplate AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcType         AS CHARACTER NO-UNDO.

    DEFINE VARIABLE hdOutputProcs AS HANDLE NO-UNDO.

    RUN system/OutputProcs.p PERSISTENT SET hdOutputProcs.
    
    RUN PrintLabelMatrixFile IN hdOutputProcs (
        INPUT ipcCompany,
        INPUT ipcPathTemplate,
        INPUT ipcType
        ).
        
    DELETE PROCEDURE hdOutputProcs.
    
END PROCEDURE.

PROCEDURE pPrintLoadTag PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttLoadTag FOR ttLoadTag.
    
    OUTPUT TO VALUE (gcLoadTagOutputPath + gcLoadTagOutputFile) APPEND.
      
    IF AVAILABLE ipbf-ttLoadTag THEN DO:
        
        PUT UNFORMATTED
        '"'  fReplaceQuotes(ipbf-ttLoadTag.custName)  '",'
             ipbf-ttLoadTag.orderID  ","
        '"'  IF ipbf-ttLoadTag.jobID EQ "" THEN STRING(ipbf-ttLoadTag.orderID) ELSE ipbf-ttLoadTag.jobID + "-" + STRING(ipbf-ttLoadTag.jobID2, "999")  '",'
        '"'  CAPS(fReplaceQuotes(ipbf-ttLoadTag.itemID))  FORM "x(15)" '",'
        '"'  fReplaceQuotes(ipbf-ttLoadTag.custPartNo) '",'
        '"'  fReplaceQuotes(ipbf-ttLoadTag.custPONo)  '",'
             ipbf-ttLoadTag.quantityInSubUnit  ","
             ipbf-ttLoadTag.subUnitsPerUnit  ","
             TRIM(STRING(ipbf-ttLoadTag.quantityInUnit, ">>>>>>>9")) ","
        '"'  fReplaceQuotes(ipbf-ttLoadTag.shipID)  '",'
        '"'  fReplaceQuotes(ipbf-ttLoadTag.shipName)  '",'
        '"'  fReplaceQuotes(ipbf-ttLoadTag.shipAddress1)  '",'
        '"'  fReplaceQuotes(ipbf-ttLoadTag.shipAddress2)  '",'
        '"'  fReplaceQuotes(ipbf-ttLoadTag.shipCity)  '",'
        '"'  fReplaceQuotes(ipbf-ttLoadTag.shipState) '",'
        '"'  fReplaceQuotes(ipbf-ttLoadTag.shipCountry)  '",'
        '"'  fReplaceQuotes(ipbf-ttLoadTag.shipZip)   '",'
        '"'  fReplaceQuotes(ipbf-ttLoadTag.soldID)  '",'
        '"'  fReplaceQuotes(ipbf-ttLoadTag.soldName)  '",'
        '"'  fReplaceQuotes(ipbf-ttLoadTag.soldAddress1)  '",'
        '"'  fReplaceQuotes(ipbf-ttLoadTag.soldAddress2)  '",'
        '"'  fReplaceQuotes(ipbf-ttLoadTag.soldCity)  '",'
        '"'  fReplaceQuotes(ipbf-ttLoadTag.soldState) '",'
        '"'  fReplaceQuotes(ipbf-ttLoadTag.soldCountry)  '",'
        '"'  fReplaceQuotes(ipbf-ttLoadTag.soldZip)   '",'
        '"'  fReplaceQuotes(ipbf-ttLoadTag.itemName) FORMAT "X(30)"  '",'
        '"'  ipbf-ttLoadTag.dueDate  '",'
        '"'  ipbf-ttLoadTag.relDate  '",'
        '"'  ipbf-ttLoadTag.upcNo FORMAT "x(20)" '",'
        '"'  ipbf-ttLoadTag.boxLen FORMAT ">>>9.99<<<" '",'
        '"'  ipbf-ttLoadTag.boxWid FORMAT ">>>9.99<<<" '",'
        '"'  ipbf-ttLoadTag.boxDep FORMAT ">>>9.99<<<" '",'
        '"'  ipbf-ttLoadTag.flute  '",'
        '"'  ipbf-ttLoadTag.test  '",'
        '"'  ipbf-ttLoadTag.vendorName  '",'
             ipbf-ttLoadTag.grossWeight  ","
             ipbf-ttLoadTag.tareWeight  ","
             ipbf-ttLoadTag.netWeight  ","
             ipbf-ttLoadTag.sheetWeight  ","
        '"'  ipbf-ttLoadTag.uom  '",'
        '"'  fReplaceQuotes(ipbf-ttLoadTag.style) '",'
        '"'  fReplaceQuotes(ipbf-ttLoadTag.styleDesc) '",'
        '"'  fReplaceQuotes(ipbf-ttLoadTag.relLotID) '",'
        '"'  /* lv-middlesex-job */ '",'
        '"'  /* lv-middlesex-po */ '",'
        '"'  ipbf-ttLoadTag.tag '",'
        '"'  ipbf-ttLoadTag.partial '",'
        '"'  ipbf-ttLoadTag.caseNo  '",'
        '"'  /* fReplaceQuotes(v-dept-note[1]) */ '",'
        '"'  /* fReplaceQuotes(v-dept-note[2]) */ '",'
        '"'  /* fReplaceQuotes(v-dept-note[3]) */ '",'
        '"'  /* fReplaceQuotes(v-dept-note[4]) */ '",'
        '"'  /* fReplaceQuotes(v-dept-note[5]) */ '",'
        '"'  /* fReplaceQuotes(v-dept-note[6]) */ '",'
        '"'  /* fReplaceQuotes(v-dept-note[7]) */ '",'
        '"'  /* fReplaceQuotes(v-dept-note[8]) */ '",'
             ipbf-ttLoadTag.poID ","
        '"'  /* fReplaceQuotes(v-dept-note[9])  */ '",'
        '"'  /* fReplaceQuotes(v-dept-note[10]) */ '",'
        '"'  /* fReplaceQuotes(v-dept-note[11]) */ '",'
        '"'  /* fReplaceQuotes(v-dept-note[12]) */ '",'
        '"'  /* fReplaceQuotes(v-dept-note[13]) */ '",'
        '"'  /* fReplaceQuotes(v-dept-note[14]) */ '",'
        '"'  /* fReplaceQuotes(v-dept-note[15]) */ '",'
        '"'  /* fReplaceQuotes(v-dept-note[16]) */ '",'
        '"'  /* fReplaceQuotes(v-dept-note[17]) */ '",'
        '"'  /* fReplaceQuotes(v-dept-note[18]) */ '",'
        '"'  fReplaceQuotes(ipbf-ttLoadTag.estID) '",'
        '"'  fReplaceQuotes(ipbf-ttLoadTag.orderDesc1)    '",'
        '"'  fReplaceQuotes(ipbf-ttLoadTag.orderDesc2)    '",'
        .
        
        IF CAN-DO("ASI,SSLABEL", gcLoadtagPrintSoftware) THEN DO:
        PUT UNFORMATTED
        '"' SUBSTR(ipbf-ttLoadTag.tag,16,5) '",'
        '"' ipbf-ttLoadTag.rfidTag '",' .
        END.
        
        PUT UNFORMATTED
        '"' ipbf-ttLoadTag.dueDateJobhdr '",'
        '"' ipbf-ttLoadTag.dueDateJob '",'
        '"' ipbf-ttLoadTag.lineID '",'
        '"' ipbf-ttLoadTag.unitWeight  '",'
        '"' ipbf-ttLoadTag.palletWeight  '",'
        '"' fReplaceQuotes(ipbf-ttLoadTag.partDscr1) '",'
        '"' fReplaceQuotes(ipbf-ttLoadTag.partDscr2) '",'
        '"' fReplaceQuotes(ipbf-ttLoadTag.partDscr3) '",'
        '"' fReplaceQuotes(ipbf-ttLoadTag.lotID) '",'
        '"' ipbf-ttLoadTag.palletID '",'
        '"' ipbf-ttLoadTag.palletCounter '",'
        '"' ipbf-ttLoadTag.tagCounter '",'
        '"' ipbf-ttLoadTag.totalTags '",'
        '"' REPLACE(ipbf-ttLoadTag.shipNotes[1],'"', '') '",'
        '"' REPLACE(ipbf-ttLoadTag.shipNotes[2],'"', '') '",'
        '"' REPLACE(ipbf-ttLoadTag.shipNotes[3],'"', '') '",'
        '"' REPLACE(ipbf-ttLoadTag.shipNotes[4],'"', '') '",'
        '"' ipbf-ttLoadTag.warehouseID '",'
        '"' ipbf-ttLoadTag.locationID '",'
        '"' ipbf-ttLoadTag.jobQuantity '",'
        '"' STRING(ipbf-ttLoadTag.runShip,"R&S/WHSE")  '",'
        '"' STRING(ipbf-ttLoadTag.itemPalletID)  '",'
        '"' STRING(ipbf-ttLoadTag.zone)  '",'
        '"' ipbf-ttLoadTag.createdUser '",'
        '"' STRING(ipbf-ttLoadTag.createdDate,"99/99/9999") '",'
        '"' STRING(ipbf-ttLoadTag.createdTime,"HH:MM AM") '",'
        '"' STRING(TODAY,"99/99/9999") '",'
        '"' STRING(TIME,"HH:MM AM") '"'.
        .
        
        IF glUpdateLoadTagSSCC THEN PUT UNFORMATTED ',"' ipbf-ttLoadTag.sscc '"'.  
        
        PUT SKIP.    
    END.
    
    OUTPUT CLOSE.
END PROCEDURE.

PROCEDURE pPrintLoadTagHeader PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    OUTPUT TO VALUE (gcLoadTagOutputPath + gcLoadTagOutputFile).
    
    PUT UNFORMATTED
        "CUSTOMER,ORDNUMBER,JOBNUMBER,ITEM,CUSTPARTNO,CUSTPONO,PCS,BUNDLE,TOTAL,"
        "SHIPCODE,SHIPNAME,SHIPADD1,SHIPADD2,SHIPCITY,SHIPSTATE,SHIPCOUNTRY,SHIPZIP,"
        "SOLDCODE,SOLDNAME,SOLDADD1,SOLDADD2,SOLDCITY,SOLDSTATE,SOLDCOUNTRY,SOLDZIP,"
        "INAME,DUEDATE,RELDATE,UPCNO,LENGTH,WIDTH,DEPTH,FLUTE,TEST,VENDOR,GROSSWGT,"
        "TAREWGT,NETWGT,SHEETWGT,UOM,STYLE,STYLEDESC,RELLOTNO,MIDDLESEXJOBNUMBER,MIDDLESEXCUSTPONO,"
        "TAG#,PARTIAL,CASECODE,SN1,SN2,SN3,SN4,SN5,SN6,SN7,SN8,PONO,DN1,DN2,DN3,DN4,"
        "DN5,DN6,DN7,DN8,DN9,DN10,EST#,ORDDESC1,ORDDESC2".
    
    IF CAN-DO("ASI,SSLABEL",gcLoadtagPrintSoftware) THEN
        PUT UNFORMATTED ",COUNTER#,RFIDTag".

    PUT UNFORMATTED 
        ",DUEDATEJOBLINE,DUEDATEJOB,LINE#,UnitWt,PalletWt,FGdesc1,FGdesc2,FGdesc3,FG Lot#,"
        "PalletCode,PalletID,TagCounter,TagCountTotal,"
        "RN1,RN2,RN3,RN4,WareHouse,Bin,JobQty,RunShip,Pallet type,Zone,CreatedBy,CreateDate,CreateTime,PrintDate,PrintTime".

    IF glUpdateLoadTagSSCC THEN 
        PUT UNFORMATTED ",SSCC".
    
    PUT SKIP.
    
    OUTPUT CLOSE.
END PROCEDURE.

PROCEDURE pPrintTTLoadTags PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLocation AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttLoadTag.
    
    DEFINE BUFFER bf-ttLoadtag        FOR ttLoadtag.
    DEFINE BUFFER bf-APIOutboundEvent FOR APIOutboundEvent.
    
    DEFINE VARIABLE hdOutboundProcs  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cTTLoadTagHandle AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMessage         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSuccess         AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cArgKeyList      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cArgValueList    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTriggerID       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcRequestData    AS LONGCHAR  NO-UNDO.
    
    RUN api/OutboundProcs.p PERSISTENT SET hdOutboundProcs.
    
    cTTLoadTagHandle = STRING(TEMP-TABLE ttLoadTag:HANDLE).        

    FOR EACH bf-ttLoadTag
        WHERE bf-ttLoadTag.tagStatus      EQ "Created"
          AND bf-ttLoadTag.isSelected     EQ TRUE
          AND bf-ttLoadTag.isError        EQ FALSE
        BREAK BY bf-ttLoadTag.exportFile:
        IF FIRST-OF (bf-ttLoadTag.exportFile) THEN DO:
            ASSIGN
                cArgKeyList   = "ExportTemplate,ExportFile,TTLoadTagHandle"
                cArgValueList = bf-ttLoadTag.exportTemplate + "," + bf-ttLoadTag.exportFile + "," + cTTLoadTagHandle
                .
            
            cTriggerID = IF bf-ttLoadTag.exportFileType EQ "loadtag" THEN
                             "PrintLoadTag"
                         ELSE IF bf-ttLoadTag.exportFileType EQ "boltag" THEN
                             "PrintBolTag"
                         ELSE
                             "".
        
            RUN Outbound_PrepareAndExecuteForScope IN hdOutboundProcs (
                INPUT  ipcCompany,                             /* Company Code (Mandatory) */
                INPUT  ipcLocation,                            /* Location Code (Mandatory) */
                INPUT  "CreateLoadtag",                        /* API ID (Mandatory) */
                INPUT  bf-ttLoadTag.custID,                    /* Scope ID */
                INPUT  "Customer",                             /* Scope Type */
                INPUT  cTriggerID,                             /* Trigger ID (Mandatory) */
                INPUT  cArgKeyList,                            /* Comma separated list of table names for which data being sent (Mandatory) */
                INPUT  cArgValueList,                          /* Comma separated list of ROWIDs for the respective table's record from the table list (Mandatory) */ 
                INPUT  bf-ttLoadTag.tag,                       /* Primary ID for which API is called for (Mandatory) */   
                INPUT  "Print loadtag from LoadTagProcs.p",    /* Event's description (Optional) */
                OUTPUT lSuccess,                               /* Success/Failure flag */
                OUTPUT cMessage                                /* Status message */
                ).

            RUN Outbound_GetEvents IN hdOutboundProcs (
                OUTPUT TABLE ttAPIOutboundEvent
                ).
            
            FOR EACH ttAPIOutboundEvent: 
                FIND FIRST bf-APIOutboundEvent NO-LOCK
                     WHERE bf-APIOutboundEvent.apiOutboundEventID EQ ttAPIOutboundEvent.apiOutboundEventID
                     NO-ERROR.
                IF AVAILABLE bf-APIOutboundEvent THEN DO:
                    lcRequestData = bf-APIOutboundEvent.requestData.
                    
                    COPY-LOB lcRequestData TO FILE bf-ttLoadTag.exportFile NO-ERROR.  
                END.      
            END.
            
            RUN Outbound_ResetContext IN hdOutboundProcs.
            
            IF bf-ttLoadTag.exportTemplate EQ "LabelMatrix" THEN
                RUN pPrintLabelMatrix (
                    INPUT ipcCompany,
                    INPUT bf-ttLoadTag.exportTemplateFile,
                    INPUT bf-ttLoadTag.exportFileType
                    ).            
            ELSE IF bf-ttLoadTag.exportTemplate EQ "XPrint" THEN DO:
                FILE-INFO:FILE-NAME = bf-ttLoadTag.exportFile.
                RUN printfile (
                    INPUT FILE-INFO:FILE-NAME
                    ).
            END.
        END.
    END.
    
    DELETE PROCEDURE hdOutboundProcs.

END PROCEDURE.

PROCEDURE pPrintView:
DEFINE INPUT PARAMETER ipcCasLabel AS CHARACTER .
DEFINE INPUT PARAMETER iplPrintView AS LOGICAL .

DEFINE VARIABLE cEmail AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPhone AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFax   AS CHARACTER NO-UNDO.

    {sys/inc/print1.i}
    {sys/inc/outprint.i value(85)}

    SESSION:SET-WAIT-STATE ("general").
   
    IF iplPrintView THEN DO:
        IF NOT lBussFormModle THEN
           PUT "<PREVIEW><MODAL=NO></PROGRESS>" FORM "x(50)".
         ELSE
           PUT "<PREVIEW></PROGRESS>" FORM "x(50)".
    END.
    ELSE DO:
       PUT "<PRINTER?><FORMAT=LEGAL></PROGRESS>" FORM "x(50)".
    END.

        iCountPallet = 0 .
        FOR EACH tt-word-print NO-LOCK BREAK
                                BY tt-word-print.ord-no 
                                BY tt-word-print.i-no:
            FIND FIRST loadtag NO-LOCK
             WHERE loadtag.company   EQ cocode
             AND loadtag.item-type EQ NO
                AND loadtag.tag-no EQ TRIM(tt-word-print.tag-no)
                USE-INDEX tag NO-ERROR.
            iCountPallet = iCountPallet + 1 .
                                
           IF ipcCasLabel EQ "loadtag.xpr" THEN DO:
               {oe/rep/lodxprntstd.i}
           END.
           ELSE IF ipcCasLabel EQ "loadtag1.xpr" THEN DO:
               {oe/rep/lodxprnt.i}
           END.
           ELSE IF ipcCasLabel EQ "loadtag2.xpr" THEN DO:
               {oe/rep/lodxprnt2.i}
           END.
           ELSE IF ipcCasLabel EQ "loadtag3.xpr" THEN DO:
               {oe/rep/lodxprnt3.i}
           END.
           ELSE IF ipcCasLabel EQ "loadtag4.xpr" THEN DO:
               {oe/rep/lodxprnt4.i}
           END.
           ELSE IF ipcCasLabel EQ "loadtag5.xpr" THEN DO:
               {oe/rep/lodxprnt5.i}
           END.
           ELSE IF ipcCasLabel EQ "loadtag6.xpr" THEN DO:
               {oe/rep/lodxprnt6.i}
           END.
           ELSE IF ipcCasLabel EQ "loadtag7.xpr" THEN DO:
               {oe/rep/lodxprnt7.i}
           END.
           ELSE IF ipcCasLabel EQ "loadtag8.xpr" THEN DO:
               {oe/rep/lodxprnt8.i}
           END.
           ELSE IF ipcCasLabel EQ "loadtag9.xpr" THEN DO:
               {oe/rep/lodxprnt9.i}
           END.
           ELSE IF ipcCasLabel EQ "loadtag10.xpr" THEN DO:
               {oe/rep/lodxprnt10.i}
           END.
           ELSE IF ipcCasLabel EQ "loadtag11.xpr" THEN DO:
               {oe/rep/lodxprnt11.i}
           END.
           ELSE IF ipcCasLabel EQ "loadtag12.xpr" THEN DO:
               {oe/rep/lodxprnt12.i} 
           END.
           ELSE IF ipcCasLabel EQ "loadtag13.xpr" THEN DO:
               {oe/rep/lodxprnt13.i}
           END.
           ELSE IF ipcCasLabel EQ "Logo.xpr" THEN DO:
               {oe/rep/lodxprntLogo.i} 
           END.
           ELSE IF ipcCasLabel EQ "NoLogo.xpr" THEN DO:
               {oe/rep/lodxprntNoLogo.i} 
           END.
    
         IF NOT LAST(tt-word-print.i-no) THEN PAGE .
        END.
   
    OUTPUT CLOSE.
    SESSION:SET-WAIT-STATE ("").

    FILE-INFO:FILE-NAME = list-name.
    RUN printfile (FILE-INFO:FILE-NAME).
 
        
END PROCEDURE.

PROCEDURE pCreateTTLoadTagFromItem:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany           AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemID            AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opTTLoadTagRecordID  AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError             AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage           AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cNote  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iIndex AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iCount AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iStart AS INTEGER   NO-UNDO.
    
    DEFINE BUFFER bf-itemfg    FOR itemfg.
    DEFINE BUFFER bf-style     FOR style.
    DEFINE BUFFER bf-ttLoadTag FOR ttLoadTag.
    DEFINE BUFFER bf-company   FOR company.
    DEFINE BUFFER bf-notes     FOR notes.
    
    MAIN-BLOCK:
    DO ON ERROR UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
        ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

        FIND FIRST bf-company NO-LOCK
             WHERE bf-company.company EQ ipcCompany
             NO-ERROR.
        IF NOT AVAILABLE bf-company THEN DO:
            ASSIGN
                oplError   = TRUE
                opcMessage = "Invalid company '" + ipcCompany + "'"
                .
            UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
        END.
        
        FIND FIRST bf-itemfg NO-LOCK
             WHERE bf-itemfg.company EQ ipcCompany
               AND bf-itemfg.i-no    EQ ipcItemID
             NO-ERROR.
        IF NOT AVAILABLE bf-itemfg THEN DO:
            ASSIGN
                oplError   = TRUE
                opcMessage = "Invalid item # '" + ipcItemID + "'"
                .
            UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
        END.
        
        CREATE bf-ttLoadTag.
        ASSIGN
            bf-ttLoadTag.recordID        = fGetNextTTLoadTagRecordID()
            bf-ttLoadTag.company         = bf-itemfg.company
            bf-ttLoadTag.itemID          = bf-itemfg.i-no
            bf-ttLoadTag.itemType        = "FG"
            bf-ttLoadTag.custPartNo      = bf-itemfg.part-no
            bf-ttLoadTag.itemName        = bf-itemfg.i-name
            bf-ttLoadTag.upcNo           = bf-itemfg.upc-no
            bf-ttLoadTag.boxLen          = bf-itemfg.l-score[50]
            bf-ttLoadTag.boxWid          = bf-itemfg.w-score[50]
            bf-ttLoadTag.boxDep          = bf-itemfg.d-score[50]
            bf-ttLoadTag.style           = bf-itemfg.style
            bf-ttLoadTag.vendorName      = bf-company.name
            bf-ttLoadTag.zoneID          = bf-itemfg.spare-char-4
            bf-ttLoadTag.sheetWeight     = bf-itemfg.weight-100 / 100
            bf-ttLoadTag.subUnitsPerUnit = bf-itemfg.case-pall
            bf-ttLoadTag.scannedDateTime = NOW
            bf-ttLoadTag.partDscr1       = bf-itemfg.part-dscr1
            bf-ttLoadTag.partDscr2       = bf-itemfg.part-dscr2
            bf-ttLoadTag.partDscr3       = bf-itemfg.part-dscr3
            bf-ttLoadTag.itemPalletID    = bf-itemfg.trno
            bf-ttLoadTag.tagStatus       = "Pending"
            bf-ttLoadTag.recordSource    = "FGITEM"
            .

        IF bf-ttLoadTag.style NE "" THEN DO:
            FIND FIRST bf-style NO-LOCK
                 WHERE bf-style.company EQ ipcCompany
                   AND bf-style.style   EQ bf-ttLoadTag.style
                 NO-ERROR.
            IF AVAILABLE bf-style THEN
                bf-ttLoadTag.styleDesc = bf-style.dscr.
        END.
        
        iCount = 1.
        
        FOR EACH bf-notes NO-LOCK 
            WHERE bf-notes.rec_key   EQ bf-itemfg.rec_key
              AND bf-notes.note_code EQ "SN":
            ASSIGN
                cNote = TRIM(REPLACE(bf-notes.note_text, CHR(10) + CHR(13), " "))
                cNote = TRIM(REPLACE(cNote, CHR(13), " "))
                cNote = TRIM(REPLACE(cNote, CHR(10), " "))
                cNote = REPLACE(cNote, '"', "''")
                .
            
            ASSIGN
                bf-ttLoadTag.deptNote[iCount] = cNote
                iCount                        = iCount + 1
                .
        END.
        
        opTTLoadTagRecordID = bf-ttLoadTag.recordID.
    END.
END PROCEDURE.

PROCEDURE pCreateTTLoadTagFromRMItem:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany           AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemID            AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opTTLoadTagRecordID  AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError             AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage           AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-item      FOR item.
    DEFINE BUFFER bf-ttLoadTag FOR ttLoadTag.
    DEFINE BUFFER bf-company   FOR company.
    
    MAIN-BLOCK:
    DO ON ERROR UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
        ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

        FIND FIRST bf-company NO-LOCK
             WHERE bf-company.company EQ ipcCompany
             NO-ERROR.
        IF NOT AVAILABLE bf-company THEN DO:
            ASSIGN
                oplError   = TRUE
                opcMessage = "Invalid company '" + ipcCompany + "'"
                .
            UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
        END.
        
        FIND FIRST bf-item NO-LOCK
             WHERE bf-item.company EQ ipcCompany
               AND bf-item.i-no    EQ ipcItemID
             NO-ERROR.
        IF NOT AVAILABLE bf-item THEN DO:
            ASSIGN
                oplError   = TRUE
                opcMessage = "Invalid item # '" + ipcItemID + "'"
                .
            UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
        END.
        
        CREATE bf-ttLoadTag.
        ASSIGN
            bf-ttLoadTag.recordID        = fGetNextTTLoadTagRecordID()
            bf-ttLoadTag.company         = bf-item.company
            bf-ttLoadTag.itemID          = bf-item.i-no
            bf-ttLoadTag.itemType        = "RM"
            bf-ttLoadTag.itemName        = bf-item.i-name
            bf-ttLoadTag.vendorName      = bf-company.name
            bf-ttLoadTag.sheetWeight     = bf-item.weight-100 / 100
            bf-ttLoadTag.subUnitsPerUnit = bf-item.case-pall
            bf-ttLoadTag.uom             = bf-item.cons-uom
            bf-ttLoadTag.basisWeight     = bf-item.basis-w
            bf-ttLoadTag.warehouseID     = bf-item.loc
            bf-ttLoadTag.locationID      = bf-item.loc-bin
            bf-ttLoadTag.scannedDateTime = NOW
            bf-ttLoadTag.tagStatus       = "Pending"
            bf-ttLoadTag.recordSource    = "RMITEM"
            .

        opTTLoadTagRecordID = bf-ttLoadTag.recordID.
    END.
END PROCEDURE.

PROCEDURE PrintTTLoadTags:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLocation AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttLoadTag.
    
    IF glCreateTagsForEmptyBOLLineTags THEN
        RUN CreateLoadTagFromTT (
            INPUT  ipcCompany,
            INPUT  ipcLocation,
            INPUT  FALSE,
            INPUT  FALSE, /* Empty ttLoadtag temp-table */
            INPUT-OUTPUT TABLE ttLoadTag
            ).
    
    RUN pPrintTTLoadTags (
        INPUT ipcCompany,
        INPUT ipcLocation,
        INPUT-OUTPUT TABLE ttLoadTag
        ).

END PROCEDURE.

PROCEDURE pUpdateConfig PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/    
    DEFINE VARIABLE cSettingValue AS CHARACTER NO-UNDO.
    
    RUN spGetSettingByName("LoadTag", OUTPUT gcLoadTag).
    RUN spGetSettingByName("LoadTagOutputFile", OUTPUT gcLoadTagOutputFile).
    RUN spGetSettingByName("LoadTagOutputFilePath", OUTPUT gcLoadTagOutputPath).
    RUN spGetSettingByName("LoadTagOutputFilePath", OUTPUT gcBOLLoadTagOutputFile).
    RUN spGetSettingByName("BOLLoadTagOutputFilePath", OUTPUT gcBOLLoadTagOutputPath).

    RUN spGetSettingByName("UpdateLoadTagSSCC", OUTPUT cSettingValue).
    glUpdateLoadTagSSCC = LOGICAL (cSettingValue) NO-ERROR.
    
    RUN spGetSettingByName("UpdateLocBinFromItemFG", OUTPUT cSettingValue).
    glUpdateLocBinFromItemFG = LOGICAL (cSettingValue) NO-ERROR.

    RUN spGetSettingByName("UpdateLocBinFromFGBin", OUTPUT cSettingValue).
    glUpdateLocBinFromFGBin = LOGICAL (cSettingValue) NO-ERROR.

    RUN spGetSettingByName("CreateFGReceipts", OUTPUT cSettingValue).
    glCreateFGReceipts = LOGICAL (cSettingValue) NO-ERROR.

    RUN spGetSettingByName("CheckClosedStatus", OUTPUT cSettingValue).
    glCheckClosedStatus = LOGICAL (cSettingValue) NO-ERROR.

    RUN spGetSettingByName("UpdateSetWithMaxQuantity", OUTPUT cSettingValue).
    glUpdateSetWithMaxQuantity = LOGICAL (cSettingValue) NO-ERROR.

    RUN spGetSettingByName("CreateRFIDTag", OUTPUT cSettingValue).
    glCreateRFIDTag = LOGICAL (cSettingValue) NO-ERROR.

    RUN spGetSettingByName("CreateComponenetTagsForSetHeaderItem", OUTPUT cSettingValue).
    glCreateComponenetTagsForSetHeaderItem = LOGICAL (cSettingValue) NO-ERROR.

    RUN spGetSettingByName("AutoPrint", OUTPUT cSettingValue).
    glAutoPrint = LOGICAL (cSettingValue) NO-ERROR.

    RUN spGetSettingByName("CreateTagsForEmptyBOLLineTags", OUTPUT cSettingValue).
    glCreateTagsForEmptyBOLLineTags = LOGICAL (cSettingValue) NO-ERROR.

    RUN spGetSettingByName("CreateTagForPartial", OUTPUT cSettingValue).
    glCreateTagForPartial = LOGICAL (cSettingValue) NO-ERROR.

    RUN spGetSettingByName("FGSetRec", OUTPUT cSettingValue).
    giFGSetRec = INTEGER (cSettingValue) NO-ERROR.
    
END PROCEDURE.

PROCEDURE pUpdateTTLoadTagFGBinDetails PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany           AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemID            AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTag               AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiTTLoadTagRecordID AS INTEGER   NO-UNDO.
    
    DEFINE BUFFER bf-fg-bin    FOR fg-bin.
    DEFINE BUFFER bf-ttLoadTag FOR ttLoadTag.
    
    MAIN-BLOCK:
    DO ON ERROR UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
        ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
        FIND FIRST bf-ttLoadTag
             WHERE bf-ttLoadTag.recordID EQ ipiTTLoadTagRecordID
             NO-ERROR.
        IF NOT AVAILABLE bf-ttLoadTag THEN
            UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.

        FIND FIRST bf-fg-bin NO-LOCK
             WHERE bf-fg-bin.company EQ ipcCompany
               AND bf-fg-bin.i-no    EQ ipcItemID
               AND bf-fg-bin.tag     EQ ipcTag
             NO-ERROR.
        IF NOT AVAILABLE bf-fg-bin THEN
            UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
        
        ASSIGN
            bf-ttLoadTag.quantityInSubUnit = bf-fg-bin.case-count
            bf-ttLoadTag.subUnitsPerUnit   = bf-fg-bin.cases-unit
            bf-ttLoadTag.quantity          = bf-fg-bin.qty
            bf-ttLoadTag.warehouseID       = bf-fg-bin.loc
            bf-ttLoadTag.locationID        = bf-fg-bin.loc-bin
            .
    END.
END PROCEDURE.

PROCEDURE pUpdateTTLoadTagLoadtagDetails PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany           AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTag               AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplItemType          AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiTTLoadTagRecordID AS INTEGER   NO-UNDO.

    DEFINE BUFFER bf-loadtag   FOR loadtag.
    DEFINE BUFFER bf-ttLoadTag FOR ttLoadTag.
    DEFINE BUFFER bf-rfidtag   FOR rfidtag.
    
    MAIN-BLOCK:
    DO ON ERROR UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
        ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
        FIND FIRST bf-ttLoadTag
             WHERE bf-ttLoadTag.recordID EQ ipiTTLoadTagRecordID
             NO-ERROR.
        IF NOT AVAILABLE bf-ttLoadTag THEN
            UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.

        FIND FIRST bf-loadtag NO-LOCK
             WHERE bf-loadtag.company   EQ ipcCompany
               AND bf-loadtag.item-type EQ iplItemType
               AND bf-loadtag.tag-no    EQ ipcTag
             NO-ERROR.
        IF NOT AVAILABLE bf-loadtag THEN
            UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
            
        ASSIGN
            bf-ttLoadTag.tag               = bf-loadtag.tag-no
            bf-ttLoadTag.lotID             = bf-loadtag.misc-char[2]
            bf-ttLoadTag.quantityInSubUnit = bf-loadtag.qty-case
            bf-ttLoadTag.subUnitsPerUnit   = bf-loadtag.case-bundle
            bf-ttLoadTag.quantity          = bf-loadtag.qty
            bf-ttLoadTag.warehouseID       = bf-loadtag.loc
            bf-ttLoadTag.locationID        = bf-loadtag.loc-bin
            bf-ttLoadTag.itemPalletID      = bf-loadtag.pallet-no
            bf-ttLoadTag.createdUser       = bf-loadtag.createUser
            bf-ttLoadTag.createdDate       = bf-loadtag.tag-date
            bf-ttLoadTag.createdTime       = bf-loadtag.tag-time
            .
        
        FIND FIRST bf-rfidtag NO-LOCK
             WHERE bf-rfidtag.company EQ bf-loadtag.company
               AND bf-rfidtag.rfidtag EQ bf-loadtag.tag-no
             NO-ERROR.
        IF AVAILABLE bf-rfidtag THEN
            bf-ttLoadTag.rfidTag = bf-rfidtag.rfidtag.
    END.
END PROCEDURE.

PROCEDURE pUpdateTTLoadTagEstimateDetails:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany           AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcEstimateID        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiTTLoadTagRecordID AS INTEGER   NO-UNDO.

    DEFINE BUFFER bf-est       FOR est.
    DEFINE BUFFER bf-eb        FOR eb.
    DEFINE BUFFER bf-ttLoadTag FOR ttLoadTag.
    
    MAIN-BLOCK:
    DO ON ERROR UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
        ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
        FIND FIRST bf-ttLoadTag
             WHERE bf-ttLoadTag.recordID EQ ipiTTLoadTagRecordID
             NO-ERROR.
        IF NOT AVAILABLE bf-ttLoadTag THEN
            UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
                        
        FIND FIRST bf-est NO-LOCK
             WHERE bf-est.company EQ ipcCompany
               AND bf-est.est-no  EQ ipcEstimateID
             NO-ERROR.

        IF AVAILABLE bf-est THEN DO:
            FIND FIRST bf-eb NO-LOCK
                 WHERE bf-eb.company   EQ bf-est.company
                   AND bf-eb.est-no    EQ bf-est.est-no
                   AND bf-eb.form-no   EQ bf-ttLoadTag.formNo
                   AND (bf-eb.blank-no EQ bf-ttLoadTag.blankNo OR bf-ttLoadTag.blankNo EQ 0)
                 NO-ERROR.

            IF bf-ttLoadTag.recordSource EQ "PO" THEN
                FIND FIRST bf-eb NO-LOCK
                     WHERE bf-eb.company  EQ bf-ttLoadTag.company
                       AND bf-eb.est-no   EQ bf-ttLoadTag.estID
                       AND bf-eb.stock-no EQ bf-ttLoadTag.itemID
                     NO-ERROR.
        END.
        
        IF AVAILABLE bf-eb THEN
            ASSIGN
                bf-ttLoadTag.flute      = bf-eb.flute
                bf-ttLoadTag.test       = bf-eb.test
                bf-ttLoadTag.caseNo     = bf-eb.cas-no
                bf-ttLoadTag.palletID   = bf-eb.tr-no
                bf-ttLoadTag.partDscr2  = bf-eb.part-dscr2
/*              The following variable assignments need to be configured if user does not choose to enter quantity */   
/*              manually. Also need to configure if this assignment is for a component of a set                    */             
/*              bf-ttLoadTag.pcs        = bf-eb.cas-cnt                                                            */
/*              bf-ttLoadTag.bundle     = bf-eb.cas-pal                                                            */
/*              bf-ttLoadTag.quantityInUnit  = bf-ttLoadTag.pcs * bf-ttLoadTag.bundle                                   */
/*              bf-ttLoadTag.formNo     = bf-eb.form-no                                                            */
/*              bf-ttLoadTag.totalTags  = ((bf-ttLoadTag.orderQuantity / bf-ttLoadTag.quantityInUnit) + .49) +          */
/*                                         (IF lookup(v-loadtag,"SSLABEL,CentBox") > 0 THEN 0 ELSE 1) .            */
                .

/*        IF AVAILABLE bf-eb AND bf-eb.est-type EQ 2 THEN                                                                           */
/*            bf-ttLoadTag.jobQuantity = bf-ttLoadTag.jobQuantity * (IF bf-eb.cust-% GT 0 THEN bf-eb.cust-% ELSE 1)  .              */
/*        ELSE IF AVAILABLE bf-eb AND bf-eb.est-type EQ 6  THEN                                                                     */
/*            bf-ttLoadTag.jobQuantity = bf-ttLoadTag.jobQuantity * (IF bf-eb.quantityPerSet GT 0 THEN bf-eb.quantityPerSet ELSE 1).*/
            
    END.    
END PROCEDURE.

PROCEDURE pUpdateTTLoadTagCustDetails:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany           AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemID            AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCustID            AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiTTLoadTagRecordID AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError             AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage           AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-ttLoadTag FOR ttLoadTag.
    DEFINE BUFFER bf-cust      FOR cust.
    DEFINE BUFFER bf-cust-part FOR cust-part.
    DEFINE BUFFER bf-shipto    FOR shipto.
    
    MAIN-BLOCK:
    DO ON ERROR UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
        ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
        FIND FIRST bf-ttLoadTag
             WHERE bf-ttLoadTag.recordID EQ ipiTTLoadTagRecordID
             NO-ERROR.
        IF NOT AVAILABLE bf-ttLoadTag THEN
            UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.

        FIND FIRST bf-cust NO-LOCK
             WHERE bf-cust.company EQ ipcCompany
               AND bf-cust.cust-no EQ ipcCustID
             NO-ERROR.
        IF NOT AVAILABLE bf-cust THEN DO:
            ASSIGN
                oplError   = TRUE
                opcMessage = "Invalid customer # '" + ipcCustID + "'"
                .
            UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
        END.
            
        ASSIGN
            bf-ttLoadTag.custID       = bf-cust.cust-no
            bf-ttLoadTag.custName     = bf-cust.name
            bf-ttLoadTag.custAddress1 = bf-cust.addr[1]
            bf-ttLoadTag.custAddress2 = bf-cust.addr[2]
            bf-ttLoadTag.custCity     = bf-cust.city
            bf-ttLoadTag.custState    = bf-cust.state
            bf-ttLoadTag.custCountry  = bf-cust.country
            bf-ttLoadTag.custEmail    = bf-cust.email
            bf-ttLoadTag.custAreaCode = bf-cust.area-code
            bf-ttLoadTag.custPhone    = bf-cust.phone            
            bf-ttLoadTag.custFax      = bf-cust.fax
            .
            
        FOR EACH bf-cust-part NO-LOCK 
            WHERE bf-cust-part.company EQ ipcCompany   
              AND bf-cust-part.i-no    EQ ipcItemID 
              AND bf-cust-part.cust-no EQ ipcCustID
              AND bf-cust-part.part-no NE "":
            bf-ttLoadTag.custPartNo = bf-cust-part.part-no.
            LEAVE.
        END.         
    END.    
END PROCEDURE.

PROCEDURE pUpdateTTLoadTagQuantites PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipiQuantity          AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiQuantityInSubUnit AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiSubUnitsPerUnit   AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiTTLoadTagRecordID AS INTEGER   NO-UNDO.
    
    DEFINE VARIABLE iQuantityOfSubUnits AS INTEGER NO-UNDO.
    DEFINE VARIABLE iQuantityInUnit     AS INTEGER NO-UNDO.
    DEFINE VARIABLE iPartial            AS INTEGER NO-UNDO.
    DEFINE VARIABLE iTotalTags          AS INTEGER NO-UNDO.
    DEFINE VARIABLE iFullTags           AS INTEGER NO-UNDO.
    DEFINE VARIABLE iPartialTags        AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bf-ttLoadTag FOR ttLoadTag.
    
    MAIN-BLOCK:
    DO ON ERROR UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
        ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
        FIND FIRST bf-ttLoadTag
             WHERE bf-ttLoadTag.recordID EQ ipiTTLoadTagRecordID
             NO-ERROR.
        IF NOT AVAILABLE bf-ttLoadTag THEN
            UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.

        ASSIGN
            iQuantityofSubUnits = TRUNCATE(ipiQuantity / ipiQuantityInSubUnit, 0)
            iQuantityInUnit     = ipiQuantityInSubUnit * ipiSubUnitsPerUnit
            iTotalTags          = TRUNCATE(ipiQuantity / iQuantityInUnit, 0) + INTEGER(NOT (ipiQuantity MOD iQuantityInUnit EQ 0))
            iFullTags           = TRUNCATE(ipiQuantity / iQuantityInUnit, 0)
            iPartialTags        = iTotalTags - iFullTags
            iPartial            = ipiQuantity - (iFullTags * iQuantityInUnit)
            .

        ASSIGN
            bf-ttLoadTag.quantityTotal      = ipiQuantity
            bf-ttLoadTag.quantityInSubUnit  = ipiQuantityInSubUnit
            bf-ttLoadTag.quantityOfSubUnits = iQuantityOfSubUnits
            bf-ttLoadTag.subUnitsPerUnit    = ipiSubUnitsPerUnit
            bf-ttLoadTag.partial            = iPartial
            bf-ttLoadTag.quantityInUnit     = iQuantityInUnit
            bf-ttLoadTag.quantity           = IF bf-ttLoadTag.quantityTotal LT bf-ttLoadTag.quantityInUnit THEN  /* Partial tag */
                                                  0
                                              ELSE
                                                  bf-ttLoadTag.quantityTotal - bf-ttLoadTag.partial
            .            

        IF glCreateTagForPartial THEN
            bf-ttLoadTag.totalTags = IF bf-ttLoadTag.isChild THEN
                                         1
                                     ELSE
                                         iTotalTags.
        ELSE
            bf-ttLoadTag.totalTags = IF iFullTags NE 0 THEN 
                                         iFullTags 
                                     ELSE 
                                         iPartialTags.

        bf-ttLoadTag.quantityOfUnits = bf-ttLoadTag.totalTags.
        
    END.

END PROCEDURE.

PROCEDURE pUpdateTTLoadTagShipToDetails:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany           AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCustID            AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcShipToID          AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiTTLoadTagRecordID AS INTEGER   NO-UNDO.
    
    DEFINE BUFFER bf-ttLoadTag FOR ttLoadTag.
    DEFINE BUFFER bf-shipto    FOR shipto.
    
    MAIN-BLOCK:
    DO ON ERROR UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
        ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
        FIND FIRST bf-ttLoadTag
             WHERE bf-ttLoadTag.recordID EQ ipiTTLoadTagRecordID
             NO-ERROR.
        IF NOT AVAILABLE bf-ttLoadTag THEN
            UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.

        RUN oe/custxship.p (
            INPUT  ipcCompany,
            INPUT  ipcCustID,
            INPUT  ipcShipToID,
            BUFFER bf-shipto
            ).
        IF AVAILABLE bf-shipto THEN
            ASSIGN
                bf-ttLoadTag.shipID       = bf-shipto.ship-id
                bf-ttLoadTag.shipName     = bf-shipto.ship-name
                bf-ttLoadTag.shipAddress1 = bf-shipto.ship-add[1]
                bf-ttLoadTag.shipAddress2 = bf-shipto.ship-add[2]
                bf-ttLoadTag.shipCity     = bf-shipto.ship-city
                bf-ttLoadTag.shipState    = bf-shipto.ship-state
                bf-ttLoadTag.shipCountry  = bf-shipto.country
                bf-ttLoadTag.shipZip      = bf-shipto.ship-zip
                bf-ttLoadTag.broker       = bf-shipto.broker
                .         
    END.    
END PROCEDURE.

PROCEDURE BuildLoadTagsFromJob:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/  
    DEFINE INPUT  PARAMETER ipcCompany           AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcJobID             AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiJobID2            AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiFormNo            AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiBlankNo           AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemID            AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiQuantity          AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiQuantityInSubUnit AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiSubUnitsPerUnit   AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiCopies            AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcUserField1        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcUserField2        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcUserField3        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcUserFieldValue1   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcUserFieldValue2   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcUserFieldValue3   AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttLoadTag.
    
    DEFINE VARIABLE lError   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
  
    RUN pBuildLoadTagsFromJob (
        INPUT  ipcCompany,
        INPUT  ipcJobID,
        INPUT  ipiJobID2,
        INPUT  ipiFormNo,
        INPUT  ipiBlankNo,
        INPUT  ipcItemID,
        INPUT  ipiQuantity,
        INPUT  ipiQuantityInSubUnit,
        INPUT  ipiSubUnitsPerUnit,
        INPUT  ipiCopies,
        INPUT  ipcUserField1,
        INPUT  ipcUserField2,
        INPUT  ipcUserField3,
        INPUT  ipcUserFieldValue1,
        INPUT  ipcUserFieldValue2,
        INPUT  ipcUserFieldValue3,
        OUTPUT lError,
        OUTPUT cMessage,
        INPUT-OUTPUT TABLE ttLoadTag
        ).
END PROCEDURE.

PROCEDURE BuildLoadTagsFromPO:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/  
    DEFINE INPUT  PARAMETER ipcCompany           AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPOID              AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPOLine            AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdQuantity          AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdQuantityInSubUnit AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdSubUnitsPerUnit   AS DECIMAL   NO-UNDO.    
    DEFINE INPUT  PARAMETER ipcQuantityUOM       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiCopies            AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcVendorTag         AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError             AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage           AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttLoadTag.
    
    RUN pBuildLoadTagsFromPO (
        INPUT  ipcCompany,
        INPUT  ipiPOID,
        INPUT  ipiPOLine,
        INPUT  ipdQuantity,
        INPUT  ipdQuantityInSubUnit,
        INPUT  ipdSubUnitsPerUnit,
        INPUT  ipcQuantityUOM,
        INPUT  ipiCopies,
        INPUT  ipcVendorTag,
        OUTPUT oplError,
        OUTPUT opcMessage,
        INPUT-OUTPUT TABLE ttLoadTag
        ).
END PROCEDURE.

PROCEDURE pBuildLoadTagsFromPO :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany           AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPOID              AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPOLine            AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdQuantity          AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdQuantityInSubUnit AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdSubUnitsPerUnit   AS DECIMAL   NO-UNDO.    
    DEFINE INPUT  PARAMETER ipcQuantityUOM       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiCopies            AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcVendorTag         AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError             AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage           AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttLoadTag.
    
    DEFINE VARIABLE iTTLoadTagRecordID      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iCount                  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cNote                   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSetsCreated            AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE dLength                 AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dWidth                  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dDepth                  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dBasisWeight            AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cQuantityUOM            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hdInventoryReceiptProcs AS HANDLE    NO-UNDO.
        
    DEFINE BUFFER bf-po-ord    FOR po-ord.
    DEFINE BUFFER bf-po-ordl   FOR po-ordl.
    DEFINE BUFFER bf-ttLoadTag FOR ttLoadTag.
    DEFINE BUFFER bf-notes     FOR notes.
    DEFINE BUFFER bf-vend      FOR vend.
    DEFINE BUFFER bf-itemfg    FOR itemfg.
    DEFINE BUFFER bf-item      FOR item.
    
    MAIN-BLOCK:
    DO ON ERROR UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
        ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:                
        FIND FIRST bf-po-ord NO-LOCK
             WHERE bf-po-ord.company EQ ipcCompany
               AND bf-po-ord.po-no   EQ ipiPOID
             NO-ERROR.
        IF NOT AVAILABLE bf-po-ord THEN DO:
            ASSIGN
                oplError = TRUE 
                opcMessage = "Invalid PO # '" + STRING(ipiPOID) + "'"
                .
            UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
        END.
                    
        FIND FIRST bf-po-ordl NO-LOCK
             WHERE bf-po-ordl.company   EQ ipcCompany
               AND bf-po-ordl.po-no     EQ ipiPOID
               AND bf-po-ordl.line      EQ ipiPOLine
             NO-ERROR.
        IF NOT AVAILABLE bf-po-ordl THEN DO:
            ASSIGN
                oplError = TRUE 
                opcMessage = "Invalid PO #/PO Line# '" + STRING(ipiPOID) + "/" + STRING(ipiPOLine) + "'"
                .
            UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
        END.

        IF NOT bf-po-ordl.item-type THEN DO:
            FIND FIRST bf-itemfg NO-LOCK
                 WHERE bf-itemfg.company EQ bf-po-ordl.company
                   AND bf-itemfg.i-no    EQ bf-po-ordl.i-no
                 NO-ERROR.
            IF NOT AVAILABLE bf-itemfg THEN DO:
                ASSIGN
                    oplError   = TRUE
                    opcMessage = "Invalid item # '" + bf-po-ordl.i-no + "'"
                    .
                UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
            END.
            
            RUN pCreateTTLoadTagFromItem (
                INPUT  bf-po-ordl.company,
                INPUT  bf-po-ordl.i-no,
                OUTPUT iTTLoadTagRecordID,
                OUTPUT oplError,
                OUTPUT opcMessage
                ).
            IF oplError THEN
                UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
                
            FIND FIRST bf-ttLoadTag
                 WHERE bf-ttLoadTag.recordID EQ iTTLoadTagRecordID
                 NO-ERROR.
            IF NOT AVAILABLE bf-ttLoadTag THEN DO:
                ASSIGN
                    oplError   = TRUE
                    opcMessage = "Error while populating loadtag record'"
                    .            
                UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
            END.
                       
            ASSIGN
                bf-ttLoadTag.vendorTag      = ipcVendorTag
                bf-ttLoadTag.poID           = bf-po-ordl.po-no
                bf-ttLoadTag.poLine         = bf-po-ordl.line
                bf-ttLoadTag.orderID        = bf-po-ordl.ord-no
                bf-ttLoadTag.itemID         = bf-po-ordl.i-no
                bf-ttLoadTag.overPct        = 0
                bf-ttLoadTag.ordQuantity    = bf-po-ordl.ord-qty
                bf-ttLoadTag.dueDate        = bf-po-ordl.due-date
                bf-ttLoadTag.netWeight      = bf-ttLoadTag.sheetWeight * bf-ttLoadTag.quantity
                bf-ttLoadTag.tareWeight     = 10
                bf-ttLoadTag.grossWeight    = bf-ttLoadTag.netWeight + bf-ttLoadTag.tareWeight
                bf-ttLoadTag.uom            = "EA"
                bf-ttLoadTag.printCopies    = ipiCopies
                bf-ttLoadTag.ipReturn       = NO
                bf-ttLoadTag.tagStatus      = "Pending"
                bf-ttLoadTag.recordSource   = "PO"
                bf-ttLoadTag.isSelected     = TRUE
                bf-ttLoadTag.exportFileType = "loadtag"
                bf-ttLoadTag.exportFile     = gcLoadTagOutputPath + gcLoadTagOutputFile
                .
    
            IF bf-po-ordl.pr-qty-uom NE "EA" THEN
                RUN Conv_QuantityFromUOMToUOM (
                    INPUT  bf-po-ordl.company,
                    INPUT  bf-po-ordl.i-no,
                    INPUT  "FG",
                    INPUT  bf-ttLoadTag.ordQuantity,
                    INPUT  bf-po-ordl.pr-qty-uom, 
                    INPUT  "EA",
                    INPUT  10 * bf-itemfg.weight-100 / bf-itemfg.t-sqft,
                    INPUT  bf-itemfg.t-len,
                    INPUT  bf-itemfg.t-wid,
                    INPUT  bf-itemfg.t-dep,
                    INPUT  0,
                    OUTPUT bf-ttLoadTag.ordQuantity,
                    OUTPUT oplError,
                    OUTPUT opcMessage
                    ).
    
            bf-ttLoadTag.quantityTotal  = bf-ttLoadTag.ordQuantity.
            
            FIND FIRST bf-vend NO-LOCK 
                 WHERE bf-vend.company EQ ipcCompany
                   AND bf-vend.vend-no EQ bf-po-ord.vend-no
                 NO-ERROR.
            IF AVAILABLE bf-vend THEN
                ASSIGN
                    bf-ttLoadTag.vendorID   = bf-vend.vend-no
                    bf-ttLoadTag.vendorName = bf-vend.name
                    .
    
            RUN pUpdateTTLoadTagOrderDetails (
                INPUT bf-po-ordl.company,
                INPUT bf-po-ordl.ord-no,
                INPUT bf-po-ordl.i-no,
                INPUT bf-ttLoadTag.recordID
                ).
                        
            IF bf-po-ordl.ord-no NE 0 THEN DO:
                iCount = 9.
                FOR EACH bf-notes NO-LOCK
                    WHERE bf-notes.rec_key EQ bf-po-ordl.rec_key:
                    ASSIGN
                        cNote = TRIM(REPLACE(bf-notes.note_text, CHR(10) + CHR(13), " "))
                        cNote = TRIM(REPLACE(cNote, CHR(13), " "))
                        cNote = TRIM(REPLACE(cNote, CHR(10), " "))
                        cNote = REPLACE(cNote, '"', "''")
                        .
                        
                    ASSIGN
                        bf-ttLoadTag.deptNote[iCount] = cNote
                        iCount                        = iCount + 1
                        .
                END.
            END.
            
            RUN pUpdateTTLoadTagOrderDetails (
                INPUT bf-po-ordl.company,
                INPUT bf-po-ordl.ord-no,
                INPUT bf-po-ordl.i-no,
                INPUT bf-ttLoadTag.recordID
                ).
                
            bf-ttLoadTag.lotID = bf-ttLoadTag.rellotID.
            
            IF bf-po-ordl.cust-no NE "" THEN DO:
                RUN pUpdateTTLoadTagCustDetails (
                    INPUT  bf-po-ordl.company,
                    INPUT  bf-po-ordl.i-no,
                    INPUT  bf-po-ordl.cust-no,
                    INPUT  bf-ttLoadTag.recordID,
                    OUTPUT oplError,
                    OUTPUT opcMessage
                    ).
                IF oplError THEN
                    UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
            END.
            
            RUN pUpdateTTLoadTagShipToDetails (
                INPUT bf-po-ordl.company,
                INPUT bf-po-ordl.cust-no,
                INPUT bf-po-ord.ship-id,
                INPUT bf-ttLoadTag.recordID
                ).
     
            IF AVAILABLE bf-itemfg AND bf-itemfg.est-no NE '' THEN
                RUN pUpdateTTLoadTagEstimateDetails (
                    INPUT bf-itemfg.company,
                    INPUT bf-itemfg.est-no,
                    INPUT bf-ttLoadTag.recordID
                    ).
    
            RUN pUpdateTTLoadTagQuantites (
                INPUT ipdQuantity,
                INPUT ipdQuantityInSubUnit,
                INPUT ipdSubUnitsPerUnit,
                INPUT bf-ttLoadTag.recordID
                ).
                                         
            IF bf-ttLoadTag.partial EQ ? THEN 
                bf-ttLoadTag.partial = 0. 
            
            RUN pGetExportTemplateFile(
                INPUT  bf-ttLoadTag.company,
                INPUT  "BARDIR",
                INPUT  bf-ttLoadTag.itemID,
                INPUT  bf-ttLoadTag.custID,
                INPUT  bf-ttLoadTag.shipID,
                OUTPUT bf-ttLoadTag.exportTemplateFile,
                OUTPUT bf-ttLoadTag.exportTemplate
                ).
            
            IF glCreateComponenetTagsForSetHeaderItem THEN DO:
                RUN pCreateSetComponentsForTTLoadTagItem (
                    INPUT  bf-ttLoadTag.recordID,
                    OUTPUT lSetsCreated
                    ).
        
                IF lSetsCreated THEN
                    DELETE bf-ttLoadTag.
            END.
        END.
        ELSE IF bf-po-ordl.item-type THEN DO:
            FIND FIRST bf-item NO-LOCK
                 WHERE bf-item.company EQ bf-po-ordl.company
                   AND bf-item.i-no    EQ bf-po-ordl.i-no
                 NO-ERROR.
            IF NOT AVAILABLE bf-item THEN DO:
                ASSIGN
                    oplError   = TRUE
                    opcMessage = "Invalid item # '" + bf-po-ordl.i-no + "'"
                    .
                UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
            END.
            
            RUN pCreateTTLoadTagFromRMItem (
                INPUT  bf-po-ordl.company,
                INPUT  bf-po-ordl.i-no,
                OUTPUT iTTLoadTagRecordID,
                OUTPUT oplError,
                OUTPUT opcMessage
                ).
            IF oplError THEN
                UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
                
            FIND FIRST bf-ttLoadTag
                 WHERE bf-ttLoadTag.recordID EQ iTTLoadTagRecordID
                 NO-ERROR.
            IF NOT AVAILABLE bf-ttLoadTag THEN DO:
                ASSIGN
                    oplError   = TRUE
                    opcMessage = "Error while populating loadtag record'"
                    .            
                UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
            END.
                       
            ASSIGN
                bf-ttLoadTag.vendorTag           = ipcVendorTag
                bf-ttLoadTag.poID                = bf-po-ordl.po-no
                bf-ttLoadTag.poLine              = bf-po-ordl.line
                bf-ttLoadTag.orderID             = bf-po-ordl.ord-no
                bf-ttLoadTag.formNo              = bf-po-ordl.s-num
                bf-ttLoadTag.blankNo             = bf-po-ordl.b-num
                bf-ttLoadTag.consumptionCost     = bf-po-ordl.cons-cost
                bf-ttLoadTag.consumptionQuantity = bf-po-ordl.cons-qty
                bf-ttLoadTag.consumptionUOM      = bf-po-ordl.cons-uom
                bf-ttLoadTag.itemName            = bf-po-ordl.i-name
                bf-ttLoadTag.jobID               = bf-po-ordl.job-no
                bf-ttLoadTag.jobID2              = bf-po-ordl.job-no2
                bf-ttLoadTag.orderID             = bf-po-ordl.ord-no
                bf-ttLoadTag.overPct             = bf-po-ord.over-pct
                bf-ttLoadTag.ordQuantity         = bf-po-ordl.ord-qty
                bf-ttLoadTag.quantity            = bf-po-ordl.ord-qty
                bf-ttLoadTag.sheetLength         = IF bf-po-ordl.pr-qty-uom EQ "ROLL" THEN 12 ELSE bf-po-ordl.s-len
                bf-ttLoadTag.sheetWidth          = bf-po-ordl.s-wid
                bf-ttLoadTag.sheetDepth          = bf-po-ordl.s-dep
                bf-ttLoadTag.dueDate             = bf-po-ordl.due-date
                bf-ttLoadTag.poSetup             = bf-po-ordl.setup
                bf-ttLoadTag.poType              = bf-po-ord.type
                bf-ttLoadTag.purchaseUOM         = bf-po-ordl.pr-uom
                bf-ttLoadTag.netWeight           = bf-ttLoadTag.sheetWeight * bf-ttLoadTag.quantity
                bf-ttLoadTag.tareWeight          = 10
                bf-ttLoadTag.grossWeight         = bf-ttLoadTag.netWeight + bf-ttLoadTag.tareWeight
                bf-ttLoadTag.printCopies         = ipiCopies
                bf-ttLoadTag.totalTags           = 1
                bf-ttLoadTag.ipReturn            = NO
                bf-ttLoadTag.tagStatus           = "Pending"
                bf-ttLoadTag.recordSource        = "PO"
                bf-ttLoadTag.isSelected          = TRUE
                bf-ttLoadTag.exportFileType      = "loadtag"
                bf-ttLoadTag.exportFile          = gcLoadTagOutputPath + gcLoadTagOutputFile
                .       

            ASSIGN
                cQuantityUOM = bf-item.cons-uom
                dLength      = bf-item.s-dep 
                dWidth       = bf-po-ordl.s-len
                dDepth       = bf-po-ordl.s-wid
                dBasisWeight = 0
                .
                
            RUN pGetJobMaterialDimensions(
                INPUT bf-po-ordl.company,
                INPUT bf-po-ordl.job-no,
                INPUT bf-po-ordl.job-no2,
                INPUT bf-po-ordl.s-num,
                INPUT bf-po-ordl.i-no,
                INPUT-OUTPUT dLength,
                INPUT-OUTPUT dWidth,
                INPUT-OUTPUT dDepth,
                INPUT-OUTPUT dBasisWeight
                ).
                            
            IF cQuantityUOM EQ ipcQuantityUOM THEN
               bf-ttLoadTag.poReceiptQuantity = ipdQuantity.
            ELSE IF bf-item.mat-type NE "P" THEN
                RUN Conv_QuantityFromUOMToUOM (
                    INPUT  bf-po-ordl.company,
                    INPUT  bf-po-ordl.i-no,
                    INPUT  "RM",
                    INPUT  ipdQuantity,
                    INPUT  ipcQuantityUOM, 
                    INPUT  cQuantityUOM,
                    INPUT  dBasisWeight,
                    INPUT  dLength,
                    INPUT  dWidth,
                    INPUT  dDepth,
                    INPUT  0,
                    OUTPUT bf-ttLoadTag.poReceiptQuantity,
                    OUTPUT oplError,
                    OUTPUT opcMessage
                    ).
            ELSE
                RUN Conv_QuantityFromUOMToUOM (
                    INPUT  bf-po-ordl.company,
                    INPUT  bf-po-ordl.i-no,
                    INPUT  "RM",
                    INPUT  ipdQuantity,
                    INPUT  bf-ttLoadTag.consumptionUOM, 
                    INPUT  cQuantityUOM,
                    INPUT  dBasisWeight,
                    INPUT  dLength,
                    INPUT  dWidth,
                    INPUT  dDepth,
                    INPUT  0,
                    OUTPUT bf-ttLoadTag.poReceiptQuantity,
                    OUTPUT oplError,
                    OUTPUT opcMessage
                    ).
            
            IF bf-ttLoadTag.poReceiptQuantity LT bf-po-ordl.ord-qty THEN
                bf-ttLoadTag.poCost = bf-po-ordl.cost + (bf-po-ordl.setup / ((bf-po-ordl.t-cost - bf-po-ordl.setup) / bf-po-ordl.cost)).
            ELSE
                ASSIGN
                    bf-ttLoadTag.poCost     = bf-po-ordl.cost
                    bf-ttLoadTag.poAddSetup = IF bf-po-ord.type NE "S" THEN YES ELSE NO
                    .
            
            ASSIGN
                bf-ttLoadTag.quantityInSubUnit = bf-ttLoadTag.poReceiptQuantity
                bf-ttLoadTag.subUnitsPerUnit   = 1 
                bf-ttLoadTag.quantityInUnit    = bf-ttLoadTag.quantityInSubUnit
                bf-ttLoadTag.partial           = 0
                .
                    
            RUN rm/getpocst.p (BUFFER bf-po-ordl, bf-ttLoadTag.purchaseUOM, INPUT-OUTPUT bf-ttLoadTag.poCost).
            
            RUN api/inbound/InventoryReceiptProcs.p PERSISTENT SET hdInventoryReceiptProcs.

            RUN InventoryReceipt_ConvertVendCompCurr IN hdInventoryReceiptProcs (ipcCompany, INPUT-OUTPUT bf-ttLoadTag.poCost) NO-ERROR.
            RUN InventoryReceipt_ConvertVendCompCurr IN hdInventoryReceiptProcs (ipcCompany, INPUT-OUTPUT bf-ttLoadTag.poSetup) NO-ERROR.
            RUN InventoryReceipt_ConvertVendCompCurr IN hdInventoryReceiptProcs (ipcCompany, INPUT-OUTPUT bf-ttLoadTag.consumptionCost) NO-ERROR.
                                    
            DELETE PROCEDURE hdInventoryReceiptProcs.        
        END.
    END.    
END PROCEDURE.
    
PROCEDURE pBuildLoadTagsFromJob PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany           AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcJobID             AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiJobID2            AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiFormNo            AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiBlankNo           AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemID            AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiQuantity          AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiQuantityInSubUnit AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiSubUnitsPerUnit   AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiCopies            AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcUserField1        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcUserField2        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcUserField3        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcUserFieldValue1   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcUserFieldValue2   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcUserFieldValue3   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError             AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage           AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttLoadTag.
    
    DEFINE VARIABLE iTTLoadTagRecordID AS INTEGER NO-UNDO.
    DEFINE VARIABLE lSetsCreated       AS LOGICAL NO-UNDO.

    DEFINE VARIABLE iQuantityOfSubUnits AS INTEGER NO-UNDO.
    DEFINE VARIABLE iQuantityInUnit     AS INTEGER NO-UNDO.
    DEFINE VARIABLE iPartial            AS INTEGER NO-UNDO.
    DEFINE VARIABLE iTotalTags          AS INTEGER NO-UNDO.
    DEFINE VARIABLE iFullTags           AS INTEGER NO-UNDO.
    DEFINE VARIABLE iPartialTags        AS INTEGER NO-UNDO.
    DEFINE VARIABLE iCount              AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE cNote AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-job       FOR job.
    DEFINE BUFFER bf-job-hdr   FOR job-hdr.
    DEFINE BUFFER bf-oe-ord    FOR oe-ord.
    DEFINE BUFFER bf-oe-ordl   FOR oe-ordl.
    DEFINE BUFFER bf-shipto    FOR shipto.
    DEFINE BUFFER bf-ttLoadTag FOR ttLoadtag.
    DEFINE BUFFER bf-notes     FOR notes.
    
    MAIN-BLOCK:
    DO ON ERROR UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
        ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
                            
        FIND FIRST bf-job NO-LOCK
             WHERE bf-job.company EQ ipcCompany
               AND bf-job.job-no  EQ ipcJobID
               AND bf-job.job-no2 EQ ipiJobID2
             NO-ERROR.
        IF AVAILABLE bf-job THEN
            FIND FIRST bf-job-hdr NO-LOCK
                 WHERE bf-job-hdr.company  EQ ipcCompany
                   AND bf-job-hdr.job      EQ bf-job.job
                   AND bf-job-hdr.job-no   EQ ipcJobID
                   AND bf-job-hdr.job-no2  EQ ipiJobID2
                   AND bf-job-hdr.frm      EQ ipiFormNo
                   AND bf-job-hdr.blank-no EQ ipiBlankNo
                   AND bf-job-hdr.i-no     EQ ipcItemID
                 NO-ERROR.
        
        IF NOT AVAILABLE bf-job-hdr THEN DO:
            ASSIGN
                oplError   = TRUE
                opcMessage = "Invalid Job # '" + ipcJobID + "-" + STRING(ipiJobID2,"999") + "'"
                .
            UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
        END.
        
        RUN pCreateTTLoadTagFromItem (
            INPUT  bf-job-hdr.company,
            INPUT  bf-job-hdr.i-no,
            OUTPUT iTTLoadTagRecordID,
            OUTPUT oplError,
            OUTPUT opcMessage
            ).
        IF oplError THEN
            UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
            
        FIND FIRST bf-ttLoadTag
             WHERE bf-ttLoadTag.recordID EQ iTTLoadTagRecordID
             NO-ERROR.
        IF NOT AVAILABLE bf-ttLoadTag THEN DO:
            ASSIGN
                oplError   = TRUE
                opcMessage = "Error while populating loadtag record'"
                .            
            UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
        END.
                   
        ASSIGN
            bf-ttLoadTag.orderID        = bf-job-hdr.ord-no
            bf-ttLoadTag.jobID          = bf-job-hdr.job-no
            bf-ttLoadTag.jobID2         = bf-job-hdr.job-no2
            bf-ttLoadTag.itemID         = bf-job-hdr.i-no
            bf-ttLoadTag.overPct        = 0
            bf-ttLoadTag.quantityTotal  = ipiQuantity
            bf-ttLoadTag.ordQuantity    = ipiQuantity
            bf-ttLoadTag.dueDate        = bf-job.start-date
            bf-ttLoadTag.estID          = bf-job.est-no
            bf-ttLoadTag.formNo         = bf-job-hdr.frm
            bf-ttLoadTag.blankNo        = bf-job-hdr.blank-no
            bf-ttLoadTag.custPONo       = bf-job-hdr.po-no
            bf-ttLoadTag.netWeight      = bf-ttLoadTag.sheetWeight * bf-ttLoadTag.quantity
            bf-ttLoadTag.tareWeight     = 10
            bf-ttLoadTag.grossWeight    = bf-ttLoadTag.netWeight + bf-ttLoadTag.tareWeight
            bf-ttLoadTag.uom            = "EA"
            bf-ttLoadTag.printCopies    = ipiCopies
            bf-ttLoadTag.dueDateJob     = IF bf-job.due-date <> ? THEN STRING(bf-job.due-date, "99/99/9999") ELSE ""
            bf-ttLoadTag.dueDateJobHdr  = IF bf-job-hdr.due-date <> ? THEN STRING(bf-job-hdr.due-date, "99/99/9999") ELSE ""
            bf-ttLoadTag.jobQuantity    = bf-job-hdr.qty
            bf-ttLoadTag.ipReturn       = NO
            bf-ttLoadTag.tagStatus      = "Pending"
            bf-ttLoadTag.recordSource   = "JOB"
            bf-ttLoadTag.isSelected     = TRUE
            bf-ttLoadTag.exportFileType = "loadtag"
            bf-ttLoadTag.exportFile     = gcLoadTagOutputPath + gcLoadTagOutputFile
            .
        
        IF bf-job-hdr.ord-no NE 0 THEN DO:
            iCount = 9.
            FOR EACH bf-notes NO-LOCK
                WHERE bf-notes.rec_key EQ bf-job.rec_key:
                IF bf-notes.note_form_no = 0 OR bf-notes.note_form_no = bf-job-hdr.frm THEN DO:
                    ASSIGN
                        cNote = TRIM(REPLACE(bf-notes.note_text, CHR(10) + CHR(13), " "))
                        cNote = TRIM(REPLACE(cNote, CHR(13), " "))
                        cNote = TRIM(REPLACE(cNote, CHR(10), " "))
                        cNote = REPLACE(cNote, '"', "''")
                        .
                        
                    ASSIGN
                        bf-ttLoadTag.deptNote[iCount] = cNote
                        iCount                        = iCount + 1
                        .
                END.
            END.
        END.
        
        RUN pUpdateTTLoadTagOrderDetails (
            INPUT bf-job-hdr.company,
            INPUT bf-job-hdr.ord-no,
            INPUT bf-job-hdr.i-no,
            INPUT bf-ttLoadTag.recordID
            ).
            
        bf-ttLoadTag.lotID = bf-ttLoadTag.rellotID.

        RUN pUpdateTTLoadTagCustDetails (
            INPUT  bf-job-hdr.company,
            INPUT  bf-job-hdr.i-no,
            INPUT  bf-job-hdr.cust-no,
            INPUT  bf-ttLoadTag.recordID,
            OUTPUT oplError,
            OUTPUT opcMessage
            ).
        IF oplError THEN
            UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.

        RUN pUpdateTTLoadTagShipToDetails (
            INPUT bf-job-hdr.company,
            INPUT bf-job-hdr.cust-no,
            INPUT bf-job-hdr.cust-no,
            INPUT bf-ttLoadTag.recordID
            ).
 
        RUN pUpdateTTLoadTagEstimateDetails (
            INPUT bf-job.company,
            INPUT bf-job.est-no,
            INPUT bf-ttLoadTag.recordID
            ).

        RUN pUpdateTTLoadTagQuantites (
            INPUT ipiQuantity,
            INPUT ipiQuantityInSubUnit,
            INPUT ipiSubUnitsPerUnit,
            INPUT bf-ttLoadTag.recordID
            ).
        
        IF ipcUserField1 EQ "Lot Number" THEN
            bf-ttLoadTag.lotID = ipcUserFieldValue1.
        ELSE IF ipcUserField2 EQ "Lot Number" THEN
            bf-ttLoadTag.lotID = ipcUserFieldValue2.
        ELSE IF ipcUserField3 EQ "Lot Number" THEN
            bf-ttLoadTag.lotID = ipcUserFieldValue3.
                
        IF ipcUserField1 EQ "Weight" THEN
            bf-ttLoadTag.unitWeight = DECIMAL(ipcUserFieldValue1).
        ELSE IF ipcUserField2 EQ "Weight" THEN
            bf-ttLoadTag.unitWeight = DECIMAL(ipcUserFieldValue2).
        ELSE IF ipcUserField3 EQ "Weight" THEN
            bf-ttLoadTag.unitWeight = DECIMAL(ipcUserFieldValue3).

        IF ipcUserField1 EQ "CustPo" THEN
            bf-ttLoadTag.custPONo = ipcUserFieldValue1.
        ELSE IF ipcUserField2 EQ "CustPo" THEN
            bf-ttLoadTag.custPONo = ipcUserFieldValue2.
        ELSE IF ipcUserField3 EQ "CustPo" THEN
            bf-ttLoadTag.custPONo = ipcUserFieldValue3.

        IF ipcUserField1 EQ "Overs" THEN
            bf-ttLoadTag.overPct = DECIMAL(ipcUserFieldValue1).
        ELSE IF ipcUserField2 EQ "Overs" THEN
            bf-ttLoadTag.overPct = DECIMAL(ipcUserFieldValue2).
        ELSE IF ipcUserField3 EQ "Overs" THEN
            bf-ttLoadTag.overPct = DECIMAL(ipcUserFieldValue3).
                         
        IF bf-ttLoadTag.partial EQ ? THEN 
            bf-ttLoadTag.partial = 0. 
        
        RUN pGetExportTemplateFile(
            INPUT  bf-ttLoadTag.company,
            INPUT  "BARDIR",
            INPUT  bf-ttLoadTag.itemID,
            INPUT  bf-ttLoadTag.custID,
            INPUT  bf-ttLoadTag.shipID,
            OUTPUT bf-ttLoadTag.exportTemplateFile,
            OUTPUT bf-ttLoadTag.exportTemplate
            ).
        
        IF glCreateComponenetTagsForSetHeaderItem THEN DO:
            RUN pCreateSetComponentsForTTLoadTagItem (
                INPUT  bf-ttLoadTag.recordID,
                OUTPUT lSetsCreated
                ).
    
            IF lSetsCreated THEN
                DELETE bf-ttLoadTag.
        END.
    END. 
END PROCEDURE.

PROCEDURE BuildLoadTagsFromTag:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTag     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiCopies  AS INTEGER   NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttLoadTag.

    DEFINE VARIABLE lError   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.

    RUN pBuildLoadTagsFromTag (
        INPUT  ipcCompany,
        INPUT  ipcTag,
        INPUT  ipiCopies,
        OUTPUT lError,
        OUTPUT cMessage,
        INPUT-OUTPUT TABLE ttLoadTag
        ).
END PROCEDURE.

PROCEDURE pBuildLoadTagsFromTag PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTag     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiCopies  AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError   AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttLoadTag.
    
    DEFINE VARIABLE iTTLoadTagRecordID AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bf-ttLoadTag FOR ttLoadtag.
    DEFINE BUFFER bf-loadtag   FOR loadtag.
        
    MAIN-BLOCK:
    DO ON ERROR UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
        ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
        
        FIND FIRST bf-loadtag NO-LOCK 
             WHERE bf-loadtag.company   EQ ipcCompany  
               AND bf-loadtag.item-type EQ FALSE
               AND bf-loadtag.tag-no    EQ ipcTag
             NO-ERROR.
        IF NOT AVAILABLE bf-loadtag THEN
            FIND FIRST bf-loadtag NO-LOCK 
                 WHERE bf-loadtag.company      EQ ipcCompany  
                   AND bf-loadtag.item-type    EQ FALSE
                   AND bf-loadtag.misc-char[1] EQ ipcTag
                 NO-ERROR.
        IF NOT AVAILABLE bf-loadtag THEN
            UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.

        RUN pCreateTTLoadTagFromItem (
            INPUT  bf-loadtag.company,
            INPUT  bf-loadtag.i-no,
            OUTPUT iTTLoadTagRecordID,
            OUTPUT oplError,
            OUTPUT opcMessage
            ).
        IF oplError THEN
            UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
            
        FIND FIRST bf-ttLoadTag
             WHERE bf-ttLoadTag.recordID EQ iTTLoadTagRecordID
             NO-ERROR.
        IF NOT AVAILABLE bf-ttLoadTag THEN DO:
            ASSIGN
                oplError   = TRUE
                opcMessage = "Error while populating loadtag record'"
                .            
            UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
        END.

        IF bf-loadtag.po-no NE 0 THEN
            RUN pUpdateTTLoadTagPOLineDetails (
                INPUT bf-loadtag.company,
                INPUT bf-loadtag.po-no,
                INPUT bf-loadtag.i-no,
                INPUT bf-ttLoadTag.recordID
                ).
        
        IF bf-loadtag.job-no NE "" THEN
            RUN pUpdateTTLoadTagJobDetails (
                INPUT bf-loadtag.company,
                INPUT bf-loadtag.job-no,
                INPUT bf-loadtag.job-no2,
                INPUT bf-loadtag.i-no,
                INPUT bf-ttLoadTag.recordID
                ).

        RUN pUpdateTTLoadTagOrderDetails (
            INPUT bf-loadtag.company,
            INPUT bf-loadtag.ord-no,
            INPUT bf-loadtag.i-no,
            INPUT bf-ttLoadTag.recordID
            ).

        RUN pUpdateTTLoadTagCustDetails (
            INPUT  bf-ttLoadTag.company,
            INPUT  bf-ttLoadTag.itemID,
            INPUT  bf-ttLoadTag.custID,
            INPUT  bf-ttLoadTag.recordID,
            OUTPUT oplError,
            OUTPUT opcMessage
            ).

        RUN pUpdateTTLoadTagShipToDetails (
            INPUT bf-ttLoadTag.company,
            INPUT bf-ttLoadTag.custID,
            INPUT bf-ttLoadTag.custID,
            INPUT bf-ttLoadTag.recordID
            ).

        RUN pUpdateTTLoadTagLoadtagDetails (
            INPUT bf-loadtag.company,
            INPUT bf-loadtag.tag-no,
            INPUT bf-loadtag.item-type,
            INPUT bf-ttLoadTag.recordID
            ).

        RUN pUpdateTTLoadTagFGBinDetails (
            INPUT bf-loadtag.company,
            INPUT bf-loadtag.i-no,
            INPUT bf-loadtag.tag-no,
            INPUT bf-ttLoadTag.recordID
            ).
        
        ASSIGN
            bf-ttLoadTag.ordQuantity       = bf-loadtag.qty
            bf-ttLoadTag.netWeight         = bf-ttLoadTag.sheetWeight * bf-ttLoadTag.quantity
            bf-ttLoadTag.tareWeight        = 10
            bf-ttLoadTag.grossWeight       = bf-ttLoadTag.netWeight + bf-ttLoadTag.tareWeight
            bf-ttLoadTag.uom               = "EA"
            bf-ttLoadTag.printCopies       = ipiCopies
            bf-ttLoadTag.tagStatus         = "Created"
            bf-ttLoadTag.recordSource      = "LOADTAG"
            bf-ttLoadTag.isSelected        = TRUE
            bf-ttLoadTag.exportFileType    = "loadtag"
            bf-ttLoadTag.exportFile        = gcLoadTagOutputPath + gcLoadTagOutputFile
            .
            
        bf-ttLoadTag.lotID = bf-ttLoadTag.rellotID.

        RUN pUpdateTTLoadTagEstimateDetails (
            INPUT bf-ttLoadTag.company,
            INPUT bf-ttLoadTag.estID,
            INPUT bf-ttLoadTag.recordID
            ).

        RUN pUpdateTTLoadTagQuantites (
            INPUT bf-ttLoadTag.quantityTotal,
            INPUT bf-ttLoadTag.quantityInSubUnit,
            INPUT bf-ttLoadTag.subUnitsPerUnit,
            INPUT bf-ttLoadTag.recordID
            ).
        
        IF bf-ttLoadTag.partial EQ ? THEN 
            bf-ttLoadTag.partial = 0. 
        
        RUN pGetExportTemplateFile(
            INPUT  bf-ttLoadTag.company,
            INPUT  "BARDIR",
            INPUT  bf-ttLoadTag.itemID,
            INPUT  bf-ttLoadTag.custID,
            INPUT  bf-ttLoadTag.shipID,
            OUTPUT bf-ttLoadTag.exportTemplateFile,
            OUTPUT bf-ttLoadTag.exportTemplate
            ).
    END. 
END PROCEDURE.

PROCEDURE pUpdateTTLoadTagPOLineDetails:
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:    
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany           AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPOID              AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemID            AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiTTLoadTagRecordID AS INTEGER   NO-UNDO.

    DEFINE BUFFER bf-po-ord    FOR po-ord.
    DEFINE BUFFER bf-po-ordl   FOR po-ordl.
    DEFINE BUFFER bf-ttLoadTag FOR ttLoadTag.
    
    MAIN-BLOCK:
    DO ON ERROR UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
        ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
        FIND FIRST bf-ttLoadTag
             WHERE bf-ttLoadTag.recordID EQ ipiTTLoadTagRecordID
             NO-ERROR.
        IF NOT AVAILABLE bf-ttLoadTag THEN
            UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
        
        FIND FIRST bf-po-ord NO-LOCK
             WHERE bf-po-ord.company EQ ipcCompany
               AND bf-po-ord.po-no   EQ ipiPOID 
               NO-ERROR.
        IF AVAILABLE bf-po-ord THEN
            FIND FIRST bf-po-ordl NO-LOCK 
                 WHERE bf-po-ordl.company EQ bf-po-ord.company
                   AND bf-po-ordl.po-no   EQ bf-po-ord.po-no
                   AND bf-po-ordl.i-no    EQ ipcItemID
                 USE-INDEX po-no NO-ERROR.
        IF AVAILABLE bf-po-ordl THEN DO:
            ASSIGN
                bf-ttLoadTag.dueDate  = bf-po-ord.due-date
                bf-ttLoadTag.custID   = bf-po-ordl.cust-no
                bf-ttLoadTag.poID     = bf-po-ord.po-no
                .
        END.
    END.    
END PROCEDURE.

PROCEDURE pUpdateTTLoadTagJobDetails PRIVATE:
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:    
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany           AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcJobID             AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiJobID2            AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemID            AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiTTLoadTagRecordID AS INTEGER   NO-UNDO.

    DEFINE BUFFER bf-job       FOR job.
    DEFINE BUFFER bf-job-hdr   FOR job-hdr.
    DEFINE BUFFER bf-ttLoadTag FOR ttLoadTag.
    
    MAIN-BLOCK:
    DO ON ERROR UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
        ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
        FIND FIRST bf-ttLoadTag
             WHERE bf-ttLoadTag.recordID EQ ipiTTLoadTagRecordID
             NO-ERROR.
        IF NOT AVAILABLE bf-ttLoadTag THEN
            UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
        
        FIND FIRST bf-job NO-LOCK 
             WHERE bf-job.company = ipcCompany
               AND bf-job.job-no  = ipcJobID
               AND bf-job.job-no2 = ipiJobID2  
             NO-ERROR.
        IF AVAILABLE bf-job THEN
            FIND FIRST bf-job-hdr NO-LOCK
                 WHERE bf-job-hdr.company EQ bf-job.company
                   AND bf-job-hdr.job     EQ bf-job.job
                   AND bf-job-hdr.job-no  EQ bf-job.job-no
                   AND bf-job-hdr.job-no2 EQ bf-job.job-no2
                   AND bf-job-hdr.i-no    EQ ipcItemID
                 NO-ERROR.
        IF NOT AVAILABLE bf-job-hdr THEN
            FIND FIRST bf-job-hdr NO-LOCK
                 WHERE bf-job-hdr.company EQ bf-job.company
                   AND bf-job-hdr.job     EQ bf-job.job
                   AND bf-job-hdr.job-no  EQ bf-job.job-no
                   AND bf-job-hdr.job-no2 EQ bf-job.job-no2
                 NO-ERROR.
        IF AVAILABLE bf-job-hdr THEN DO:
            ASSIGN
                bf-ttLoadTag.jobID          = bf-job-hdr.job-no
                bf-ttLoadTag.jobID2         = bf-job-hdr.job-no2
                bf-ttLoadTag.orderID        = bf-job-hdr.ord-no
                bf-ttLoadTag.itemID         = bf-job-hdr.i-no
                bf-ttLoadTag.dueDate        = bf-job.start-date
                bf-ttLoadTag.custID         = bf-job-hdr.cust-no
                bf-ttLoadTag.estID          = bf-job.est-no
                bf-ttLoadTag.formNo         = bf-job-hdr.frm
                bf-ttLoadTag.blankNo        = bf-job-hdr.blank-no
                bf-ttLoadTag.custPONo       = bf-job-hdr.po-no
                bf-ttLoadTag.dueDateJob     = IF bf-job.due-date <> ? THEN STRING(bf-job.due-date, "99/99/9999") ELSE ""
                bf-ttLoadTag.dueDateJobHdr  = IF bf-job-hdr.due-date <> ? THEN STRING(bf-job-hdr.due-date, "99/99/9999") ELSE ""
                bf-ttLoadTag.jobQuantity    = bf-job-hdr.qty
                .
        END.
    END.
    
END PROCEDURE.    
    
PROCEDURE pUpdateTTLoadTagOrderDetails:
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:    
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany           AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiOrderID           AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemID            AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiTTLoadTagRecordID AS INTEGER   NO-UNDO.

    DEFINE VARIABLE cReleaseStat AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lShiptoFound AS LOGICAL   NO-UNDO.
    
    DEFINE BUFFER bf-oe-rell   FOR oe-rell.
    DEFINE BUFFER bf-oe-ordl   FOR oe-ordl.
    DEFINE BUFFER bf-oe-ord    FOR oe-ord.
    DEFINE BUFFER bf-oe-rel    FOR oe-rel.
    DEFINE BUFFER bf-oe-relh   FOR oe-relh.
    DEFINE BUFFER bf-shipto    FOR shipto.
    DEFINE BUFFER bf-ttLoadTag FOR ttLoadTag.
    
    MAIN-BLOCK:
    DO ON ERROR UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
        ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
        FIND FIRST bf-ttLoadTag
             WHERE bf-ttLoadTag.recordID EQ ipiTTLoadTagRecordID
             NO-ERROR.
        IF NOT AVAILABLE bf-ttLoadTag THEN
            UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
    
        FIND FIRST bf-oe-ordl NO-LOCK
             WHERE bf-oe-ordl.company EQ ipcCompany
               AND bf-oe-ordl.ord-no  EQ ipiOrderID
               AND bf-oe-ordl.i-no    EQ ipcItemID
             NO-ERROR.
        
        IF NOT AVAILABLE bf-oe-ordl THEN
            RETURN.
        
        FIND FIRST bf-oe-ord NO-LOCK
             WHERE bf-oe-ord.company EQ ipcCompany
               AND bf-oe-ord.ord-no  EQ ipiOrderID
             NO-ERROR.
        IF AVAILABLE bf-oe-ord THEN
            ASSIGN
                bf-ttLoadTag.custID       = bf-oe-ord.cust-no
                bf-ttLoadTag.soldID       = bf-oe-ord.sold-id
                bf-ttLoadTag.soldName     = bf-oe-ord.sold-name
                bf-ttLoadTag.soldAddress1 = bf-oe-ord.sold-add[1]
                bf-ttLoadTag.soldAddress2 = bf-oe-ord.sold-add[2]
                bf-ttLoadTag.soldCity     = bf-oe-ord.sold-city
                bf-ttLoadTag.soldState    = bf-oe-ord.sold-state
                bf-ttLoadTag.soldZip      = bf-oe-ord.sold-zip
                bf-ttLoadTag.dueDate      = bf-oe-ord.due-date
                .
                  
        ASSIGN
            bf-ttLoadTag.ordQuantity = bf-oe-ordl.qty
            bf-ttLoadTag.dontRunSet  = bf-oe-ordl.is-a-component
            bf-ttLoadTag.orderDesc1  = bf-oe-ordl.part-dscr1
            bf-ttLoadTag.orderDesc2  = bf-oe-ordl.part-dscr2
            bf-ttLoadTag.runShip     = bf-oe-ordl.whsed
            .
       
        IF bf-ttLoadTag.recordSource NE "Job" THEN
            ASSIGN
                bf-ttLoadTag.poID        = bf-oe-ordl.po-no-po
                bf-ttLoadTag.lineID      = bf-oe-ordl.e-num
                bf-ttLoadTag.poLineID    = bf-oe-ordl.e-num
                .
        
        IF bf-ttLoadTag.dueDate EQ ? THEN
            bf-ttLoadTag.dueDate = bf-oe-ordl.req-date.
        
        IF bf-ttLoadTag.dueDate EQ ? THEN
            bf-ttLoadTag.dueDate = TODAY.
            
        FOR EACH bf-oe-rell NO-LOCK
            WHERE bf-oe-rell.company  EQ bf-oe-ordl.company
              AND bf-oe-rell.ord-no   EQ bf-oe-ordl.ord-no
              AND bf-oe-rell.i-no     EQ bf-oe-ordl.i-no
              AND bf-oe-rell.line     EQ bf-oe-ordl.line,
            FIRST bf-oe-relh NO-LOCK
            WHERE bf-oe-relh.r-no     EQ bf-oe-rell.r-no
               BY bf-oe-relh.rel-date
               BY bf-oe-relh.r-no:
    
            ASSIGN
                bf-ttLoadTag.custPONo     = bf-oe-rell.po-no
                bf-ttLoadTag.relDate      = bf-oe-relh.rel-date
                bf-ttLoadTag.shipNotes[1] = bf-oe-relh.ship-i[1]
                bf-ttLoadTag.shipNotes[2] = bf-oe-relh.ship-i[2]
                bf-ttLoadTag.shipNotes[3] = bf-oe-relh.ship-i[3]
                bf-ttLoadTag.shipNotes[4] = bf-oe-relh.ship-i[4]
                bf-ttLoadTag.relQuantity  = bf-oe-rell.qty 
                .
            LEAVE.
        END.
    
        IF AVAILABLE bf-oe-rell THEN
            FIND FIRST bf-oe-rel NO-LOCK
                 WHERE bf-oe-rel.r-no EQ bf-oe-rell.link-no
                 NO-ERROR.
        ELSE DO:
            FOR EACH bf-oe-rel NO-LOCK
                WHERE bf-oe-rel.company  EQ bf-oe-ordl.company
                  AND bf-oe-rel.ord-no   EQ bf-oe-ordl.ord-no
                  AND bf-oe-rel.i-no     EQ bf-oe-ordl.i-no
                  AND bf-oe-rel.line     EQ bf-oe-ordl.line
                  AND bf-oe-rel.rel-no   EQ 0
                BY bf-oe-rel.rel-date
                BY bf-oe-rel.r-no:
                ASSIGN
                    bf-ttLoadTag.custPONo     = (IF bf-oe-rel.po-no GT "" THEN bf-oe-rel.po-no ELSE bf-oe-ordl.po-no)
                    bf-ttLoadTag.relDate      = bf-oe-rel.rel-date
                    bf-ttLoadTag.shipNotes[1] = bf-oe-rel.ship-i[1]
                    bf-ttLoadTag.shipNotes[2] = bf-oe-rel.ship-i[2]
                    bf-ttLoadTag.shipNotes[3] = bf-oe-rel.ship-i[3]
                    bf-ttLoadTag.shipNotes[4] = bf-oe-rel.ship-i[4]
                    .
                LEAVE.
            END.
        END.  
    
        IF NOT AVAILABLE bf-oe-rel THEN DO:
            FOR EACH bf-oe-rel NO-LOCK
                WHERE bf-oe-rel.company  EQ bf-oe-ordl.company
                  AND bf-oe-rel.ord-no   EQ bf-oe-ordl.ord-no
                  AND bf-oe-rel.i-no     EQ bf-oe-ordl.i-no
                  AND bf-oe-rel.line     EQ bf-oe-ordl.line
                   BY bf-oe-rel.rel-date
                   BY bf-oe-rel.r-no:
    
                ASSIGN 
                    bf-ttLoadTag.custPONo     = (IF bf-oe-rel.po-no GT "" THEN bf-oe-rel.po-no ELSE bf-oe-ordl.po-no)
                    bf-ttLoadTag.relDate      = bf-oe-rel.rel-date
                    bf-ttLoadTag.shipNotes[1] = bf-oe-rel.ship-i[1]
                    bf-ttLoadTag.shipNotes[2] = bf-oe-rel.ship-i[2]
                    bf-ttLoadTag.shipNotes[3] = bf-oe-rel.ship-i[3]
                    bf-ttLoadTag.shipNotes[4] = bf-oe-rel.ship-i[4]
                    .
                LEAVE.
            END.
        END.

        IF AVAILABLE bf-oe-rel THEN 
            bf-ttLoadTag.rellotID = bf-oe-rel.lot-no.

        FOR EACH bf-oe-rel NO-LOCK
            WHERE bf-oe-rel.company EQ bf-oe-ordl.company
              AND bf-oe-rel.i-no    EQ bf-oe-ordl.i-no
              AND bf-oe-rel.ord-no  EQ bf-oe-ordl.ord-no
              AND bf-oe-rel.line    EQ bf-oe-ordl.line
            BREAK BY bf-oe-rel.rel-date:

            RUN oe/custxship.p(
                INPUT  bf-oe-rel.company,
                INPUT  bf-oe-rel.cust-no,
                INPUT  bf-oe-rel.ship-id,
                BUFFER bf-shipto
                ).
              
            IF NOT AVAILABLE bf-shipto THEN
                NEXT.
                
            RUN oe/rel-stat.p(
                INPUT  ROWID(bf-oe-rel), 
                OUTPUT cReleaseStat
                ).
            
            IF LOOKUP(cReleaseStat, "L,S,I") GT 0 OR LAST(bf-oe-rel.rel-date) THEN DO:
                RUN pUpdateTTLoadTagShiptoDetails (
                    INPUT bf-oe-rel.company,
                    INPUT bf-oe-rel.cust-no,
                    INPUT bf-oe-rel.ship-id,
                    INPUT ipiTTLoadTagRecordID
                    ).
                
                lShiptoFound = TRUE.
                
                LEAVE.
            END.
        END.

        IF NOT lShiptoFound AND AVAILABLE bf-oe-ord THEN              
            RUN pUpdateTTLoadTagShiptoDetails (
                INPUT bf-oe-ord.company,
                INPUT bf-oe-ord.cust-no,
                INPUT bf-oe-ord.cust-no,
                INPUT ipiTTLoadTagRecordID
                ).
    END.
END PROCEDURE.


/* ************************  Function Implementations ***************** */


FUNCTION fGetNextTTLoadTagRecordID RETURNS INTEGER PRIVATE
	(  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/	
    DEFINE VARIABLE iNextRecordID AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bf-ttLoadTag FOR ttLoadTag.
    
    FIND LAST bf-ttLoadTag NO-LOCK NO-ERROR.
    IF AVAILABLE bf-ttLoadTag THEN
        iNextRecordID = bf-ttLoadTag.recordID.
        
    iNextRecordID = iNextRecordID + 1.
    
    RETURN iNextRecordID.
END FUNCTION.

FUNCTION fReplaceQuotes RETURNS CHARACTER PRIVATE
	(INPUT ipcField AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/	
    ipcField = REPLACE(ipcField, '"', "''").

    RETURN ipcField.
END FUNCTION.
