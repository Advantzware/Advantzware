
/*------------------------------------------------------------------------
    File        : api/SendJob.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Mon Apr 18 22:56:17 IST 2022
    Notes       :
  ----------------------------------------------------------------------*/

FUNCTION fConvertBase16 RETURNS DECIMAL 
    ( ipdValue AS DECIMAL ) FORWARD.

FUNCTION k16 RETURNS DECIMAL 
    ( ipArrayValue AS DECIMAL, ipKFrac AS DECIMAL ) FORWARD.

PROCEDURE pUpdateCustomerInfo PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER ipcCompany       AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcCustomerID    AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER ioplcRequestData AS LONGCHAR  NO-UNDO.
    
    DEFINE BUFFER bf-cust FOR cust.

    FIND FIRST bf-cust NO-LOCK
         WHERE bf-cust.company EQ ipcCompany
           AND bf-cust.cust-no EQ ipcCustomerID
         NO-ERROR.
    ioplcRequestData = oAttribute:ReplaceAttributes(ioplcRequestData, BUFFER bf-cust:HANDLE).
END PROCEDURE.

PROCEDURE pUpdateRMItemInfo PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER ipcCompany       AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcItemID        AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER ioplcRequestData AS LONGCHAR  NO-UNDO.    
    
    DEFINE BUFFER bf-item FOR item.

    FIND FIRST bf-item NO-LOCK
         WHERE bf-item.company EQ ipcCompany
           AND bf-item.i-no    EQ ipcItemID
         NO-ERROR.
    ioplcRequestData = oAttribute:ReplaceAttributes(ioplcRequestData, BUFFER bf-item:HANDLE).
END PROCEDURE.

PROCEDURE pUpdateEstimateBlankInfo PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER ipcCompany       AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcEstimateID    AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipiFormNo        AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipiBlankNo       AS INTEGER   NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER ioplcRequestData AS LONGCHAR  NO-UNDO.    
    
    DEFINE VARIABLE iIndex          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cInk            AS CHARACTER NO-UNDO EXTENT 10.
    DEFINE VARIABLE cLength         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cWidth          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dInternalLength AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dEndCellLength  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dInternalWidth  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dEndCellWidth   AS DECIMAL   NO-UNDO.
    
    DEFINE BUFFER bf-eb    FOR eb.
    DEFINE BUFFER bf-est   FOR est.
    DEFINE BUFFER bf-style FOR style.
    
    DEFINE VARIABLE dLength16ths AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dWidth16ths  AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dDepth16ths  AS DECIMAL NO-UNDO.
    
    FIND FIRST bf-eb NO-LOCK
         WHERE bf-eb.company   EQ ipcCompany
           AND bf-eb.est-no    EQ ipcEstimateID
           AND bf-eb.form-no   EQ ipiFormNo
           AND (bf-eb.blank-no EQ ipiBlankNo OR ipiBlankNo EQ 0)
         NO-ERROR.
    IF AVAILABLE bf-eb THEN
        ASSIGN
            dLength16ths = fConvertBase16(bf-eb.len)
            dWidth16ths  = fConvertBase16(bf-eb.wid)
            dDepth16ths  = fConvertBase16(bf-eb.dep)
            .
            
    FIND FIRST bf-est NO-LOCK
         WHERE bf-est.company EQ ipcCompany
           AND bf-est.est-no  EQ ipcEstimateID
         NO-ERROR.
    
    oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "Length16ths", STRING(dLength16ths)).
    oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "Width16ths", STRING(dWidth16ths)).
    oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "Depth16ths", STRING(dDepth16ths)).
         
    ioplcRequestData = oAttribute:ReplaceAttributes(ioplcRequestData, BUFFER bf-eb:HANDLE).
    
    IF AVAILABLE bf-est AND AVAILABLE bf-eb THEN DO:
        DO iIndex = 1 TO 10:
            IF bf-eb.i-dscr[iIndex] NE "" AND bf-est.est-type GE 5 THEN
                cInk[iIndex] = bf-eb.i-dscr[iIndex].
            IF bf-eb.i-dscr2[iIndex] NE "" AND bf-est.est-type LT 5 THEN
                cInk[iIndex] = bf-eb.i-dscr2[iIndex].
        END.
        
        DO iIndex = 1 TO EXTENT(bf-eb.k-wid-array2):
            IF bf-eb.k-wid-array2[iIndex] NE 0 THEN
                cWidth = cWidth + STRING(bf-eb.k-wid-array2[iIndex]) + "x".
            IF bf-eb.k-len-array2[iIndex] NE 0 THEN
                cLength = cLength + STRING(bf-eb.k-len-array2[iIndex]) + "x".
        END.
        
        ASSIGN
            cLength = TRIM(cLength,"x")
            cWidth  = TRIM(cWidth,"x")
            .
    END.
              
    IF AVAILABLE bf-eb THEN
        FIND FIRST bf-style NO-LOCK 
             WHERE bf-style.company EQ bf-eb.company
               AND bf-style.style   EQ bf-eb.style 
             NO-ERROR.
    
    IF AVAILABLE bf-style THEN DO:
        RUN pGetEstimateDimensions (
            INPUT  ipcCompany, 
            INPUT  ipcEstimateID, 
            INPUT  bf-style.dim-df,
            INPUT  kFrac,
            OUTPUT dInternalLength,
            OUTPUT dEndCellLength,
            OUTPUT dInternalWidth,
            OUTPUT dEndCellWidth
            ).    
    END.
    
    DO iIndex = 1 TO 10:
        oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "Ink" + STRING(iIndex), cInk[iIndex]).
    END.
    
    oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "InternalLength", STRING(dInternalLength)).
    oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "InternalWidth", STRING(dInternalWidth)).
    oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "EndCellLength", STRING(dEndCellLength)).
    oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "EndCellWidth", STRING(dEndCellWidth)).
    oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "ScoreLength", cLength).
    oAttribute:UpdateRequestData(INPUT-OUTPUT ioplcRequestData, "ScoreWidth", cWidth).
    
    ioplcRequestData = oAttribute:ReplaceAttributes(ioplcRequestData, BUFFER bf-style:HANDLE).
END PROCEDURE.

PROCEDURE pUpdateFGItemInfo PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER ipcCompany       AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcItemID        AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER ioplcRequestData AS LONGCHAR  NO-UNDO.

    
    DEFINE BUFFER bf-itemfg FOR itemfg.
    
    FIND FIRST bf-itemfg NO-LOCK
         WHERE bf-itemfg.company EQ ipcCompany 
           AND bf-itemfg.i-no    EQ ipcItemID
         NO-ERROR.
    ioplcRequestData = oAttribute:ReplaceAttributes(ioplcRequestData, BUFFER bf-itemfg:HANDLE).
END PROCEDURE.

PROCEDURE pUpdateEstimateFormInfo:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER ipcCompany       AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcEstimateID    AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipiFormNo        AS INTEGER   NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER ioplcRequestData AS LONGCHAR  NO-UNDO.    
    DEFINE INPUT-OUTPUT PARAMETER iopdMatLength    AS DECIMAL   NO-UNDO.
    DEFINE BUFFER bf-ef FOR ef.

    FIND FIRST bf-ef NO-LOCK
         WHERE bf-ef.company   EQ ipcCompany
           AND bf-ef.est-no    EQ ipcEstimateID
           AND bf-ef.form-no   EQ ipiFormNo
         NO-ERROR.
    ioplcRequestData = oAttribute:ReplaceAttributes(ioplcRequestData, BUFFER bf-ef:HANDLE).
    
    IF AVAILABLE bf-ef AND iopdMatLength EQ ? THEN
        iopdMatLength = bf-ef.gsh-len.
END PROCEDURE.

PROCEDURE pGetJobMaterialInfo PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipCompany        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipJob            AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipJobNo          AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipJobNo2         AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipForm           AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipBlankNo        AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipItemNo         AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opBoardLength    AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opBoardWidth     AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opjobBoardIssued AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opBoard          AS CHARACTER NO-UNDO.       
    DEFINE OUTPUT PARAMETER opInk            AS CHARACTER NO-UNDO.         
    DEFINE OUTPUT PARAMETER opInkQty         AS DECIMAL   NO-UNDO.        
    DEFINE OUTPUT PARAMETER opPallet         AS CHARACTER NO-UNDO.      
    DEFINE OUTPUT PARAMETER opTotMRP         AS CHARACTER NO-UNDO.      
    DEFINE OUTPUT PARAMETER opMatType5       AS CHARACTER NO-UNDO.    
    DEFINE OUTPUT PARAMETER opMatType5Qty    AS DECIMAL   NO-UNDO. 
    DEFINE OUTPUT PARAMETER opMatType6       AS CHARACTER NO-UNDO.    
    DEFINE OUTPUT PARAMETER opMatType6Qty    AS DECIMAL   NO-UNDO. 
    DEFINE OUTPUT PARAMETER opVarnish        AS CHARACTER NO-UNDO.     
    DEFINE OUTPUT PARAMETER opAdders         AS CHARACTER NO-UNDO.      
    DEFINE OUTPUT PARAMETER opNoCases        AS DECIMAL   NO-UNDO.     
    DEFINE OUTPUT PARAMETER opCasesName      AS CHARACTER NO-UNDO.   
    DEFINE OUTPUT PARAMETER opFilmName       AS CHARACTER NO-UNDO.    
    DEFINE OUTPUT PARAMETER opRequiredQty    AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opTotalMRP       AS DECIMAL   NO-UNDO. 
    DEFINE OUTPUT PARAMETER opMatLength      AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opNumUp          AS INTEGER   NO-UNDO.
  
    DEFINE VARIABLE noCases     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE matType5Qty AS DECIMAL NO-UNDO.
    DEFINE VARIABLE matType6Qty AS DECIMAL NO-UNDO.
    DEFINE VARIABLE requiredQty AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lFirst      AS LOGICAL NO-UNDO.
    
    ASSIGN            
        opTotalMRP  = ? 
        opMatLength = ?
        .
    
    FOR EACH job-mat NO-LOCK
        WHERE job-mat.company EQ ipCompany
          AND job-mat.job     EQ ipJob
          AND job-mat.job-no  EQ ipJobNo
          AND job-mat.job-no2 EQ ipJobNo2
          AND job-mat.frm     EQ ipForm:
        
        IF job-mat.qty GT 0 AND NOT lFirst THEN
            ASSIGN
                opTotalMRP  = job-mat.qty
                opMatLength = job-mat.len
                opNumUp     = job-mat.n-up
                lFirst      = TRUE
                .
                          
        FOR EACH item OF job-mat NO-LOCK:
            CASE item.mat-type:
                WHEN '5' THEN DO:
                    IF NOT CAN-DO(opMatType5,job-mat.i-no) THEN
                        opMatType5 = opMatType5 + job-mat.i-no.
                    IF ipBlankNo EQ job-mat.blank-no THEN
                        matType5Qty = matType5Qty + job-mat.qty.
                        
                    opMatType5 = TRIM(opMatType5, ",").
                END.
                WHEN '6' THEN DO:
                    IF NOT CAN-DO(opMatType6,job-mat.i-no) THEN
                        opMatType6 = opMatType6 + job-mat.i-no.
                    IF ipBlankNo EQ job-mat.blank-no THEN
                        matType6Qty = matType6Qty + job-mat.qty.
                        
                    opMatType6 = TRIM(opMatType6, ",").
                END.
                WHEN 'A' THEN DO:
                    IF NOT CAN-DO(opAdders,job-mat.i-no) THEN
                        opAdders = opAdders + job-mat.i-no.
                        
                    opAdders = TRIM(opAdders, ",").
                END.
                WHEN 'B' THEN DO:
                    IF NOT opjobBoardIssued THEN
                        ASSIGN
                            opBoardLength    = job-mat.len
                            opBoardWidth     = job-mat.wid
                            opJobBoardIssued = CAN-FIND(FIRST mat-act
                                                        WHERE mat-act.company EQ job-mat.company
                                                          AND mat-act.job     EQ job-mat.job
                                                          AND mat-act.job-no  EQ job-mat.job-no
                                                          AND mat-act.job-no2 EQ job-mat.job-no2
                                                          AND mat-act.i-no    EQ job-mat.i-no
                                                          AND mat-act.s-num   EQ job-mat.frm
                                                          AND mat-act.b-num   EQ job-mat.blank-no
                                                        USE-INDEX job).
                    IF NOT CAN-DO(opBoard,job-mat.i-no) THEN
                        opBoard = opBoard + job-mat.i-no.
                    
                    opBoard = TRIM(opBoard, ",").
                END. /* B */
                WHEN 'C' THEN
                    IF ipBlankNo EQ job-mat.blank-no THEN
                        ASSIGN
                            noCases     = noCases + job-mat.qty
                            opCasesName = item.i-name
                            .
                WHEN 'D' THEN
                    IF opPallet EQ '' AND opTotMRP EQ '' THEN
                        ASSIGN
                            opPallet = item.i-name
                            opTotMRP = STRING(job-mat.qty,'>>,>>>,>>9.9<<<<<')
                            .
                WHEN 'I' THEN DO:
                    IF NOT CAN-DO(opInk,job-mat.i-no) THEN
                        opInk = opInk + job-mat.i-no.

                    ASSIGN
                        opInkQty = opInkQty + job-mat.qty.
                        opInk    = TRIM(opInk, ",")
                        .
                END. /* I */
                WHEN 'V' THEN DO:
                    IF NOT CAN-DO(opVarnish,job-mat.i-no) THEN
                        opVarnish = opVarnish + job-mat.i-no.
                    
                    opVarnish = TRIM(opVarnish, ",").
                END.
                WHEN 'W' THEN
                    opFilmName = REPLACE(item.i-name,'"','').
            END CASE.
        END. /* each item */
        
        requiredQty = requiredQty + job-mat.qty.
    END. /* each job-mat */
    
    ASSIGN
        opNoCases     = noCases
        opMatType5Qty = matType5Qty
        opMatType6Qty = matType6Qty
        requiredQty   = TRUNCATE(requiredQty,0)
                      + IF requiredQty - TRUNCATE(requiredQty,0) GT 0 THEN 1 ELSE 0
        opRequiredQty = requiredQty
        .
END PROCEDURE.

PROCEDURE pGetEstimateDimensions PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipCompany        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipEstNo          AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipDimDF          AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipKFrac          AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opInternalLength AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opEndCellLength  AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opInternalWidth  AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opEndCellWidth   AS DECIMAL   NO-UNDO.
    
    DEFINE BUFFER bf-eb1 FOR eb.
    DEFINE BUFFER bf-eb2 FOR eb.
    
    FIND FIRST bf-eb1 NO-LOCK
         WHERE bf-eb1.company  EQ ipCompany
           AND bf-eb1.est-no   EQ ipEstNo
           AND bf-eb1.form-no  NE 0
           AND bf-eb1.blank-no NE 0
         USE-INDEX est-qty
         NO-ERROR.
    
    IF AVAIL bf-eb1 THEN DO:
      ASSIGN
          opInternalLength = k16(bf-eb1.k-len-array2[2],ipKFrac)
          opEndCellLength  = k16(bf-eb1.k-len-array2[ipDimDF + 1],ipKFrac)
          .
      FIND FIRST bf-eb2 NO-LOCK
           WHERE bf-eb2.company  EQ ipCompany
             AND bf-eb2.est-no   EQ ipEstNo
             AND bf-eb2.form-no  NE 0
             AND bf-eb2.blank-no NE 0
             AND ROWID(bf-eb2) NE ROWID(bf-eb1)
           USE-INDEX est-qty
           NO-ERROR.
      IF AVAILABLE bf-eb2 THEN
          ASSIGN
            opInternalWidth = k16(bf-eb2.k-len-array2[2],ipKFrac)
            opEndCellWidth  = k16(bf-eb2.k-len-array2[ipDimDF + 1],ipKFrac)
            .
    END.
END PROCEDURE.

PROCEDURE pUpdatePOInfo PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipiPOID         AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipcJobNo        AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipiJobNo2       AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipcItemID       AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipiFormNo       AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipiBlankNo      AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipcSource       AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER ioplRequestData AS LONGCHAR  NO-UNDO.
    
    DEFINE BUFFER bf-po-ord  FOR po-ord.
    DEFINE BUFFER bf-po-ordl FOR po-ordl.
    
    FIND FIRST bf-po-ord NO-LOCK 
         WHERE bf-po-ord.company EQ ipcCompany
           AND bf-po-ord.po-no   EQ ipiPOID
         NO-ERROR.
    ioplcRequestData = oAttribute:ReplaceAttributes(ioplcRequestData, BUFFER bf-po-ord:HANDLE).
         
    IF AVAILABLE bf-po-ord THEN DO:
        FIND FIRST bf-po-ordl NO-LOCK
             WHERE bf-po-ordl.company EQ bf-po-ord.company
               AND (bf-po-ordl.po-no  EQ bf-po-ord.po-no OR ipcSource EQ "Machine")
               AND bf-po-ordl.job-no  EQ ipcJobNo
               AND bf-po-ordl.job-no2 EQ ipiJobNo2
               AND bf-po-ordl.i-no    EQ ipcItemID
               AND bf-po-ordl.s-num   EQ ipiFormNo
               AND bf-po-ordl.b-num   EQ ipiBlankNo
             NO-ERROR.
        IF NOT AVAILABLE bf-po-ordl THEN
            FIND FIRST bf-po-ordl NO-LOCK
                 WHERE bf-po-ordl.company EQ bf-po-ord.company
                   AND (bf-po-ordl.po-no  EQ bf-po-ord.po-no OR ipcSource EQ "Machine")
                   AND bf-po-ordl.job-no  EQ ipcJobNo
                   AND bf-po-ordl.job-no2 EQ ipiJobNo2
                   AND bf-po-ordl.i-no    EQ ipcItemID
                   AND bf-po-ordl.s-num   EQ ipiFormNo
                  NO-ERROR.
        IF NOT AVAILABLE bf-po-ordl THEN
            FIND FIRST bf-po-ordl NO-LOCK
                 WHERE bf-po-ordl.company EQ bf-po-ord.company
                   AND (bf-po-ordl.po-no  EQ bf-po-ord.po-no OR ipcSource EQ "Machine")
                   AND bf-po-ordl.job-no  EQ ipcJobNo
                   AND bf-po-ordl.job-no2 EQ ipiJobNo2
                   AND bf-po-ordl.s-num   EQ ipiFormNo
                   AND bf-po-ordl.b-num   EQ ipiBlankNo
                 NO-ERROR.
        IF NOT AVAILABLE bf-po-ordl THEN
            FIND FIRST bf-po-ordl NO-LOCK
                 WHERE bf-po-ordl.company EQ bf-po-ord.company
                   AND (bf-po-ordl.po-no  EQ bf-po-ord.po-no OR ipcSource EQ "Machine")
                   AND bf-po-ordl.job-no  EQ ipcJobNo
                   AND bf-po-ordl.job-no2 EQ ipiJobNo2
                   AND bf-po-ordl.s-num   EQ ipiFormNo
                 NO-ERROR.
        IF NOT AVAILABLE bf-po-ordl THEN
            FIND FIRST bf-po-ordl NO-LOCK
                 WHERE bf-po-ordl.company EQ bf-po-ord.company
                   AND (bf-po-ordl.po-no  EQ bf-po-ord.po-no OR ipcSource EQ "Machine")
                   AND bf-po-ordl.job-no  EQ ipcJobNo
                   AND bf-po-ordl.job-no2 EQ ipiJobNo2
                 NO-ERROR.
        IF NOT AVAILABLE bf-po-ordl THEN
            FIND FIRST bf-po-ordl NO-LOCK
                 WHERE bf-po-ordl.company EQ bf-po-ord.company
                   AND bf-po-ordl.job-no  EQ ipcJobNo
                   AND bf-po-ordl.job-no2 EQ ipiJobNo2
                 NO-ERROR.
        IF NOT AVAILABLE bf-po-ordl THEN
            FIND FIRST bf-po-ordl NO-LOCK
                 WHERE bf-po-ordl.company EQ bf-po-ord.company
                   AND bf-po-ordl.po-no   EQ bf-po-ord.po-no 
                 NO-ERROR.
    END.    
    
    ioplcRequestData = oAttribute:ReplaceAttributes(ioplcRequestData, BUFFER bf-po-ordl:HANDLE).    
END PROCEDURE.

/* ************************  Function Implementations ***************** */

FUNCTION fConvertBase16 RETURNS DECIMAL 
    ( ipdValue AS DECIMAL ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/    

    RETURN TRUNCATE(ipdValue,0) + ((ipdValue - TRUNCATE(ipdValue,0)) / 6.25).
END FUNCTION.

FUNCTION k16 RETURNS DECIMAL 
    ( ipArrayValue AS DECIMAL, ipKFrac AS DECIMAL):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/    
    RETURN TRUNC(ipArrayValue,0) + ((ipArrayValue - TRUNC(ipArrayValue,0)) / ipKFrac).
END FUNCTION.
  