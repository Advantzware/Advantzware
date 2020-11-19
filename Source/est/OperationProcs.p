
/*------------------------------------------------------------------------
    File        : OperationProcs.p
    Purpose     : 

    Syntax      :

    Description : Procedure for calculating machine standards, routing, etc.			

    Author(s)   : BV
    Created     : Wed Sep 16 14:09:32 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{est\ttAttribute.i}

DEFINE TEMP-TABLE ttAxis NO-UNDO
    FIELD axisType       AS CHARACTER 
    FIELD axisCoordinate AS INTEGER 
    FIELD axisValue      AS DECIMAL
    FIELD axisPage       AS INTEGER
    .

DEFINE TEMP-TABLE ttJobMch NO-UNDO LIKE job-mch 
    FIELD riJobMch    AS ROWID
    FIELD isExtraCopy AS LOGICAL
    .
    
    
DEFINE VARIABLE giAttributeIDStyle       AS INTEGER NO-UNDO INITIAL 101.
DEFINE VARIABLE giAttributeIDBoardItemID AS INTEGER NO-UNDO INITIAL 102.
DEFINE VARIABLE giAttributeIDCaliper     AS INTEGER NO-UNDO INITIAL 103.
DEFINE VARIABLE giAttributeIDBoxDepth    AS INTEGER NO-UNDO INITIAL 104.
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fGetAttributeValue RETURNS DECIMAL PRIVATE
    (ipiAttributeID AS INTEGER) FORWARD.

FUNCTION fHasDataCollected RETURNS LOGICAL PRIVATE
    (BUFFER ipbf-job-mch FOR job-mch) FORWARD.

FUNCTION fIsOperationFound RETURNS LOGICAL PRIVATE
    (ipcCompany AS CHARACTER,
    ipcOperationID AS CHARACTER,
    ipiJob AS INTEGER,
    ipiFormNo AS INTEGER,
    ipiBlankNo AS INTEGER,
    ipiPass AS INTEGER,
    ipcDepartmentID AS CHARACTER) FORWARD.

/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */
PROCEDURE ClearAttributes:
    /*------------------------------------------------------------------------------
     Purpose:  Clears all attributes
     Notes:
    ------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttAttribute.

END PROCEDURE.


PROCEDURE GetAttributes:
    /*------------------------------------------------------------------------------
     Purpose: Returns the temp-table to caller
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER TABLE FOR ttAttribute.

END PROCEDURE.

PROCEDURE GetOperationRates:
    /*------------------------------------------------------------------------------
     Purpose:  Returns Rates for given machine
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcLocationID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcOperationID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdRateMRLabor AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdRateMRFixedOverhead AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdRateMRVariableOverhead AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdRateRunLabor AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdRateRunFixedOverhead AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdRateRunVariableOverhead AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-mach FOR mach.
    
    RUN pGetMachineBuffer(ipcCompany, ipcLocationID, ipcOperationID, BUFFER bf-mach, OUTPUT oplError, OUTPUT opcMessage).
    IF AVAILABLE bf-mach THEN DO:
        ASSIGN 
            opdRateMRFixedOverhead = bf-mach.mr-fixoh
            opdRateMRVariableOverhead = bf-mach.mr-varoh
            opdRateRunFixedOverhead = bf-mach.run-fixoh
            opdRateRunVariableOverhead = bf-mach.run-varoh
            opdRateMRLabor = bf-mach.mr-rate  // Refactor:  OpRatesSeparate & variable crew size
            opdRateRunLabor = bf-mach.run-rate //Refactor: OpRatesSeparate & variable crew size
            .
            
    END.
END PROCEDURE.

PROCEDURE GetOperationStandards:
    /*------------------------------------------------------------------------------
     Purpose: Given a company and machine code, return standards for the machine
        based on the current context of attributes
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcLocationID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcOperationID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdOpMRWaste AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdOpMRHours AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdOpRunSpeed AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdOpRunSpoil AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-mach FOR mach.
    
    RUN pGetMachineBuffer(ipcCompany, ipcLocationID, ipcOperationID, BUFFER bf-mach, OUTPUT oplError, OUTPUT opcMessage).

    IF AVAILABLE bf-mach THEN DO:
        opdOpMRWaste = bf-mach.mr-waste.
        RUN pGetRunSpeed(BUFFER bf-mach, OUTPUT opdOpRunSpeed, OUTPUT oplError, OUTPUT opcMessage).
        RUN pGetMRHours(BUFFER bf-mach, OUTPUT opdOpMRHours, OUTPUT oplError, OUTPUT opcMessage).
        RUN pGetRunSpoil(BUFFER bf-mach, OUTPUT opdOpRunSpoil, OUTPUT oplError, OUTPUT opcMessage).
    END.

END PROCEDURE.


PROCEDURE GetOperationStandardsForJobMch:
/*------------------------------------------------------------------------------
 Purpose: given job-mch rowid, get updated standards
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriJobMch AS ROWID.
    
    DEFINE BUFFER bf-job-mch FOR job-mch.
    DEFINE BUFFER bf-job FOR job.
    DEFINE BUFFER bf-eb FOR eb.
    
    DEFINE VARIABLE lError AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    
    FIND bf-job-mch NO-LOCK 
        WHERE ROWID(bf-job-mch) EQ ipriJobMch
        NO-ERROR.
    IF AVAILABLE bf-job-mch THEN DO:
        FIND FIRST bf-job NO-LOCK 
            WHERE bf-job.company EQ bf-job-mch.company
            AND bf-job.job EQ bf-job-mch.job
            NO-ERROR.
        IF NOT AVAILABLE bf-job THEN RETURN.
        FIND FIRST bf-eb NO-LOCK 
            WHERE bf-eb.company EQ bf-job-mch.company
            AND bf-eb.est-no EQ bf-job.est-no
            AND bf-eb.form-no EQ bf-job-mch.frm
            AND bf-eb.blank-no EQ MAX(bf-job-mch.blank-no,1)
            NO-ERROR.
            
        RUN SetAttributesFromEb (ROWID(bf-eb), OUTPUT lError, OUTPUT cMessage).
        IF NOT lError THEN DO:
            FIND CURRENT bf-job-mch EXCLUSIVE-LOCK.
            RUN GetOperationRates(bf-job-mch.company, bf-job.loc, bf-job-mch.m-code, 
                              OUTPUT bf-job-mch.mr-rate, 
                              OUTPUT bf-job-mch.mr-fixoh, 
                              OUTPUT bf-job-mch.mr-varoh,
                              OUTPUT bf-job-mch.run-rate,
                              OUTPUT bf-job-mch.run-fixoh,
                              OUTPUT bf-job-mch.run-varoh,
                              OUTPUT lError, OUTPUT cMessage).
            RUN GetOperationStandards(bf-job-mch.company, bf-job.loc, bf-job-mch.m-code,
                                      OUTPUT bf-job-mch.mr-waste, 
                                      OUTPUT bf-job-mch.mr-hr, 
                                      OUTPUT bf-job-mch.speed, 
                                      OUTPUT bf-job-mch.wst-prct, 
                                      OUTPUT lError, OUTPUT cMessage).
        END.
    END.

END PROCEDURE.

PROCEDURE pAddAxis PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcAxisType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiPage AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipdHeaderValues AS DECIMAL NO-UNDO EXTENT.
    
    DEFINE VARIABLE iIndex  AS INTEGER NO-UNDO.
    DEFINE VARIABLE iExtent AS INTEGER NO-UNDO.
    
    iExtent = EXTENT(ipdHeaderValues).
    DO iIndex = 1 TO iExtent:
        IF ipdHeaderValues[iIndex] NE 0 THEN 
        DO:
            CREATE ttAxis.
            ASSIGN 
                ttAxis.axisType       = ipcAxisType
                ttAxis.axisValue      = ipdHeaderValues[iIndex]
                ttAxis.axisCoordinate = iIndex
                ttAxis.axisPage       = ipiPage 
                .
        END.
    END.

END PROCEDURE.

PROCEDURE pGetMachineBuffer PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:  Return the machine buffer
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcLocationID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcOperationID AS CHARACTER NO-UNDO.
    DEFINE PARAMETER BUFFER opbf-mach FOR mach.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    FIND FIRST opbf-mach NO-LOCK 
        WHERE opbf-mach.company EQ ipcCompany
        AND opbf-mach.loc EQ ipcLocationID
        AND opbf-mach.m-code EQ ipcOperationID
        NO-ERROR.
    IF NOT AVAILABLE opbf-mach THEN 
    DO:
        ASSIGN 
            oplError   = YES
            opcMessage = "Invalid Machine Code"
            .
        RETURN. 
    END. 
    
    

END PROCEDURE.

PROCEDURE pGetMatrixSpeedReduction PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-mstd FOR mstd.
    DEFINE OUTPUT PARAMETER opdReduction AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE VARIABLE iIndex   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dCaliper AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cCaliper AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDepth   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dDepth   AS DECIMAL   NO-UNDO.
     
    RUN pGetAttribute(giAttributeIDCaliper, OUTPUT cCaliper, OUTPUT oplError, OUTPUT opcMessage).
    dCaliper = DECIMAL(cCaliper).
    RUN pGetAttribute(giAttributeIDBoxDepth, OUTPUT cDepth, OUTPUT oplError, OUTPUT opcMessage).
    dDepth = DECIMAL(cDepth).
    IF AVAILABLE ipbf-mstd THEN 
    DO:
        DO iIndex = 1 TO EXTENT(ipbf-mstd.board-cal):
            IF ipbf-mstd.board-cal[iIndex] GE dCaliper AND ipbf-mstd.board-cal[iIndex] NE 0 THEN
            DO:
                opdReduction = ipbf-mstd.spd-reduc[iIndex] / 100.
                LEAVE.
            END. 
        END.
        DO iIndex = 1 TO EXTENT(ipbf-mstd.board-depth):
            IF ipbf-mstd.board-depth[iIndex] GE dDepth AND ipbf-mstd.board-depth[iIndex] NE 0 THEN 
            DO:
                opdReduction = opdReduction + ipbf-mstd.depth-reduc[iIndex] / 100.
                LEAVE.
            END.
        END.
    END.
END PROCEDURE.

PROCEDURE pGetValue PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a standards buffer and type, return the value based on attribute context
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-mstd FOR mstd.
    DEFINE INPUT PARAMETER ipcType AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdValue AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-mmtx FOR mmtx.
    DEFINE BUFFER bf-mmty FOR mmty.
    
    DEFINE VARIABLE dXAttributeValue AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dYAttributeValue AS DECIMAL NO-UNDO.
    DEFINE VARIABLE iX               AS INTEGER NO-UNDO.
    DEFINE VARIABLE iY               AS INTEGER NO-UNDO.
    DEFINE VARIABLE iAcross          AS INTEGER NO-UNDO.
    DEFINE VARIABLE iPage            AS INTEGER NO-UNDO.
    
    EMPTY TEMP-TABLE ttAxis.
    CASE ipcType:
        WHEN "RunSpeed" THEN 
            DO:
                ASSIGN 
                    dXAttributeValue = fGetAttributeValue(ipbf-mstd.rs-x)
                    dYAttributeValue = fGetAttributeValue(ipbf-mstd.rs-y)
                    .
                FOR EACH bf-mmtx NO-LOCK OF ipbf-mstd
                    WHERE bf-mmtx.mr-run EQ NO
                    AND bf-mmtx.page-no EQ 0
                    BY bf-mmtx.across-no:
                    RUN pAddAxis("X", bf-mmtx.across-no, bf-mmtx.col-value).
                END.
                FOR EACH bf-mmtx NO-LOCK OF ipbf-mstd
                    WHERE bf-mmtx.mr-run EQ NO
                    AND bf-mmtx.across-no EQ 0
                    BY bf-mmtx.page-no:
                    RUN pAddAxis("Y", bf-mmtx.page-no, bf-mmtx.row-value).
                END. 
                RUN pGetCoordinates(dXAttributeValue, "X", OUTPUT iX, OUTPUT iAcross).
                RUN pGetCoordinates(dYAttributeValue, "Y", OUTPUT iY, OUTPUT iPage).
                IF iX NE 0 AND iY NE 0 THEN 
                    FIND FIRST bf-mmtx NO-LOCK OF ipbf-mstd
                        WHERE bf-mmtx.mr-run EQ NO
                        AND bf-mmtx.page-no EQ iPage
                        AND bf-mmtx.across-no EQ iAcross
                        NO-ERROR.
                IF AVAILABLE bf-mmtx THEN 
                    opdValue = bf-mmtx.vals[10 * iY + iX].
                
            END.
        WHEN "RunSpoil" THEN 
            DO:
                ASSIGN 
                    dXAttributeValue = fGetAttributeValue(ipbf-mstd.sp-x)
                    dYAttributeValue = fGetAttributeValue(ipbf-mstd.sp-y)
                    .
                FOR EACH bf-mmtx NO-LOCK OF ipbf-mstd
                    WHERE bf-mmtx.mr-run EQ YES
                    AND bf-mmtx.page-no EQ 0
                    BY bf-mmtx.across-no:
                    RUN pAddAxis("X", bf-mmtx.across-no, bf-mmtx.col-value).
                END.
                FOR EACH bf-mmtx NO-LOCK OF ipbf-mstd
                    WHERE bf-mmtx.mr-run EQ YES
                    AND bf-mmtx.across-no EQ 0
                    BY bf-mmtx.page-no:
                    RUN pAddAxis("Y",bf-mmtx.page-no, bf-mmtx.row-value).
                END.
                RUN pGetCoordinates(dXAttributeValue, "X", OUTPUT iX, OUTPUT iAcross).
                RUN pGetCoordinates(dYAttributeValue, "Y", OUTPUT iY, OUTPUT iPage).
                IF iX NE 0 AND iY NE 0 THEN 
                    FIND FIRST bf-mmtx NO-LOCK OF ipbf-mstd
                        WHERE bf-mmtx.mr-run EQ YES
                        AND bf-mmtx.page-no EQ iPage
                        AND bf-mmtx.across-no EQ iAcross
                        NO-ERROR.
                IF AVAILABLE bf-mmtx THEN 
                    opdValue = bf-mmtx.vals[10 * iY + iX].
                
            END.        
        WHEN "MRHours" THEN 
            DO:
                ASSIGN 
                    dXAttributeValue = fGetAttributeValue(ipbf-mstd.mr-x)
                    dYAttributeValue = fGetAttributeValue(ipbf-mstd.mr-y)
                    .
                FOR EACH bf-mmty NO-LOCK OF ipbf-mstd
                    WHERE bf-mmty.page-no EQ 0
                    BY bf-mmty.across-no:
                    RUN pAddAxis("X", bf-mmty.across-no, bf-mmty.col-value).
                END.
                FOR EACH bf-mmty NO-LOCK OF ipbf-mstd
                    WHERE bf-mmty.across-no EQ 0
                    BY bf-mmty.page-no:
                    RUN pAddAxis("Y",bf-mmty.page-no, bf-mmty.row-value).
                END.
                RUN pGetCoordinates(dXAttributeValue, "X", OUTPUT iX, OUTPUT iAcross).
                RUN pGetCoordinates(dYAttributeValue, "Y", OUTPUT iY, OUTPUT iPage).
                IF iX NE 0 AND iY NE 0 THEN 
                    FIND FIRST bf-mmty NO-LOCK OF ipbf-mstd
                        WHERE bf-mmty.page-no EQ iPage
                        AND bf-mmty.across-no EQ iAcross
                        NO-ERROR.
                IF AVAILABLE bf-mmty THEN 
                    opdValue = bf-mmty.vals[10 * iY + iX].
                
            END.
    END CASE.
    

END PROCEDURE.

PROCEDURE pGetAttribute PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Given an attribute type, get the current value from context
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiAttributeID AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcAttributeValue AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    FIND FIRST ttAttribute NO-LOCK 
        WHERE ttAttribute.attributeID EQ ipiAttributeID
        NO-ERROR.
    IF AVAILABLE ttAttribute THEN 
        ASSIGN opcAttributeValue = ttAttribute.attributeValue.
    ELSE 
        ASSIGN 
            oplError   = YES
            opcMessage = "Attribute: " + STRING(ipiAttributeID) + " not available"
            .
            
END PROCEDURE.

PROCEDURE pGetItemSpeedReduction PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Get Speed Reduction for the Board item
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcDepartmentID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdReduction AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-item FOR item.
    
    DEFINE VARIABLE cDept      AS CHARACTER NO-UNDO.  
    DEFINE VARIABLE cBoard     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iIndex     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dReduction AS DECIMAL   NO-UNDO.
      
    RUN pGetAttribute(giAttributeIDBoardItemID, OUTPUT cBoard, OUTPUT oplError, OUTPUT opcMessage).
    
    IF oplError THEN RETURN.
    
    FIND FIRST bf-item NO-LOCK 
        WHERE bf-item.company EQ ipcCompany
        AND bf-item.i-no EQ cBoard
        NO-ERROR.
    IF NOT AVAILABLE bf-item THEN 
        ASSIGN 
            oplError   = YES
            opcMessage = "Invalid Board Attribute: " + cBoard
            .
    ELSE 
    DO:
        DO iIndex = 1 TO 10:
            IF bf-item.dept-name[iIndex] EQ ipcDepartmentID AND bf-item.speed%[iIndex] NE 0 THEN  
            DO:
                opdReduction = bf-item.speed%[iIndex] / 100.
                LEAVE.
            END.
        END.
    END.       
    
END PROCEDURE.

PROCEDURE pGetAttributeStyle PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Given an attribute type, get the current value from context
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcStyle AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-style FOR style.
   
    RUN pGetAttribute(giAttributeIDStyle, OUTPUT opcStyle, OUTPUT oplError, OUTPUT opcMessage).
    
    IF oplError THEN RETURN.
    
    FIND FIRST bf-style NO-LOCK 
        WHERE bf-style.company EQ ipcCompany
        AND bf-style.style EQ opcStyle
        NO-ERROR.
    IF NOT AVAILABLE bf-style THEN 
        ASSIGN 
            oplError   = YES
            opcMessage = "Invalid Style Attribute: " + opcStyle
            .
                    
END PROCEDURE.

PROCEDURE pGetCoordinates PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Given lookup values for X
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipdLookup AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcAxisType AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiCoordinate AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiPage AS INTEGER NO-UNDO.
    
    FOR EACH ttAxis NO-LOCK
        WHERE ttAxis.axisType EQ ipcAxisType
        BY ttAxis.axisPage
        BY ttAxis.axisCoordinate:
        IF ipdLookup LE ttAxis.axisValue THEN LEAVE.
    END.
    IF AVAILABLE ttAxis THEN 
        ASSIGN 
            opiCoordinate = ttAxis.axisCoordinate
            opiPage       = ttAxis.axisPage
            . 
 
END PROCEDURE.

PROCEDURE pGetMRHours PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given company and machine code, return the MR Hours based on the 
        current attribute context.
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-mach FOR mach.
    DEFINE OUTPUT PARAMETER opdMRHours AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-mstd FOR mstd.
    
    RUN pGetStandardBuffer(ipbf-mach.company, ipbf-mach.loc, ipbf-mach.m-code, BUFFER bf-mstd, OUTPUT oplError, OUTPUT opcMessage).
    IF AVAILABLE bf-mstd THEN 
    DO:
        RUN pGetValue(BUFFER bf-mstd, "MRHours", OUTPUT opdMRHours, OUTPUT oplError, OUTPUT opcMessage).
        
    END.
    
END PROCEDURE.


PROCEDURE pGetRunSpeed PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given company and machine code, return the Run Speed based on the 
        current attribute context.
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-mach FOR mach.
    DEFINE OUTPUT PARAMETER opdRunSpeed AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-mstd FOR mstd.
    
    DEFINE VARIABLE dReduction      AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dReductionTotal AS DECIMAL NO-UNDO.
    
    RUN pGetStandardBuffer(ipbf-mach.company, ipbf-mach.loc, ipbf-mach.m-code, BUFFER bf-mstd, OUTPUT oplError, OUTPUT opcMessage).
    IF AVAILABLE bf-mstd THEN 
    DO:
        RUN pGetValue(BUFFER bf-mstd, "RunSpeed", OUTPUT opdRunSpeed, OUTPUT oplError, OUTPUT opcMessage).
        RUN pGetItemSpeedReduction(ipbf-mach.company, ipbf-mach.dept[1], OUTPUT dReductionTotal, OUTPUT oplError, OUTPUT opcMessage).
        RUN pGetMatrixSpeedReduction(BUFFER bf-mstd, OUTPUT dReduction, OUTPUT oplError, OUTPUT opcMessage).
        ASSIGN 
            dReductionTotal = dReductionTotal + dReduction
            opdRunSpeed     = opdRunSpeed * (1 - dReductionTotal).
        .
    END.
    
END PROCEDURE.

PROCEDURE pGetRunSpoil PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given company and machine code, return the Run Spoil % based on the 
        current attribute context.
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-mach FOR mach.
    DEFINE OUTPUT PARAMETER opdRunSpoil AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-mstd FOR mstd.
    
    RUN pGetStandardBuffer(ipbf-mach.company, ipbf-mach.loc, ipbf-mach.m-code, BUFFER bf-mstd, OUTPUT oplError, OUTPUT opcMessage).
    IF AVAILABLE bf-mstd THEN 
    DO:
        RUN pGetValue(BUFFER bf-mstd, "RunSpoil", OUTPUT opdRunSpoil, OUTPUT oplError, OUTPUT opcMessage).
        
    END.
    
END PROCEDURE.

PROCEDURE pGetStandardBuffer PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcLocationID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcOpID AS CHARACTER NO-UNDO.
    DEFINE PARAMETER BUFFER opbf-mstd FOR mstd.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cStyle AS CHARACTER NO-UNDO.
    
    RUN pGetAttributeStyle(ipcCompany, OUTPUT cStyle, OUTPUT oplError, OUTPUT opcMessage).
    
    IF oplError THEN RETURN.
    
    FIND FIRST opbf-mstd NO-LOCK
        WHERE opbf-mstd.company EQ ipcCompany
        AND opbf-mstd.loc EQ ipcLocationID
        AND opbf-mstd.m-code EQ ipcOpID
        AND opbf-mstd.style EQ cStyle
        NO-ERROR.
    IF NOT AVAILABLE opbf-mstd THEN       /* maybe there's only a blank style mstd */
        FIND FIRST opbf-mstd NO-LOCK
            WHERE opbf-mstd.company EQ ipcCompany
            AND opbf-mstd.loc EQ ipcLocationID
            AND opbf-mstd.m-code EQ ipcOpID
            AND opbf-mstd.style EQ ""
            NO-ERROR.    
    IF NOT AVAILABLE opbf-mstd THEN       /* get any mstd */
        FIND FIRST opbf-mstd NO-LOCK
            WHERE opbf-mstd.company EQ ipcCompany
            AND opbf-mstd.loc EQ ipcLocationID
            AND opbf-mstd.m-code EQ ipcOpID
            NO-ERROR  .      

END PROCEDURE.

PROCEDURE pOperationChangeAddDepartment PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Given all inputs, add a operation to the job
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcOperationID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiJob AS INTEGER NO-UNDO. 
    DEFINE INPUT PARAMETER ipiFormNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiPass AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipcDepartmentID AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-job-mch FOR job-mch.

    DEFINE BUFFER bf-job FOR job.
    
    FIND bf-job NO-LOCK 
        WHERE bf-job.company EQ ipcCompany
        AND bf-job.job EQ ipiJob
        NO-ERROR.
    IF NOT AVAILABLE bf-job THEN RETURN.
    
    CREATE bf-job-mch.
    ASSIGN
        bf-job-mch.company        = ipcCompany
        bf-job-mch.job            = bf-job.job
        bf-job-mch.job-no         = bf-job.job-no
        bf-job-mch.job-no2        = bf-job.job-no2
        bf-job-mch.frm            = ipiFormNo
        bf-job-mch.blank-no       = ipiBlankNo
        bf-job-mch.pass           = ipiPass
        bf-job-mch.m-code         = ipcOperationID
        bf-job-mch.dept           = ipcDepartmentID
        
        /* this let's SB know touch screen data collection made changes */
        bf-job-mch.est-op_rec_key = 'TS ' + STRING(TODAY) + ' ' + STRING(TIME,'HH:MM:SS')
        .
    RUN GetOperationStandardsForJobMch(ROWID(bf-job-mch)).
    
END PROCEDURE.

PROCEDURE pOperationChangeAddMachine PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:  Adds a new machine to the job for the new operation ID.
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-job-mch FOR job-mch.
    DEFINE INPUT PARAMETER ipcOperationID AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-job-mch FOR job-mch.
    
    CREATE bf-job-mch.
    BUFFER-COPY ipbf-job-mch EXCEPT m-code job-mchID TO bf-job-mch.
    ASSIGN
        bf-job-mch.m-code   = ipcOperationID
        .
    RUN GetOperationStandardsForJobMch(ROWID(bf-job-mch)).
        
END PROCEDURE.

PROCEDURE pOperationChangeDetermineAction PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Given job-mch buffer and machine code, determine through prompts or settings
        which action to take
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-job-mch FOR job-mch.
    DEFINE INPUT PARAMETER ipcOperationID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcAction AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cMessage1 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMessage2 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lChoice AS LOGICAL NO-UNDO.
    
    IF AVAILABLE ipbf-job-mch THEN 
    DO:
        cMessage1 = "Machine " + TRIM(ipcOperationID) + " is not defined in job standards for this job/form/blank/pass.".
        IF fHasDataCollected(BUFFER ipbf-job-mch) THEN              
            ASSIGN
                cMessage2 = "Data has already been collected for " + TRIM(ipbf-job-mch.m-code) + ".  Would you like to add machine " + TRIM(ipcOperationID) + "?"
                opcAction = "Add"
                .
        ELSE
            ASSIGN
                cMessage2 = "Would you like to replace machine " + TRIM(ipbf-job-mch.m-code) + " with machine " + TRIM(ipcOperationID) + "?"
                opcAction = "Replace"
                .
    END.
    ELSE
        ASSIGN
            cMessage1 = "Machine " + TRIM(ipcOperationID) + " does not have a department that is valid for this job/form/blank/pass."
            cMessage2 = "Would you like to add the department to job standards?"
            opcAction = "AddDept"
            .

    MESSAGE cMessage1 SKIP cMessage2
    VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE lChoice.        
    
    IF NOT lChoice THEN 
        opcAction = "Cancel".

END PROCEDURE.

PROCEDURE pOperationChangeReplaceMachine PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:  Given an existing job-mch, replace the machine code
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-job-mch FOR job-mch.
    DEFINE INPUT PARAMETER ipcOperationID AS CHARACTER NO-UNDO.
    
    FIND CURRENT ipbf-job-mch EXCLUSIVE-LOCK.
    ipbf-job-mch.m-code = ipcOperationID.
    FIND CURRENT ipbf-job-mch NO-LOCK.
    RUN GetOperationStandardsForJobMch(ROWID(ipbf-job-mch)).    

END PROCEDURE.

PROCEDURE ProcessOperationChange:
    /*------------------------------------------------------------------------------
     Purpose:  Given an operationID (mach code) and job, confirm that this machine
     exists on job
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcOperationID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiJob AS INTEGER NO-UNDO. 
    DEFINE INPUT PARAMETER ipiFormNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiPass AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipcDepartmentID AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-job-mch FOR job-mch.
    
    DEFINE VARIABLE cAction AS CHARACTER NO-UNDO.
    
    IF fIsOperationFound(ipcCompany, 
        ipcOperationID,
        ipiJob,
        ipiFormNo,
        ipiBlankNo,
        ipiPass,
        ipcDepartmentID) THEN

        RETURN.  /*If a match is found, return with no action*/
                             
    /*Find machine from same department*/
    FIND FIRST bf-job-mch NO-LOCK
        WHERE bf-job-mch.company EQ ipcCompany
        AND bf-job-mch.job EQ ipiJob
        AND bf-job-mch.frm     EQ ipiFormNo
        AND bf-job-mch.blank-no EQ ipiBlankNo
        AND bf-job-mch.dept    EQ ipcDepartmentID
        AND bf-job-mch.pass    EQ ipiPass
        NO-ERROR.

    RUN pOperationChangeDetermineAction(BUFFER bf-job-mch, ipcOperationID, OUTPUT cAction).
    
    CASE cAction:
        WHEN "Cancel" THEN 
            RETURN.
        WHEN "Add" THEN 
            RUN pOperationChangeAddMachine(BUFFER bf-job-mch, ipcOperationID).
        WHEN "Replace" THEN 
            RUN pOperationChangeReplaceMachine(BUFFER bf-job-mch, ipcOperationID).
        WHEN "AddDept" THEN 
            RUN pOperationChangeAddDepartment(ipcCompany, ipcOperationID, ipiJob, ipiFormNo, ipiBlankNo, ipiPass, ipcDepartmentID).
    END CASE.        
                          
END PROCEDURE.

PROCEDURE pSetAttributeFromStandard PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Sets an attribute value (Creates if doesn't exist)
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiAttributeID AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipcAttributeValue AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-std-code FOR std-code.
    
    DEFINE VARIABLE cAttributeName AS CHARACTER NO-UNDO.
    
    FIND FIRST bf-std-code NO-LOCK 
        WHERE bf-std-code.code EQ STRING(ipiAttributeID,"99")
        NO-ERROR.
    IF AVAILABLE bf-std-code THEN 
        cAttributeName = bf-std-code.dscr.
    
    RUN pSetAttribute(ipiAttributeID, cAttributeName, ipcAttributeValue).
    
END PROCEDURE.

PROCEDURE pSetAttribute PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Sets an attribute value (Creates if doesn't exist)
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiAttributeID AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipcAttributeName AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcAttributeValue AS CHARACTER NO-UNDO.
    
    FIND FIRST ttAttribute
        WHERE ttAttribute.attributeID EQ ipiAttributeID
        NO-ERROR.
    IF NOT AVAILABLE ttAttribute THEN 
    DO:
        CREATE ttAttribute.
        ASSIGN 
            ttAttribute.attributeID = ipiAttributeID.
    END.
    ASSIGN 
        ttAttribute.attributeName  = ipcAttributeName
        ttAttribute.attributeValue = ipcAttributeValue
        .

END PROCEDURE.

PROCEDURE pSetFormAttributes PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Given a form, sets attributes that are form dependent
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ef FOR ef.
    
    DEFINE           BUFFER bf-eb   FOR eb.
        
    RUN pSetAttribute(giAttributeIDBoardItemID, "BoardItemID", ipbf-ef.board).
    RUN pSetAttribute(giAttributeIDCaliper, "Caliper", STRING(ipbf-ef.cal)).
    RUN pSetAttributeFromStandard(ipbf-ef.company,  9, STRING(ipbf-ef.weight)). 
    RUN pSetAttributeFromStandard(ipbf-ef.company,  10, STRING(ipbf-ef.roll-wid)).
    RUN pSetAttributeFromStandard(ipbf-ef.company,  11, STRING(ipbf-ef.nsh-wid)).
    RUN pSetAttributeFromStandard(ipbf-ef.company,  12, STRING(ipbf-ef.nsh-len)).
    RUN pSetAttributeFromStandard(ipbf-ef.company,  14, STRING(ipbf-ef.leaf-l[1])).
    RUN pSetAttributeFromStandard(ipbf-ef.company,  15, STRING(ipbf-ef.leaf-w[1])).
    RUN pSetAttributeFromStandard(ipbf-ef.company,  16, STRING(ipbf-ef.gsh-len)).
    RUN pSetAttributeFromStandard(ipbf-ef.company,  17, STRING(ipbf-ef.gsh-wid)).
    RUN pSetAttributeFromStandard(ipbf-ef.company,  19, STRING(ipbf-ef.die-in)).
    RUN pSetAttributeFromStandard(ipbf-ef.company,  21, STRING(ipbf-ef.nsh-len * ipbf-ef.nsh-wid / 144)). //v-ssqft
    RUN pSetAttributeFromStandard(ipbf-ef.company,  22, STRING(ipbf-ef.f-col + ipbf-ef.f-coat)).
    RUN pSetAttributeFromStandard(ipbf-ef.company,  23, STRING(ipbf-ef.n-cuts)). //v-cut
    RUN pSetAttributeFromStandard(ipbf-ef.company,  24, STRING(ipbf-ef.blank-qty)).
    RUN pSetAttributeFromStandard(ipbf-ef.company,  26, STRING(ipbf-ef.n-out-l)).
    
    FOR EACH bf-eb NO-LOCK
        WHERE bf-eb.company EQ ipbf-ef.company
        AND bf-eb.est-no  EQ ipbf-ef.est-no
        AND bf-eb.form-no EQ ipbf-ef.form-no:
    
        
    END.

END PROCEDURE.

PROCEDURE SetAttribute:
    /*------------------------------------------------------------------------------
     Purpose: Public Wrapper to Set Attribute
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiAttributeID AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipcAttributeName AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcAttributeValue AS CHARACTER NO-UNDO.
    
    RUN pSetAttribute(ipiAttributeID, ipcAttributeName, ipcAttributeValue).

END PROCEDURE.

PROCEDURE SetAttributes:
    /*------------------------------------------------------------------------------
     Purpose:  Sets Attributes for ttAttribute Table
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER TABLE FOR ttAttribute.
    
END PROCEDURE.

PROCEDURE SetAttributesFromEb:
    /*------------------------------------------------------------------------------
     Purpose: Given a rowid (for eb), build out the attributes required
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriRowid AS ROWID NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE iForm  AS INTEGER NO-UNDO.
    DEFINE VARIABLE iBlank AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bf-eb FOR eb. 
    DEFINE BUFFER bf-ef FOR ef.
            
    FIND FIRST bf-eb NO-LOCK 
        WHERE ROWID(bf-eb) EQ ipriRowID
        NO-ERROR.
    
    IF AVAILABLE bf-eb THEN 
    DO:
        RUN pSetAttribute(giAttributeIDStyle, "Style", bf-eb.style).
        RUN pSetAttribute(giAttributeIDBoxDepth, "Box Depth", STRING(bf-eb.dep)).
        RUN pSetAttributeFromStandard(bf-eb.company,  1, "0").  //Maxco
        RUN pSetAttributeFromStandard(bf-eb.company,  2, STRING(bf-eb.len)).
        RUN pSetAttributeFromStandard(bf-eb.company,  3, STRING(bf-eb.wid)).
        RUN pSetAttributeFromStandard(bf-eb.company,  4, STRING(bf-eb.t-len)). //refactor dBlankLen
        RUN pSetAttributeFromStandard(bf-eb.company,  5, STRING(bf-eb.t-wid)). //refactor dBlankWid
        RUN pSetAttributeFromStandard(bf-eb.company,  6, STRING(bf-eb.lin-in)).
        RUN pSetAttributeFromStandard(bf-eb.company,  7, STRING(bf-eb.t-sqft)). 
        RUN pSetAttributeFromStandard(bf-eb.company,  8, "0"). //v-dep * 1000
        RUN pSetAttributeFromStandard(bf-eb.company,  13, "0"). //v-up
        RUN pSetAttributeFromStandard(bf-eb.company,  18, "0").  //qty
        RUN pSetAttributeFromStandard(bf-eb.company,  20, "0"). //(qty * v-yld / xeb.num-up / v-n-out)
        RUN pSetAttributeFromStandard(bf-eb.company,  25, "0"). //v-out
        RUN pSetAttributeFromStandard(bf-eb.company,  27, "0"). //ld-parts[2]
        RUN pSetAttributeFromStandard(bf-eb.company,  28, "0"). //ld-ink-frm
        RUN pSetAttributeFromStandard(bf-eb.company,  29, "0"). //ld-parts[1]
        RUN pSetAttributeFromStandard(bf-eb.company,  30, "0"). //none?
        RUN pSetAttributeFromStandard(bf-eb.company,  31, "0"). //v-long-qty-set
        RUN pSetAttributeFromStandard(bf-eb.company,  32, "0"). //v-short-qty-set
        RUN pSetAttributeFromStandard(bf-eb.company,  33, STRING(bf-eb.num-wid)).
        RUN pSetAttributeFromStandard(bf-eb.company,  34, STRING(bf-eb.num-len)).
        
        FIND FIRST bf-ef OF bf-eb NO-LOCK.
        IF AVAILABLE bf-ef THEN 
            RUN pSetFormAttributes(BUFFER bf-ef).
    END.
    ELSE 
        ASSIGN 
            oplError   = YES
            opcMessage = "Invalid Blank for est-op"
            .
    
END PROCEDURE.


/* ************************  Function Implementations ***************** */

FUNCTION fGetAttributeValue RETURNS DECIMAL PRIVATE
    (ipiAttributeID AS INTEGER):
    /*------------------------------------------------------------------------------
     Purpose: Given an attribute ID, return the value in decimal
     Notes:
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE cAttributeValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dAttributeValue AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lError          AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage        AS CHARACTER NO-UNDO.
    
    RUN pGetAttribute(ipiAttributeID, OUTPUT cAttributeValue, OUTPUT lError, OUTPUT cMessage).
    IF lError THEN 
        dAttributeValue = 0.
    ELSE 
        dAttributeValue = DECIMAL(cAttributeValue) NO-ERROR.
    
    RETURN dAttributeValue.
        		
END FUNCTION.

FUNCTION fHasDataCollected RETURNS LOGICAL PRIVATE
    (BUFFER ipbf-job-mch FOR job-mch):
    /*------------------------------------------------------------------------------
    Purpose: Given job-mch buffer, determine if there is data collected for it
    Notes:
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE lHasData AS LOGICAL NO-UNDO.
    
    lHasData = CAN-FIND(FIRST mch-act
        WHERE mch-act.company EQ ipbf-job-mch.company
        AND mch-act.job EQ ipbf-job-mch.job
        AND mch-act.m-code EQ ipbf-job-mch.m-code
        AND mch-act.frm EQ ipbf-job-mch.frm
        AND mch-act.blank-no EQ ipbf-job-mch.blank-no).

    RETURN lHasData.
		
END FUNCTION.

FUNCTION fIsOperationFound RETURNS LOGICAL PRIVATE
    (ipcCompany AS CHARACTER,
    ipcOperationID AS CHARACTER,
    ipiJob AS INTEGER, 
    ipiFormNo AS INTEGER,
    ipiBlankNo AS INTEGER,
    ipiPass AS INTEGER,
    ipcDepartmentID AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose:  Given a job and operation, determine return if found or not
     Notes:
    ------------------------------------------------------------------------------*/	

    DEFINE VARIABLE lFound AS LOGICAL NO-UNDO.

    lFound = CAN-FIND(
        FIRST job-mch 
        WHERE job-mch.company EQ ipcCompany
        AND job-mch.job  EQ ipiJob
        AND job-mch.m-code  EQ ipcOperationID
        AND job-mch.frm     EQ ipiFormNo
        AND job-mch.blank-no EQ ipiBlankNo
        AND job-mch.dept    EQ ipcDepartmentID
        AND job-mch.pass    EQ ipiPass).

    IF NOT lFound THEN
        /* search without using dept */
        lFound = CAN-FIND(
            FIRST job-mch
            WHERE job-mch.company EQ ipcCompany
            AND job-mch.job EQ ipiJob
            AND job-mch.m-code  EQ ipcOperationID
            AND job-mch.frm     EQ ipiFormNo
            AND job-mch.blank-no EQ ipiBlankNo
            AND job-mch.pass    EQ ipiPass
            ).
    
    RETURN lFound.


		
END FUNCTION.

