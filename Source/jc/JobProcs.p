
/*------------------------------------------------------------------------
    File        : JobProcs.p
    Purpose     : 

    Syntax      :

    Description : All procedures for creating and printing Loadtags for FG, RM, and WIP

    Author(s)   : Mithun Porandla
    Created     : Tue Apr 09 18:31:30 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE VARIABLE iJobFormatLength AS INTEGER NO-UNDO INITIAL 6.

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */

FUNCTION fAddSpacesToString RETURNS CHARACTER 
    ( ipcString AS CHARACTER,
    ipiFormatLength AS INTEGER,      
    iplIsLeading AS LOGICAL ) FORWARD.

/* ***************************  Main Block  *************************** */

/* **********************  Internal Procedures  *********************** */

PROCEDURE GetRecalcJobCostForJobHdr:
/*------------------------------------------------------------------------------
 Purpose: given a job-hdr rowid, return the new values for standard costs
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriJobHdr AS ROWID NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostMat AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostLab AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostVO AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostFO AS DECIMAL NO-UNDO.
    
    DEFINE BUFFER bf-blank-job-mat FOR job-mat.
    DEFINE BUFFER bf-blank-job-mch FOR job-mch.
    DEFINE VARIABLE dCostMat AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dCostLab AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dCostVO AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dCostFO AS DECIMAL NO-UNDO.
        
    FIND FIRST job-hdr NO-LOCK
        WHERE ROWID(job-hdr) EQ ipriJobHdr
        NO-ERROR.
    
    IF NOT AVAILABLE job-hdr THEN RETURN.
    
    FOR EACH job-mat NO-LOCK 
        WHERE job-mat.company EQ job-hdr.company
        AND job-mat.job EQ job-hdr.job
        AND job-mat.job-no EQ job-hdr.job-no
        AND job-mat.job-no2 EQ job-hdr.job-no2
        AND (job-mat.frm EQ job-hdr.frm OR job-hdr.frm EQ 0)
        :
            dCostMat = 0.
            IF job-hdr.blank-no EQ job-mat.blank-no OR job-mat.blank-no EQ 0 THEN DO: 
                dCostMat = job-mat.cost-m.
                IF job-mat.blank-no EQ 0 THEN 
                    
            END.
            opdCostMat = opdCostMat + dCostMat.
    END.
    FOR EACH job-mch NO-LOCK
        WHERE job-mch.company EQ job-hdr.company
        AND job-mch.job EQ job-hdr.job
        AND job-mch.job-no EQ job-hdr.job-no
        AND job-mch.job-no2 EQ job-hdr.job-no2
        AND (job-mch.frm EQ job-hdr.frm OR job-hdr.frm EQ 0)
        :
            ASSIGN 
                dCostLab = (job-mch.run-hr * job-mch.run-rate + job-mch.mr-hr * job-mch.mr-rate) / job-hdr.qty * 1000
                dCostVO = (job-mch.run-hr * job-mch.run-varoh + job-mch.mr-hr * job-mch.mr-varoh) / job-hdr.qty * 1000
                dCostFO = (job-mch.run-hr * job-mch.run-fixoh + job-mch.mr-hr * job-mch.mr-fixoh) / job-hdr.qty * 1000
                .
             ASSIGN 
                opdCostLab = opdCostLab + dCostLab
                opdCostVO = opdCostVO + dCostVO
                opdCostFO = opdCostFO + dCostFO
                .
    END.


END PROCEDURE.

PROCEDURE RecalcJobCostForJob:
    /*------------------------------------------------------------------------------
     Purpose:  This will do a recalculation of the job costs based on the 
     job-mat, job-mch, and job-prep calculations.
     Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriJob AS ROWID NO-UNDO.

    DEFINE BUFFER bf-job-hdr FOR job-hdr.

    FIND FIRST job NO-LOCK
        WHERE ROWID(job) EQ ipriJob NO-ERROR.
    IF NOT AVAILABLE job THEN 
    DO: 
        FIND FIRST job-hdr NO-LOCK 
            WHERE ROWID(job-hdr) EQ ipriJob NO-ERROR.
        RUN RecalcJobCostForJobHdr(ROWID(job-hdr)).
    END.
    ELSE 
        FOR EACH job-hdr NO-LOCK 
            WHERE job-hdr.company EQ job.company
            AND job-hdr.job EQ job.job
            AND job-hdr.job-no EQ job.job-no
            AND job-hdr.job-no2 EQ job.job-no2:
            RUN RecalcJobCostForJobHdr(ROWID(job-hdr)).
        END. 
END PROCEDURE.

PROCEDURE GetSecondaryJobForJob:
    /*------------------------------------------------------------------------------
     Purpose: Returns all available secondary job list for a given jobID
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcJobno        AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER opcJobno2List   AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-job-hdr FOR job-hdr.

    FOR EACH bf-job-hdr NO-LOCK
        WHERE bf-job-hdr.company EQ ipcCompany
        AND bf-job-hdr.job-no  EQ ipcJobno:
        opcJobno2List = IF opcJobno2List EQ "" THEN 
            STRING(bf-job-hdr.job-no2,"99")
            ELSE IF INDEX(opcJobno2List,STRING(bf-job-hdr.job-no2,"99")) GT 0 THEN 
            opcJobno2List
            ELSE 
            opcJobno2List + "," + STRING(bf-job-hdr.job-no2,"99").        
    END.

    RELEASE bf-job-hdr.
END PROCEDURE.

PROCEDURE GetFormnoForJob:
    /*------------------------------------------------------------------------------
     Purpose: Returns form no list for a given jobID
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcJobno        AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipiJobno2       AS INTEGER   NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER opcFormnoList   AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-job     FOR job.
    DEFINE BUFFER bf-job-mat FOR job-mat.
    
    FOR EACH bf-job NO-LOCK
        WHERE bf-job.company EQ ipcCompany
        AND bf-job.job-no  EQ ipcJobno
        AND bf-job.job-no2 EQ ipiJobno2,
        EACH bf-job-mat NO-LOCK
        WHERE bf-job-mat.company EQ bf-job.company
        AND bf-job-mat.job     EQ bf-job.job
        AND bf-job-mat.job-no  EQ bf-job.job-no
        AND bf-job-mat.job-no2 EQ bf-job.job-no2
        USE-INDEX seq-idx:
        opcFormnoList = IF opcFormnoList EQ "" THEN 
            STRING(bf-job-mat.frm,"99")
            ELSE IF INDEX(opcFormnoList,STRING(bf-job-mat.frm,"99")) GT 0 THEN 
            opcFormnoList
            ELSE 
            opcFormnoList + "," + STRING(bf-job-mat.frm,"99").
    END.

    RELEASE bf-job.
    RELEASE bf-job-mat.
END PROCEDURE.

PROCEDURE GetBlanknoForJob:
    /*------------------------------------------------------------------------------
     Purpose: Returns blank no list for a given jobID
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcJobno        AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipiJobno2       AS INTEGER   NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER opcBlanknoList  AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-job     FOR job.
    DEFINE BUFFER bf-job-mat FOR job-mat.

    FOR EACH bf-job NO-LOCK
        WHERE bf-job.company EQ ipcCompany
        AND bf-job.job-no  EQ ipcJobno
        AND bf-job.job-no2 EQ ipiJobno2,
        EACH bf-job-mat NO-LOCK
        WHERE bf-job-mat.company EQ bf-job.company
        AND bf-job-mat.job     EQ bf-job.job
        AND bf-job-mat.job-no  EQ bf-job.job-no
        AND bf-job-mat.job-no2 EQ bf-job.job-no2
        USE-INDEX seq-idx:
        opcBlanknoList = IF opcBlanknoList EQ "" THEN 
            STRING(bf-job-mat.blank-no,"99")
            ELSE IF INDEX(opcBlanknoList,STRING(bf-job-mat.blank-no,"99")) GT 0 THEN 
            opcBlanknoList
            ELSE 
            opcBlanknoList + "," + STRING(bf-job-mat.blank-no,"99").        
    END.

    RELEASE bf-job.
    RELEASE bf-job-mat.
END PROCEDURE.

PROCEDURE GetOperationsForJob:
    /*------------------------------------------------------------------------------
     Purpose: Returns machine code list for a given jobID
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcJobno        AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipiJobno2       AS INTEGER   NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER opcMachineList  AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-job     FOR job.
    DEFINE BUFFER bf-job-mch FOR job-mch.

    FOR EACH bf-job NO-LOCK
        WHERE bf-job.company EQ ipcCompany
        AND bf-job.job-no  EQ ipcJobno
        AND bf-job.job-no2 EQ ipiJobno2,
        EACH bf-job-mch NO-LOCK
        WHERE bf-job-mch.company EQ bf-job.company
        AND bf-job-mch.job     EQ bf-job.job
        AND bf-job-mch.job-no  EQ bf-job.job-no
        AND bf-job-mch.job-no2 EQ bf-job.job-no2
        USE-INDEX line-idx:
        opcMachineList = IF opcMachineList EQ "" THEN 
            STRING(bf-job-mch.m-code)
            ELSE IF INDEX(opcMachineList,STRING(bf-job-mch.m-code)) GT 0 THEN
            opcMachineList
            ELSE
            opcMachineList + "," + STRING(bf-job-mch.m-code).           
    END.

    RELEASE bf-job.
    RELEASE bf-job-mch.
END PROCEDURE.

PROCEDURE GetRMItemsForJob:
    /*------------------------------------------------------------------------------
     Purpose: Returns RM Item list for a given jobID
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcJobno        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiJobno2       AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiFormno       AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiBlankno      AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcRMListItems  AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-job     FOR job.
    DEFINE BUFFER bf-job-mat FOR job-mat.
    
    FOR EACH bf-job NO-LOCK
        WHERE bf-job.company EQ ipcCompany
        AND bf-job.job-no  EQ ipcJobno
        AND bf-job.job-no2 EQ ipiJobno2,
        EACH bf-job-mat NO-LOCK
        WHERE bf-job-mat.company  EQ bf-job.company
        AND bf-job-mat.job      EQ bf-job.job
        AND bf-job-mat.job-no   EQ bf-job.job-no
        AND bf-job-mat.job-no2  EQ bf-job.job-no2
        AND bf-job-mat.frm      EQ ipiFormno
        AND bf-job-mat.blank-no EQ ipiBlankno
        USE-INDEX seq-idx:
        opcRMListItems = IF opcRMListItems EQ "" THEN 
            STRING(bf-job-mat.rm-i-no)
            ELSE IF INDEX(opcRMListItems,STRING(bf-job-mat.rm-i-no)) GT 0 THEN
            opcRMListItems
            ELSE 
            opcRMListItems + "," + STRING(bf-job-mat.rm-i-no).           
    END.

    RELEASE bf-job.
    RELEASE bf-job-mat.
END PROCEDURE.

PROCEDURE GetOperation:
    /*------------------------------------------------------------------------------
     Purpose: Returns machine code list for a given jobID
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcJobno        AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipiJobno2       AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipiForm         AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipcType         AS CHARACTER   NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopcMachineList AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-job     FOR job.
    DEFINE BUFFER bf-job-mch FOR job-mch.

    Main-Loop-Mach:
    FOR EACH bf-job NO-LOCK
        WHERE bf-job.company EQ ipcCompany
        AND bf-job.job-no  EQ ipcJobno
        AND bf-job.job-no2 EQ ipiJobno2,
        EACH bf-job-mch NO-LOCK
        WHERE bf-job-mch.company EQ bf-job.company
        AND bf-job-mch.job     EQ bf-job.job
        AND bf-job-mch.job-no  EQ bf-job.job-no
        AND bf-job-mch.job-no2 EQ bf-job.job-no2
        AND bf-job-mch.frm     EQ ipiForm
        USE-INDEX line-idx:
        
        IF ipcType EQ "First" AND iopcMachineList EQ "" THEN 
        DO:
            iopcMachineList = bf-job-mch.m-code.
            LEAVE.
        END.

        FIND FIRST mach NO-LOCK
            WHERE mach.company EQ ipcCompany
            AND mach.m-code  EQ bf-job-mch.m-code 
            NO-ERROR.

        IF AVAIL mach AND mach.dept[1] EQ "PR" AND ipcType EQ "Press" AND iopcMachineList EQ "" THEN 
        DO:
            iopcMachineList = bf-job-mch.m-code.
            LEAVE.
        END.
        
        IF AVAIL mach AND mach.dept[1] BEGINS "F" AND ipcType EQ "Internal" THEN 
            NEXT Main-Loop-Mach.
        
        IF iopcMachineList EQ "" AND ipcType EQ "Internal" THEN 
        DO:
            iopcMachineList = bf-job-mch.m-code.
            LEAVE.
        END.
    END.

    RELEASE bf-job.
    RELEASE bf-job-mch.
END PROCEDURE.

PROCEDURE JobParser:
    /*------------------------------------------------------------------------------
     Purpose: Parses the job given in XXXXX-99.99.99 format 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcJobno        AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcJobno        AS CHARACTER NO-UNDO.    
    DEFINE OUTPUT PARAMETER opcJobno2       AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcFormno       AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcBlankno      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplIsParsed     AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage      AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE iValid AS INTEGER NO-UNDO.
        
    IF INDEX(ipcJobno,"-") NE 0 THEN 
    DO:        
        iValid = (IF NUM-ENTRIES(ipcJobno,"-") EQ 2 AND
            NUM-ENTRIES(ENTRY(2,ipcJobno,"-"),".") GE 1 THEN
            INTEGER(ENTRY(1,ENTRY(2,ipcJobno,"-"),"."))
            ELSE IF NUM-ENTRIES(ipcJobno,"-") GE 2 THEN
            INTEGER(ENTRY(2,ipcJobno,"-"))
            ELSE 
            0) NO-ERROR.
                 
        IF ERROR-STATUS:ERROR THEN 
        DO:
            opcMessage = "Invalid Jobno2 value".
            RETURN.
        END.
        
        iValid = (IF NUM-ENTRIES(ipcJobno,"-") EQ 2 AND
            NUM-ENTRIES(ENTRY(2,ipcJobno,"-"),".") GE 2 THEN
            INTEGER(ENTRY(2,ENTRY(2,ipcJobno,"-"),"."))
            ELSE IF NUM-ENTRIES(ipcJobno,"-") GE 3 THEN
            INTEGER(ENTRY(3,ipcJobno,"-"))
            ELSE 
            0) NO-ERROR.
                  
        IF ERROR-STATUS:ERROR THEN 
        DO:
            opcMessage = "Invalid Formno value".
            RETURN.
        END.

        iValid = (IF NUM-ENTRIES(ipcJobno,"-") EQ 2 AND
            NUM-ENTRIES(ENTRY(2,ipcJobno,"-"),".") GE 3 THEN
            INTEGER(ENTRY(3,ENTRY(2,ipcJobno,"-"),"."))
            ELSE IF NUM-ENTRIES(ipcJobno,"-") GE 4 THEN
            INTEGER(ENTRY(4,ipcJobno,"-"))
            ELSE 
            0) NO-ERROR.
                  
        IF ERROR-STATUS:ERROR THEN 
        DO:
            opcMessage = "Invalid Blankno value".
            RETURN.
        END.

        ASSIGN
            opcJobNo   = ENTRY(1,ipcJobno,"-")        
            opcJobNo2  = IF NUM-ENTRIES(ipcJobno,"-") EQ 2 AND
                            NUM-ENTRIES(ENTRY(2,ipcJobno,"-"),".") GE 1 THEN
                             ENTRY(1,ENTRY(2,ipcJobno,"-"),".")
                         ELSE IF NUM-ENTRIES(ipcJobno,"-") GE 2 THEN
                             ENTRY(2,ipcJobno,"-")
                         ELSE
                             ""
            opcFormno  = IF NUM-ENTRIES(ipcJobno,"-") EQ 2 AND
                            NUM-ENTRIES(ENTRY(2,ipcJobno,"-"),".") GE 2 THEN
                             ENTRY(2,ENTRY(2,ipcJobno,"-"),".")
                         ELSE IF NUM-ENTRIES(ipcJobno,"-") GE 3 THEN
                             ENTRY(3,ipcJobno,"-")
                         ELSE
                             ""
            opcBlankno = IF NUM-ENTRIES(ipcJobno,"-") EQ 2 AND
                            NUM-ENTRIES(ENTRY(2,ipcJobno,"-"),".") GE 3 THEN
                             ENTRY(3,ENTRY(2,ipcJobno,"-"),".")
                         ELSE IF NUM-ENTRIES(ipcJobno,"-") GE 4 THEN
                             ENTRY(4,ipcJobno,"-")
                         ELSE
                             ""
            NO-ERROR.
            
        IF ERROR-STATUS:ERROR THEN 
        DO:
            opcMessage = ERROR-STATUS:GET-MESSAGE(1).
            RETURN.
        END.            
        
        oplIsParsed = TRUE.
    END.

END PROCEDURE.

PROCEDURE RecalcJobCostForJobHdr:
    /*------------------------------------------------------------------------------
     Purpose:  Given a specific ROWID for job-hdr, recalculate stanard costs and
     assign the new values.
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriJobHdr AS ROWID NO-UNDO.

    DEFINE BUFFER bf-job-hdr FOR job-hdr.

    DEFINE VARIABLE dCostMat   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dCostLab   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dCostVO    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dCostFO    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dCostTotal AS DECIMAL NO-UNDO.

    RUN GetRecalcJobCostForJobHdr(ipriJobHdr, OUTPUT dCostMat, OUTPUT dCostLab, OUTPUT dCostVO, OUTPUT dCostFO).

    dCostTotal = dCostMat + dCostLab + dCostVO + dCostFO.

    IF dCostTotal NE 0 THEN 
    DO:
        FIND FIRST bf-job-hdr EXCLUSIVE-LOCK 
            WHERE ROWID(bf-job-hdr) EQ ipriJobHdr NO-ERROR. 
        IF AVAILABLE bf-job-hdr THEN 
            ASSIGN 
                bf-job-hdr.std-mat-cost = dCostMat
                bf-job-hdr.std-lab-cost = dCostLab
                bf-job-hdr.std-var-cost = dCostVO
                bf-job-hdr.std-fix-cost = dCostFO
                bf-job-hdr.std-tot-cost = dCostTotal
                . 
    END.
END PROCEDURE.

PROCEDURE ValidateJob:
    /*------------------------------------------------------------------------------
     Purpose: Validate Job 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcJobno        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcMachine      AS CHARACTER NO-UNDO.    
    DEFINE INPUT  PARAMETER ipiJobno2       AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiFormno       AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiBlankno      AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValidJob     AS LOGICAL   NO-UNDO.

    DEFINE BUFFER bf-job FOR job.

    Main-Loop-Mach:
    FOR EACH bf-job NO-LOCK
        WHERE bf-job.company EQ ipcCompany
        AND bf-job.job-no  EQ ipcJobno
        AND bf-job.job-no2 EQ ipiJobno2:
        
        IF ipcMachine NE "" THEN
            oplValidJob = CAN-FIND(FIRST job-mch NO-LOCK
                WHERE job-mch.company  EQ bf-job.company
                AND job-mch.job      EQ bf-job.job
                AND job-mch.m-code   EQ ipcMachine
                AND job-mch.job-no   EQ bf-job.job-no
                AND job-mch.job-no2  EQ bf-job.job-no2
                AND job-mch.frm      EQ ipiFormno
                AND job-mch.blank-no EQ ipiBlankno).
        ELSE
            oplValidJob = CAN-FIND(FIRST job-mat NO-LOCK
                WHERE job-mat.company  EQ bf-job.company
                AND job-mat.job      EQ bf-job.job
                AND job-mat.job-no   EQ bf-job.job-no
                AND job-mat.job-no2  EQ bf-job.job-no2
                AND job-mat.frm      EQ ipiFormno
                AND job-mat.blank-no EQ ipiBlankno).
        
        IF oplValidJob THEN
            LEAVE.
    END.
    
    RELEASE bf-job.
END PROCEDURE.

PROCEDURE ValidateJobHdr:
    /*------------------------------------------------------------------------------
     Purpose: Validate Job Header
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcJobno        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiJobno2       AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValidJob     AS LOGICAL   NO-UNDO.
    
    oplValidJob = CAN-FIND(FIRST job-hdr NO-LOCK
        WHERE job-hdr.company  EQ ipcCompany
        AND job-hdr.job-no   EQ ipcJobno
        AND job-hdr.job-no2  EQ ipiJobno2).          
END PROCEDURE.

PROCEDURE GetJobHdrDetails:
    /*------------------------------------------------------------------------------
     Purpose: Fetch Order number, customer number and item number form job-mch 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcJobno        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiJobno2       AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiFormno       AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opiOrdno        AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCustno       AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcIno          AS CHARACTER NO-UNDO.
    
    FIND FIRST job-hdr NO-LOCK 
        WHERE job-hdr.company EQ ipcCompany
        AND job-hdr.job-no  EQ ipcJobno
        AND job-hdr.job-no2 EQ ipiJobno2
        AND job-hdr.frm     EQ ipiFormno NO-ERROR.
    IF AVAILABLE job-hdr THEN
        ASSIGN
            opiOrdno  = job-hdr.ord-no
            opcCustno = job-hdr.cust-no
            opcIno    = job-hdr.i-no.
                                
END PROCEDURE.

/* ************************  Function Implementations ***************** */

FUNCTION fAddSpacesToString RETURNS CHARACTER 
    ( ipcString AS CHARACTER,
    ipiFormatLength AS INTEGER,
    iplIsLeading AS LOGICAL ):
    /*------------------------------------------------------------------------------
     Purpose: Gets formatted string adding spaces
     Notes:
    ------------------------------------------------------------------------------*/	

    DEFINE VARIABLE result AS CHARACTER NO-UNDO.
    
    result = ipcString.
    
    IF LENGTH(ipcString) LT ipiFormatLength THEN 
    DO:
        IF iplIsLeading THEN
            result = FILL (" ", ipiFormatLength - LENGTH(ipcString)) + ipcString.
        ELSE 
            result = ipcString + FILL (" ", ipiFormatLength - LENGTH(ipcString)).   
    END.
    
    RETURN result.

END FUNCTION.

PROCEDURE GetOperationsForEst:
    /*------------------------------------------------------------------------------
     Purpose: Returns machine code list for a given jobID
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcEst         AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMachineList AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-est-op FOR est-op.
    
    Main-Loop-Mach:
    FOR EACH bf-est-op NO-LOCK
        WHERE bf-est-op.company EQ ipcCompany 
        AND bf-est-op.est-no  EQ ipcEst 
        AND bf-est-op.line    LT 500
        BREAK BY bf-est-op.est-no:
        IF NOT LAST( bf-est-op.est-no) THEN
            opcMachineList = opcMachineList + bf-est-op.m-code + "," .
        ELSE 
            opcMachineList = opcMachineList + bf-est-op.m-code .
    END.
    RELEASE bf-est-op.
END PROCEDURE.
