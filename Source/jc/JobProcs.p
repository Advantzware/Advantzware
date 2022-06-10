/*------------------------------------------------------------------------
    File        : JobProcs.p
    Purpose     : 

    Syntax      :

    Description : All procedures for creating and printing Loadtags for FG, RM, and WIP

    Author(s)   : Mithun Porandla
    Created     : Tue Apr 09 18:31:30 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/
/*  Mod: Ticket - 103137  Format Change for Order No. and Job No.       */       

/* ***************************  Definitions  ************************** */

DEFINE VARIABLE iJobFormatLength AS INTEGER NO-UNDO INITIAL 6.

DEFINE TEMP-TABLE w-job NO-UNDO 
    FIELD job like job.job.

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */

FUNCTION fAddSpacesToString RETURNS CHARACTER 
    ( ipcString AS CHARACTER,
    ipiFormatLength AS INTEGER,      
    iplIsLeading AS LOGICAL ) FORWARD.

FUNCTION fGetJobPrefix RETURNS CHARACTER PRIVATE
	(ipcCompany AS CHARACTER) FORWARD.

FUNCTION fGetJobSubAssemblyPrefix RETURNS CHARACTER PRIVATE
    (ipcCompany AS CHARACTER) FORWARD.

/* ***************************  Main Block  *************************** */

/* **********************  Internal Procedures  *********************** */

PROCEDURE CheckJobStatus:
    /*------------------------------------------------------------------------------
     Purpose: To check a job's status
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobNo   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiJobNo2  AS INTEGER   NO-UNDO.
    
    FIND FIRST job NO-LOCK 
        WHERE job.company EQ ipcCompany
        AND job.job-No  EQ ipcJobNo
        AND job.job-No2 EQ ipiJobNo2
        NO-ERROR.  
    IF AVAILABLE job AND NOT job.opened THEN 
    DO:
        RUN DisplayMessage("19").
        RETURN ERROR.
    END. 

END PROCEDURE.

PROCEDURE IsJobClosed:
    /*------------------------------------------------------------------------------
     Purpose: To check a job's status
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER  ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER  ipcJobNo   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER  ipiJobNo2  AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplClosed  AS LOGICAL   NO-UNDO.
    
    oplClosed = CAN-FIND(FIRST job NO-LOCK
        WHERE job.company EQ ipcCompany
        AND job.job-No  EQ ipcJobNo
        AND job.job-No2 EQ ipiJobNo2
        AND NOT job.opened).
END PROCEDURE.

PROCEDURE GetFormAndBlankFromJobAndFGItem:
    /*------------------------------------------------------------------------------
     Purpose: Returns the list of form and blank no list for a given job and item
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcJobno       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiJobno2      AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemID      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcFormNoList  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcBlankNoList AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-job     FOR job.
    DEFINE BUFFER bf-job-hdr FOR job-hdr.

    FOR EACH bf-job NO-LOCK
        WHERE bf-job.company EQ ipcCompany
        AND bf-job.job-no  EQ ipcJobno
        AND bf-job.job-no2 EQ ipiJobno2,
        EACH bf-job-hdr NO-LOCK
        WHERE bf-job-hdr.company EQ bf-job.company
        AND bf-job-hdr.job     EQ bf-job.job
        AND bf-job-hdr.job-no  EQ bf-job.job-no
        AND bf-job-hdr.job-no2 EQ bf-job.job-no2
        AND bf-job-hdr.i-no    EQ ipcItemID
        AND bf-job-hdr.opened  EQ TRUE:
        ASSIGN
            opcFormNoList  = STRING(bf-job-hdr.frm) + ","
            opcBlankNoList = STRING(bf-job-hdr.blank-no) + ","
            .
    END.

    ASSIGN
        opcFormNoList  = TRIM(opcFormNoList, ",")
        opcBlankNoList = TRIM(opcBlankNoList, ",")
        .
    
    RELEASE bf-job.
    RELEASE bf-job-hdr.

END PROCEDURE.


PROCEDURE Job_GetJobNo:
    /*------------------------------------------------------------------------------
     Purpose:  Given inputs, produce the job-no and job-no2 for a new job
     Notes: Deprecates jc/job-no.p
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiOrderNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiJobInternal AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER iplIsSubAssembly AS LOGICAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopcJobNo AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiJobNo2 AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE cJobFormat AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound     AS LOGICAL   NO-UNDO.
    
    RUN sys/ref/nk1look.p (ipcCompany, "JOB#", "C", NO, NO, "", "", OUTPUT cJobFormat, OUTPUT lFound).
    
    IF iplIsSubAssembly THEN DO:
        iopcJobNo = DYNAMIC-FUNCTION('sfFormat_SingleJob', fGetJobPrefix(ipcCompany) + iopcJobNo).
    END.
    ELSE          
        CASE cJobFormat:
            WHEN "YYMMSEQ#" THEN 
                DO:
                    iopcJobNo = SUBSTRING(STRING(YEAR(TODAY),"9999"),3,2) + STRING(MONTH(TODAY),"99").
                    FIND LAST job NO-LOCK
                        WHERE job.company EQ ipcCompany
                        AND job.job-no BEGINS iopcJobNo
                        USE-INDEX job-no NO-ERROR.
            
                    iopiJobNo2 = IF AVAIL job THEN INT(SUBSTR(TRIM(job.job-no),5,2)) ELSE 0 NO-ERROR.
    
                    IF ERROR-STATUS:ERROR THEN iopiJobNo2 = 0.
    
                    ASSIGN
                        iopcJobNo  = iopcJobNo + STRING(iopiJobNo2 + 1,"999")
                        iopiJobNo2 = 0.
                END.
            WHEN "Order#" OR 
            WHEN "" THEN 
                DO:  
                    IF ipiOrderNo NE 0 THEN 
                        iopcJobNo = DYNAMIC-FUNCTION('sfFormat_SingleJob', STRING(ipiOrderNo)). 
                    ELSE 
                        iopcJobNo = DYNAMIC-FUNCTION('sfFormat_SingleJob', fGetJobPrefix(ipcCompany) + ipcEstNo).
                END.
            WHEN "Estimate#" THEN 
                DO:
                    iopcJobNo = DYNAMIC-FUNCTION('sfFormat_SingleJob', ipcEstNo).
                END.
            WHEN "PLine&Order#" THEN 
                DO:
                    IF ipiOrderNo GT 0 THEN DO:
                        FIND FIRST itemfg NO-LOCK 
                            WHERE itemfg.company EQ ipcCompany
                            AND itemfg.i-no EQ ipcItemID
                            NO-ERROR.
                        IF AVAILABLE itemfg THEN 
                            FIND FIRST prodl NO-LOCK  
                                WHERE prodl.procat EQ itemfg.procat
                                NO-ERROR.
                        IF AVAIL prodl THEN 
                            iopcJobNo =  DYNAMIC-FUNCTION('sfFormat_SingleJob', SUBSTRING(prodl.prolin,1,1) + STRING(ipiOrderNo)).
                    END.
                END.       
            WHEN "InternalJob" THEN
                iopcJobNo = DYNAMIC-FUNCTION('sfFormat_SingleJob', STRING(ipiJobInternal)). 
        END.
    
    IF iopcJobNo EQ "" THEN 
        iopcJobNo = DYNAMIC-FUNCTION('sfFormat_SingleJob', ipcEstNo).

    
    IF iopcJobNo NE "" THEN
    DO WHILE CAN-FIND(FIRST job
            WHERE job.company EQ ipcCompany
            AND job.job-no  EQ iopcJobNo
            AND job.job-no2 EQ iopiJobNo2)    
        OR
        CAN-FIND(FIRST oe-ord
            WHERE oe-ord.company EQ ipcCompany
            AND oe-ord.job-no  EQ iopcJobNo
            AND oe-ord.job-no2 EQ iopiJobNo2) 
        OR
        CAN-FIND(FIRST oe-ordl
            WHERE oe-ordl.company EQ ipcCompany
            AND oe-ordl.job-no  EQ iopcJobNo
            AND oe-ordl.job-no2 EQ iopiJobNo2):
            
        iopiJobNo2 = iopiJobNo2 + 1.
    END.
    
END PROCEDURE.

PROCEDURE Job_GetJobNoInternal:
    /*------------------------------------------------------------------------------
     Purpose: Gets the next internal job # (eg job.job) for a company
     Notes:
    ------------------------------------------------------------------------------*/

    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiJob     AS INTEGER   NO-UNDO.
    
    DEFINE BUFFER bf-job     FOR job.
    DEFINE BUFFER bf-job-hdr FOR job-hdr.
    
    opiJob = 1.
        
    FIND LAST bf-job NO-LOCK
        WHERE bf-job.company EQ ipcCompany 
        USE-INDEX job NO-ERROR.
    
    FIND LAST bf-job-hdr NO-LOCK
        WHERE bf-job-hdr.company EQ ipcCompany
        USE-INDEX job NO-ERROR.
    
    IF AVAILABLE bf-job AND AVAILABLE bf-job-hdr THEN 
    DO:
        IF bf-job-hdr.job GT bf-job.job THEN 
            opiJob = bf-job-hdr.job + 1.
        
        IF bf-job.job GE bf-job-hdr.job THEN
            opiJob = bf-job.job + 1.
    END.
    
END PROCEDURE.

PROCEDURE Job_GetMatActQuantity:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcJobNo       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiJobNo2      AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemID      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTag         AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdQuantity    AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcQuantityUOM AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-job FOR job.
    
    FIND FIRST bf-job NO-LOCK
        WHERE bf-job.company EQ ipcCompany
        AND bf-job.job-no  EQ ipcJobNo
        AND bf-job.job-no2 EQ ipiJobNo2
        NO-ERROR.
    IF AVAILABLE bf-job THEN
        RUN pGetMatActQuantity (bf-job.company, bf-job.job, ipcItemID, ipcTag, OUTPUT opdQuantity, OUTPUT opcQuantityUOM).
END PROCEDURE.

PROCEDURE pGetMatActQuantity PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiJob         AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemID      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTag         AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdQuantity    AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcQuantityUOM AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-mat-act FOR mat-act.
    
    FOR EACH bf-mat-act NO-LOCK
        WHERE bf-mat-act.company EQ ipcCompany
        AND bf-mat-act.job     EQ ipiJob
        AND bf-mat-act.i-no    EQ ipcItemID
        AND bf-mat-act.tag     EQ ipcTag:
        ASSIGN
            opdQuantity    = opdQuantity + bf-mat-act.qty
            opcQuantityUOM = bf-mat-act.qty-uom
            .
    END.
END PROCEDURE.

PROCEDURE Job_GetNextOperation:
    /*------------------------------------------------------------------------------
     Purpose: Returns machine code list for a given jobID
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcJobno         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiJobno2        AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiFormNo        AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPassNo        AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcMachineID     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNextMachineID AS CHARACTER NO-UNDO.

    DEFINE VARIABLE lMachineFound AS LOGICAL NO-UNDO.
    
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
        AND bf-job-mch.frm     EQ ipiFormNo
        USE-INDEX line-idx:
        /* Set the lMachineFound to true only if the given machine and pass is found. */
        IF bf-job-mch.pass EQ ipiPassNo AND bf-job-mch.m-code EQ ipcMachineID THEN
            lMachineFound = TRUE.

        /* As the records are iterated with line-idx index, we should find the next 
           machine which is not the input machine in the next iterations if available*/
        IF lMachineFound AND bf-job-mch.m-code NE ipcMachineID THEN 
        DO:              
            opcNextMachineID = bf-job-mch.m-code.
            LEAVE.
        END.
    END.

    RELEASE bf-job.
    RELEASE bf-job-mch.
END PROCEDURE.

PROCEDURE GetSecondaryJobForJobByStatus:
    /*------------------------------------------------------------------------------
     Purpose: Returns all available secondary job list for a given jobID which are open
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcJobno        AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER iplJobStatus    AS LOGICAL   NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER opcJobno2List   AS CHARACTER NO-UNDO.

    RUN pGetSecondaryJobForJob (
        INPUT  ipcCompany,
        INPUT  ipcJobNo,
        INPUT  iplJobStatus,
        INPUT-OUTPUT opcJobno2List
        ).
END PROCEDURE.

PROCEDURE GetSecondaryJobForJob:
    /*------------------------------------------------------------------------------
     Purpose: Returns all available secondary job list for a given jobID which are open
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcJobno        AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER opcJobno2List   AS CHARACTER NO-UNDO.

    RUN pGetSecondaryJobForJob (
        INPUT  ipcCompany,
        INPUT  ipcJobNo,
        INPUT  TRUE, /* Open Jobs */
        INPUT-OUTPUT opcJobno2List
        ).
END PROCEDURE.


PROCEDURE pAllocationJobMaterial PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: add or update job material 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcType         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iprwRowIdJob    AS ROWID     NO-UNDO.    
    DEFINE INPUT PARAMETER ipiFormNo       AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankNo      AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipcRmItem       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdAllocation   AS INTEGER   NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER ioprwRowId AS ROWID NO-UNDO.
    
    DEFINE BUFFER bf-job     FOR job.
    DEFINE BUFFER bf-job-mat FOR job-mat.
    DEFINE BUFFER bf-item    FOR ITEM.
    
    FIND FIRST bf-job NO-LOCK
        WHERE ROWID(bf-job) EQ iprwRowIdJob NO-ERROR.
    
    IF ipcType EQ "Add" THEN
    DO:
        CREATE bf-job-mat.
        ASSIGN
            bf-job-mat.company  = ipcCompany
            bf-job-mat.job      = bf-job.job
            bf-job-mat.job-no   = bf-job.job-no
            bf-job-mat.job-no2  = bf-job.job-no2
            bf-job-mat.rm-i-no  = ipcRmItem
            bf-job-mat.i-no     = ipcRmItem
            bf-job-mat.frm      = ipiFormNo
            bf-job-mat.blank-no = ipiBlankNo
            bf-job-mat.qty-all  = ipdAllocation
            ioprwRowId          = ROWID(bf-job-mat)
            .  
               
        FIND FIRST bf-item NO-LOCK
            WHERE bf-item.company EQ ipcCompany 
            AND bf-item.i-no EQ ipcRmItem NO-ERROR.
        IF AVAIL bf-item THEN
        DO:
            bf-job-mat.basis-w = bf-item.basis-w.        
            bf-job-mat.sc-uom = bf-item.cons-uom.
            bf-job-mat.qty-uom  = bf-item.cons-uom.
            IF bf-item.r-wid NE 0 THEN
                bf-job-mat.wid = bf-item.r-wid.
            ELSE
                ASSIGN
                    bf-job-mat.wid = bf-item.s-wid
                    bf-job-mat.len = bf-item.s-len.       
               
        END.    
             
    END.
    ELSE IF ipcType EQ "Update" THEN
        DO:
            FIND FIRST bf-job-mat EXCLUSIVE-LOCK
                WHERE ROWID(bf-job-mat) EQ ioprwRowId NO-ERROR.
              
            IF AVAILABLE bf-job-mat THEN     
                ASSIGN            
                    bf-job-mat.rm-i-no = ipcRmItem
                    bf-job-mat.i-no    = ipcRmItem
                    bf-job-mat.qty-all = ipdAllocation
                    .
        END.
    RELEASE bf-job-mat.  
    
END PROCEDURE.

PROCEDURE pGetSecondaryJobForJob PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Returns all available secondary job list for a given jobID
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcJobno        AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER iplJobStatus    AS LOGICAL   NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER opcJobno2List   AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-job-hdr FOR job-hdr.

    FOR EACH bf-job-hdr NO-LOCK
        WHERE bf-job-hdr.company EQ ipcCompany
        AND bf-job-hdr.job-no  EQ ipcJobno
        AND (bf-job-hdr.opened EQ iplJobStatus OR iplJobStatus EQ ?)
        BY bf-job-hdr.opened DESCENDING
        BY bf-job-hdr.job-no2:
        opcJobno2List = IF opcJobno2List EQ "" THEN 
            STRING(bf-job-hdr.job-no2,"999")
            ELSE IF INDEX(opcJobno2List,STRING(bf-job-hdr.job-no2,"999")) GT 0 THEN 
            opcJobno2List
            ELSE 
            opcJobno2List + "," + STRING(bf-job-hdr.job-no2,"999").        
    END.

    RELEASE bf-job-hdr.
END PROCEDURE.

PROCEDURE GetFormNoForJobHeaderByStatus:
    /*------------------------------------------------------------------------------
     Purpose: Returns all available secondary job list for a given jobID
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcJobno        AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipiJobno2       AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER iplJobStatus    AS LOGICAL   NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER opcFormnoList   AS CHARACTER NO-UNDO.    

    RUN pGetFormNoForJobHeader(
        INPUT ipcCompany,
        INPUT ipcJobno,
        INPUT ipiJobno2,
        INPUT iplJobStatus, /* job status */
        INPUT-OUTPUT opcFormnoList
        ).    
END PROCEDURE.

PROCEDURE GetFormNoForJobHeader:
    /*------------------------------------------------------------------------------
     Purpose: Returns all available secondary job list for a given jobID
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcJobno        AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipiJobno2       AS INTEGER   NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER opcFormnoList   AS CHARACTER NO-UNDO.    

    RUN pGetFormNoForJobHeader(
        INPUT ipcCompany,
        INPUT ipcJobno,
        INPUT ipiJobno2,
        INPUT TRUE, /* job status */
        INPUT-OUTPUT opcFormnoList
        ).    
END PROCEDURE.

PROCEDURE pGetFormNoForJobHeader PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Returns all available secondary job list for a given jobID
     Notes: Send iplJobStatus value with ? for fetching both opened and closed
    ------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcJobno        AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipiJobno2       AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER iplJobStatus    AS LOGICAL   NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER opcFormnoList   AS CHARACTER NO-UNDO.    

    DEFINE BUFFER bf-job-hdr FOR job-hdr.
    DEFINE BUFFER bf-job     FOR job.
    
    FOR EACH bf-job NO-LOCK
        WHERE bf-job.company EQ ipcCompany
        AND bf-job.job-no  EQ ipcJobno
        AND bf-job.job-no2 EQ ipiJobno2
        AND (bf-job.opened EQ iplJobStatus OR iplJobStatus EQ ?),
        EACH bf-job-hdr NO-LOCK
        WHERE bf-job-hdr.company EQ ipcCompany
        AND bf-job-hdr.job     EQ bf-job.job
        AND bf-job-hdr.job-no  EQ ipcJobno
        AND bf-job-hdr.job-no2 EQ ipiJobNo2
        AND (bf-job-hdr.opened EQ iplJobStatus OR iplJobStatus EQ ?)
        BY bf-job-hdr.job-no2:
        opcFormnoList = IF opcFormnoList EQ "" THEN 
            STRING(bf-job-hdr.frm,"99")
            ELSE IF INDEX(opcFormnoList,STRING(bf-job-hdr.frm,"99")) GT 0 THEN 
            opcFormnoList
            ELSE 
            opcFormnoList + "," + STRING(bf-job-hdr.frm,"99").        
    END.    
END PROCEDURE.

PROCEDURE GetBlankNoForJobHeaderByStatus:
    /*------------------------------------------------------------------------------
     Purpose: Returns all available secondary job list for a given jobID
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcJobno        AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipiJobno2       AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER iplJobStatus    AS LOGICAL   NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER opcBlankNoList  AS CHARACTER NO-UNDO.    

    RUN pGetBlankNoForJobHeader (
        INPUT ipcCompany,
        INPUT ipcJobno,
        INPUT ipiJobno2,
        INPUT iplJobStatus,
        INPUT-OUTPUT opcBlankNoList
        ).      
END PROCEDURE.

PROCEDURE GetBlankNoForJobHeader:
    /*------------------------------------------------------------------------------
     Purpose: Returns all available secondary job list for a given jobID
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcJobno        AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipiJobno2       AS INTEGER   NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER opcBlankNoList  AS CHARACTER NO-UNDO.    

    RUN pGetBlankNoForJobHeader (
        INPUT ipcCompany,
        INPUT ipcJobno,
        INPUT ipiJobno2,
        INPUT TRUE, /* job status */
        INPUT-OUTPUT opcBlankNoList
        ).      
END PROCEDURE.

PROCEDURE pGetBlankNoForJobHeader PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Returns all available secondary job list for a given jobID
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcJobno        AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipiJobno2       AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER iplJobStatus    AS LOGICAL   NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER opcBlankNoList  AS CHARACTER NO-UNDO.    

    DEFINE BUFFER bf-job-hdr FOR job-hdr.
    DEFINE BUFFER bf-job     FOR job.
    
    FOR EACH bf-job NO-LOCK
        WHERE bf-job.company EQ ipcCompany
        AND bf-job.job-no  EQ ipcJobno
        AND bf-job.job-no2 EQ ipiJobno2
        AND (bf-job.opened EQ iplJobStatus OR iplJobStatus EQ ?),
        EACH bf-job-hdr NO-LOCK
        WHERE bf-job-hdr.company EQ ipcCompany
        AND bf-job-hdr.job     EQ bf-job.job
        AND bf-job-hdr.job-no  EQ ipcJobno
        AND bf-job-hdr.job-no2 EQ ipiJobNo2
        AND (bf-job-hdr.opened EQ iplJobStatus OR iplJobStatus EQ ?)
        BY bf-job-hdr.job-no2:
        opcBlankNoList = IF opcBlankNoList EQ "" THEN 
            STRING(bf-job-hdr.blank-no,"99")
            ELSE IF INDEX(opcBlankNoList,STRING(bf-job-hdr.blank-no,"99")) GT 0 THEN 
            opcBlankNoList
            ELSE 
            opcBlankNoList + "," + STRING(bf-job-hdr.blank-no,"99").        
    END.    
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
        AND bf-job.job-no2 EQ ipiJobno2
        AND bf-job.opened,
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

PROCEDURE job_AllocationJobMaterial:
    /*------------------------------------------------------------------------------
     Purpose: Returns blank no list for a given jobID
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcType         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iprwRowIdJob    AS ROWID     NO-UNDO.     
    DEFINE INPUT PARAMETER ipiFormNo       AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankNo      AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipcRmItem       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdAllocation   AS INTEGER   NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER ioprwRowId AS ROWID NO-UNDO.
      
    RUN pAllocationJobMaterial(INPUT ipcType, INPUT ipcCompany, INPUT iprwRowIdJob, INPUT ipiFormNo, INPUT ipiBlankNo, INPUT ipcRmItem, INPUT ipdAllocation, INPUT-OUTPUT ioprwRowId).
   
    
END PROCEDURE.

PROCEDURE job_CopyMaterialPreviousJob:
    /*------------------------------------------------------------------------------
     Purpose: Returns blank no list for a given jobID
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iprwRowIdJob AS ROWID     NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobNo     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiJobNo2    AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiFormNo    AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankNo   AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplComplete AS LOGICAL NO-UNDO.
      
    RUN pCopyMaterialPreviousJob(INPUT ipcCompany, INPUT iprwRowIdJob, INPUT ipcJobNo, INPUT ipiJobNo2, INPUT ipiFormNo, INPUT ipiBlankNo, OUTPUT oplComplete).
   
    
END PROCEDURE.

PROCEDURE job_CloseJob_DCPost:
    /*------------------------------------------------------------------------------
     Purpose: Returns blank no list for a given jobID
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER TABLE FOR w-job.
      
    RUN CloseJob_DCPost(INPUT ipcCompany,TABLE w-job).
   
    
END PROCEDURE.

PROCEDURE job_Delete_JobMaterial:
    /*------------------------------------------------------------------------------
     Purpose: Returns blank no list for a given jobID
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprwRowid      AS ROWID NO-UNDO.
          
    RUN pDelete_JobMaterial(INPUT iprwRowid).      
    
END PROCEDURE.


PROCEDURE CloseJob_DCPost PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Returns blank no list for a given jobID
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER TABLE FOR w-job.
    DEFINE VARIABLE close_date            AS DATE      NO-UNDO.
    DEFINE VARIABLE v-fin-qty             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lRecFound             AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cCloseJob             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cocode                AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lLastOpAllRunComplete AS LOGICAL   NO-UNDO.
    
    cocode = ipcCompany.
    RUN sys/ref/nk1look.p (
        INPUT ipcCompany,         /* Company Code */ 
        INPUT "CLOSEJOB", /* sys-ctrl name */
        INPUT "C",                /* Output return value */
        INPUT NO,                 /* Use ship-to */
        INPUT NO,                 /* ship-to vendor */
        INPUT "",                 /* ship-to vendor value */
        INPUT "",                 /* shi-id value */
        OUTPUT cCloseJob, 
        OUTPUT lRecFound
        ).
        
    IF cCloseJob EQ "DCPost" THEN
    DO:
        close_date = TODAY.  
        FOR EACH w-job,
            FIRST job
            WHERE job.company eq ipcCompany
            and job.job     eq w-job.job       
            NO-LOCK,
            EACH job-hdr
            WHERE job-hdr.company eq ipcCompany
            AND job-hdr.job     eq job.job
            AND job-hdr.job-no  eq job.job-no
            AND job-hdr.job-no2 eq job.job-no2          
            USE-INDEX job 
            TRANSACTION:
        
            RUN pGetMachRunComplete(INPUT ipcCompany, INPUT job.job-no, INPUT job.job-no2,  OUTPUT lLastOpAllRunComplete).
           
            IF lLastOpAllRunComplete THEN
            DO:         
                {jc/job-clos.i}

                FIND CURRENT reftable NO-LOCK NO-ERROR.

                job-hdr.opened = job.opened.
            END.
        END.         
    END. 
    
END PROCEDURE.  


PROCEDURE pDelete_JobMaterial PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iprwRowid   AS ROWID NO-UNDO.
    DEFINE BUFFER bf-job-mat FOR job-mat.
    DEFINE BUFFER bf-job     FOR job.
    
    FIND FIRST bf-job-mat EXCLUSIVE-LOCK 
        WHERE rowid(bf-job-mat) EQ iprwRowid NO-ERROR.
    
    IF AVAIL bf-job-mat THEN
    DO:
        FIND FIRST bf-job EXCLUSIVE-LOCK
            WHERE bf-job.company EQ bf-job-mat.company
            AND bf-job.job-no  EQ bf-job-mat.job-no
            AND bf-job.job-no2 EQ bf-job-mat.job-no2
            AND bf-job.job     EQ bf-job-mat.job NO-ERROR.
               
        RUN jc/jc-all.p (ROWID(bf-job-mat), -1, INPUT-OUTPUT bf-job.stat). 
        FIND CURRENT bf-job NO-LOCK.
        DELETE bf-job-mat.
    END.
    
END PROCEDURE.  

PROCEDURE pCopyMaterialPreviousJob PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Returns blank no list for a given jobID
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iprwRowIdJob AS ROWID     NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobNo     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiJobNo2    AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiFormNo    AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankNo   AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplComplete AS LOGICAL NO-UNDO.
                 
    DEFINE BUFFER bf-job-mat  FOR job-mat.
    DEFINE BUFFER bff-job-mat FOR job-mat.
    DEFINE BUFFER bf-job      FOR job.
    DEFINE BUFFER bf-item     FOR ITEM.
    DEFINE BUFFER bff-job     FOR job.
    DEFINE BUFFER bf-job-hdr  FOR job-hdr.
    
    FIND FIRST bff-job NO-LOCK
        WHERE ROWID(bff-job) EQ iprwRowIdJob NO-ERROR.
         
    IF NOT AVAIL bff-job THEN
        RETURN NO-APPLY.
    
    RUN spProgressBar ("Running Copy process", 90, 100).
    FOR EACH bff-job-mat EXCLUSIVE-LOCK
        WHERE bff-job-mat.company EQ ipcCompany
        AND bff-job-mat.job-no EQ bff-job.job-no 
        AND bff-job-mat.job-no NE ""   
        AND bff-job-mat.job-no2 EQ bff-job.job-no2,
        FIRST bf-item NO-LOCK WHERE bf-item.company EQ bff-job-mat.company 
        AND bf-item.i-no    EQ bff-job-mat.rm-i-no 
        and lookup(bf-item.mat-type,"1,2,3,4,A,B,R,P") GT 0:           
         
        IF bff-job-mat.all-flg EQ YES THEN
            RUN jc/jc-all2.p (ROWID(bff-job-mat), - 1).
         
        DELETE  bff-job-mat.         
    END.
     
    FOR EACH bf-job-mat NO-LOCK
        WHERE bf-job-mat.company EQ ipcCompany
        and bf-job-mat.job-no    EQ ipcJobNo 
        AND bf-job-mat.job-no ne ""   
        AND bf-job-mat.job-no2 EQ ipiJobNo2 
        AND (bf-job-mat.frm      EQ ipiFormNo OR ipiFormNo EQ ?)         
        AND (bf-job-mat.blank-no EQ ipiBlankNo OR ipiBlankNo EQ ?) 
        AND bf-job-mat.qty-all GT 0 USE-INDEX seq-idx, 
        FIRST bf-job       WHERE bf-job.company EQ bf-job-mat.company 
        AND bf-job.job     EQ bf-job-mat.job       
        AND bf-job.job-no  EQ bf-job-mat.job-no         
        AND bf-job.job-no2 EQ bf-job-mat.job-no2, 
        FIRST bf-item NO-LOCK WHERE bf-item.company EQ bf-job-mat.company 
        AND bf-item.i-no    EQ bf-job-mat.rm-i-no 
        and lookup(bf-item.mat-type,"1,2,3,4,A,B,R,P") GT 0 :
                         
        CREATE bff-job-mat.
        BUFFER-COPY bf-job-mat EXCEPT rec_key job job-no job-no2 TO bff-job-mat.
        ASSIGN
            bff-job-mat.job     = bff-job.job
            bff-job-mat.job-no  = bff-job.job-no
            bff-job-mat.job-no2 = bff-job.job-no2
            oplComplete         = YES
            .         
        IF bff-job-mat.all-flg EQ YES THEN
            RUN jc/jc-all2.p (ROWID(bff-job-mat), 1).
    END.   
    RUN spProgressBar (?, ?, 100).
    
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
        AND bf-job.job-no2 EQ ipiJobno2
        AND bf-job.opened,
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
    DEFINE INPUT        PARAMETER ipiFormNo       AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipiBlankNo      AS INTEGER   NO-UNDO.
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
        AND (bf-job-mch.frm     EQ ipiFormNo OR ipiFormNo EQ ?)
        AND (bf-job-mch.blank-no EQ ipiBlankNo OR bf-job-mch.blank-no EQ 0 OR ipiBlankNO EQ ?)
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

PROCEDURE GetOperationsForJobNotCompleted:
    /*------------------------------------------------------------------------------
     Purpose: Returns machine code list for a given jobID Not Completed
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcJobno        AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipiJobno2       AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipiFormNo       AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipiBlankNo      AS INTEGER   NO-UNDO.
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
        AND bf-job-mch.frm     EQ ipiFormNo
        AND (bf-job-mch.blank-no EQ ipiBlankNo OR bf-job-mch.blank-no EQ 0)
        AND NOT bf-job-mch.run-complete
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

PROCEDURE GetFGItemForJob:
    /*------------------------------------------------------------------------------
     Purpose: Returns machine code list for a given jobID
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcJobno        AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipiJobno2       AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipiFormNo       AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipiBlankNo      AS INTEGER   NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER opcFGItemList  AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-job     FOR job.
    DEFINE BUFFER bf-job-hdr FOR job-hdr.

    FOR EACH bf-job NO-LOCK
        WHERE bf-job.company EQ ipcCompany
        AND bf-job.job-no  EQ ipcJobno
        AND bf-job.job-no2 EQ ipiJobno2,
        EACH bf-job-hdr NO-LOCK
        WHERE bf-job-hdr.company EQ bf-job.company
        AND bf-job-hdr.job     EQ bf-job.job
        AND bf-job-hdr.job-no  EQ bf-job.job-no
        AND bf-job-hdr.job-no2 EQ  bf-job.job-no2:
        /* Put the item first in the list if the job-hdr record's form and blank matching the input form and blank no */
        IF bf-job-hdr.frm  EQ ipiFormNo AND bf-job-hdr.blank-no EQ ipiBlankNo THEN
            ASSIGN
                opcFGItemList = STRING(bf-job-hdr.i-no) + "," + TRIM(opcFGItemList, ",")
                opcFGItemList = TRIM(opcFGItemList, ",")
                .
        ELSE    
            opcFGItemList = opcFGItemList + "," + STRING(bf-job-hdr.i-no).           
    END.

    opcFGItemList = TRIM(opcFGItemList, ",").
    
    RELEASE bf-job.
    RELEASE bf-job-hdr.
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

    DEFINE VARIABLE cSSIssueDefaultRM AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lRecFound         AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lFirstSelected    AS LOGICAL   NO-UNDO.
    
    DEFINE BUFFER bf-job     FOR job.
    DEFINE BUFFER bf-job-mat FOR job-mat.
    DEFINE BUFFER bf-item    FOR item.
    
    RUN sys/ref/nk1look.p (
        INPUT ipcCompany,         /* Company Code */ 
        INPUT "SSIssueDefaultRM", /* sys-ctrl name */
        INPUT "C",                /* Output return value */
        INPUT NO,                 /* Use ship-to */
        INPUT NO,                 /* ship-to vendor */
        INPUT "",                 /* ship-to vendor value */
        INPUT "",                 /* shi-id value */
        OUTPUT cSSIssueDefaultRM, 
        OUTPUT lRecFound
        ).
    
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
        IF (cSSIssueDefaultRM EQ "First Board" OR cSSIssueDefaultRM EQ "First Item") AND NOT lFirstSelected THEN 
        DO:
            FIND FIRST bf-item NO-LOCK
                WHERE bf-item.company EQ ipcCompany
                AND bf-item.i-no    EQ bf-job-mat.rm-i-no
                NO-ERROR.
            IF AVAILABLE bf-item AND bf-item.mat-type EQ "B" AND cSSIssueDefaultRM EQ "First Board" THEN
                ASSIGN
                    opcRMListItems = bf-job-mat.rm-i-no + "," + opcRMListItems
                    lFirstSelected = TRUE
                    .
            ELSE IF AVAILABLE bf-item AND bf-item.mat-type NE "B" AND cSSIssueDefaultRM EQ "First Item" THEN
                    ASSIGN
                        opcRMListItems = bf-job-mat.rm-i-no + "," + opcRMListItems
                        lFirstSelected = TRUE
                        .
        END.

        opcRMListItems = IF opcRMListItems EQ "" THEN 
            bf-job-mat.rm-i-no
            ELSE IF INDEX(opcRMListItems, bf-job-mat.rm-i-no) GT 0 THEN
            opcRMListItems
            ELSE 
            opcRMListItems + "," + bf-job-mat.rm-i-no.           
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
     Notes:   If any of the three elements (jobno2, formno, blankno) are present, and are 
              NOT an integer,  the parser fail reporting an error.
              Is completely uncaring as to the length of the job-no field.
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcJobno        AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcJobno        AS CHARACTER NO-UNDO.    
    DEFINE OUTPUT PARAMETER opcJobno2       AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcFormno       AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcBlankno      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplIsParsed     AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage      AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE iNumElements AS INT  NO-UNDO.
    DEFINE VARIABLE cInputTest   AS CHAR NO-UNDO.
    DEFINE VARIABLE iIntTest     AS INT  NO-UNDO.
    
    ASSIGN 
        cInputTest   = REPLACE(ipcJobNo,"-",".")
        iNumElements = NUM-ENTRIES(cInputTest,".")
        opcJobno     = ENTRY(1,cInputTest,".")
        opcJobNo2    = IF iNumElements GT 1 THEN ENTRY(2,cInputTest,".") ELSE ""
        opcFormNo    = IF iNumElements GT 2 THEN ENTRY(3,cInputTest,".") ELSE ""
        opcBlankNo   = IF iNumElements GT 3 THEN ENTRY(4,cInputTest,".") ELSE "".
        
    /* Is job-no2 an integer? */
    ASSIGN 
        iIntTest = IF opcJobNo2 NE "" THEN INTEGER(opcJobNo2) ELSE 0 NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
    DO:
        ASSIGN 
            opcMessage = "Invalid Job-no2 value".
        RETURN.
    END.
    /* Is form-no an integer? */
    ASSIGN 
        iIntTest = IF opcFormNo NE "" THEN INTEGER(opcFormNo) ELSE 0 NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
    DO:
        ASSIGN 
            opcMessage = "Invalid Form No value".
        RETURN.
    END.
    /* Is blank-no an integer? */
    ASSIGN 
        iIntTest = IF opcBlankNo NE "" THEN INTEGER(opcBlankNo) ELSE 0 NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
    DO:
        ASSIGN 
            opcMessage = "Invalid Blank No value".
        RETURN.
    END.
            
    ASSIGN 
        oplIsParsed = TRUE.

END PROCEDURE.

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
    DEFINE VARIABLE dCostMat           AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dCostLab           AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dCostVO            AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dCostFO            AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dPercentage        AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQtyInM            AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQtyInMComponent   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dPercentageApplied AS DECIMAL NO-UNDO.
    DEFINE BUFFER bf-component-job-hdr FOR job-hdr.
        
    FIND FIRST job-hdr NO-LOCK
        WHERE ROWID(job-hdr) EQ ipriJobHdr
        NO-ERROR.
    
    IF NOT AVAILABLE job-hdr THEN RETURN.
    ASSIGN 
        dQtyInM     = job-hdr.qty / 1000
        dPercentage = IF job-hdr.sq-in NE 0 THEN job-hdr.sq-in / 100 ELSE  1
        .
    FOR EACH job-mat NO-LOCK 
        WHERE job-mat.company EQ job-hdr.company
        AND job-mat.job EQ job-hdr.job
        AND job-mat.job-no EQ job-hdr.job-no
        AND job-mat.job-no2 EQ job-hdr.job-no2
        AND (job-mat.frm EQ job-hdr.frm OR job-hdr.frm EQ 0 OR (job-mat.frm EQ 0 AND job-hdr.frm EQ 1))
        :
        dCostMat = 0.
        IF job-hdr.blank-no EQ job-mat.blank-no 
            OR job-mat.blank-no EQ 0 
            OR job-hdr.frm EQ 0 THEN 
        DO: 
            IF job-hdr.frm EQ 0 THEN 
            DO:
                dQtyInMComponent = 0.
                FOR EACH bf-component-job-hdr NO-LOCK
                    WHERE bf-component-job-hdr.company EQ job-mat.company
                    AND bf-component-job-hdr.job EQ job-mat.job
                    AND bf-component-job-hdr.job-no EQ job-mat.job-no
                    AND bf-component-job-hdr.job-no2 EQ job-mat.job-no2
                    AND bf-component-job-hdr.frm EQ job-mat.frm
                    AND (bf-component-job-hdr.blank-no EQ job-mat.blank-no OR job-mat.blank-no EQ 0):
                    dQtyInMComponent = dQtyInMComponent + bf-component-job-hdr.qty / 1000.
                END.
                ASSIGN 
                    dCostMat   = job-mat.cost-m * (dQtyInMComponent / dQtyInM)
                    opdCostMat = opdCostMat + dCostMat
                    .
            END.
            ELSE 
            DO:
                IF job-mat.frm NE 0 OR (job-mat.frm EQ 0 AND job-hdr.frm EQ 1) THEN 
                    ASSIGN 
                        dCostMat   = job-mat.cost-m
                        opdCostMat = opdCostMat + dCostMat
                        .
            END.             
        END.
            
    END.
    
    FOR EACH job-mch NO-LOCK
        WHERE job-mch.company EQ job-hdr.company
        AND job-mch.job EQ job-hdr.job
        AND job-mch.job-no EQ job-hdr.job-no
        AND job-mch.job-no2 EQ job-hdr.job-no2
        AND (job-mch.frm EQ job-hdr.frm OR job-hdr.frm EQ 0)
        :
        IF job-hdr.blank-no EQ job-mch.blank-no OR job-mch.blank-no EQ 0 OR job-hdr.frm EQ 0 THEN 
        DO: 
            IF job-mch.blank-no EQ 0 THEN 
                dPercentageApplied = dPercentage.
            ELSE 
                dPercentageApplied = 1.
            ASSIGN 
                dCostLab   = ((job-mch.run-hr * job-mch.run-rate  + job-mch.mr-hr * job-mch.mr-rate) * dPercentageApplied ) / dQtyInM
                dCostVO    = ((job-mch.run-hr * job-mch.run-varoh + job-mch.mr-hr * job-mch.mr-varoh) * dPercentageApplied )  / dQtyInM
                dCostFO    = ((job-mch.run-hr * job-mch.run-fixoh + job-mch.mr-hr * job-mch.mr-fixoh) * dPercentageApplied )  / dQtyInM
                opdCostLab = opdCostLab + dCostLab
                opdCostVO  = opdCostVO + dCostVO
                opdCostFO  = opdCostFO + dCostFO
                .
        END.
        
    END.
    FOR EACH job-prep NO-LOCK
        WHERE job-prep.company EQ job-hdr.company
        AND job-prep.job EQ job-hdr.job
        AND job-prep.job-no EQ job-hdr.job-no
        AND job-prep.job-no2 EQ job-hdr.job-no2
        AND job-prep.frm EQ job-hdr.frm
        AND (job-prep.blank-no EQ job-hdr.blank-no OR job-prep.blank-no EQ 0):
        
        IF job-prep.ml THEN 
            opdCostMat = opdCostMat + job-prep.cost-m.
        ELSE 
            opdCostLab = opdCostLab + job-prep.cost-m.
    END.

END PROCEDURE.

PROCEDURE pGetMachRunComplete PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  get job machine is completed or not (job-mch.run-complete)
     Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiJobNo2 AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplLastOpAllRunComplete AS LOGICAL NO-UNDO.
    
    FOR EACH job-mch NO-LOCK
        WHERE job-mch.company EQ ipcCompany
        AND job-mch.job-no EQ ipcJobNo
        AND job-mch.job-no2 EQ ipiJobNo2
        BREAK BY job-mch.frm
        BY job-mch.line:
        
        IF LAST-OF(job-mch.frm) THEN 
        DO:
            IF job-mch.run-complete THEN
                oplLastOpAllRunComplete = YES.
            ELSE 
            DO:
                oplLastOpAllRunComplete = NO.
                LEAVE.
            END.    
        END.    
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
        FIND FIRST bf-job-hdr NO-LOCK 
            WHERE ROWID(bf-job-hdr) EQ ipriJob NO-ERROR.
        IF AVAILABLE bf-job-hdr THEN 
            RUN RecalcJobCostForJobHdr(ROWID(bf-job-hdr)).
    END.
    ELSE 
        FOR EACH bf-job-hdr NO-LOCK 
            WHERE bf-job-hdr.company EQ job.company
            AND bf-job-hdr.job EQ job.job
            AND bf-job-hdr.job-no EQ job.job-no
            AND bf-job-hdr.job-no2 EQ job.job-no2:
            RUN RecalcJobCostForJobHdr(ROWID(bf-job-hdr)).
        END. 
        
END PROCEDURE.

PROCEDURE RecalcJobCostForJobHdr:
    /*------------------------------------------------------------------------------
     Purpose:  Gets updated cost values for job-hdr and assigns them
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriJobHdr AS ROWID NO-UNDO.
    
    DEFINE BUFFER bf-job-hdr FOR job-hdr.
    
    DEFINE VARIABLE dCostMat AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dCostLab AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dCostVO  AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dCostFO  AS DECIMAL NO-UNDO.
    
    FIND FIRST bf-job-hdr EXCLUSIVE-LOCK 
        WHERE ROWID(bf-job-hdr) EQ ipriJobHdr NO-ERROR.
    IF AVAILABLE bf-job-hdr THEN 
    DO:
        RUN GetRecalcJobCostForJobHdr (ROWID(bf-job-hdr), OUTPUT dCostMat, OUTPUT dCostLab, OUTPUT dCostVO, OUTPUT dCostFO).
        ASSIGN 
            bf-job-hdr.std-mat-cost = dCostMat
            bf-job-hdr.std-lab-cost = dCostLab
            bf-job-hdr.std-var-cost = dCostVO
            bf-job-hdr.std-fix-cost = dCostFO
            bf-job-hdr.std-tot-cost = dCostMat + dCostLab + dCostVO + dCostFO
            .
    END.
    RELEASE bf-job-hdr.
    
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
                AND job-mch.frm      EQ ipiFormno).
        ELSE
            oplValidJob = CAN-FIND(FIRST job-mat NO-LOCK
                WHERE job-mat.company  EQ bf-job.company
                AND job-mat.job      EQ bf-job.job
                AND job-mat.job-no   EQ bf-job.job-no
                AND job-mat.job-no2  EQ bf-job.job-no2
                AND job-mat.frm      EQ ipiFormno).
        
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

PROCEDURE getLastActivity:
    /*------------------------------------------------------------------------------
     Purpose:  
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcJobno        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiJobno2       AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcLastActivity AS CHARACTER NO-UNDO.

    /*    FIND FIRST mch-act NO-LOCK                    */
    /*         WHERE mch-act.company EQ ipcCompany      */
    /*           AND mch-act.job-no  EQ ipcJobno        */
    /*           AND mch-act.job-no2 EQ ipiJobno2       */
    /*           NO-ERROR.                              */
    /*    IF AVAILABLE mch-act AND mch-act.complete THEN*/
    /*    ASSIGN                                        */
    /*         opcLastActivity  = "Posted".             */
    /*    ELSE IF AVAILABLE mch-act THEN                */
    /*         opcLastActivity = "Unposted".            */
    FOR EACH mch-act NO-LOCK 
        WHERE mch-act.company EQ ipcCompany
        AND mch-act.job-no  EQ ipcJobno
        AND mch-act.job-no2 EQ ipiJobno2
        BREAK BY mch-act.op-date
        BY mch-act.stopp
        :
        IF LAST(mch-act.stopp) THEN
            opcLastActivity = mch-act.m-code.
    END. /* each mch-act */
                                         
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

FUNCTION fGetJobPrefix RETURNS CHARACTER PRIVATE
	( ipcCompany AS CHARACTER):

/*------------------------------------------------------------------------------
 Purpose: Gets the Job Prefix for Stock Jobs created through 
 Notes:
------------------------------------------------------------------------------*/	
    DEFINE VARIABLE lFound AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cJobPrefix AS CHARACTER NO-UNDO.
    
    RUN sys/ref/nk1look.p (ipcCompany, "JOBCREAT", "C", NO, NO, "", "", OUTPUT cJobPrefix, OUTPUT lFound).
    IF cJobPrefix NE "" THEN cJobPrefix = SUBSTRING(cJobPrefix,1,1).
    
    RETURN TRIM(cJobPrefix).
    
END FUNCTION.

FUNCTION fGetJobSubAssemblyPrefix RETURNS CHARACTER PRIVATE
    ( ipcCompany AS CHARACTER):

/*------------------------------------------------------------------------------
 Purpose: Gets the Job Prefix for Stock Jobs created through 
 Notes:
------------------------------------------------------------------------------*/    
    DEFINE VARIABLE lFound AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cJobSubAssemblyPrefix AS CHARACTER NO-UNDO.
    
    RUN sys/ref/nk1look.p (ipcCompany, "JOBSubAssemblyPrefix", "C", NO, NO, "", "", OUTPUT cJobSubAssemblyPrefix, OUTPUT lFound).
    IF cJobSubAssemblyPrefix EQ "" THEN cJobSubAssemblyPrefix = "A".
    cJobSubAssemblyPrefix = SUBSTRING(cJobSubAssemblyPrefix,1,1).
    
    RETURN TRIM(cJobSubAssemblyPrefix).
    
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
