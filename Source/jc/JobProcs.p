
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
PROCEDURE GetSecondaryJobForJob:
    /*------------------------------------------------------------------------------
     Purpose: Returns all available secondary job list for a given jobID
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcJobno        AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER opcJobno2List   AS CHARACTER NO-UNDO.

    DEFINE BUFFER buf-job-mch FOR job-mch.
    
    FOR EACH buf-job-mch NO-LOCK
        WHERE buf-job-mch.company EQ ipcCompany
          AND buf-job-mch.job-no  EQ ipcJobno
        BY buf-job-mch.line
        :
        opcJobno2List = IF opcJobno2List EQ "" THEN STRING(buf-job-mch.job-no2,"99")
                        ELSE IF INDEX(opcJobno2List,STRING(buf-job-mch.job-no2,"99")) GT 0 THEN opcJobno2List
                        ELSE opcJobno2List + "," + STRING(buf-job-mch.job-no2,"99").        
    END.

    RELEASE buf-job-mch.
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

    DEFINE BUFFER buf-job-mch FOR job-mch.
    
    FOR EACH buf-job-mch NO-LOCK
        WHERE buf-job-mch.company EQ ipcCompany
          AND buf-job-mch.job-no  EQ ipcJobno
          AND buf-job-mch.job-no2 EQ ipiJobno2
        BY buf-job-mch.line
        :        
        opcFormnoList = IF opcFormnoList EQ "" THEN STRING(buf-job-mch.frm,"99")
                        ELSE IF INDEX(opcFormnoList,STRING(buf-job-mch.frm,"99")) GT 0 THEN opcFormnoList
                        ELSE opcFormnoList + "," + STRING(buf-job-mch.frm,"99").
    END.

    RELEASE buf-job-mch.
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

    DEFINE BUFFER buf-job-mch FOR job-mch.
    
    FOR EACH buf-job-mch NO-LOCK
        WHERE buf-job-mch.company EQ ipcCompany
          AND buf-job-mch.job-no  EQ ipcJobno
          AND buf-job-mch.job-no2 EQ ipiJobno2
        BY buf-job-mch.line
        :        
        opcBlanknoList = IF opcBlanknoList EQ "" THEN STRING(buf-job-mch.blank-no,"99")
                         ELSE IF INDEX(opcBlanknoList,STRING(buf-job-mch.blank-no,"99")) GT 0 THEN opcBlanknoList
                         ELSE opcBlanknoList + "," + STRING(buf-job-mch.blank-no,"99").        
    END.

    RELEASE buf-job-mch.
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

    DEFINE BUFFER buf-job-mch FOR job-mch.
    
    FOR EACH buf-job-mch NO-LOCK
        WHERE buf-job-mch.company EQ ipcCompany
          AND buf-job-mch.job-no  EQ ipcJobno
          AND buf-job-mch.job-no2 EQ ipiJobno2
        BY buf-job-mch.line
        :
        opcMachineList = IF opcMachineList EQ "" THEN STRING(buf-job-mch.m-code)
                         ELSE IF INDEX(opcMachineList,STRING(buf-job-mch.m-code)) GT 0 THEN opcMachineList
                         ELSE opcMachineList + "," + STRING(buf-job-mch.m-code).           
    END.

    RELEASE buf-job-mch.
END PROCEDURE.

PROCEDURE GetRMItemsForJob:
    /*------------------------------------------------------------------------------
     Purpose: Returns machine code list for a given jobID
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcJobno        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiJobno2       AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiFormno       AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiBlankno      AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcRMListItems  AS CHARACTER NO-UNDO.

    DEFINE BUFFER buf-job-mat FOR job-mat.
    
    FOR EACH buf-job-mat NO-LOCK
        WHERE buf-job-mat.company  EQ ipcCompany
          AND buf-job-mat.job-no   EQ ipcJobno
          AND buf-job-mat.job-no2  EQ ipiJobno2
          AND buf-job-mat.frm      EQ ipiFormno
          AND buf-job-mat.blank-no EQ ipiBlankno
        BY buf-job-mat.line
        :
        opcRMListItems = IF opcRMListItems EQ "" THEN STRING(buf-job-mat.i-no)
                         ELSE IF INDEX(opcRMListItems,STRING(buf-job-mat.i-no)) GT 0 THEN opcRMListItems
                         ELSE opcRMListItems + "," + STRING(buf-job-mat.i-no).           
    END.

    RELEASE buf-job-mat.
END PROCEDURE.

PROCEDURE GetOperationForPO:
    /*------------------------------------------------------------------------------
     Purpose: Returns machine code for a purchase order line
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcJobno        AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipiJobno2       AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipiOrdno        AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipcItemno       AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipiType         AS CHARACTER NO-UNDO. /* "First" - sends the first record */
    DEFINE INPUT-OUTPUT PARAMETER opcMachineList  AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER buf-job FOR job.
    DEFINE BUFFER buf-job-hdr FOR job-hdr.
    DEFINE BUFFER buf-job-mch FOR job-mch.
                
    FIND FIRST job-hdr NO-LOCK
         WHERE job-hdr.company EQ ipcCompany
           AND job-hdr.job-no  EQ ipcJobno
           AND job-hdr.job-no2 EQ ipiJobno2
           AND job-hdr.ord-no  EQ ipiOrdno
           AND job-hdr.i-no    EQ ipcItemno
         NO-ERROR.
    IF NOT AVAILABLE buf-job-hdr THEN
       FIND FIRST buf-job-hdr NO-LOCK
            WHERE buf-job-hdr.company EQ ipcCompany
              AND buf-job-hdr.job-no  EQ ipcJobno
              AND buf-job-hdr.job-no2 EQ ipiJobno2
              AND buf-job-hdr.ord-no  EQ ipiOrdno
            NO-ERROR.

    IF AVAILABLE buf-job-hdr THEN
        FIND FIRST buf-job NO-LOCK
             WHERE buf-job.company EQ buf-job-hdr.company
               AND buf-job.job     EQ buf-job-hdr.job
               AND buf-job.job-no  EQ buf-job-hdr.job-no
               AND buf-job.job-no2 EQ buf-job-hdr.job-no2
             NO-ERROR.

    IF AVAILABLE buf-job THEN DO:
        FOR EACH buf-job-mch NO-LOCK
            WHERE buf-job-mch.company EQ buf-job.company
              AND buf-job-mch.job     EQ buf-job.job
              AND buf-job-mch.job-no  EQ buf-job.job-no
              AND buf-job-mch.job-no2 EQ buf-job.job-no2
              AND buf-job-mch.frm     EQ buf-job-hdr.frm
            USE-INDEX line-idx:        
            IF ipiType EQ "First" AND opcMachineList EQ "" THEN do:
                opcMachineList = buf-job-mch.m-code.
                LEAVE .
            END.
            
            opcMachineList = opcMachineList +  "," + buf-job-mch.m-code.
        END.
    END.
    
    opcMachineList = TRIM(opcMachineList,",").
    
    RELEASE buf-job-mch.
    RELEASE buf-job.
    RELEASE buf-job-hdr.
END PROCEDURE.

PROCEDURE GetOperation:
    /*------------------------------------------------------------------------------
     Purpose: Returns machine code list for a given jobID
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcJobno        AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipiJobno2       AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipifrm          AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipiType         AS CHARACTER   NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER opcMachineList  AS CHARACTER NO-UNDO.

    DEFINE BUFFER buf-job-mch FOR job-mch.
    Main-Loop-Mach:
    FOR EACH buf-job-mch NO-LOCK
        WHERE buf-job-mch.company EQ ipcCompany
          AND buf-job-mch.job-no  EQ ipcJobno
          AND buf-job-mch.job-no2 EQ ipiJobno2
          AND buf-job-mch.frm EQ ipifrm use-index line-idx  :
        
        IF ipiType EQ "First" AND opcMachineList EQ "" THEN do:
            opcMachineList = buf-job-mch.m-code .
            LEAVE .
        END.

        FIND FIRST mach NO-LOCK
                WHERE mach.company EQ ipcCompany
                AND mach.m-code EQ buf-job-mch.m-code NO-ERROR  .

          IF AVAIL mach AND mach.dept[1] EQ "PR" AND ipiType EQ "Press" AND opcMachineList EQ "" THEN do:
              ASSIGN opcMachineList = buf-job-mch.m-code .
              LEAVE .
          END.

          IF AVAIL mach AND mach.dept[1] BEGINS "F" AND ipiType EQ "Internal" THEN NEXT Main-Loop-Mach .

          IF opcMachineList EQ "" AND ipiType EQ "Internal" THEN do:
              ASSIGN opcMachineList = buf-job-mch.m-code .
              LEAVE .
          END.
    END.

    RELEASE buf-job-mch.
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
        
    IF INDEX(ipcJobno,"-") NE 0 THEN DO:        
        iValid = (IF NUM-ENTRIES(ipcJobno,"-") EQ 2 AND
                     NUM-ENTRIES(ENTRY(2,ipcJobno,"-"),".") GE 1 THEN
                      INTEGER(ENTRY(1,ENTRY(2,ipcJobno,"-"),"."))
                  ELSE IF NUM-ENTRIES(ipcJobno,"-") GE 2 THEN
                      INTEGER(ENTRY(2,ipcJobno,"-"))
                  ELSE 
                      0) NO-ERROR.
                 
        IF ERROR-STATUS:ERROR THEN DO:
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
                  
        IF ERROR-STATUS:ERROR THEN DO:
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
                  
        IF ERROR-STATUS:ERROR THEN DO:
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
                             "00"
            opcFormno  = IF NUM-ENTRIES(ipcJobno,"-") EQ 2 AND
                            NUM-ENTRIES(ENTRY(2,ipcJobno,"-"),".") GE 2 THEN
                             ENTRY(2,ENTRY(2,ipcJobno,"-"),".")
                         ELSE IF NUM-ENTRIES(ipcJobno,"-") GE 3 THEN
                             ENTRY(3,ipcJobno,"-")
                         ELSE
                             "00"
            opcBlankno = IF NUM-ENTRIES(ipcJobno,"-") EQ 2 AND
                            NUM-ENTRIES(ENTRY(2,ipcJobno,"-"),".") GE 3 THEN
                             ENTRY(3,ENTRY(2,ipcJobno,"-"),".")
                         ELSE IF NUM-ENTRIES(ipcJobno,"-") GE 4 THEN
                             ENTRY(4,ipcJobno,"-")
                         ELSE
                             "00"
            NO-ERROR.
            
        IF ERROR-STATUS:ERROR THEN DO:
            opcMessage = ERROR-STATUS:GET-MESSAGE(1).
            RETURN.
        END.            
        
        oplIsParsed = TRUE.
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
    
    oplValidJob = CAN-FIND(FIRST job-mch NO-LOCK
            WHERE job-mch.company  EQ ipcCompany
              AND job-mch.job-no   EQ ipcJobno
              AND (IF ipcMachine   EQ "" THEN TRUE 
              ELSE job-mch.m-code  EQ ipcMachine)
              AND job-mch.job-no2  EQ ipiJobno2
              AND job-mch.frm      EQ ipiFormno
              AND job-mch.blank-no EQ ipiBlankno).
          
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
    
    IF LENGTH(ipcString) LT ipiFormatLength THEN DO:
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
    DEFINE INPUT        PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipEst        AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMachineList  AS CHARACTER NO-UNDO.

    DEFINE BUFFER buf-est-op FOR est-op.
    Main-Loop-Mach:
    FOR EACH buf-est-op NO-LOCK
            WHERE buf-est-op.company = ipcCompany 
            AND buf-est-op.est-no = ipEst 
            AND buf-est-op.line < 500  BREAK BY buf-est-op.est-no :
            IF NOT LAST( buf-est-op.est-no) THEN
                opcMachineList = opcMachineList + buf-est-op.m-code + "," .
            ELSE opcMachineList = opcMachineList + buf-est-op.m-code .
    END.
    RELEASE buf-est-op.
END PROCEDURE.
