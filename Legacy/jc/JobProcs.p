
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
        iValid = (IF NUM-ENTRIES(ENTRY(2,ipcJobno,"-"),".") GE 1 THEN
                  INTEGER(ENTRY(1,ENTRY(2,ipcJobno,"-"),"."))
                  ELSE 0) NO-ERROR.
                 
        IF ERROR-STATUS:ERROR THEN DO:
            opcMessage = "Invalid Jobno2 value".
            RETURN.
        END.
        
        iValid = (IF NUM-ENTRIES(ENTRY(2,ipcJobno,"-"),".") GE 2 THEN
                  INTEGER(ENTRY(2,ENTRY(2,ipcJobno,"-"),"."))
                  ELSE 0) NO-ERROR.
                  
        IF ERROR-STATUS:ERROR THEN DO:
            opcMessage = "Invalid Formno value".
            RETURN.
        END.

        iValid = (IF NUM-ENTRIES(ENTRY(2,ipcJobno,"-"),".") GE 3 THEN
                  INTEGER(ENTRY(3,ENTRY(2,ipcJobno,"-"),"."))
                  ELSE 0) NO-ERROR.
                  
        IF ERROR-STATUS:ERROR THEN DO:
            opcMessage = "Invalid Blankno value".
            RETURN.
        END.

        ASSIGN
            opcJobNo   = ENTRY(1,ipcJobno,"-")        
            opcJobNo2  = IF NUM-ENTRIES(ENTRY(2,ipcJobno,"-"),".") GE 1 THEN
                            ENTRY(1,ENTRY(2,ipcJobno,"-"),".") 
                         ELSE
                            "00"
            opcFormno  = IF NUM-ENTRIES(ENTRY(2,ipcJobno,"-"),".") GE 2 THEN
                            ENTRY(2,ENTRY(2,ipcJobno,"-"),".") 
                         ELSE
                            "00"
            opcBlankno = IF NUM-ENTRIES(ENTRY(2,ipcJobno,"-"),".") GE 3 THEN
                            ENTRY(3,ENTRY(2,ipcJobno,"-"),".") 
                         ELSE
                            "00"
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
