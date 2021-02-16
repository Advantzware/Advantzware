
/*------------------------------------------------------------------------
    File        : Browsers/BrowserProcs.p
    Purpose     : 

    Syntax      :

    Description : Procedures and functions related to browsers

    Author(s)   : Rahul Rawat
    Created     : Wed Jan 13 07:49:02 EST 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* **********************  Internal Procedures  *********************** */

PROCEDURE Browse_PrepareAndExecuteBrowseQuery:
/*------------------------------------------------------------------------------
 Purpose: Public procedure which will prepare and execute the query for Browse 
          at run time and alert the user if they reach a certain limit 
          (Record or Time),if iRecordLimit is YES      
 Notes:   
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iphdBrowseQuery    AS HANDLE    NO-UNDO.
    DEFINE INPUT  PARAMETER ipcQueryString     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplCheckLimit      AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiRecordLimit     AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdQueryTimeLimit  AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER iplEnableShowAll   AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcReturnValue     AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE hdQuery       AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hdBuffer      AS HANDLE    NO-UNDO.
    DEFINE VARIABLE iCount        AS INTEGER   NO-UNDO.   
    DEFINE VARIABLE hdTableBuffer AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cMessage      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iTimeTaken    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iTotalCount   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iIndex        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cTitle        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cResponse     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lResponse     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE iStartTime    AS INTEGER   NO-UNDO.
     
    hdQuery = iphdBrowseQuery.
        
    hdQuery:QUERY-PREPARE(ipcQueryString). 
    hdQuery:QUERY-OPEN().
    
    /* Convert seconds in milliseconds */
    ipdQueryTimeLimit = ipdQueryTimeLimit * 1000.  
    
    /* Perform the limit check */
    IF iplCheckLimit THEN DO:
        hdQuery:GET-NEXT().
        
        MainLoop:    
        REPEAT:   
            ASSIGN  
                iCount     = 0
                iStartTime = ETIME
                .
            DO WHILE (ETIME - iStartTime) LT ipdQueryTimeLimit AND NOT hdQuery:QUERY-OFF-END:                      
                ASSIGN 
                    iTotalCount = iTotalCount + 1
                    iCount      = iCount      + 1
                    .
                IF iCount GE ipiRecordLimit THEN DO:                   
                    iTimeTaken = iTimeTaken + (ETIME - iStartTime).
                    LEAVE.
                END.     
             
                hdQuery:GET-NEXT().                          
            END.
    
            /* If Query has ended, leave the mainloop */
            IF hdQuery:QUERY-OFF-END THEN 
                LEAVE MainLoop. 
                
            ELSE DO:
            IF iCount GE ipiRecordLimit THEN 
                ASSIGN 
                    cMessage = "You have reached a limit of " + STRING(iTotalCount) + " records in " + STRING(iTimeTaken / 1000) + " seconds" + ". Continue searching?"
                    cTitle   = "Record Limit Reached"
                    .
            ELSE 
                ASSIGN 
                    iIndex   = iIndex + 1
                    cMessage = "You have reached a limit of " + STRING((ipdQueryTimeLimit * iIndex) / 1000) + " seconds, Records searched= " + STRING(iTotalCount) + ". Continue searching?"        
                    cTitle   = "Time Limit Reached"
                    .
                RUN system/d-QueryLimitAlert.w(
                    INPUT  cMessage,
                    INPUT  cTitle,
                    INPUT  iplEnableShowAll,
                    OUTPUT cResponse
                    ) NO-ERROR.
                
                IF cResponse EQ "ShowAll" THEN DO: 
                    opcReturnValue = "ShowAll".
                    LEAVE MainLoop.
                END.        
                lResponse = LOGICAL(cResponse) NO-ERROR.  
                  
                /* Continue searching if user selected YES */    
                IF lResponse THEN 
                    NEXT MainLoop. 
                ELSE 
                    LEAVE MainLoop. /* Break the query if user doesnot want to proceed */ 
            END. /* End of Else IF */         
        END.
    END. 
        
END PROCEDURE.

PROCEDURE Browse_PrepareAndExecuteLimitingQuery:
/*------------------------------------------------------------------------------
 Purpose: Public procedure which will prepare and execute the query at run time
          and alert the user if they reach a certain limit (Record or Time)
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcQueryString     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcBufferString    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiRecordLimit     AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdTimeLimit       AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER iplEnableShowAll   AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTableName       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFieldName       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplInitialQuery    AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER iplIsBreakByUsed   AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcReturnValue     AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE hdQuery       AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hdBuffer      AS HANDLE    NO-UNDO.
    DEFINE VARIABLE iCount        AS INTEGER   NO-UNDO.   
    DEFINE VARIABLE hdTableBuffer AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cMessage      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iTimeTaken    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iTotalCount   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iIndex        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cTitle        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cResponse     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lResponse     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE iStartTime    AS INTEGER   NO-UNDO.
    
    /* Convert seconds in milliseconds */
    ipdTimeLimit = ipdTimeLimit * 1000.
    
    /* If is a limiting query then create query and set the buffers */    
    CREATE QUERY hdQuery.       
    DO iCount = 1 TO NUM-ENTRIES(ipcBufferString):
        CREATE BUFFER hdBuffer FOR TABLE ENTRY(iCount,ipcBufferString).
        hdQuery:ADD-BUFFER(hdBuffer).
        IF hdBuffer:NAME EQ ipcTableName THEN
            hdTableBuffer = hdBuffer.                                    
    END.  
    hdQuery:QUERY-PREPARE(ipcQueryString). 
    hdQuery:QUERY-OPEN().
    hdQuery:GET-NEXT().
    
    MainLoop:    
    REPEAT:
        ASSIGN 
            iCount     = 0
            iStartTime = ETIME
            .
            
        IF iplInitialQuery THEN DO WHILE NOT hdQuery:QUERY-OFF-END:
            IF NOT iplIsBreakByUsed OR hdQuery:FIRST-OF(1) THEN DO:
                opcReturnValue = STRING(hdBuffer:BUFFER-FIELD(ipcFieldName):BUFFER-VALUE).
                iCount = iCount + 1.
                IF iCount GE 30 THEN                   
                    LEAVE. 
                hdQuery:GET-NEXT().   
            END.                         
        END.
        ELSE DO:
            DO WHILE (ETIME - iStartTime) LT ipdTimeLimit AND NOT hdQuery:QUERY-OFF-END:
                IF NOT iplIsBreakByUsed OR hdQuery:FIRST-OF(1) THEN DO:
                    opcReturnValue = STRING(hdBuffer:BUFFER-FIELD(ipcFieldName):BUFFER-VALUE).
                    ASSIGN 
                        iTotalCount = iTotalCount + 1
                        iCount      = iCount      + 1
                        .
                    IF iCount GE ipiRecordLimit THEN DO:                   
                        iTimeTaken = iTimeTaken + (ETIME - iStartTime).
                        LEAVE.
                    END.  
                END.
                hdQuery:GET-NEXT().                          
            END.
        END.
        /* If Query has ended or if its initialQuery then leave the mainloop */
        IF hdQuery:QUERY-OFF-END OR iplInitialQuery THEN 
            LEAVE MainLoop.
             
        ELSE IF NOT iplInitialQuery THEN DO:
            IF iCount GE ipiRecordLimit THEN 
                ASSIGN 
                    cMessage = "You have reached a limit of " + STRING(iTotalCount) + " records in " + STRING(iTimeTaken / 1000) + " seconds" + ". Continue searching?"
                    cTitle   = "Record Limit Reached"
                    .
            ELSE 
                ASSIGN 
                    iIndex   = iIndex + 1
                    cMessage = "You have reached a limit of " + STRING((ipdTimeLimit * iIndex) / 1000) + " seconds, Records searched= " + STRING(iTotalCount) + ". Continue searching?"        
                    cTitle   = "Time Limit Reached"
                    .
                    
            RUN system/d-QueryLimitAlert.w(
                INPUT  cMessage,
                INPUT  cTitle,
                INPUT  iplEnableShowAll,
                OUTPUT cResponse
                ) NO-ERROR.
            
            IF cResponse EQ "ShowAll" THEN DO: 
                opcReturnValue = "ShowAll".
                LEAVE MainLoop.
            END.        
            lResponse = LOGICAL(cResponse) NO-ERROR.  
              
            /* Continue searching if user selected YES */  
            IF lResponse THEN 
                NEXT MainLoop. 
            ELSE 
                LEAVE MainLoop. /* Break the query if user doesnot want to proceed */ 
        END. /* End of Else IF */         
    END.  
    
    /*Delete Query Handle*/
    IF VALID-HANDLE(hdQuery) THEN DO: 
        IF hdQuery:IS-OPEN THEN
            hdQuery:QUERY-CLOSE().
        DELETE OBJECT hdQuery.
    END.
END PROCEDURE.

PROCEDURE Browser_GetRecordAndTimeLimit:
/*------------------------------------------------------------------------------
 Purpose: Public procedure which returns 'SearchLimits' NK1 integer,decimal and 
          logical value
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcHotKey        AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiRecordLimit   AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdTimeLimit     AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplEnableShowAll AS LOGICAL   NO-UNDO.
    
    DEFINE VARIABLE cRtnValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lRecFound AS LOGICAL   NO-UNDO.
    
    RUN sys/ref/nk1look.p(
        INPUT  ipcCompany,
        INPUT  "SearchLimits",
        INPUT  "I",
        INPUT  NO,
        INPUT  NO,
        INPUT  "",
        INPUT  "",
        OUTPUT cRtnValue,
        OUTPUT lRecFound    
        ). 
    opiRecordLimit = INTEGER(cRtnValue).
    
    RUN sys/ref/nk1look.p(
        INPUT  ipcCompany,
        INPUT  "SearchLimits",
        INPUT  "D",
        INPUT  NO,
        INPUT  NO,
        INPUT  "",
        INPUT  "",
        OUTPUT cRtnValue,
        OUTPUT lRecFound    
        ). 
    opdTimeLimit = DECIMAL(cRtnValue).
    
        RUN sys/ref/nk1look.p(
        INPUT  ipcCompany,
        INPUT  "SearchLimits",
        INPUT  "L",
        INPUT  NO,
        INPUT  NO,
        INPUT  "",
        INPUT  "",
        OUTPUT cRtnValue,
        OUTPUT lRecFound    
        ). 
    oplEnableShowAll = LOGICAL(cRtnValue) NO-ERROR.
    
    FIND FIRST sys-ctrl-shipto NO-LOCK 
         WHERE sys-ctrl-shipto.company  EQ ipcCompany
           AND sys-ctrl-shipto.name     EQ "SearchLimits"
           AND sys-ctrl-shipto.char-fld EQ ipcHotKey
        NO-ERROR.
    IF AVAILABLE sys-ctrl-shipto THEN 
        ASSIGN
            opiRecordLimit   = sys-ctrl-shipto.int-fld
            opdTimeLimit     = sys-ctrl-shipto.dec-fld
            oplEnableShowAll = sys-ctrl-shipto.log-fld
            .
                         
END PROCEDURE.

