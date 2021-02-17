/*------------------------------------------------------------------------
    File        : api\inbound\DeleteBillOfLading.p
    Purpose     : Processes request data

    Syntax      :

    Description : Processes request data

    Author(s)   : Vishnu Vellanki
    Created     : Thu Apr 09 07:33:22 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipiBOLID    AS INTEGER   NO-UNDO.
DEFINE OUTPUT PARAMETER oplSuccess  AS LOGICAL   NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage  AS CHARACTER NO-UNDO.

DEFINE VARIABLE hdOrderProcs AS HANDLE NO-UNDO.

/* This will eventually move to setsession approach */
{methods/defines/globdefs.i}  
{sys/inc/var.i "NEW SHARED"}

g_company=ipcCompany.

{sys/inc/varasgn.i}

oplSuccess = YES.

/* Validates company */
IF NOT CAN-FIND(FIRST company NO-LOCK
                WHERE company.company EQ ipcCompany) THEN DO:
    ASSIGN 
        opcMessage = "Invalid Company"
        oplSuccess = NO
        .
    
    RETURN.
END.

/* Validates BOLID */
FIND FIRST oe-bolh NO-LOCK
     WHERE oe-bolh.company EQ ipcCompany
       AND oe-bolh.bol-no  EQ ipiBOLID
     NO-ERROR.
IF NOT AVAILABLE oe-bolh THEN DO:
    ASSIGN 
        opcMessage = "Invalid BOLID"
        oplSuccess = NO
        .
    
    RETURN.
END.

/* Validats BOLID posted or not */
IF oe-bolh.posted EQ TRUE THEN DO:
    ASSIGN
        opcMessage = "BOL has been posted, delete not allowed"
        oplSuccess = NO
        .

    RETURN.
END.

DO TRANSACTION ON ERROR UNDO,LEAVE:
    RUN oe/OrderProcs.p PERSISTENT SET hdOrderProcs.

    /* Deletes a BOL and unposts related release */
    RUN Order_DeleteBOL IN hdOrderProcs (
        INPUT  ipcCompany,
        INPUT  ipiBOLID,
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ) NO-ERROR.
    IF ERROR-STATUS:ERROR OR NOT oplSuccess THEN DO:
        opcMessage = IF ERROR-STATUS:ERROR THEN 
                         ERROR-STATUS:GET-MESSAGE(1)
                     ELSE
                         opcMessage.
        UNDO, LEAVE.
    END.
    
    FINALLY:
        IF VALID-HANDLE(hdOrderProcs) THEN
            DELETE PROCEDURE hdOrderProcs.        
    END.
END.      
