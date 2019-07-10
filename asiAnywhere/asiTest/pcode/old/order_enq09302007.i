
/*------------------------------------------------------------------------
    File        : order_enq.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Order Enquiry Maintenance

    Author(s)   : Sewa Singh
    Created     : Sat August 25 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttOrder NO-UNDO 
    FIELD due-date LIKE oe-ord.due-date
    FIELD ord-date LIKE oe-ord.ord-date
     FIELD cust-name LIKE oe-ord.cust-name

    FIELD onhandqty       AS INTEGER
    FIELD blue            AS LOGICAL INIT FALSE
    FIELD showadd         AS LOGICAL INIT FALSE
    FIELD releasedd       AS CHARACTER
    .
  DEFINE TEMP-TABLE ttOrder2 NO-UNDO LIKE oe-ordl
BEFORE-TABLE beforeOrder2
    FIELD Prodqty AS INTEGER.
    .
      
DEFINE DATASET dsOrder FOR ttOrder, ttOrder2.

DEFINE VARIABLE q-OrderQuery AS HANDLE.
DEFINE VARIABLE src-Order AS HANDLE.


CREATE QUERY q-OrderQuery.
q-OrderQuery:SET-BUFFERS(BUFFER oe-ordl:HANDLE,BUFFER oe-ord:Handle).

CREATE DATA-SOURCE src-Order.
src-Order:QUERY = q-OrderQuery.

BUFFER ttOrder:ATTACH-DATA-SOURCE(src-Order).

/*
DEFINE VARIABLE v-Order AS HANDLE     NO-UNDO. 
v-Order = DATASET dsOrder:HANDLE.   
v-Order:GET-BUFFER-HANDLE("ttOrder"):SET-CALLBACK-PROCEDURE("AFTER-ROW-FILL", "set-image", THIS-PROCEDURE). 
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

/* ***************************  Procedures  *************************** */
PROCEDURE set-image:
    DEFINE INPUT PARAMETER DATASET FOR dsOrder.
    
    ASSIGN ttOrder.Order_image = "images/cat" + STRING(ttOrder.po-no, "99999") + ".jpg".
END PROCEDURE. 
*/

/*
FUNCTION CustomLookup RETURNS CHARACTER (cTagName AS CHARACTER ) :
  FIND FIRST afCustom NO-LOCK WHERE tagName = cTagName NO-ERROR.
  IF AVAILABLE afCustom THEN DO:
      RETURN customValue.
  END.
  ELSE DO:
      RETURN "".
  END. 
END FUNCTION.
*/

FUNCTION CalcQuoteDate RETURNS DATE:
    DEFINE VAR OutQuoteDate AS DATE.
    /*
    IF  CustomLookup("QuoteOutOfDate") > "" THEN DO:
        ASSIGN OutQuoteDate = (TODAY - (INTEGER(CustomLookup("QuoteOutOfDate")))).
    END.
    ELSE DO:
        ASSIGN outQuoteDate = (TODAY - 365).
    END.
    IF  CustomLookup("QuoteOutOfDateDate") > "" AND Date(CustomLookup("QuoteOutOfDateDate")) > OutQuoteDate THEN DO:
        ASSIGN OutQuoteDate = Date(CustomLookup("QuoteOutOfDateDate")).
    END.
    */
    ASSIGN outQuoteDate = (TODAY - 365).
    RETURN OutQuoteDate.   /* Function return value. */
END FUNCTION.


FUNCTION GetCurrentUser RETURNS CHARACTER (wuser AS CHARACTER ):
    /*
    IF wuser > "" THEN DO:
        FIND FIRST webuser NO-LOCK WHERE ROWID(webuser) = TO-ROWID(wuser) NO-ERROR.
        RETURN IF AVAILABLE webuser THEN STRING(ROWID(webuser)) ELSE ''.
    END.
    ELSE DO:
        RETURN "".
    END.   
    */
    return "".       
END FUNCTION.

FUNCTION GetCurrentCust RETURNS CHARACTER (prmCust AS CHARACTER ):
    IF prmCust > "" THEN DO:
        FIND FIRST cust NO-LOCK WHERE cust.cust-no = prmCust NO-ERROR.
        RETURN IF AVAIL cust THEN cust.cust-no ELSE ''.
    END.
    ELSE DO:
        RETURN "".
    END.
END FUNCTION.

FUNCTION CanAccess RETURNS LOGICAL ( INPUT prmUser AS CHARACTER, INPUT ModNum AS CHARACTER ) :
    /*
    GetCurrentUser(prmUser).
    IF AVAIL webuser AND INDEX(webuser.modaccess, STRING(modnum)) > 0 THEN DO: 
        RETURN TRUE.
    END.
    ELSE DO:
        RETURN FALSE.   /* Function return value. */
    END.
    */
    RETURN TRUE.
END FUNCTION.



