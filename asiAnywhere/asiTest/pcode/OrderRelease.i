
/*------------------------------------------------------------------------
    File        : QuoteDetail.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Quote Detail

    Author(s)   : Kuldeep
    Created     : Aug 27 2007
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttOrderRelease NO-UNDO 
BEFORE-TABLE beforeOrderRelease
        FIELD Part-no AS character
        FIELD ord-no AS CHARACTER
        FIELD line AS CHARACTER
        FIELD descriptiond AS CHARACTER
        
        
        FIELD Vdate AS CHARACTER 
      FIELD Vstatus as character

        FIELD qty    AS CHARACTER
        FIELD po-no AS CHARACTER
        FIELD ship-no AS CHARACTER
        FIELD addr AS CHARACTER
        FIELD city  AS CHARACTER
        FIELD state  AS CHARACTER
        .
        


                                        
    
DEFINE DATASET dsOrderRelease FOR ttOrderRelease .

DEFINE QUERY q-OrderReleaseQuery FOR ttOrderRelease .

DEFINE DATA-SOURCE src-OrderRelease  FOR QUERY q-OrderReleaseQuery.

BUFFER ttOrderRelease :ATTACH-DATA-SOURCE(DATA-SOURCE src-OrderRelease  :HANDLE).
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
FUNCTION GetCurrentCust RETURNS CHARACTER (prmCust AS CHARACTER ):
    IF prmCust > "" THEN DO:
        FIND FIRST cust NO-LOCK WHERE cust.cust-no = prmCust NO-ERROR.
        RETURN IF AVAIL cust THEN cust.cust-no ELSE ''.
    END.
    ELSE DO:
        RETURN "".
    END.
END FUNCTION.

