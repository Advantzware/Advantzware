
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
DEFINE TEMP-TABLE ttPartBalance NO-UNDO 
BEFORE-TABLE beforePartBalance
        FIELD Part-no AS character
        FIELD orderno  AS CHARACTER
        FIELD qtyship AS CHARACTER
        FIELD qtyr    AS CHARACTER
        FIELD qtyoh   AS CHARACTER 
        FIELD paloh   as character
        FIELD qtypal  AS CHARACTER
        FIELD qtyoo    AS CHARACTER
        FIELD qtya     AS CHARACTER
        FIELD pono     AS CHARACTER
        FIELD shipdated AS CHARACTER
        FIELD shipqtyd  AS CHARACTER
        FIELD palletsd  AS CHARACTER
        FIELD palletqtyd AS CHARACTER
        field descriptiond  as character
                                
        .
        

                                        
    
DEFINE DATASET dsPartBalance FOR ttPartBalance .

DEFINE QUERY q-PartBalanceQuery FOR ttPartBalance .

DEFINE DATA-SOURCE src-PartBalance  FOR QUERY q-PartBalanceQuery.

BUFFER ttPartBalance :ATTACH-DATA-SOURCE(DATA-SOURCE src-PartBalance  :HANDLE).
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

