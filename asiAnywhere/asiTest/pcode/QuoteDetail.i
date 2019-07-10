
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
DEFINE TEMP-TABLE ttQuote NO-UNDO 
BEFORE-TABLE beforeQuote
        FIELD q-no AS integer
        FIELD billto AS CHARACTER
        FIELD shipto        AS CHARACTER
        FIELD sname  AS CHARACTER
        FIELD quo-date AS DATE
        FIELD estDate  as date
        FIELD Part-no AS CHARACTER
        FIELD qty    AS CHARACTER
        FIELD uom  AS CHARACTER
        FIELD price   AS CHARACTER
        FIELD part_no AS CHARACTER
        FIELD style  AS CHARACTER
        FIELD colord  AS CHARACTER
        FIELD size  AS CHARACTER
        FIELD material  AS CHARACTER
        FIELD Orderno  AS CHARACTER
        .


                                        
    
DEFINE DATASET dsQuoteDetail FOR ttQuote.

DEFINE QUERY q-quotehdQuery FOR ttQuote.

DEFINE DATA-SOURCE src-quotehd  FOR QUERY q-quotehdQuery.

BUFFER ttQuote:ATTACH-DATA-SOURCE(DATA-SOURCE src-quotehd :HANDLE).
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

