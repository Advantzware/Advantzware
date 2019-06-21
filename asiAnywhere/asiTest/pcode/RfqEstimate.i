


/*------------------------------------------------------------------------
    File        : RfqItem.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Rfq Maintenance

    Author(s)   : Sewa Singh
    Created     : Sat Feb 16, 2008
    Notes       :
  ----------------------------------------------------------------------*/
 
/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttRfqEstimate NO-UNDO 
    FIELD RfqNo       AS INTEGER  FORMAT "->,>>>,>>9"
    FIELD RfqFormNo   AS INTEGER FORMAT ">>9"
    FIELD RfqBlank    AS INTEGER FORMAT ">>9"
    FIELD RfqSeq      AS INTEGER  FORMAT "->,>>>,>>9"
    FIELD RfqStock    AS CHARACTER FORMAT "x(15)"
    FIELD RfqName     AS CHARACTER  FORMAT "x(60)"
    FIELD RfqPart     AS CHARACTER  FORMAT "x(15)"
    FIELD RfqStyle    AS CHARACTER  FORMAT "x(4)"
    FIELD RfqProcat   AS CHARACTER  FORMAT "x(4)"
    FIELD RfqEst      AS CHARACTER  FORMAT "x(5)"
    
    FIELD RfqERowid AS RECID
    .
DEFINE DATASET dsRfqEstimate FOR ttRfqEstimate.
DEFINE QUERY q-RfqEstimateQuery FOR ttRfqEstimate.
DEFINE DATA-SOURCE src-RfqEstimate  FOR QUERY q-RfqEstimateQuery.
BUFFER ttRfqEstimate :ATTACH-DATA-SOURCE(DATA-SOURCE src-RfqEstimate  :HANDLE).


