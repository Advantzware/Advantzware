

/*------------------------------------------------------------------------
    File        History.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for item History Detail

    Author(s)   : Jyoti Bajaj
    Created     : Aug 27 2007
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttHistory NO-UNDO
BEFORE-TABLE beforeHistory
        FIELD ItemNum AS CHARACTER
        FIELD Po-no AS CHARACTER
        FIELD job-no AS CHARACTER
        FIELD job-no2 AS INTEGER
        FIELD TDate AS DATE
        FIELD RCode AS CHARACTER
        FIELD Cust AS CHARACTER
        FIELD Loc AS CHARACTER
        FIELD Loc-Bin AS CHARACTER
        FIELD Qty AS DECIMAL   
        FIELD Tag  as CHARACTER
        FIELD Cost as DECIMAL
        Field Qty-Case as DECIMAL
        FIELD Pallet as DECIMAL
        FIELD Unit  as DECIMAL
        FIELD QtyPlt as DECIMAL
        FIELD usr as Char
     FIELD q-onh AS Decimal   
        FIELD i-name AS CHAR
        FIELD q-avail AS Decimal  
        FIELD cRecKey as CHARACTER
                                    
   . 
DEFINE DATASET dsHistory FOR ttHistory .

DEFINE QUERY q-HistoryQuery FOR ttHistory.

DEFINE DATA-SOURCE src-History  FOR QUERY q-HistoryQuery.

BUFFER ttHistory :ATTACH-DATA-SOURCE(DATA-SOURCE src-History  :HANDLE).
/**********************************************************************************************/

