
/*------------------------------------------------------------------------
    File        Hist.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Order History Detail

    Author(s)   : Jyoti Bajaj
    Created     : Aug 27 2007
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttHist NO-UNDO 
        FIELD Item AS char
        FIELD Date AS Date
        FIELD Name AS char
        FIELD Dscr AS char
        FIELD Cost AS Decimal
        FIELD Sell AS Decimal
        FIELD Uom AS char
        FIELD qty AS Decimal
   .
        
                                    
    
DEFINE DATASET dsHist FOR ttHist .

DEFINE QUERY q-HistQuery FOR ttHist.

DEFINE DATA-SOURCE src-Hist  FOR QUERY q-HistQuery.

BUFFER ttHist :ATTACH-DATA-SOURCE(DATA-SOURCE src-Hist  :HANDLE).
