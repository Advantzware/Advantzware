
/*------------------------------------------------------------------------
    File        jobhdr.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Order jobhdr Detail

    Author(s)   : Jyoti Bajaj
    Created     : 
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttjobhdr NO-UNDO 
        FIELD Sheet AS Integer
        FIELD BlankNum AS Integer
        FIELD Customer AS char
        FIELD Item AS char
        FIELD Qty  AS Decimal
        FIELD Inch AS Decimal
        FIELD Order AS Integer
        FIELD Mat AS Decimal
        FIELD DL AS Decimal
        FIELD Var AS Decimal
        FIELD Fixed AS Decimal
        

  .
DEFINE DATASET dsjobhdr FOR ttjobhdr .

DEFINE QUERY q-jobhdrQuery FOR ttjobhdr.

DEFINE DATA-SOURCE src-jobhdr  FOR QUERY q-jobhdrQuery.

BUFFER ttjobhdr :ATTACH-DATA-SOURCE(DATA-SOURCE src-jobhdr  :HANDLE).
