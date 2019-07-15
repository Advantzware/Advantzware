

/*------------------------------------------------------------------------
    File        : ItemPoLook.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for ItemPoLookup

    Author(s)   : Jyoti Bajaj
    Created     : Jan 23 2008
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttItemPoLook NO-UNDO 
          FIELD ItemName AS CHARACTER 
          FIELD ItemPo AS CHARACTER
          FIELD Po-no AS INT
          FIELD cust-num AS CHAR
          .

DEFINE DATASET dsItemPoLook FOR ttItemPoLook .
DEFINE VARIABLE q-ItemPoLookQuery AS HANDLE.
DEFINE VARIABLE src-ItemPoLook AS HANDLE.

DEFINE QUERY q-ItemPoLookQuery FOR ttItemPoLook .

DEFINE DATA-SOURCE src-ItemPoLook  FOR QUERY q-ItemPoLookQuery.


BUFFER ttItemPoLook :ATTACH-DATA-SOURCE(DATA-SOURCE src-ItemPoLook  :HANDLE).


