
/*------------------------------------------------------------------------
    File        : ItemLook.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for ItemLookup

    Author(s)   : Jyoti Bajaj
    Created     : Oct 02 2007
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttItemLook NO-UNDO 
          FIELD Name AS CHARACTER 
          FIELD Item AS CHARACTER
          FIELD Dscr AS CHARACTER
          FIELD cust-no AS CHAR
          .

DEFINE DATASET dsItemLook FOR ttItemLook .
DEFINE VARIABLE q-ItemLookQuery AS HANDLE.
DEFINE VARIABLE src-ItemLook AS HANDLE.

DEFINE QUERY q-ItemLookQuery FOR ttItemLook .

DEFINE DATA-SOURCE src-ItemLook  FOR QUERY q-ItemLookQuery.


BUFFER ttItemLook :ATTACH-DATA-SOURCE(DATA-SOURCE src-ItemLook  :HANDLE).

