
/*------------------------------------------------------------------------
    File        : stylei
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Category

    Author(s)   : Kuldeep
    Created     : Aug 27 2007
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttordl NO-UNDO LIKE oe-ord.

DEFINE DATASET dsordl FOR ttordl.

DEFINE QUERY q-ordlQuery FOR oe-ord.

DEFINE DATA-SOURCE src-ordl FOR QUERY q-ordlQuery.

BUFFER ttordl:ATTACH-DATA-SOURCE(DATA-SOURCE src-ordl:HANDLE).

