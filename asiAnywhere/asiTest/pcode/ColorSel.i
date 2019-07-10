

/*------------------------------------------------------------------------
    File        : Colors.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Item Color Detail

    Author(s)   : Jyoti Bajaj
    Created     : Aug 27 2007
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttColSel NO-UNDO
BEFORE-TABLE beforeColSel
        FIELD press AS char
        FIELD rm-no AS char
        FIELD Dscr AS char
        FIELD pass AS integer
        FIELD in-out AS CHAR
        FIELD cover AS integer
        FIELD Occurs AS INTEGER FORMAT ">>":U INITIAL 0 
        
        
   . 
DEFINE DATASET dsColSel FOR ttColSel .

DEFINE QUERY q-ColSelQuery FOR ttColSel.

DEFINE DATA-SOURCE src-ColSel  FOR QUERY q-ColSelQuery.

BUFFER ttColSel :ATTACH-DATA-SOURCE(DATA-SOURCE src-ColSel  :HANDLE).

