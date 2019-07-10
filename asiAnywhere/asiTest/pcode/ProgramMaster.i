
/*------------------------------------------------------------------------
    File        : ProgramMaster.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for ProgramMaster

    Author(s)   : Kuldeep
    Created     : 2 oct 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


DEFINE TEMP-TABLE ttProg NO-UNDO LIKE prgrms
BEFORE-TABLE beforeProg.
DEFINE DATASET dsProg FOR ttProg.
DEFINE QUERY q-ProgQuery FOR prgrms.
DEFINE DATA-SOURCE src-Prog FOR QUERY q-ProgQuery.
BUFFER ttProg:ATTACH-DATA-SOURCE(DATA-SOURCE src-Prog:HANDLE).

