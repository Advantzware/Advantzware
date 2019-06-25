

/*------------------------------------------------------------------------
    File        : RawMat.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for RawMatup

    Author(s)   : Jyoti Bajaj
    Created     : Oct 02 2007
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttRawMat NO-UNDO 
          FIELD RName1 AS CHARACTER 
          FIELD RItem1 AS CHARACTER
          FIELD RDscr1 AS CHARACTER
          FIELD Rcust-no AS CHAR
    FIELD Rraw1 AS CHAR
          .

DEFINE DATASET dsRawMat FOR ttRawMat .
DEFINE VARIABLE q-RawMatQuery AS HANDLE.
DEFINE VARIABLE src-RawMat AS HANDLE.

DEFINE QUERY q-RawMatQuery FOR ttRawMat .

DEFINE DATA-SOURCE src-RawMat  FOR QUERY q-RawMatQuery.


BUFFER ttRawMat :ATTACH-DATA-SOURCE(DATA-SOURCE src-RawMat  :HANDLE).


