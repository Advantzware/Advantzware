
/*------------------------------------------------------------------------
    File        ViewJobeases.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Order ViewJobeases Detail

    Author(s)   : Jyoti Bajaj
    Created     : Aug 27 2007
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttViewJob NO-UNDO 
        
        FIELD Job AS char
        FIELD Job2 AS Integer
        FIELD est AS Character
        FIELD Stats AS character
        FIELD Startdate AS date
        FIELD Enddate AS date
        FIELD Duedate AS date
        FIELD vUser AS CHAR

  .
DEFINE DATASET dsViewJob FOR ttViewJob .

DEFINE QUERY q-ViewJobQuery FOR ttViewJob.

DEFINE DATA-SOURCE src-ViewJob  FOR QUERY q-ViewJobQuery.

BUFFER ttViewJob :ATTACH-DATA-SOURCE(DATA-SOURCE src-ViewJob  :HANDLE).
