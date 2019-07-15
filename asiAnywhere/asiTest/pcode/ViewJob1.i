
/*------------------------------------------------------------------------
    File        ViewJob.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Order ViewJob1 Detail

    Author(s)   : Jyoti Bajaj
    Created     : Aug 27 2007
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttViewJob1 NO-UNDO 
       
        FIELD Job AS char
        FIELD Job2 AS Integer
        FIELD est AS Character
        FIELD Stats AS character
        FIELD Startdate AS date
        FIELD Enddate AS date
        FIELD Duedate AS date

  .
DEFINE DATASET dsViewJob1 FOR ttViewJob1 .

DEFINE QUERY q-ViewJob1Query FOR ttViewJob1.

DEFINE DATA-SOURCE src-ViewJob1  FOR QUERY q-ViewJob1Query.

BUFFER ttViewJob1 :ATTACH-DATA-SOURCE(DATA-SOURCE src-ViewJob1  :HANDLE).
