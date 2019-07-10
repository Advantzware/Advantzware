
/*------------------------------------------------------------------------
    File        : Users1Comp.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Users Maintenance

    Author(s)   : Brent Erdman
    Created     : Fri May 04 03:58:38 GMT 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttUserComp NO-UNDO LIKE usercomp

BEFORE-TABLE beforeUserComp
field CompanyName as char.
  
DEFINE DATASET dsUserComp FOR ttUserComp.  
DEFINE VARIABLE q-UserCompQuery AS HANDLE.
DEFINE VARIABLE src-UserComp AS HANDLE.


CREATE QUERY q-UserCompQuery.
q-UserCompQuery:SET-BUFFERS(BUFFER users:HANDLE,BUFFER usercomp:Handle).

CREATE DATA-SOURCE src-UserComp.
src-UserComp:QUERY = q-UserCompQuery.

BUFFER ttUserComp:ATTACH-DATA-SOURCE(src-UserComp).

DEFINE VARIABLE v-UserComp AS HANDLE     NO-UNDO. 
v-UserComp = DATASET dsUserComp:HANDLE.   
v-UserComp:GET-BUFFER-HANDLE("ttUserComp"):SET-CALLBACK-PROCEDURE("AFTER-ROW-FILL", "set-lang", THIS-PROCEDURE). 
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

/* ***************************  Procedures  *************************** */
PROCEDURE set-lang:
    DEFINE INPUT PARAMETER DATASET FOR dsUserComp.
    find first company where company.company EQ ttUserComp.company NO-LOCK .
       

    ASSIGN ttUserComp.CompanyName = company.name.
    
END PROCEDURE. 

