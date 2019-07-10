
/*------------------------------------------------------------------------
    File        : UsersComp_l.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Users Maintenance

    Author(s)   : Kuldeep
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttUserComp_l NO-UNDO LIKE usercomp

BEFORE-TABLE beforeUserComp_l
field locDscr as char.
  
DEFINE DATASET dsUserComp_l FOR ttUserComp_l.  
DEFINE VARIABLE q-UserComp_lQuery AS HANDLE.
DEFINE VARIABLE src-UserComp_l AS HANDLE.


CREATE QUERY q-UserComp_lQuery.
q-UserComp_lQuery:SET-BUFFERS(BUFFER users:HANDLE,BUFFER usercomp:Handle).

CREATE DATA-SOURCE src-UserComp_l.
src-UserComp_l:QUERY = q-UserComp_lQuery.

BUFFER ttUserComp_l:ATTACH-DATA-SOURCE(src-UserComp_l).


DEFINE VARIABLE v-UserComp_l AS HANDLE     NO-UNDO. 
v-UserComp_l = DATASET dsUserComp_l:HANDLE.   
v-UserComp_l:GET-BUFFER-HANDLE("ttUserComp_l"):SET-CALLBACK-PROCEDURE("AFTER-ROW-FILL", "set-lang", THIS-PROCEDURE). 
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

/* ***************************  Procedures  *************************** */
PROCEDURE set-lang:
    DEFINE INPUT PARAMETER DATASET FOR dsUserComp_l.
    find first loc where loc.company EQ ttUserComp_l.company NO-LOCK .
       

    ASSIGN ttUserComp_l.locDscr = loc.dscr.
    
END PROCEDURE. 

