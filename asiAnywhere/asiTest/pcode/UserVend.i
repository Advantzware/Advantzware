
/*------------------------------------------------------------------------
    File        : UserVend.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Users Customer Maintenance

    Author(s)   :Kuldeep
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttUserVend NO-UNDO 
    BEFORE-TABLE beforeUserVend
    FIELD user_id AS CHAR 
    FIELD vend_no LIKE vend.vend-no
    FIELD vend_default AS LOGICAL
    
    field VendName as char
    .
  
DEFINE DATASET dsUserVend FOR ttUserVend.  
DEFINE VARIABLE q-UserVendQuery AS HANDLE.
DEFINE VARIABLE src-UserVend AS HANDLE.


CREATE QUERY q-UserVendQuery.
q-UserVendQuery:SET-BUFFERS(BUFFER users:HANDLE,BUFFER uservend:Handle).

CREATE DATA-SOURCE src-UserVend.
src-UserVend:QUERY = q-UserVendQuery.

BUFFER ttUserVend:ATTACH-DATA-SOURCE(src-UserVend).

DEFINE VARIABLE v-UserVend AS HANDLE     NO-UNDO. 
v-UserVend = DATASET dsUserVend:HANDLE.   
v-UserVend:GET-BUFFER-HANDLE("ttUserVend"):SET-CALLBACK-PROCEDURE("AFTER-ROW-FILL", "set-lang", THIS-PROCEDURE). 
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

/* ***************************  Procedures  *************************** */
PROCEDURE set-lang:
    DEFINE INPUT PARAMETER DATASET FOR dsUserVend.
    find first vend where vend.vend-no EQ ttUserVend.vend_no NO-LOCK .
       

    ASSIGN ttUserVend.VendName = vend.name.
    
END PROCEDURE. 

