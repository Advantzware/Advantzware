
/*------------------------------------------------------------------------
    File        : Users1.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Users Maintenance

    Author(s)   : Brent Erdman
    Created     : Fri May 04 03:58:38 GMT 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttUsers NO-UNDO LIKE users

BEFORE-TABLE beforeUsers
field Language as char.
  
DEFINE DATASET dsUsers FOR ttUsers.
DEF BUFFER bf-usercomp FOR usercomp.
DEF VAR lv-default-comp AS cha NO-UNDO.
DEF VAR lv-default-loc AS cha NO-UNDO.
  

DEFINE QUERY q-UsersQuery FOR users.

DEFINE DATA-SOURCE src-Users FOR QUERY q-UsersQuery.

BUFFER ttUsers:ATTACH-DATA-SOURCE(DATA-SOURCE src-Users:HANDLE).

DEFINE VARIABLE v-Users AS HANDLE     NO-UNDO. 
v-Users = DATASET dsUsers:HANDLE.   
v-Users:GET-BUFFER-HANDLE("ttUsers"):SET-CALLBACK-PROCEDURE("AFTER-ROW-FILL", "set-lang", THIS-PROCEDURE). 
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

/* ***************************  Procedures  *************************** */
PROCEDURE set-lang:
    DEFINE INPUT PARAMETER DATASET FOR dsUsers.
    find first usr where usr.uid EQ ttUsers.user_id NO-LOCK .

    ASSIGN ttUsers.Language = usr.Usr-Lang.
   /* FIND FIRST bf-usercomp WHERE bf-usercomp.USER_id = "ASI" AND
                                  bf-usercomp.company_default NO-LOCK NO-ERROR.
     lv-default-comp = IF AVAIL bf-usercomp THEN bf-usercomp.company ELSE "001".

     FIND FIRST usercomp WHERE usercomp.USER_id = users.USER_id 
                           AND usercomp.company = lv-default-comp AND
                               usercomp.loc = ""
         NO-LOCK NO-ERROR.
     IF NOT AVAIL usercomp THEN DO:
        CREATE usercomp.
        ASSIGN usercomp.user_id = users.USER_id
            usercomp.company = IF AVAIL bf-usercomp THEN bf-usercomp.company ELSE "001"
            usercomp.loc = ""
            usercomp.company_default = YES.
     END.
     FIND FIRST bf-usercomp WHERE bf-usercomp.USER_id = "ASI" AND
                                  bf-usercomp.loc_default NO-LOCK NO-ERROR.
     lv-default-loc = IF AVAIL bf-usercomp THEN bf-usercomp.loc ELSE "MAIN".

     FIND FIRST usercomp WHERE usercomp.USER_id = users.USER_id 
                           AND usercomp.company = lv-default-comp AND
                               usercomp.loc = lv-default-loc NO-LOCK NO-ERROR.
     
     IF NOT AVAIL usercomp THEN DO:
        CREATE usercomp.
        ASSIGN usercomp.user_id = users.USER_id
            usercomp.company = IF AVAIL bf-usercomp THEN bf-usercomp.company ELSE "001"
            usercomp.loc = IF AVAIL bf-usercomp THEN bf-usercomp.loc ELSE "MAIN"
            usercomp.loc_DEFAULT = YES.
     END.*/

END PROCEDURE. 

