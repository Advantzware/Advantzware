
/*------------------------------------------------------------------------
    File        : User.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for User

    Author(s)   : Kuldeep
    Created     : Sep 27 2007
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttUser NO-UNDO LIKE users
BEFORE-TABLE beforeUser
    FIELD language    like usr.Usr-Lang
    .

    
DEFINE DATASET dsUser FOR ttUser.

DEFINE QUERY q-UserQuery FOR fgcat.

DEFINE DATA-SOURCE src-User FOR QUERY q-UserQuery.

BUFFER ttUser:ATTACH-DATA-SOURCE(DATA-SOURCE src-User:HANDLE).

