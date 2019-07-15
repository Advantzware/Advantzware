
/*------------------------------------------------------------------------
    File        : ViewUser.p
    Purpose     : Users

    Syntax      :

    Description : Update Advantzware DB

    Author(s)   : Jyoti Bajaj
    Created     : 03/24/2008
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER vCompany     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER vUserId      AS CHARACTER    NO-UNDO.
DEFINE INPUT PARAMETER vUname       AS CHARACTER    NO-UNDO.
DEFINE INPUT PARAMETER vEmail       AS CHARACTER    NO-UNDO. 
DEFINE INPUT PARAMETER vPassword       AS CHARACTER    NO-UNDO. 

DEFINE BUFFER buff-users FOR users.
    DEFINE BUFFER buff_User FOR _User.
    
    DEFINE BUFFER buff-usr FOR usr.
        DEFINE VARIABLE vLan AS CHARACTER FORMAT "xxxxxxx" INITIAL "English".


    IF prmAction = "Add" THEN DO:
    FIND buff-users WHERE buff-users.user_id = vUserId NO-LOCK NO-ERROR.
    IF NOT AVAILABLE buff-users  THEN DO:
        CREATE buff-users.
        ASSIGN
           buff-users.user_id          = vUserId
            buff-users.user_name       = vUname
            buff-users.image_filename  = vEmail
           
            .
        RELEASE buff-users.
        CREATE _User.
        ASSIGN
            _User._Userid      = vUserId
            _User._Password    = vPassword
            _User._User-Name   = vUname
            _User._Email       = vEmail.
    END.
    FIND buff-usr WHERE buff-usr.uid = vUserId NO-LOCK NO-ERROR.
    IF NOT AVAILABLE buff-usr  THEN DO:
        CREATE buff-usr.
        ASSIGN
            buff-usr.uid          = vUserId
            buff-usr.usr-Name     = vUname
            buff-usr.Usr-Passwd   = vPassword
            buff-usr.Usr-Lang     = vLan
           
            .
        RELEASE buff-usr.
    END.
END.    /*IF prmAction = "Add" THEN DO:*/



IF prmAction = "Update" THEN DO:
    FIND FIRST buff-users WHERE  buff-users.user_id = vUserId  EXCLUSIVE-LOCK NO-ERROR.
    IF  AVAILABLE buff-users  THEN DO:
       ASSIGN
          
            buff-users.user_name       = vUname
            buff-users.image_filename  = vEmail
          
            .
        RELEASE buff-users.
    END.  /* IF  AVAILABLE buff-users  THEN DO:*/
        FIND FIRST _User WHERE  _User._Userid = vUserId  EXCLUSIVE-LOCK NO-ERROR.
    IF  AVAILABLE _User  THEN DO:
        ASSIGN
            _User._Password    = vPassword
            _User._User-Name   = vUname
            _User._Email       = vEmail.
    END.  /* IF  AVAILABLE _User  THEN DO*/
    FIND buff-usr WHERE buff-usr.uid = vUserId EXCLUSIVE-LOCK NO-ERROR.
    IF  AVAILABLE buff-usr  THEN DO:
        ASSIGN
            buff-usr.usr-Name        = vUname
            buff-usr.Usr-Passwd      = vPassword
          
            .
        RELEASE buff-usr.
    END.   /* IF  AVAILABLE buff-usr*/
END.    /*IF prmAction = "Add" THEN DO:*/

 IF prmAction = "Delete" THEN DO:
    FIND FIRST users WHERE  users.user_id = vUserId  EXCLUSIVE-LOCK NO-ERROR.
    IF  AVAILABLE users  THEN DO:
    FIND FIRST usr WHERE usr.uid = users.user_id EXCLUSIVE-LOCK NO-ERROR.
    IF  AVAILABLE usr  THEN DO:
    DELETE usr.
    END.
    FIND FIRST _User WHERE  _User._Userid = buff-users.user_id  EXCLUSIVE-LOCK NO-ERROR.
    IF  AVAILABLE _User  THEN DO:
    DELETE _User.
    END.
        DELETE users.
        
   END .  /*IF  AVAILABLE users  THEN DO:*/
 END.  /*IF prmAction = "Delete" THEN DO:*/
