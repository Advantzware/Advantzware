DEF VAR cUserID AS CHAR NO-UNDO INITIAL 'mark'.
DEF VAR cNewPassword AS CHAR NO-UNDO INITIAL 'tyndall'.
DEFINE TEMP-TABLE tempUser NO-UNDO LIKE _User.

FIND FIRST _User EXCLUSIVE WHERE 
    _User._UserId = cUserID 
    NO-ERROR.
IF AVAIL _User THEN DO:
    FIND usr EXCLUSIVE WHERE
        usr.uid = _user._userID
        NO_ERROR.
     IF AVAIL usr THEN DO:
        ASSIGN
            usr.usr-passwd = ENCODE(cNewPassword).
     END.
     ELSE DO:
        MESSAGE
            "There is no usr table entry for this user."
            VIEW-AS ALERT-BOX WARNING.
     END.
     BUFFER-COPY _User EXCEPT _User._Password TO tempUser ASSIGN tempUser._Password = ENCODE(cNewPassword).
     DELETE _User.
     CREATE _User.
     BUFFER-COPY tempUser EXCEPT _tenantID TO _User.
END.
ELSE MESSAGE 
    "This Userid does not exist in the _user table."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.