/* sys/inc/pushpin.i */

DEFINE BUFFER bf-usercust FOR usercust.
DEF VAR v-check-blank AS LOG INIT NO NO-UNDO.

FOR EACH bf-usercust 
    WHERE bf-usercust.company EQ cocode 
      AND bf-usercust.user_id EQ USERID("nosweat") 
    NO-LOCK,
    FIRST cust WHERE cust.company EQ cocode 
     AND cust.cust-no = bf-usercust.cust-no NO-LOCK :
    ASSIGN v-check-blank = YES.
    LEAVE .
END.

IF NOT v-check-blank THEN 
FOR EACH sys-ctrl-shipto
    WHERE sys-ctrl-shipto.company EQ cocode
      AND sys-ctrl-shipto.NAME EQ "CustomerList"
      AND (sys-ctrl-shipto.char-fld EQ "" OR sys-ctrl-shipto.char-fld EQ ({1}) )
      AND sys-ctrl-shipto.log-fld
    NO-LOCK
    BY sys-ctrl-shipto.cust-vend-no:
    IF sys-ctrl-shipto.cust-vend-no <> "" THEN DO:
        ASSIGN v-check-blank = YES.
        LEAVE.
     END.
END.

IF NOT v-check-blank THEN 
    ASSIGN custcount = ""
           ou-log    = NO .
