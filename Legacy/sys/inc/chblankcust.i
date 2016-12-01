/* sys/inc/pushpin.i */

DEFINE BUFFER bf-usercust FOR usercust.
DEF VAR v-check-blank AS LOG INIT NO NO-UNDO.

FOR EACH bf-usercust 
    WHERE bf-usercust.company EQ cocode 
      AND bf-usercust.user_id EQ USERID("ASI") 
    NO-LOCK,
    FIRST cust WHERE cust.company EQ cocode 
     AND cust.cust-no = bf-usercust.cust-no NO-LOCK :
    ASSIGN v-check-blank = YES.
    LEAVE .
END.
IF NOT v-check-blank THEN 
    ASSIGN custcount = ""
           ou-log    = NO .
