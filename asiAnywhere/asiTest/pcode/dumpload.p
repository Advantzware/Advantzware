 
DEFINE TEMP-TABLE tt_prgrms LIKE prgrms 
FIELD abc AS CHAR.
DEFINE TEMP-TABLE tt_users LIKE users 
    FIELD bbb AS CHAR.
DEFINE DATASET dsDumpLoad FOR tt_prgrms.


DEFINE INPUT PARAMETER prmAct1 AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER prmAct2 AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER PrmPath  AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsDumpLoad.
    
    IF prmAct1 = ? THEN ASSIGN prmAct1 = "".
    IF prmAct2 = ? THEN ASSIGN prmAct2 = "".
    IF PrmPath = ? THEN ASSIGN PrmPath = "".
    MESSAGE "innn" prmAct1 prmAct2 PrmPath .
IF prmAct1 = "Security" THEN DO:
    
    IF prmAct2 = "Dump" THEN DO:
        OUTPUT TO VALUE(PrmPath + "prgrms.d") APPEND.
        FOR EACH prgrms NO-LOCK WHERE prgrms.prgmname MATCHES "*aspx*":
            EXPORT prgrms.
        END.
        OUTPUT CLOSE.
    END.
    IF prmAct2 = "Load" THEN DO:

        
        INPUT FROM VALUE(PrmPath + "prgrms.d") APPEND.                                                  
        REPEAT TRANSACTION ON ERROR UNDO, LEAVE:
            CREATE tt_prgrms.
            IMPORT tt_prgrms.   
        END. /*REPEAT TRANSACTION*/
        INPUT CLOSE.
        
        FOR EACH tt_prgrms :
            FIND prgrms WHERE prgrms.prgmname = tt_prgrms.prgmname
                NO-LOCK NO-ERROR.
            IF AVAILABLE prgrms THEN NEXT.
            CREATE prgrms.
            BUFFER-COPY tt_prgrms TO prgrms.
            DELETE tt_prgrms.
        END. /*FOR EACH tt_prgrms*/
        

    END.     /**prmAct2= laod*/

END.  /**prmAct1 = "Security"*/



IF prmAct1 = "Users" THEN DO:
    
    IF prmAct2 = "Dump" THEN DO:
        OUTPUT TO VALUE(PrmPath + "users.d") APPEND.
        FOR EACH users:
            EXPORT users.
        END.
        OUTPUT CLOSE.
    END.
    IF prmAct2 = "Load" THEN DO:
       INPUT FROM VALUE(PrmPath + "users.d") APPEND.
       REPEAT TRANSACTION ON ERROR UNDO, LEAVE:
           CREATE tt_users.
           IMPORT tt_users.   
       END. /*REPEAT TRANSACTION*/
       INPUT CLOSE.
       FOR EACH tt_users :
           FIND users WHERE users.user_id = tt_users.user_id
               NO-LOCK NO-ERROR.
           IF AVAILABLE users THEN NEXT.
           CREATE users.
           BUFFER-COPY tt_users TO users.
           DELETE tt_users.
       END. /*FOR EACH tt_users*/

    END.     /**prmAct2= laod*/

END.  /**prmAct1 = "Users"*/


