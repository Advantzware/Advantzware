/* removeUser.i - rstark - 1.11.2021 */

DEFINE VARIABLE iEmailCount AS INTEGER NO-UNDO.

DEFINE BUFFER bUsers FOR users.

/* delete any user specific dynamic subjects */
/* delete trigger for dynParamValue handles other related dAOA tables */
FOR EACH dynParamValue EXCLUSIVE-LOCK
    WHERE dynParamValue.user-id EQ users.user_id
    :
    DELETE dynParamValue.
END. /* each dynParamValue */

/* find out if deleted/inactive user's email is used in more than one account */
FOR EACH bUsers NO-LOCK
    WHERE bUsers.email EQ users.email
    :
    iEmailCount = iEmailCount + 1.
END. /* each busers */

/* if only one email instance, remove it from dAOA email recipients */
IF iEmailCount EQ 1 THEN  DO:
    FOR EACH dynValueParam EXCLUSIVE-LOCK
        WHERE dynValueParam.paramName  EQ "svRecipients"
          AND dynValueParam.paramValue GT ""
          AND LOOKUP(users.email, dynValueParam.paramValue) NE 0
        :
        ASSIGN
            dynValueParam.paramValue = REPLACE(dynValueParam.paramValue, users.email, "")
            dynValueParam.paramValue = REPLACE(dynValueParam.paramValue, ",,",",")
            dynValueParam.paramValue = LEFT-TRIM(dynValueParam.paramValue, ",")
            dynValueParam.paramValue = TRIM(dynValueParam.paramValue, ",")
            .
    END. /* each task */
    
    FOR EACH Task NO-LOCK
        WHERE LOOKUP(users.email, Task.recipients) NE 0
        :
        ASSIGN
            Task.recipients = REPLACE(Task.recipients, users.email, "")
            Task.recipients = REPLACE(Task.recipients, ",,",",")
            Task.recipients = LEFT-TRIM(Task.recipients, ",")
            Task.recipients = TRIM(Task.recipients, ",")
            .
    END. /* each task */
END. /* if iemailcount */
