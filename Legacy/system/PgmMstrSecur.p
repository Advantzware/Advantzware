PROCEDURE epCanAccess:
    DEF INPUT PARAMETER ipProgName AS CHAR.
    DEF INPUT PARAMETER ipFunction AS CHAR.
    DEF OUTPUT PARAMETER opCanAccess AS LOG.
  
    FIND FIRST users NO-LOCK WHERE
        users.user_id EQ USERID(LDBNAME(1))
        NO-ERROR.
    IF AVAIL users THEN DO:
        ASSIGN
            opCanAccess = TRUE.
        
        /* Use this construct when the prgrms table has security definitions */
        IF <is this a prgrms file?> 0 EQ 1 THEN DO:
        END. /* program master exclusions */
    
        /* Otherwise, depend on hard-coded lists */
        ELSE DO:
            /* First group - programs/functions ONLY available to ASI user */
            /* Note: logic is 'if secLevel less than 1000, then disable' */
            IF users.securityLevel LT 1000 AND
            (
                (ipProgName EQ "methods/template/viewer4.i" AND ipFunction EQ "") OR
                (ipProgName EQ "sys/ref/hlp-ctrl.w" AND ipFunction EQ "EnableHelpUpdate") OR
                (ipProgName EQ "sys/ref/hlp.w" AND ipFunction EQ "Access2") OR
                (ipProgName EQ "viewers/company.w" AND ipFunction EQ "") OR
                (ipProgName EQ "viewers/usercontrol.w" AND ipFunction EQ "Access1") OR
                (ipProgName EQ "viewers/vp-rmov.w" AND ipFunction EQ "") OR
                (ipProgName EQ "windows/company.w" AND ipFunction EQ "") OR
                (ipProgName EQ "windows/dept.w" AND ipFunction EQ "") OR
                (ipProgName EQ "windows/file.w" AND ipFunction EQ "") OR
                (ipProgName EQ "windows/job-cat.w" AND ipFunction EQ "") OR
                (ipProgName EQ "windows/mat.w" AND ipFunction EQ "") OR
                (ipProgName EQ "windows/matprep.w" AND ipFunction EQ "") OR
                (ipProgName EQ "windows/prgrms.w" AND ipFunction EQ "") OR
                (ipProgName EQ "windows/std-code.w" AND ipFunction EQ "") OR
                (ipProgName EQ "windows/uom.w" AND ipFunction EQ "") OR
                (ipProgName EQ "windows/xref.w" AND ipFunction EQ "") OR
                
                (ipProgName EQ "" AND ipFunction EQ "")
            ) THEN ASSIGN
                opCanAccess = FALSE.
            /* Second group - programs/functions ONLY available to Administrators */
            /* Note: logic is 'if secLevel less than 900, then disable' */
            ELSE IF users.securityLevel LT 900 AND
            (
                (ipProgName EQ "browsers/rm-ibin.w" AND ipFunction EQ "") OR
                (ipProgName EQ "jcinq/b-updmat.w" AND ipFunction EQ "") OR
                (ipProgName EQ "oeinq/b-ordfgi.w" AND ipFunction EQ "") OR
                (ipProgName EQ "sys/help/w-head.w" AND ipFunction EQ "") OR
                (ipProgName EQ "sys/ref/hlp-ctrl.w" AND ipFunction EQ "EnableUpdateButton") OR
                (ipProgName EQ "sys/ref/hlp.w" AND ipFunction EQ "Access1") OR
                (ipProgName EQ "viewers/usercontrol.w" AND ipFunction EQ "Access2") OR
                
                (ipProgName EQ "" AND ipFunction = "")
            ) THEN ASSIGN
                opCanAccess = FALSE.
        END. /* list-based exclusions */
       
    END.
    ELSE ASSIGN
        opCanAccess = FALSE.
        
END PROCEDURE.
    