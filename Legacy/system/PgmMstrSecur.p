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
        IF /* is this a prgrms file? */ 0 EQ 1 THEN DO:
        END. /* program master exclusions */
    
        /* Otherwise, depend on hard-coded lists */
        ELSE DO:
            /* First group - programs/functions ONLY available to ASI user */
            /* Note: logic is 'if secLevel less than 1000, then disable' */
            IF users.securityLevel LT 1000 AND
            (
                
                (ipProgName EQ "methods/template/viewer4.i" AND ipFunction EQ "") OR  /*NK2 Help Editor - No Password*/
                (ipProgName EQ "sys/ref/hlp-ctrl.w" AND ipFunction EQ "EnableHelpUpdate") OR  /*Update Help Button*/
                (ipProgName EQ "sys/ref/hlp.w" AND ipFunction EQ "Access2") OR /*Update Help Button*/
                (ipProgName EQ "viewers/company.w" AND ipFunction EQ "") OR /*GF1 Add/Copy Company*/
                (ipProgName EQ "viewers/usercontrol.w" AND ipFunction EQ "Access1") OR  /*NK5 User Control - Num Concurrent/Over limit*/
                (ipProgName EQ "viewers/vp-rmov.w" AND ipFunction EQ "") OR /*MF1 Override Button (Inventory)*/
                (ipProgName EQ "windows/company.w" AND ipFunction EQ "") OR /*GF1 Add/Copy Company*/
                (ipProgName EQ "windows/dept.w" AND ipFunction EQ "") OR  /*NZ4 (NZ3) Departments - Edits*/
                (ipProgName EQ "windows/file.w" AND ipFunction EQ "") OR  /*NZ1 System Reference Files*/
                (ipProgName EQ "windows/job-cat.w" AND ipFunction EQ "") OR  /*NZ2 (NZ1) Job Categories: MR, Run, DT,NC */
                (ipProgName EQ "windows/mat.w" AND ipFunction EQ "") OR  /*NZ3 (NZ2) Material Types*/
                (ipProgName EQ "windows/matprep.w" AND ipFunction EQ "") OR /*NZ5 (NZ4) Preparation Material Types*/
                (ipProgName EQ "windows/std-code.w" AND ipFunction EQ "") OR /*NZ7 (NZ6) Standards Matrix*/
                (ipProgName EQ "windows/uom.w" AND ipFunction EQ "") OR /*NZ8 (NZ7) Units of Measure*/
                (ipProgName EQ "windows/xref.w" AND ipFunction EQ "") OR /*NZ11 (NZ10) Cross References*/
                
                (ipProgName EQ "" AND ipFunction EQ "")
            ) THEN ASSIGN
                opCanAccess = FALSE.
            /* Second group - programs/functions ONLY available to Administrators */
            /* Note: logic is 'if secLevel less than 900, then disable' */
            ELSE IF users.securityLevel LT 900 AND
            (
                (ipProgName EQ "browsers/rm-ibin.w" AND ipFunction EQ "") OR  /*MF1/MF2 Bins Tab - Update Cost/Unit/Count - No Password*/
                (ipProgName EQ "windows/prgrms.w" AND ipFunction EQ "") OR /*NS8 Program Master*/
                (ipProgName EQ "jcinq/b-updmat.w" AND ipFunction EQ "") OR /*IF1 History Tab - Enable Edit Buttons*/
                (ipProgName EQ "oeinq/b-ordfgi.w" AND ipFunction EQ "") OR /*JQ1 Materials Tab - Edit Issues*/
                (ipProgName EQ "sys/help/w-head.w" AND ipFunction EQ "") OR /*NK2 Help Editor - Password*/
                (ipProgName EQ "sys/ref/hlp-ctrl.w" AND ipFunction EQ "EnableUpdateButton") OR  /*Update Help Button - redundant with above?*/
                (ipProgName EQ "sys/ref/hlp.w" AND ipFunction EQ "Access1") OR  /*Update Help Button - redundant with above?*/
                (ipProgName EQ "viewers/usercontrol.w" AND ipFunction EQ "Access2") OR /*NK5 User Control - Max Sessions Per User*/
                
                (ipProgName EQ "" AND ipFunction = "")
            ) THEN ASSIGN
                opCanAccess = FALSE.
        END. /* list-based exclusions */
       
    END.
    ELSE ASSIGN
        opCanAccess = FALSE.
        
END PROCEDURE.
    