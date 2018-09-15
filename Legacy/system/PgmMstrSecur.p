

/* **********************  Internal Procedures  *********************** */


PROCEDURE epCanAccess:
    DEF INPUT PARAMETER ipProgName AS CHAR.
    DEF INPUT PARAMETER ipFunction AS CHAR.
    DEF OUTPUT PARAMETER opCanAccess AS LOG.
  
  RUN epCanAccessUser (INPUT ipProgName,
                                            INPUT ipFunction,
                                            INPUT USERID(LDBNAME(1)),
                                            OUTPUT opCanAccess).
        
END PROCEDURE.

PROCEDURE epCanAccessUser:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipProgName AS CHAR.
    DEF INPUT PARAMETER ipFunction AS CHAR.
    DEFINE INPUT PARAMETER ipUser AS CHARACTER.
    DEF OUTPUT PARAMETER opCanAccess AS LOG.
  
    FIND FIRST users NO-LOCK WHERE
        users.user_id EQ ipUser
        NO-ERROR.
    IF AVAIL users THEN 
    DO:
        ASSIGN
            opCanAccess = TRUE.
        
        /* Use this construct when the prgrms table has security definitions */
        IF /* is this a prgrms file? */ 0 EQ 1 THEN 
        DO: 
        END. /* program master exclusions */
    
        /* Otherwise, depend on hard-coded lists */
        ELSE 
        DO: 
            /* First group - programs/functions ONLY available to ASI user */
            /* Note: logic is 'if secLevel less than 1000, then disable' */
            IF users.securityLevel LT 1000 AND (                
                (ipProgName EQ "methods/template/viewer4.i" AND ipFunction EQ "") OR  /*NK2 Help Editor - No Password*/
                (ipProgName EQ "sys/ref/hlp-ctrl.w" AND ipFunction EQ "EnableHelpUpdate") OR  /*Update Help Button*/
                (ipProgName EQ "sys/ref/hlp.w" AND ipFunction EQ "Access2") OR /*Update Help Button*/
                (ipProgName EQ "sys/ref/hlp.w" AND ipFunction EQ "UpdateHelp") OR /*Update Help Button*/
                (ipProgName EQ "sys/ref/hlp.w" AND ipFunction EQ "DataDigger") OR /*DataDigger Button*/
                (ipProgName EQ "viewers/company.w" AND ipFunction EQ "") OR /*GF1 Add/Copy Company*/
                (ipProgName EQ "viewers/usercontrol.w" AND ipFunction EQ "Access1") OR  /*NK5 User Control - Num Concurrent/Over limit*/
                (ipProgName EQ "viewers/vp-rmov.w" AND ipFunction EQ "") OR /*MF1 Override Button (Inventory)*/
                (ipProgName EQ "windows/company.w" AND ipFunction EQ "") OR /*GF1 Add/Copy Company*/
                (ipProgName EQ "windows/file.w" AND ipFunction EQ "") OR  /*NZ1 System Reference Files*/
                (ipProgName EQ "windows/job-cat.w" AND ipFunction EQ "") OR  /*NZ2 (NZ1) Job Categories: MR, Run, DT,NC */
                (ipProgName EQ "windows/mat.w" AND ipFunction EQ "") OR  /*NZ3 (NZ2) Material Types*/
                (ipProgName EQ "windows/matprep.w" AND ipFunction EQ "") OR /*NZ5 (NZ4) Preparation Material Types*/
                (ipProgName EQ "windows/std-code.w" AND ipFunction EQ "") OR /*NZ7 (NZ6) Standards Matrix*/
                (ipProgName EQ "windows/uom.w" AND ipFunction EQ "") OR /*NZ8 (NZ7) Units of Measure*/
                (ipProgName EQ "windows/xref.w" AND ipFunction EQ "") OR /*NZ11 (NZ10) Cross References*/
                (ipProgName EQ "browsers/fgijob.w" AND ipFunction EQ "Access1") OR /*IF1 Bon/Job tab  allow edit only super  admin */
                (ipProgName EQ "system/mainmenu.w" AND ipFunction EQ "") OR /*Main menu load NZ1 System Reference Files Access*/
                (ipProgName EQ "system/sys-ctrl.w" AND ipFunction EQ "SuperAdmin") OR /*system controls, sys-ctrl*/
                (ipProgName EQ "util/CheckModule.p" AND ipFunction EQ "SuperAdmin") OR /*run util module*/
                (ipProgName EQ "util/module.w" AND ipFunction EQ "") OR /*Ability to run util module*/
                (ipProgName EQ "viewers/file-seq.w" AND ipFunction EQ "") OR /*NZ1 System ref Files */
                (ipProgName EQ "viewers/users.w" AND ipFunction EQ "SuperAdmin") OR /*users superadmin*/
                (ipProgName EQ "viewers/vend.w" AND ipFunction EQ "") OR /*NK1 view control tab */
                (ipProgName EQ "sys/ref/hlp.w" AND ipFunction EQ "ProTools") OR /*ProTools Button*/  
                (ipProgName EQ "windows/dept.w" AND ipFunction EQ "delete") OR /*NZ4 (NZ3) Departments - Delete button*/
                (ipProgName EQ "util/utilsN.w" AND ipFunction EQ "") OR /*Utils Button*/
                (ipProgName EQ "" AND ipFunction EQ "")
                ) THEN ASSIGN opCanAccess = FALSE.
            /* Second group - programs/functions ONLY available to Administrators */
            /* Note: logic is 'if secLevel less than 900, then disable' */
            ELSE IF users.securityLevel LT 900 AND (
                (ipProgName EQ "browsers/rm-ibin.w" AND ipFunction EQ "") OR  /*MF1/MF2 Bins Tab - Update Cost/Unit/Count - No Password*/
                (ipProgName EQ "windows/prgrms.w" AND ipFunction EQ "") OR /*NS8 Program Master*/
                (ipProgName EQ "jcinq/b-updmat.w" AND ipFunction EQ "") OR /*IF1 History Tab - Enable Edit Buttons*/
                (ipProgName EQ "oeinq/b-ordfgi.w" AND ipFunction EQ "") OR /*JQ1 Materials Tab - Edit Issues*/
                (ipProgName EQ "sys/help/w-head.w" AND ipFunction EQ "") OR /*NK2 Help Editor - Password*/
                (ipProgName EQ "sys/ref/hlp-ctrl.w" AND ipFunction EQ "EnableUpdateButton") OR  /*Update Help Button - redundant with above?*/
                (ipProgName EQ "sys/ref/hlp.w" AND ipFunction EQ "Access1") OR  /*Update Help Button - redundant with above?*/
                (ipProgName EQ "viewers/usercontrol.w" AND ipFunction EQ "Access2") OR /*NK5 User Control - Max Sessions Per User*/
                (ipProgName EQ "browsers/userlog.w" AND ipFunction EQ "") OR /*NK5 User Control -delete button */
                (ipProgName EQ "browsers/fgijob.w" AND ipFunction EQ "Access2") OR /*IF1 Bon/Job tab allow edit 'Total Weight" to admin*/
                (ipProgName EQ "cec/v-item2.w" AND ipFunction EQ "") OR /*MF2 inventory tab overright button */
                (ipProgName EQ "jcinq/b-updmac.w" AND ipFunction EQ "") OR /* JQ1 Mach hrs Tab  */
                (ipProgName EQ "jcinq/b-updmat.w" AND ipFunction EQ "") OR /* JQ1 MAterial Tab   */
                (ipProgName EQ "rminq/b-rmiinq.w" AND ipFunction EQ "") OR /*MF1 or MF2 Hostory tab   */
                (ipProgName EQ "sys/ref/hlp.w" AND ipFunction EQ "LockMon") OR /*Lock Monitor Button*/
                (ipProgName EQ "system/audit.w") AND ipFunction EQ "Restore" OR /*restore deleted audit record*/
                (ipProgName EQ "system/mainmenu.w" AND ipFunction EQ "Access1") OR /*Main menu load Run Custom Utility Program access*/
                (ipProgName EQ "system/mainmenu.w" AND ipFunction EQ "CanUpgrade") OR /*Main menu upgrade notification*/
                (ipProgName EQ "system/sys-ctrl.w" AND ipFunction EQ "") OR /*NK1 view control tab value security */
                (ipProgName EQ "windows/dept.w" AND ipFunction EQ "") OR  /*NZ4 (NZ3) Departments - Edits*/
                (ipProgName EQ "viewers/p-fg-bj-l.w" AND ipFunction EQ "") OR /*IF1 Bin/Jobs tab */
                (ipProgName EQ "viewers/sys-ctrl.w" AND ipFunction EQ "") OR /*NK1 view control tab value security */
                (ipProgName EQ "viewers/users.w" AND ipFunction EQ "Admin") OR /*users admin*/
                (ipProgName EQ "windows/dept.w" AND ipFunction EQ "") OR /*NZ4 (NZ3) Departments - Edits*/
                (ipProgName EQ "" AND ipFunction = "")
                ) THEN ASSIGN opCanAccess = FALSE.
        END. /* list-based exclusions */       
    END.
    ELSE ASSIGN
            opCanAccess = FALSE.

END PROCEDURE.
    
