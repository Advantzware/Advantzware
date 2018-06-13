&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-epCheckExpiration) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE epCheckExpiration Procedure 
PROCEDURE epCheckExpiration :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF OUTPUT PARAMETER lOK AS LOG NO-UNDO.
    /* From chkdate.p */
    DEF VAR lv-date AS DATE NO-UNDO.

    ASSIGN
        lOK = YES.
        
    IF CONNECTED(LDBNAME(1)) THEN DO:
        FIND FIRST expiration NO-LOCK NO-ERROR.
        IF NOT AVAIL expiration THEN DO:
            CREATE expiration.
            ASSIGN
                expiration.this-a-trial = YES
                expiration.expire-date  = 12/31/2099.
        END.

        IF expiration.this-a-trial 
        AND TODAY GT expiration.expire-date THEN DO:

            IF USERID(LDBNAME(1)) EQ "asi" THEN DO:
                FIND CURRENT expiration EXCLUSIVE.
                MESSAGE 
                    "Set New Expiration Date:" 
                    UPDATE expiration.expire-date.
                MESSAGE 
                    "Logging out. Please re-login to continue."
                    VIEW-AS ALERT-BOX.
                QUIT.
            END.
            ELSE DO:
                FIND CURRENT expiration EXCLUSIVE.
                ASSIGN
                    lOK = FALSE
                    lv-date = expiration.expire-date
                    expiration.expire-date = 01/01/0001.
                MESSAGE 
                    "You have exceeded your trial period for this system." SKIP
                    "Please contact Advantzware Sales for assistance." SKIP(1)
                    "Make note of this error code: " 
                    "(" + STRING((MONTH(lv-date) * 100) + DAY(lv-date),"9999") + ")"
                    VIEW-AS ALERT-BOX ERROR.
            END.
        END.
        FIND CURRENT expiration NO-LOCK.
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-epCheckPwdExpire) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE epCheckPwdExpire Procedure 
PROCEDURE epCheckPwdExpire :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT-OUTPUT PARAMETER iopOK AS LOG INITIAL TRUE NO-UNDO.
    DEF VAR iDaysToExpire AS INT NO-UNDO.
    
    FIND FIRST usr NO-LOCK WHERE 
        usr.uid = USERID(LDBNAME(1))
        NO-ERROR.
    IF NOT AVAIL usr THEN DO:
        MESSAGE
            "Your userid is not set up correctly." SKIP
            "Please contact your System Administrator."
            VIEW-AS ALERT-BOX ERROR.
        ASSIGN
            iopOK = FALSE.
        RETURN.
    END.
    ELSE DO:
        FIND FIRST usercontrol NO-LOCK NO-ERROR.
        IF NOT AVAIL usercontrol THEN DO:
            MESSAGE
                "There is no usercontrol record for this system." SKIP
                "Please contact your System Administrator."
                VIEW-AS ALERT-BOX ERROR.
            ASSIGN
                iopOK = FALSE.
            RETURN.
        END.
        ELSE DO:
            IF usercontrol.pwdChgLen = 0 THEN DO:
                ASSIGN
                    iopOK = TRUE.
                RETURN.
            END.
            ASSIGN
                iDaysToExpire = usr.last-chg + usercontrol.pwdChgLen - today.
            IF iDaysToExpire LT 0 THEN DO:
                MESSAGE 
                    "Your password has expired.  It must be" SKIP
                    "changed by a System Administrator."
                    VIEW-AS ALERT-BOX ERROR.
                ASSIGN
                    iopOK = FALSE.
                RETURN.
            END.
            ELSE IF iDaysToExpire LT 16 THEN DO:
                MESSAGE
                    "Your password will expire in " + string(iDaysToExpire) + " days." SKIP
                    "You should change it before expiration."
                    VIEW-AS ALERT-BOX WARNING.
                RETURN.
            END.
        END.
    END.            
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-epCheckUserLocked) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE epCheckUserLocked Procedure 
PROCEDURE epCheckUserLocked :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT-OUTPUT PARAMETER iopOK AS LOG INITIAL TRUE NO-UNDO.
    DEF VAR iDaysToExpire AS INT NO-UNDO.

    FIND FIRST users NO-LOCK WHERE
        users.user_id EQ USERID(LDBNAME(1))
        NO-ERROR.
    IF NOT AVAIL users THEN DO:
        ASSIGN
            iopOK = FALSE.
        RETURN.
    END.
    IF users.isLocked EQ TRUE THEN DO:
        MESSAGE
            "Your Advantzware account has been locked." SKIP
            "Please contact a Systems Administrator."
            VIEW-AS ALERT-BOX ERROR.
        ASSIGN
            iopOK = FALSE.
        RETURN.
    END.           
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-epConnectDB) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE epConnectDB Procedure 
PROCEDURE epConnectDB :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER cStatement AS CHAR NO-UNDO.
    DEF INPUT PARAMETER cUser AS CHAR NO-UNDO.
    DEF INPUT PARAMETER cPassword AS CHAR NO-UNDO.
    DEF OUTPUT PARAMETER lError AS LOG NO-UNDO.

    CONNECT VALUE(cStatement + 
                  " -U " + cUser + 
                  " -P " + cPassword) NO-ERROR.
    IF CONNECTED(LDBNAME(1))
    AND LDBNAME(1) = "ASI" THEN DO:
        CREATE ALIAS nosweat FOR DATABASE VALUE(LDBNAME(1)).
        CREATE ALIAS emptrack FOR DATABASE VALUE(LDBNAME(1)).
        CREATE ALIAS jobs FOR DATABASE VALUE(LDBNAME(1)).
        CREATE ALIAS rfq FOR DATABASE VALUE(LDBNAME(1)).
        CREATE ALIAS asihelp FOR DATABASE VALUE(LDBNAME(1)).
        CREATE ALIAS asihlp FOR DATABASE VALUE(LDBNAME(1)).
        CREATE ALIAS asinos FOR DATABASE VALUE(LDBNAME(1)).
    END.
    ASSIGN
        lError = NOT CONNECTED(LDBNAME(1)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-epDisconnectDB) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE epDisconnectDB Procedure 
PROCEDURE epDisconnectDB :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF CONNECTED(LDBNAME(2)) THEN
        DISCONNECT VALUE(LDBNAME(2)).

    IF CONNECTED(LDBNAME(1)) THEN
        DISCONNECT VALUE(LDBNAME(1)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-epGetDeveloperList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE epGetDeveloperList Procedure 
PROCEDURE epGetDeveloperList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF OUTPUT PARAMETER cList AS CHAR NO-UNDO.

    FOR EACH users NO-LOCK WHERE 
        users.developer = YES:
        ASSIGN
            cList = IF cList = "" THEN users.user_id ELSE cList + "," + users.user_id.
    END.

    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-epGetUserComp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE epGetUserComp Procedure 
PROCEDURE epGetUserComp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF OUTPUT PARAMETER cComp AS CHAR.
    DEF OUTPUT PARAMETER cLocn AS CHAR.

    /* From custom/getcomp.p */
    FIND FIRST usercomp NO-LOCK WHERE 
        usercomp.user_id = USERID(LDBNAME(1)) AND
        usercomp.company_default = YES 
        NO-ERROR.
    ASSIGN 
        cComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
    FIND FIRST usercomp NO-LOCK WHERE 
        usercomp.user_id = USERID(LDBNAME(1)) AND
        usercomp.company = cComp AND
        usercomp.loc NE "" AND
        usercomp.loc_default = yes
        NO-ERROR.
    ASSIGN
        cLocn = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN".


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-epGetUserGroups) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE epGetUserGroups Procedure 
PROCEDURE epGetUserGroups :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF OUTPUT PARAMETER cGroups AS CHAR.
    
    ASSIGN 
        cGroups = "".

    FOR EACH usergrps NO-LOCK:
        IF CAN-DO(usergrps.users,USERID(LDBNAME(1))) THEN ASSIGN
            cGroups = cGroups + usergrps.usergrps + ",".
    END.

    ASSIGN
        cGroups = TRIM(cGroups,",").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-epSetUpEDI) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE epSetUpEDI Procedure 
PROCEDURE epSetUpEDI :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    FIND FIRST module NO-LOCK WHERE 
        module.module EQ "m" AND 
        module.is-used EQ TRUE AND 
        module.expire-date GE TODAY
        NO-ERROR.

    IF AVAILABLE module 
    AND SEARCH("rc/genrcvar.r") NE ? THEN 
        RUN rc/genrcvar.p. 
     

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-epTouchLogin) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE epTouchLogin Procedure 
PROCEDURE epTouchLogin :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF OUTPUT PARAMETER lOut AS LOG NO-UNDO.
    /* From sys/inc/tslogin.p */
    FIND FIRST sys-ctrl NO-LOCK WHERE
        sys-ctrl.name eq "TSLOGIN"
        NO-ERROR.
    IF NOT AVAIL sys-ctrl THEN DO:
        FIND FIRST company NO-LOCK NO-ERROR.
        CREATE sys-ctrl.
        ASSIGN
            sys-ctrl.company = IF AVAIL company THEN company.company ELSE "" 
            sys-ctrl.name = "TSLOGIN"
            sys-ctrl.descrip = "Prompt Login ID/Password for Touch-Screen"
            sys-ctrl.log-fld = no  .
    END.
    ASSIGN 
        lOut = sys-ctrl.log-fld.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-epUpdateUsrFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE epUpdateUsrFile Procedure 
PROCEDURE epUpdateUsrFile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF OUTPUT PARAMETER opcUserList AS CHAR NO-UNDO.
    
    FOR EACH _user NO-LOCK:
        ASSIGN
            opcUserList = opcUserList + _user._userid + ",".
    END.
    ASSIGN
        opcUserList = TRIM(opcUserList,",").
        
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-epUserLogin) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE epUserLogin Procedure 
PROCEDURE epUserLogin :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF OUTPUT PARAMETER lExit AS LOG.
    DEF VAR lResult AS LOG.
    RUN system\userLogin.p (OUTPUT lResult).
    IF lResult THEN ASSIGN 
        lExit = TRUE.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-epUserRecordCheck) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE epUserRecordCheck Procedure 
PROCEDURE epUserRecordCheck :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF OUTPUT PARAMETER lOK AS LOG NO-UNDO.
    DEF OUTPUT PARAMETER lTrack AS LOG NO-UNDO.
    
    ASSIGN
        lOK = TRUE
        lTrack = FALSE.
        
    FIND users NO-LOCK WHERE 
        users.user_id = USERID(LDBNAME(1)) 
        NO-ERROR.
    IF NOT AVAIL users THEN FIND users NO-LOCK WHERE 
        users.user_id = USERID(LDBNAME(2)) 
        NO-ERROR.
    IF NOT AVAILABLE users THEN DO:     
        MESSAGE 
            "User Login Does Not Exist in Users File."
            "Please Contact Your System Manager." 
            VIEW-AS ALERT-BOX ERROR.
        ASSIGN lOK = FALSE.
    END.
    ELSE ASSIGN 
        lTrack = users.track_usage.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

