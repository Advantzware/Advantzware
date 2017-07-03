&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
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
DEF VAR cLogModule AS CHAR NO-UNDO.
ASSIGN
    cLogModule = "{1}".

FIND login WHERE 
    login.terminal-no = terminalid AND
    login.user-id = ENTRY(1,USERID("ptdb"),"@") AND 
    login.system = cLogModule AND
    login.logged = TRUE
    NO-LOCK NO-ERROR.
IF AVAIL login THEN DO:
    FIND z_user OF login
        NO-LOCK NO-ERROR.
    FIND entity OF login 
        NO-LOCK NO-ERROR.
    IF AVAIL entity THEN DO:
        FIND {1}-control OF entity 
            NO-LOCK NO-ERROR.
        IF NOT AVAIL {1}-CONTROL THEN DO:
            MESSAGE
                "There is no " + cLogModule + "-control record for this entity."
                VIEW-AS ALERT-BOX ERROR.
            RETURN.
        END.
    END.
    ELSE DO:
        MESSAGE
            "There is no entity record available for this login."
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.
END.
ELSE DO:
    MESSAGE
        "You are not logged in to module " + cLogModule + "."
        VIEW-AS ALERT-BOX.
    RETURN.
END.

&IF LOOKUP("{1}","AP,AR,CP,FA,FF,IN,OP,PO,PR") <> 0 &THEN
    IF AVAIL {1}-control THEN FIND {1}-control-d OF {1}-control NO-LOCK NO-ERROR.
&ENDIF

CASE cLogModule:
    WHEN "AP" THEN DO:
        IF AVAIL ap-control THEN ASSIGN
            v-ap-entity = ap-control.ap-entity.
        FIND gl-control OF entity
            NO-LOCK NO-ERROR.
        IF AVAIL gl-control THEN ASSIGN
            v-gl-entity = gl-control.gl-entity.
    END.
    WHEN "AR" THEN DO:
        IF AVAIL ar-control THEN ASSIGN
            v-ar-entity = ar-control.ar-entity.
        FIND gl-control OF entity
            NO-LOCK NO-ERROR.
        IF AVAIL gl-control THEN ASSIGN
            v-gl-entity = gl-control.gl-entity.
    END.
    WHEN "BM" THEN DO:
        FIND pr-control OF entity
            NO-LOCK NO-ERROR.
        IF AVAIL pr-control THEN ASSIGN
            v-in-entity = pr-control.in-entity.
    END.
    WHEN "CA" THEN DO:
        FIND ff-control OF entity
            NO-LOCK NO-ERROR.
        IF AVAIL ff-control THEN ASSIGN
            v-ar-entity = ff-control.ar-entity
            v-in-entity = ff-control.in-entity.
    END.
    WHEN "FA" THEN DO:
        IF AVAIL fa-control THEN ASSIGN
            v-fa-entity = fa-control.fa-entity.
        FIND gl-control OF entity
            NO-LOCK NO-ERROR.
        IF AVAIL gl-control THEN ASSIGN
            v-gl-entity = gl-control.gl-entity.
    END.
    WHEN "FF" THEN DO:
        IF AVAIL ff-control THEN ASSIGN
            v-ar-entity = ff-control.ar-entity
            v-in-entity = ff-control.in-entity.
    END.
    WHEN "GL" THEN DO:
        IF AVAIL gl-control THEN ASSIGN
            v-gl-entity = gl-control.gl-entity.
    END.
    WHEN "IN" THEN DO:
        IF AVAIL in-control THEN ASSIGN
            v-in-entity = in-control.in-entity.
    END.
    WHEN "OP" THEN DO:
        IF AVAIL op-control THEN ASSIGN
            v-ar-entity = IF op-control.ar-control THEN op-control.in-entity ELSE op-control.ar-entity
            v-in-entity = op-control.in-entity
            v-ap-entity = op-control.ap-entity.
    END.
    WHEN "PO" THEN DO:
        IF AVAIL po-control THEN ASSIGN
            v-ap-entity = po-control.ap-entity
            v-in-entity = po-control.in-entity.
        FIND gl-control OF entity
            NO-LOCK NO-ERROR.
        IF AVAIL gl-control THEN ASSIGN
            v-gl-entity = gl-control.gl-entity.
    END.
    WHEN "PP" THEN DO:
        FIND pr-control OF entity
            NO-LOCK NO-ERROR.
        IF AVAIL pr-control THEN ASSIGN
            v-in-entity = pr-control.in-entity.
    END.
    WHEN "PR" THEN DO:
        IF AVAIL pr-control THEN ASSIGN
            v-in-entity = pr-control.in-entity.
    END.
END CASE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


