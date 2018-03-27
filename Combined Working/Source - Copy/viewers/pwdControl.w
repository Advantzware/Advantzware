&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/<table>.w

  Description: from VIEWER.W - Template for SmartViewer Objects

  Input Parameters:
      <none>

  Output Parameters:
      <none>

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE NEW GLOBAL SHARED VAR cIniLoc AS CHAR NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR cIniLoc AS CHAR NO-UNDO.

DEF VAR cLockoutTries AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES userControl
&Scoped-define FIRST-EXTERNAL-TABLE userControl


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR userControl.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS userControl.pwdChgLen ~
userControl.minPasswordLen userControl.minLC userControl.minUC ~
userControl.minNC userControl.minSC 
&Scoped-define ENABLED-TABLES userControl
&Scoped-define FIRST-ENABLED-TABLE userControl
&Scoped-Define DISPLAYED-FIELDS userControl.pwdChgLen ~
userControl.minPasswordLen userControl.minLC userControl.minUC ~
userControl.minNC userControl.minSC userControl.autoLockoutTries 
&Scoped-define DISPLAYED-TABLES userControl
&Scoped-define FIRST-DISPLAYED-TABLE userControl


/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
/**************************
</EXECUTING-CODE> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     userControl.pwdChgLen AT ROW 2.91 COL 39 COLON-ALIGNED WIDGET-ID 34 FORMAT ">>>9"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     userControl.minPasswordLen AT ROW 4.81 COL 39 COLON-ALIGNED WIDGET-ID 28
          LABEL "Minimum Password Length" FORMAT ">>9"
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     userControl.minLC AT ROW 6 COL 39 COLON-ALIGNED WIDGET-ID 24
          LABEL "Minimumn lower case chars" FORMAT ">9"
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     userControl.minUC AT ROW 7.19 COL 39 COLON-ALIGNED WIDGET-ID 32
          LABEL "Minimum UPPER CASE chars" FORMAT ">9"
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     userControl.minNC AT ROW 8.38 COL 39 COLON-ALIGNED WIDGET-ID 26
          LABEL "Minimum numeric chars" FORMAT ">9"
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     userControl.minSC AT ROW 9.57 COL 39 COLON-ALIGNED WIDGET-ID 30
          LABEL "Minimum special chars" FORMAT ">9"
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     userControl.autoLockoutTries AT ROW 11.48 COL 39 COLON-ALIGNED WIDGET-ID 18
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     "Password Restrictions:" VIEW-AS TEXT
          SIZE 27 BY .62 AT ROW 1.71 COL 8 WIDGET-ID 36
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: asi.userControl
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 17.14
         WIDTH              = 81.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{methods/template/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN userControl.autoLockoutTries IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN userControl.minLC IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN userControl.minNC IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN userControl.minPasswordLen IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN userControl.minSC IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN userControl.minUC IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN userControl.pwdChgLen IN FRAME F-Main
   EXP-FORMAT                                                           */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME userControl.minPasswordLen
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL userControl.minPasswordLen V-table-Win
ON LEAVE OF userControl.minPasswordLen IN FRAME F-Main /* Minimum Password Length */
OR LEAVE OF userControl.minLC
OR LEAVE OF userControl.minUC
OR LEAVE OF userControl.minNC
OR LEAVE OF userControl.minSC
DO:
    DEF VAR iMinLen AS INT NO-UNDO.
    ASSIGN
        iMinLen = INTEGER(userControl.minLC:SCREEN-VALUE IN FRAME {&FRAME-NAME}) +
                  INTEGER(userControl.minUC:SCREEN-VALUE) +
                  INTEGER(userControl.minNC:SCREEN-VALUE) +
                  INTEGER(userControl.minSC:SCREEN-VALUE).
    IF iMinLen GT INTEGER(userControl.minPasswordLen:SCREEN-VALUE) THEN DO:
        MESSAGE
            "The minimum password length value will not support" SKIP
            "this entry.  Modifying Minimum Password Length value."
            VIEW-AS ALERT-BOX.
        ASSIGN
            userControl.minPasswordLen:SCREEN-VALUE = STRING(iMinLen).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

    RUN ipReadIniFile.

    FIND FIRST usercontrol EXCLUSIVE NO-ERROR.
    IF AVAIL usercontrol THEN DO:
        IF INTEGER(cLockoutTries) NE usercontrol.autoLockoutTries THEN DO:
            ASSIGN
                usercontrol.autoLockoutTries = INTEGER(cLockoutTries).
        END.
    END.
    RELEASE usercontrol.
    
  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "userControl"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "userControl"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipReadIniFile V-table-Win 
PROCEDURE ipReadIniFile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR cIniLine AS CHAR NO-UNDO.
    
    INPUT FROM VALUE(SEARCH(cIniLoc)).
    REPEAT:
        IMPORT UNFORMATTED cIniLine.
        IF cIniLine BEGINS "LockoutTries" THEN ASSIGN
            cLockoutTries = ENTRY(2,cIniLine,"=").
        ELSE NEXT.
    END.
    INPUT CLOSE.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "userControl"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/vstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

