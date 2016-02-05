&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Password
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Password 
/*------------------------------------------------------------------------

  File: sys\ref\d-passwd.w
  
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF INPUT  PARAMETER ip-type      AS INT         NO-UNDO.
DEF OUTPUT PARAMETER op-validated AS LOG INIT NO NO-UNDO.

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/var.i new shared}
    
assign
 cocode = g_company
 locode = g_loc.

IF ip-type EQ 5 THEN
FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.NAME    EQ "JOBPASS"
    NO-LOCK NO-ERROR.

ELSE
IF ip-type EQ 2 THEN
FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.NAME    EQ "CUSTPASS"
    NO-LOCK NO-ERROR.
ELSE
IF ip-type EQ 6 THEN
FIND FIRST sys-ctrl
     WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.NAME    EQ "INVPASS"
     NO-LOCK NO-ERROR.

/* gdm - 03090901 */
ELSE
IF ip-type EQ 8 THEN
FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.NAME    EQ "RELCREDT"
    NO-LOCK NO-ERROR.
/* gdm - 03090901 end */

ELSE 
FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.NAME    EQ "SECURITY"
    NO-LOCK NO-ERROR.
    
IF AVAIL sys-ctrl THEN DO:
  IF (ip-type EQ 3 OR ip-type EQ 6) AND NOT sys-ctrl.log-fld THEN DO:
    op-validated = YES.
    RETURN.
  END.
  ELSE
  IF ip-type = 4 AND sys-ctrl.int-fld <> 1 THEN DO:
    op-validated = YES.
    RETURN.
  END.
  ELSE /* gdm - 03090901 */
  IF ip-type = 8 AND NOT sys-ctrl.log-fld THEN DO:
    op-validated = YES.
    RETURN.
  END.
  ELSE
  IF (ip-type = 1 OR ip-type = 2 OR ip-type = 5) AND
     NOT sys-ctrl.log-fld THEN DO:
    op-validated = NO.
    RETURN.
  END.
END.

ELSE DO:
  MESSAGE "No System Control record exists for " +
          IF ip-type EQ 5 THEN "JOBPASS"  ELSE
          IF ip-type EQ 2 THEN "CUSTPASS" ELSE
          IF ip-type EQ 6 THEN "INVPASS"  ELSE "SECURITY..."
          VIEW-AS ALERT-BOX ERROR.
  RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Password

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-29 pass_word Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS pass_word 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE pass_word AS CHARACTER FORMAT "x(256)":U 
     LABEL "Please enter password" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-29
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 55 BY 2.86.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Password
     pass_word AT ROW 2.19 COL 25 COLON-ALIGNED BLANK 
     Btn_OK AT ROW 4.57 COL 8
     Btn_Cancel AT ROW 4.57 COL 34
     RECT-29 AT ROW 1.24 COL 1
     SPACE(0.19) SKIP(2.13)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Password"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Password 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Password
                                                                        */
ASSIGN 
       FRAME Password:SCROLLABLE       = FALSE
       FRAME Password:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Password
/* Query rebuild information for DIALOG-BOX Password
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Password */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Password
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Password Password
ON WINDOW-CLOSE OF FRAME Password /* Password */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  op-validated = NO.
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Password
ON CHOOSE OF Btn_Cancel IN FRAME Password /* Cancel */
DO:
  op-validated = NO.
  APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Password
ON CHOOSE OF Btn_OK IN FRAME Password /* OK */
DO:
  ASSIGN pass_word.

  op-validated = pass_word EQ "advance4me".

  IF NOT op-validated THEN 
    MESSAGE "Invalid password, access denied..." VIEW-AS ALERT-BOX ERROR.
     APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME pass_word
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pass_word Password
ON LEAVE OF pass_word IN FRAME Password /* Please enter password */
DO:
  IF LASTKEY NE -1 THEN APPLY "choose" TO btn_ok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Password 


/* ***************************  Main Block  *************************** */

{src/adm2/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects Password  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Password  _DEFAULT-DISABLE
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
  HIDE FRAME Password.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Password  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY pass_word 
      WITH FRAME Password.
  ENABLE RECT-29 pass_word Btn_OK Btn_Cancel 
      WITH FRAME Password.
  VIEW FRAME Password.
  {&OPEN-BROWSERS-IN-QUERY-Password}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject Password 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

