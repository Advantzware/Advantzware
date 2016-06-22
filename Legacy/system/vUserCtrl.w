&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject NO-UNDO
       {"system/dusercontrol.i"}.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS vTable 
/*------------------------------------------------------------------------

  File:

  Description: from viewer.w - Template for SmartDataViewer objects

  Input Parameters:
      <none>

  Output Parameters:
      <none>

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

/* Local Variable Definitions ---                                       */

{src/adm2/widgetprto.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "system/dusercontrol.i"

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-16 cbHours cbNumUsersOver ~
fiNotificationEmailAddr fiSecondEmail cbTimeDirective btUpdate btCancel 
&Scoped-Define DISPLAYED-OBJECTS cbHours cbNumUsersOver ~
fiNotificationEmailAddr fiSecondEmail cbTimeDirective 

/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btCancel 
     LABEL "Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btUpdate 
     LABEL "Update" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE cbHours AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 18 
     LABEL "Time-out user when logged in for" 
     VIEW-AS COMBO-BOX INNER-LINES 8
     LIST-ITEM-PAIRS "6 Hours",6,
                     "8 Hours",8,
                     "12 Hours",12,
                     "18 Hours",18,
                     "24 Hours",24,
                     "36 Hours",36
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE cbNumUsersOver AS INTEGER FORMAT ">>9":U INITIAL 2 
     LABEL "Allowable Users Over Limit" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEMS "         0","         1","         2","         3","         4","         5","         6","         7","         8","         9","        10","        11","        12","        13","        14","        15","        16","        17","        18","        19","        20" 
     DROP-DOWN-LIST
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE cbTimeDirective AS CHARACTER FORMAT "X(256)":U 
     LABEL "Over Limit Behavior" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "No Warning","Warning Prompt","Warning + Email","Warning Prompt + Email" 
     DROP-DOWN-LIST
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE fiNotificationEmailAddr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Notification Email Addr" 
     VIEW-AS FILL-IN 
     SIZE 53 BY 1 NO-UNDO.

DEFINE VARIABLE fiSecondEmail AS CHARACTER FORMAT "X(256)":U 
     LABEL "2nd Notification Email Addr" 
     VIEW-AS FILL-IN 
     SIZE 53 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 33 BY 1.67.

DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 91 BY 9.76.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     cbHours AT ROW 3.38 COL 48 COLON-ALIGNED WIDGET-ID 18
     cbNumUsersOver AT ROW 4.81 COL 48 COLON-ALIGNED WIDGET-ID 22
     fiNotificationEmailAddr AT ROW 6.48 COL 48 COLON-ALIGNED WIDGET-ID 30
     fiSecondEmail AT ROW 7.91 COL 48 COLON-ALIGNED WIDGET-ID 32
     cbTimeDirective AT ROW 9.81 COL 48 COLON-ALIGNED WIDGET-ID 20
     btUpdate AT ROW 14.33 COL 37 WIDGET-ID 24
     btCancel AT ROW 14.33 COL 53 WIDGET-ID 26
     RECT-1 AT ROW 14.1 COL 36 WIDGET-ID 28
     RECT-16 AT ROW 2.52 COL 15 WIDGET-ID 34
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "system\dusercontrol.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" NO-UNDO  
      ADDITIONAL-FIELDS:
          {system/dusercontrol.i}
      END-FIELDS.
   END-TABLES.
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
  CREATE WINDOW vTable ASSIGN
         HEIGHT             = 16.43
         WIDTH              = 114.8.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB vTable 
/* ************************* Included-Libraries *********************** */

{src/adm2/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW vTable
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

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

&Scoped-define SELF-NAME btCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCancel vTable
ON CHOOSE OF btCancel IN FRAME F-Main /* Cancel */
DO:
        DO WITH FRAME {&frame-name}:
            btUpdate:LABEL = "Update".
            DISABLE cbHours cbNumUsersOver cbTimeDirective fiNotificationEmailAddr fiSecondEmail  .
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btUpdate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btUpdate vTable
ON CHOOSE OF btUpdate IN FRAME F-Main /* Update */
DO:

        IF SELF:LABEL = "Update" THEN DO:
          SELF:LABEL = "Save".
       
            DO WITH FRAME f-main:
                
                ENABLE cbHours cbNumUsersOver cbTimeDirective fiNotificationEmailAddr fiSecondEmail  .

            END.
            
        END.
        ELSE DO:

            SELF:LABEL = "Update".
            DO WITH FRAME f-main:

                FIND FIRST userControl EXCLUSIVE-LOCK.
                ASSIGN 
                    cbHours cbNumUsersOver cbTimeDirective fiNotificationEmailAddr fiSecondEmail
                    .
                ASSIGN 
                    userControl.adminEmailAddr     = fiNotificationEmailAddr:SCREEN-VALUE
                    userControl.autoLogoutTime     = INTEGER(cbHours:SCREEN-VALUE)
                    userControl.loginTimeDirective = cbTimeDirective:SCREEN-VALUE
                    userControl.numUsersOverLimit  = INTEGER(cbNumUsersOver:SCREEN-VALUE)
                    userControl.secondAdminEmail   = fiSecondEmail:SCREEN-VALUE.
                .
                FIND CURRENT userControl NO-LOCK.
                RELEASE userControl.
                DISABLE cbHours cbNumUsersOver cbTimeDirective fiNotificationEmailAddr fiSecondEmail  .
                
            END.        
                
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK vTable 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN initializeObject.
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects vTable  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI vTable  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject vTable 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

  
    RUN SUPER.

/* Code placed here will execute AFTER standard behavior.    */
    FIND FIRST userControl NO-LOCK NO-ERROR.
    IF NOT AVAILABLE userControl THEN 
      CREATE userControl.
    FIND CURRENT userControl NO-LOCK.
    DO WITH FRAME {&frame-name}:
        
        ENABLE cbHours cbNumUsersOver cbTimeDirective fiNotificationEmailAddr fiSecondEmail.
        cbHours:SCREEN-VALUE = STRING(userControl.autoLogoutTime).
        cbTimeDirective:SCREEN-VALUE = userControl.loginTimeDirective.
        cbNumUsersOver:SCREEN-VALUE = STRING( userControl.numUsersOverLimit).
        
        DISABLE cbHours cbNumUsersOver cbTimeDirective fiNotificationEmailAddr fiSecondEmail.
        
        DISPLAY 
            userControl.adminEmailAddr @  fiNotificationEmailAddr
            userControl.secondAdminEmail @ fiSecondEmail
            .
     

        DISABLE cbHours cbNumUsersOver cbTimeDirective fiNotificationEmailAddr fiSecondEmail.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

