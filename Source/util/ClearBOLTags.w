&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME gDialog
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS gDialog 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrdlg.w - ADM2 SmartDialog Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
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
{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/getcmpny.i}
{custom/gloc.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

ASSIGN
    cocode = gcompany
    locode = gloc.

DEF TEMP-TABLE ttBolList
    FIELD iBolNo AS INT.
    

{src/adm2/widgetprto.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME gDialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_OK Btn_Cancel fiFromCustomer ~
fiToCustomer tbOnHold 
&Scoped-Define DISPLAYED-OBJECTS eInstructions fiFromCustomer fiToCustomer ~
tbOnHold fiStatus 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Exit" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "Start" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE eInstructions AS CHARACTER INITIAL "Instructions from Main Block" 
     VIEW-AS EDITOR
     SIZE 83 BY 2.86 NO-UNDO.

DEFINE VARIABLE fiFromCustomer AS CHARACTER FORMAT "X(256)":U 
     LABEL "Customer Begin" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE fiStatus AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 86 BY 1 NO-UNDO.

DEFINE VARIABLE fiToCustomer AS CHARACTER FORMAT "X(256)":U INITIAL "zzzzzzzzzzzz" 
     LABEL "End" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE tbOnHold AS LOGICAL INITIAL yes 
     LABEL "Include BOLs on HOLD?" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME gDialog
     eInstructions AT ROW 1.48 COL 6 NO-LABEL WIDGET-ID 10
     Btn_OK AT ROW 1.48 COL 93
     Btn_Cancel AT ROW 3.14 COL 93
     fiFromCustomer AT ROW 5.05 COL 21 COLON-ALIGNED WIDGET-ID 14
     fiToCustomer AT ROW 5.05 COL 51 COLON-ALIGNED WIDGET-ID 16
     tbOnHold AT ROW 6.48 COL 23 WIDGET-ID 6
     fiStatus AT ROW 7.67 COL 4 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     SPACE(18.79) SKIP(0.03)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Remove BOL Tags"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target
   Other Settings: APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB gDialog 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX gDialog
   FRAME-NAME                                                           */
ASSIGN 
       FRAME gDialog:SCROLLABLE       = FALSE
       FRAME gDialog:HIDDEN           = TRUE.

/* SETTINGS FOR EDITOR eInstructions IN FRAME gDialog
   NO-ENABLE                                                            */
ASSIGN 
       eInstructions:READ-ONLY IN FRAME gDialog        = TRUE.

/* SETTINGS FOR FILL-IN fiStatus IN FRAME gDialog
   NO-ENABLE                                                            */
ASSIGN 
       fiStatus:READ-ONLY IN FRAME gDialog        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX gDialog
/* Query rebuild information for DIALOG-BOX gDialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX gDialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME gDialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gDialog gDialog
ON WINDOW-CLOSE OF FRAME gDialog /* Remove BOL Tags */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK gDialog
ON CHOOSE OF Btn_OK IN FRAME gDialog /* Start */
DO:
    DISABLE TRIGGERS FOR LOAD OF oe-boll.

    FOR EACH oe-bolh NO-LOCK
        WHERE oe-bolh.company EQ cocode 
        AND oe-bolh.cust-no GE fiFromCustomer:SCREEN-VALUE 
        AND oe-bolh.cust-no LE fiToCustomer:SCREEN-VALUE 
        AND oe-bolh.posted EQ NO:
        
        /* Exclude On Hold BOLs if option chosen */                
        IF oe-bolh.stat EQ "H" AND 
        NOT tbOnHold:CHECKED THEN NEXT.
        
        ASSIGN 
            fiStatus:SCREEN-VALUE = "Processing BOL#: " + STRING(oe-bolh.bol-no).
                     
        /* Remove tags from BOL lines */
        FOR EACH oe-boll OF oe-bolh EXCLUSIVE-LOCK
            WHERE oe-boll.tag NE "":
            ASSIGN 
                oe-boll.tag = "".
        END.
    END.
    MESSAGE 
        "Process complete." 
        VIEW-AS ALERT-BOX.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiFromCustomer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiFromCustomer gDialog
ON HELP OF fiFromCustomer IN FRAME gDialog /* Customer Begin */
OR HELP OF fiToCustomer IN FRAME gDialog
DO:
   DEFINE VARIABLE cMainField AS CHARACTER NO-UNDO.
   DEFINE VARIABLE cAllFields AS CHARACTER NO-UNDO.
   DEFINE VARIABLE recRecordID AS RECID    NO-UNDO.
   
   RUN system/openlookup.p (g_company, "cust-no", 0, "", 0, OUTPUT cAllFields, OUTPUT cMainField, OUTPUT recRecordID).
          IF cMainField <> "" THEN {&SELF-NAME}:SCREEN-VALUE = cMainField.
          RETURN NO-APPLY. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK gDialog 


/* ***************************  Main Block  *************************** */
ASSIGN
    eInstructions =
        "This function will review BOLs for the specified company and customer, and will remove all inventory tags from the selected BOL lines.".

{src/adm2/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects gDialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI gDialog  _DEFAULT-DISABLE
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
  HIDE FRAME gDialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI gDialog  _DEFAULT-ENABLE
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
  DISPLAY eInstructions fiFromCustomer fiToCustomer tbOnHold fiStatus 
      WITH FRAME gDialog.
  ENABLE Btn_OK Btn_Cancel fiFromCustomer fiToCustomer tbOnHold 
      WITH FRAME gDialog.
  VIEW FRAME gDialog.
  {&OPEN-BROWSERS-IN-QUERY-gDialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

