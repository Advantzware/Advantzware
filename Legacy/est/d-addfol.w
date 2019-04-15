&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: est\d-addfol.w
  
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

DEF INPUT PARAM ip-corr AS LOG NO-UNDO.
def output param ls-add-what as cha no-undo.

def SHARED var cocode     as   char  format "x(3)"  no-undo.

{sys/inc/cadcam.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-25 Btn_itm Btn_itm-cad Btn_tandem ~
Btn_set Btn_frm-out Btn_est Btn-Copy Btn_est-2 Btn_Cancel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fEnableImportForm D-Dialog 
FUNCTION fEnableImportForm RETURNS LOGICAL
  (ipcCompany AS CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn-Copy AUTO-GO 
     LABEL "&Copy" 
     SIZE 26 BY 2.14
     BGCOLOR 8 .

DEFINE BUTTON btnImportForm AUTO-GO 
     LABEL "&Import Form to Estimate" 
     SIZE 26 BY 2.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Ca&ncel" 
     SIZE 26 BY 2.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_est AUTO-GO 
     LABEL "Add &Form to Estimate" 
     SIZE 26 BY 2.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_est-2 AUTO-GO 
     LABEL "Add &Blank to Form" 
     SIZE 26 BY 2.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_frm-out AUTO-GO 
     LABEL "Farm Out" 
     SIZE 26 BY 2.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_itm AUTO-GO 
     LABEL "New &Estimate" 
     SIZE 26 BY 2.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_itm-cad AUTO-GO 
     LABEL "New &Estimate from CAD" 
     SIZE 26 BY 2.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_part AUTO-GO 
     LABEL "&Assembled Partition" 
     SIZE 26 BY 2.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_set AUTO-GO 
     LABEL "&Set Estimate" 
     SIZE 26 BY 2.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_tandem AUTO-GO 
     LABEL "Create from &Tandem" 
     SIZE 26 BY 2.14
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-25
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 97 BY 14.76.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     Btn_itm AT ROW 1.48 COL 18
     Btn_itm-cad AT ROW 1.48 COL 50 WIDGET-ID 2
     Btn_tandem AT ROW 3.86 COL 18
     Btn_part AT ROW 3.86 COL 50 WIDGET-ID 4
     Btn_set AT ROW 6.24 COL 18
     Btn_frm-out AT ROW 6.24 COL 50 WIDGET-ID 6
     Btn_est AT ROW 8.62 COL 18
     Btn-Copy AT ROW 8.62 COL 50
     Btn_est-2 AT ROW 11 COL 18
     btnImportForm AT ROW 11 COL 50 WIDGET-ID 8
     Btn_Cancel AT ROW 13.38 COL 18
     RECT-25 AT ROW 1 COL 1
     SPACE(0.59) SKIP(0.00)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Adding Option"
         DEFAULT-BUTTON Btn_est CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   FRAME-NAME                                                           */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON btnImportForm IN FRAME D-Dialog
   NO-ENABLE                                                            */
ASSIGN 
       btnImportForm:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR BUTTON Btn_part IN FRAME D-Dialog
   NO-ENABLE                                                            */
ASSIGN 
       Btn_part:HIDDEN IN FRAME D-Dialog           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Adding Option */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Copy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Copy D-Dialog
ON CHOOSE OF Btn-Copy IN FRAME D-Dialog /* Copy */
DO:
    assign ls-add-what = "copy-est".
    apply "window-close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnImportForm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnImportForm D-Dialog
ON CHOOSE OF btnImportForm IN FRAME D-Dialog /* Import Form to Estimate */
DO:
    assign ls-add-what = "ImportForm".
    apply "window-close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel D-Dialog
ON CHOOSE OF Btn_Cancel IN FRAME D-Dialog /* Cancel */
DO:
  assign ls-add-what = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_est
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_est D-Dialog
ON CHOOSE OF Btn_est IN FRAME D-Dialog /* Add Form to Estimate */
DO:
    assign ls-add-what = "form".
    apply "window-close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_est-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_est-2 D-Dialog
ON CHOOSE OF Btn_est-2 IN FRAME D-Dialog /* Add Blank to Form */
DO:
    assign ls-add-what = "blank".
    apply "window-close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_frm-out
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_frm-out D-Dialog
ON CHOOSE OF Btn_frm-out IN FRAME D-Dialog /* Farm Out */
DO:
    assign ls-add-what = "farm".
    apply "window-close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_itm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_itm D-Dialog
ON CHOOSE OF Btn_itm IN FRAME D-Dialog /* New Estimate */
DO:
    assign ls-add-what = "est".
    apply "window-close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_itm-cad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_itm-cad D-Dialog
ON CHOOSE OF Btn_itm-cad IN FRAME D-Dialog /* New Estimate from CAD */
DO:

    FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
                          AND sys-ctrl.name    EQ "CADCAM" NO-LOCK NO-ERROR.

    assign ls-add-what = IF AVAILABLE sys-ctrl AND sys-ctrl.char-fld = "Arden" THEN "estImpact"
                         ELSE IF AVAILABLE sys-ctrl AND sys-ctrl.char-fld = "Artios2" THEN "estCadNew"
                         ELSE IF AVAILABLE sys-ctrl AND sys-ctrl.char-fld = "ArtiosTest" THEN "estCadTest"
                         ELSE "estCad".

    apply "window-close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_part
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_part D-Dialog
ON CHOOSE OF Btn_part IN FRAME D-Dialog /* Assembled Partition */
DO:
    assign ls-add-what = "estsetasspart".
    apply "window-close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_set
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_set D-Dialog
ON CHOOSE OF Btn_set IN FRAME D-Dialog /* Set Estimate */
DO:
    assign ls-add-what = "estset".
    apply "window-close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_tandem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_tandem D-Dialog
ON CHOOSE OF Btn_tandem IN FRAME D-Dialog /* Create from Tandem */
DO:
    assign ls-add-what = "est-from-tandem".
    apply "window-close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */
IF ip-corr = YES THEN DO:
   ASSIGN
      btn_part:HIDDEN = NO
      btn_part:SENSITIVE = YES
      .
END.
IF fEnableImportForm(cocode) THEN 
    ASSIGN  
        btnImportForm:HIDDEN = NO
        btnImportForm:SENSITIVE = YES .
    
{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog  _ADM-ROW-AVAILABLE
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

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog  _DEFAULT-DISABLE
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
  HIDE FRAME D-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog  _DEFAULT-ENABLE
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
  ENABLE RECT-25 Btn_itm Btn_itm-cad Btn_tandem Btn_set Btn_frm-out Btn_est 
         Btn-Copy Btn_est-2 Btn_Cancel 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartDialog, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed D-Dialog 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fEnableImportForm D-Dialog 
FUNCTION fEnableImportForm RETURNS LOGICAL
  (ipcCompany AS CHARACTER):

 /*------------------------------------------------------------------------------
     Purpose: Returns a logical value based on the value of the CEImportForm NK1 
     Notes:
    ------------------------------------------------------------------------------*/    
    DEFINE VARIABLE lResult AS LOGICAL   NO-UNDO.
    
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound  AS LOGICAL   NO-UNDO. 
    
    RUN sys\ref\nk1look.p (ipcCompany,
        'CEImportForm',
        'L',
        NO,
        NO,
        '',
        '', 
        OUTPUT cReturn,
        OUTPUT lFound).

    lResult = lFound AND cReturn EQ 'YES'.
    RETURN lResult.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

