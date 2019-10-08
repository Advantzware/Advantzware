&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrdlg.w - ADM SmartDialog Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
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
{custom/globdefs.i}

def input param ip-est-no as cha no-undo.
def input-output param iop-i-no as cha no-undo.
def input-output param iop-part-no as cha no-undo.
def input-output param iop-uom as cha no-undo.
DEFINE INPUT-OUTPUT PARAMETER iopcLoc AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopcLocBin AS CHARACTER NO-UNDO.

/*def var lv-uom-list as cha init "M,EA,L,CS,C" no-undo.*/
def var lv-uom-list as cha init "M,EA,L,CS,C,LB,DRM,ROL,PKG,SET,DOZ,BDL" no-undo.

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
&Scoped-Define ENABLED-OBJECTS ls-part-no ls-uom fiLoc fiLocBin Btn_OK ~
Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS ls-i-no ls-part-no ls-uom fiLoc fiLocBin 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE fiLoc AS CHARACTER FORMAT "X(5)":U 
     LABEL "Whse" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE fiLocBin AS CHARACTER FORMAT "X(10)":U 
     LABEL "Bin" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE ls-i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "FG Item#" 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1 NO-UNDO.

DEFINE VARIABLE ls-part-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "Cust Part #" 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1 NO-UNDO.

DEFINE VARIABLE ls-uom AS CHARACTER FORMAT "X(4)":U 
     LABEL "Cost UOM" 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     ls-i-no AT ROW 1.71 COL 17 COLON-ALIGNED
     ls-part-no AT ROW 2.91 COL 17 COLON-ALIGNED
     ls-uom AT ROW 4.1 COL 17 COLON-ALIGNED
     fiLoc AT ROW 5.29 COL 17 COLON-ALIGNED WIDGET-ID 2
     fiLocBin AT ROW 5.29 COL 36 COLON-ALIGNED WIDGET-ID 4
     Btn_OK AT ROW 8.86 COL 18
     Btn_Cancel AT ROW 8.86 COL 38
     "This item does not exist, would you like to add it ?" VIEW-AS TEXT
          SIZE 58 BY 1.43 AT ROW 6.71 COL 6
     SPACE(5.79) SKIP(2.85)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "Create Fg Item?"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


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

/* SETTINGS FOR FILL-IN ls-i-no IN FRAME D-Dialog
   NO-ENABLE                                                            */
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
ON WINDOW-CLOSE OF FRAME D-Dialog /* Create Fg Item? */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel D-Dialog
ON CHOOSE OF Btn_Cancel IN FRAME D-Dialog /* Cancel */
DO:
      assign iop-i-no = ""
             iop-part-no = ""
             iopcLoc = ""
             iopcLocBin = "".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK D-Dialog
ON CHOOSE OF Btn_OK IN FRAME D-Dialog /* OK */
DO:
    RUN valid-uom NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    IF ls-part-no:SCREEN-VALUE EQ "" THEN
       ASSIGN ls-part-no:SCREEN-VALUE = ls-i-no:SCREEN-VALUE .

    do with frame {&frame-name} :
       assign ls-i-no ls-part-no ls-uom fiLoc fiLocBin.
    end.
    
    FIND FIRST ITEMfg WHERE itemfg.company = g_company
                       AND itemfg.i-no = ls-i-no
                       NO-LOCK NO-ERROR.
   IF AVAIL itemfg THEN DO:
      MESSAGE "FG Item already exist. Try other item. " VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO ls-i-no.
      RETURN NO-APPLY.
   END.

    assign iop-i-no = ls-i-no
           iop-part-no = ls-part-no
           iop-uom = ls-uom
           iopcLoc = fiLoc
           iopcLocBin = fiLocBin.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ls-i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ls-i-no D-Dialog
ON LEAVE OF ls-i-no IN FRAME D-Dialog /* FG Item# */
DO:
   FIND FIRST ITEMfg WHERE itemfg.company = g_company
                       AND itemfg.i-no = SELF:SCREEN-VALUE
                       NO-LOCK NO-ERROR.
   IF AVAIL itemfg THEN DO:
      MESSAGE "FG Item already exist. Try other item. " VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ls-part-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ls-part-no D-Dialog
ON LEAVE OF ls-part-no IN FRAME D-Dialog /* Cust Part # */
DO:
  IF LASTKEY NE -1 THEN DO:

   IF ls-part-no:SCREEN-VALUE EQ "" THEN
       ASSIGN ls-part-no:SCREEN-VALUE = ls-i-no:SCREEN-VALUE .

    RUN valid-part-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ls-uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ls-uom D-Dialog
ON HELP OF ls-uom IN FRAME D-Dialog /* Cost UOM */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    run windows/l-stduom.w (g_company,lv-uom-list,ls-uom:screen-value, output char-val).
    IF char-val <> "" then self:screen-value = entry(1,char-val).
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ls-uom D-Dialog
ON LEAVE OF ls-uom IN FRAME D-Dialog /* Cost UOM */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-uom NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME fiLoc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLoc D-Dialog
ON HELP OF fiLoc IN FRAME D-Dialog /* Cost UOM */
DO:
    DEF VAR cReturn AS cha NO-UNDO.

    run windows/l-loc.w (g_company,fiLoc:SCREEN-VALUE, OUTPUT cReturn).
    IF cReturn <> "" then SELF:SCREEN-VALUE  = ENTRY(1,cReturn).
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLoc D-Dialog
ON LEAVE OF fiLoc IN FRAME D-Dialog /* Cost UOM */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-loc NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME fiLocBin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLocBin D-Dialog
ON HELP OF fiLocBin IN FRAME D-Dialog /* Cost UOM */
DO:
    DEF VAR cReturn AS cha NO-UNDO.

    run windows/l-locbin.w (g_company,fiLoc:SCREEN-VALUE, "", OUTPUT cReturn).
    IF cReturn <> "" then SELF:SCREEN-VALUE  = ENTRY(1,cReturn).
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLocBin D-Dialog
ON LEAVE OF fiLocBin IN FRAME D-Dialog /* Cost UOM */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-loc-bin NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */
assign ls-i-no = iop-i-no
       ls-part-no = iop-part-no
       ls-uom = IF ip-est-no EQ "" AND iop-uom NE "" THEN iop-uom ELSE "M"
       fiLoc = iopcLoc
       fiLocBin = iopcLocBin.
       
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
  DISPLAY ls-i-no ls-part-no ls-uom fiLoc fiLocBin 
      WITH FRAME D-Dialog.
  ENABLE ls-part-no ls-uom fiLoc fiLocBin Btn_OK Btn_Cancel 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable D-Dialog 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    IF ip-est-no NE "" THEN DISABLE ls-uom.
  END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-part-no D-Dialog 
PROCEDURE valid-part-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:

    IF ls-part-no:SCREEN-VALUE EQ "" THEN
       ASSIGN ls-part-no:SCREEN-VALUE = ls-i-no:SCREEN-VALUE .

    RUN sys/inc/valpart#.p (ls-part-no:SCREEN-VALUE,
                            ls-i-no:SCREEN-VALUE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN ERROR.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-loc D-Dialog 
PROCEDURE valid-loc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    fiLoc:SCREEN-VALUE = CAPS(fiLoc:SCREEN-VALUE).
    FIND FIRST loc NO-LOCK 
        WHERE loc.company EQ g_company 
        AND loc.loc EQ fiLoc:SCREEN-VALUE
        NO-ERROR.
    IF NOT AVAIL loc THEN DO:
      MESSAGE fiLoc:SCREEN-VALUE " is an invalid warehouse"
              VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-loc D-Dialog 
PROCEDURE valid-loc-bin :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    fiLoc:SCREEN-VALUE = CAPS(fiLoc:SCREEN-VALUE).
    FIND FIRST fg-bin NO-LOCK 
        WHERE fg-bin.company EQ g_company 
        AND fg-bin.loc EQ fiLoc:SCREEN-VALUE
        AND fg-bin.i-no EQ ""
        AND fg-bin.loc-bin EQ fiLocBin:SCREEN-VALUE 
        NO-ERROR.
    IF NOT AVAIL fg-bin THEN DO:
      MESSAGE fiLocBin:SCREEN-VALUE " is a not valid bin for warehouse " fiLoc:SCREEN-VALUE 
              VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-uom D-Dialog 
PROCEDURE valid-uom :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    ls-uom:SCREEN-VALUE = CAPS(ls-uom:SCREEN-VALUE).
    FIND FIRST uom
        WHERE uom.uom EQ ls-uom:SCREEN-VALUE
          AND LOOKUP(uom.uom,lv-uom-list) NE 0
        NO-LOCK NO-ERROR.
    IF NOT AVAIL uom THEN DO:
      MESSAGE "Invalid Unit Of Measure, Must Enter - C, CS, L, M, or EA..."
              VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

