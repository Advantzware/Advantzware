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
DEF INPUT        PARAM ip-rowid AS ROWID NO-UNDO.
DEF INPUT-OUTPUT PARAM io-price AS DEC NO-UNDO.

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

DEF VAR ld-msf AS DEC NO-UNDO.
DEF VAR ld-yld AS DEC NO-UNDO.

{cec/msfcalc.i}

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
&Scoped-Define ENABLED-OBJECTS RECT-26 fi_prmsf fi_setup Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS fi_msf fi_prmsf fi_setup fi_price 

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

DEFINE VARIABLE fi_msf AS DECIMAL FORMAT "->>,>>>9.9<<<<<":U INITIAL 0 
     LABEL "Total MSF" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE fi_price AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "New Price" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi_prmsf AS DECIMAL FORMAT "->>,>>9.99<<<":U INITIAL 0 
     LABEL "Price per MSF" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi_setup AS DECIMAL FORMAT "->>,>>9.99<<<":U INITIAL 0 
     LABEL "Setup Charge" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1
     FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-26
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 67 BY 7.14.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     fi_msf AT ROW 2.67 COL 31 COLON-ALIGNED
     fi_prmsf AT ROW 4.1 COL 31 COLON-ALIGNED
     fi_setup AT ROW 5.29 COL 31 COLON-ALIGNED
     fi_price AT ROW 6.71 COL 31 COLON-ALIGNED
     Btn_OK AT ROW 8.86 COL 16
     Btn_Cancel AT ROW 8.86 COL 39
     "Calculate New Sell Price by entering the fields below..." VIEW-AS TEXT
          SIZE 65 BY 1 AT ROW 1.24 COL 2
          FONT 6
     RECT-26 AT ROW 1 COL 1
     SPACE(0.00) SKIP(2.23)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "Per MSF Sell Price"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
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

/* SETTINGS FOR FILL-IN fi_msf IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_price IN FRAME D-Dialog
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
ON WINDOW-CLOSE OF FRAME D-Dialog /* Per MSF Sell Price */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK D-Dialog
ON CHOOSE OF Btn_OK IN FRAME D-Dialog /* OK */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

  io-price = fi_price.

  RUN reftable-values (NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_prmsf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_prmsf D-Dialog
ON VALUE-CHANGED OF fi_prmsf IN FRAME D-Dialog /* Price per MSF */
DO:
  RUN new-price.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_setup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_setup D-Dialog
ON VALUE-CHANGED OF fi_setup IN FRAME D-Dialog /* Setup Charge */
DO:
  RUN new-price.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */
DEF VAR ld-sqi AS DEC NO-UNDO.
DEF VAR ld-len AS DEC NO-UNDO.
DEF VAR ld-wid AS DEC NO-UNDO.


FIND probe WHERE ROWID(probe) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL probe THEN DO:
  FOR EACH ef NO-LOCK
      WHERE ef.company EQ probe.company
        AND ef.est-no  EQ probe.est-no,
      EACH eb NO-LOCK
      WHERE eb.company EQ ef.company
        AND eb.est-no  EQ ef.est-no
        AND eb.form-no EQ ef.form-no,
      FIRST style NO-LOCK
      WHERE style.company EQ eb.company
        AND style.style   EQ eb.style: 
     
      ASSIGN
       ld-len = ((style.sqft-len-trim - TRUNC(style.sqft-len-trim,0)) * 6.25) +
                TRUNC(style.sqft-len-trim,0)
       ld-wid = ((style.sqft-wid-trim - TRUNC(style.sqft-wid-trim,0)) * 6.25) +
                TRUNC(style.sqft-wid-trim,0).

    ld-sqi = eb.t-sqin / (eb.t-len * eb.t-wid).

    IF ld-sqi LE 0 THEN ld-sqi = 1.

    ASSIGN
     ld-sqi = ((eb.t-len * ld-sqi) + ld-len) * ((eb.t-wid * ld-sqi) + ld-wid)
     ld-yld = IF eb.est-type EQ 5 THEN 1 ELSE
              IF eb.quantityPerSet LT 0 THEN (-1 / eb.quantityPerSet) ELSE eb.quantityPerSet
     ld-msf = ld-msf + (ld-sqi * probe.est-qty * ld-yld).
  END.
  IF ld-msf NE 0 THEN
    IF v-corr THEN ld-msf = ld-msf * .000007.
              ELSE ld-msf = ld-msf / 144000.

  RUN reftable-values (YES).

  ASSIGN
   fi_price = io-price
   fi_msf   = ld-msf.
  
  {src/adm/template/dialogmn.i}
END.

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
  DISPLAY fi_msf fi_prmsf fi_setup fi_price 
      WITH FRAME D-Dialog.
  ENABLE RECT-26 fi_prmsf fi_setup Btn_OK Btn_Cancel 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-price D-Dialog 
PROCEDURE new-price :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    fi_price:SCREEN-VALUE = STRING(ROUND(((DEC(fi_prmsf:SCREEN-VALUE) * ld-msf) +
                                        DEC(fi_setup:SCREEN-VALUE)) /
                                       (probe.est-qty / 1000),2)).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reftable-values D-Dialog 
PROCEDURE reftable-values :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-display AS LOG NO-UNDO.


  FIND FIRST reftable
      WHERE reftable.reftable EQ "probe.per-msf"
        AND reftable.company  EQ probe.company
        AND reftable.loc      EQ ""
        AND reftable.code     EQ probe.est-no
        AND reftable.code2    EQ STRING(probe.line,"9999999999")
      NO-ERROR.
  IF NOT AVAIL reftable THEN DO:
    CREATE reftable.
    ASSIGN
     reftable.reftable = "probe.per-msf"
     reftable.company  = probe.company
     reftable.loc      = ""
     reftable.code     = probe.est-no
     reftable.code2    = STRING(probe.line,"9999999999")
     reftable.val[1]   = io-price * (probe.est-qty / 1000) / ld-msf * 100000
     reftable.val[2]   = 0.
  END.
  IF ip-display THEN
    ASSIGN
     fi_setup = reftable.val[2] / 100000
     fi_prmsf = ((io-price * (probe.est-qty / 1000)) - fi_setup) / ld-msf.
  ELSE
    ASSIGN
     reftable.val[1] = fi_prmsf * 100000
     reftable.val[2] = fi_setup * 100000.

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

