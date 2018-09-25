&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
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
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
DEFINE OUTPUT PARAMETER opcReasonCode AS CHARACTER NO-UNDO.
DEFINE VARIABLE hPgmReason AS HANDLE NO-UNDO.
/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME D-Dialog

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES rm-bin

/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define FIELDS-IN-QUERY-D-Dialog rm-bin.qty rm-bin.cost 
&Scoped-define ENABLED-FIELDS-IN-QUERY-D-Dialog rm-bin.qty rm-bin.cost 
&Scoped-define ENABLED-TABLES-IN-QUERY-D-Dialog rm-bin
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-D-Dialog rm-bin
&Scoped-define QUERY-STRING-D-Dialog FOR EACH rm-bin SHARE-LOCK
&Scoped-define OPEN-QUERY-D-Dialog OPEN QUERY D-Dialog FOR EACH rm-bin SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-D-Dialog rm-bin
&Scoped-define FIRST-TABLE-IN-QUERY-D-Dialog rm-bin


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS rm-bin.qty rm-bin.cost 
&Scoped-define ENABLED-TABLES rm-bin
&Scoped-define FIRST-ENABLED-TABLE rm-bin
&Scoped-Define ENABLED-OBJECTS tb_last-cost cb_reatype Btn_OK Btn_Cancel RECT-27 
&Scoped-Define DISPLAYED-FIELDS rm-bin.qty rm-bin.cost 
&Scoped-define DISPLAYED-TABLES rm-bin
&Scoped-define FIRST-DISPLAYED-TABLE rm-bin
&Scoped-Define DISPLAYED-OBJECTS tb_last-cost cb_reatype

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

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 75 BY 4.90.

DEFINE VARIABLE tb_last-cost AS LOGICAL INITIAL no 
     LABEL "Move to RM Last Cost?" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.

DEFINE VARIABLE cb_reatype AS CHARACTER FORMAT "X(256)":U 
     LABEL "Adjustment Reason" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 30 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY D-Dialog FOR 
      rm-bin SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     rm-bin.qty AT ROW 1.48 COL 32 COLON-ALIGNED
          LABEL "On-Hand Qty" FORMAT "->>>,>>>,>>9.9<<<<<"
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     rm-bin.cost AT ROW 2.43 COL 32 COLON-ALIGNED
          LABEL "Avg Cost" FORMAT "->>>,>>>,>>9.99<<<<"
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     tb_last-cost AT ROW 3.62 COL 34
     cb_reatype AT ROW 4.62 COL 32 COLON-ALIGNED 
     Btn_OK AT ROW 6.20 COL 18
     Btn_Cancel AT ROW 6.20 COL 47
     RECT-27 AT ROW 1 COL 1
     SPACE(0.59) SKIP(2.61)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "RM Bin Update"
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
                                                                        */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN rm-bin.cost IN FRAME D-Dialog
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN rm-bin.qty IN FRAME D-Dialog
   EXP-LABEL EXP-FORMAT                                                 */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _TblList          = "asi.rm-bin"
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* RM Bin Update */
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
  apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK D-Dialog
ON CHOOSE OF Btn_OK IN FRAME D-Dialog /* OK */
DO:
  FIND CURRENT rm-bin.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     rm-bin.qty
     rm-bin.cost
     tb_last-cost
     cb_reatype .
  END.

  IF tb_last-cost THEN DO:
    FIND FIRST item
        WHERE item.company EQ rm-bin.company
          AND item.i-no    EQ rm-bin.i-no
        NO-ERROR.
    IF AVAIL item THEN item.last-cost = rm-bin.cost.
  END.

  FIND CURRENT rm-bin NO-LOCK.
  ASSIGN opcReasonCode = cb_reatype:SCREEN-VALUE IN FRAME {&frame-name} .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */
FIND rm-bin NO-LOCK WHERE ROWID(rm-bin) EQ ip-rowid NO-ERROR.

IF AVAIL rm-bin THEN DO:

 RUN build-type-list .
 FOR EACH rm-rdtlh NO-LOCK
     WHERE rm-rdtlh.company = rm-bin.company
       AND rm-rdtlh.i-no    = rm-bin.i-no
       AND rm-rdtlh.loc     = rm-bin.loc
       AND rm-rdtlh.loc-bin = rm-bin.loc-bin
       AND rm-rdtlh.tag     = rm-bin.tag
       AND rm-rdtlh.reject-code[1] NE "",

    FIRST rm-rcpth NO-LOCK
      WHERE rm-rcpth.r-no         EQ rm-rdtlh.r-no
        AND rm-rcpth.rita-code    EQ rm-rdtlh.rita-code
        AND rm-rcpth.i-no         EQ rm-bin.i-no
        AND rm-rcpth.po-no        EQ TRIM(STRING(rm-bin.po-no,">>>>>>>>>>")) 
      BY rm-rcpth.trans-date :

    cb_reatype:SCREEN-VALUE IN FRAME {&frame-name} = rm-rdtlh.reject-code[1] NO-ERROR. 
    LEAVE .
END.


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
  DISPLAY tb_last-cost cb_reatype
      WITH FRAME D-Dialog.
  IF AVAILABLE rm-bin THEN 
    DISPLAY rm-bin.qty rm-bin.cost 
      WITH FRAME D-Dialog.
  ENABLE rm-bin.qty rm-bin.cost tb_last-cost cb_reatype Btn_OK Btn_Cancel RECT-27 
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

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "rm-bin"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-type-list D-Dialog 
PROCEDURE build-type-list :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   DEFINE VARIABLE cComboList AS CHARACTER NO-UNDO .
     
     RUN "fg/ReasonCode.p" PERSISTENT SET hPgmReason.
             RUN pBuildReasonCode IN hPgmReason ("ADJ",OUTPUT cComboList).
    DELETE OBJECT hPgmReason.

  DO WITH FRAME {&FRAME-NAME}:
      cb_reatype:LIST-ITEM-PAIRS = cComboList .
  END.

    
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

