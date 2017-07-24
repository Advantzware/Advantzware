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
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
DEF INPUT PARAM ip-die-no LIKE eb.die-no NO-UNDO.
DEF INPUT PARAM ip-cad-no LIKE eb.cad-no NO-UNDO.
DEF INPUT PARAM ip-cimage LIKE ef.cad-image NO-UNDO.
DEF INPUT PARAM ip-pgm-name AS CHAR NO-UNDO.
DEF INPUT PARAM ip-upd-die AS LOG NO-UNDO.

/* Local Variable Definitions ---                                       */
DEF VAR ll-combo AS LOG NO-UNDO.
DEF VAR ll-tandem AS LOG NO-UNDO.
DEF VAR ll-est AS LOG NO-UNDO.
DEF VAR ll-fgp AS LOG NO-UNDO.

DEF BUFFER b-eb FOR eb.
DEF BUFFER b-ef FOR ef.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME D-Dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-26 RECT-27 RECT-28 tb_est-die ~
tb_fgp-die tb_est-img tb_fgp-img tb_est-cad tb_fgp-cad Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS tb_est-die tb_fgp-die tb_est-img ~
tb_fgp-img tb_est-cad tb_fgp-cad 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 tb_est-die tb_fgp-die tb_est-img tb_fgp-img ~
tb_est-cad tb_fgp-cad 

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

DEFINE RECTANGLE RECT-26
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 102 BY 6.91.

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 23 BY 6.19.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 63 BY 6.19.

DEFINE VARIABLE tb_est-cad AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tb_est-die AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tb_est-img AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tb_fgp-cad AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tb_fgp-die AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tb_fgp-img AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     tb_est-die AT ROW 2.67 COL 26
     tb_fgp-die AT ROW 2.67 COL 69
     tb_est-img AT ROW 4.1 COL 26
     tb_fgp-img AT ROW 4.1 COL 69
     tb_est-cad AT ROW 5.52 COL 26
     tb_fgp-cad AT ROW 5.52 COL 69
     Btn_OK AT ROW 8.38 COL 31
     Btn_Cancel AT ROW 8.38 COL 63
     "Die#?" VIEW-AS TEXT
          SIZE 8 BY 1 AT ROW 2.67 COL 5
     "CAD#?" VIEW-AS TEXT
          SIZE 9 BY 1 AT ROW 5.52 COL 5
     "Image?" VIEW-AS TEXT
          SIZE 9 BY 1 AT ROW 4.1 COL 5
     "Within This Est#" VIEW-AS TEXT
          SIZE 19 BY 1 AT ROW 1.48 COL 17
     "Copy..." VIEW-AS TEXT
          SIZE 10 BY 1 AT ROW 1.24 COL 3
     "Other Single/Tandem Est with this FG Item/CustPart" VIEW-AS TEXT
          SIZE 60 BY 1 AT ROW 1.48 COL 39
     RECT-26 AT ROW 1 COL 1
     RECT-27 AT ROW 1.24 COL 15
     RECT-28 AT ROW 1.24 COL 38
     SPACE(2.00) SKIP(2.70)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "Die/CAD/Image Copy For Estimate:"
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
                                                                        */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR TOGGLE-BOX tb_est-cad IN FRAME D-Dialog
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX tb_est-die IN FRAME D-Dialog
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX tb_est-img IN FRAME D-Dialog
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX tb_fgp-cad IN FRAME D-Dialog
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX tb_fgp-die IN FRAME D-Dialog
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX tb_fgp-img IN FRAME D-Dialog
   1                                                                    */
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
ON WINDOW-CLOSE OF FRAME D-Dialog /* Die/CAD/Image Copy For Estimate: */
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
  DEF VAR ll AS LOG NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

  ll = tb_est-die OR tb_est-img OR tb_est-cad.
  
  IF ll THEN
  FOR EACH b-ef
      WHERE b-ef.company  EQ ef.company
        AND b-ef.est-no   EQ ef.est-no:

    IF tb_est-img AND ROWID(b-ef) NE ROWID(ef) THEN
      b-ef.cad-image = ef.cad-image.

    FOR EACH b-eb
        WHERE b-eb.company  EQ b-ef.company
          AND b-eb.est-no   EQ b-ef.est-no
          AND ROWID(b-eb)   NE ROWID(eb):

      IF tb_est-die THEN b-eb.die-no = eb.die-no.
      IF tb_est-cad THEN b-eb.cad-no = eb.cad-no.

      IF b-eb.stock-no NE "" THEN RUN update-itemfg (1).
    END.
  END.

  ll = tb_fgp-die OR tb_fgp-img OR tb_fgp-cad.

  IF ll THEN DO:
  
    IF AVAIL itemfg THEN
    FOR EACH b-eb
        WHERE b-eb.company  EQ itemfg.company
          AND b-eb.stock-no EQ itemfg.i-no
          AND ROWID(b-eb)   NE ROWID(eb),
        FIRST b-ef
        WHERE b-ef.company EQ b-eb.company
          AND b-ef.est-no  EQ b-eb.est-no
          AND b-ef.form-no EQ b-eb.form-no:

      IF tb_fgp-die THEN b-eb.die-no = eb.die-no.
      IF tb_est-img THEN b-ef.cad-image = ef.cad-image.
      IF tb_fgp-cad THEN b-eb.cad-no = eb.cad-no.

      IF b-eb.stock-no NE "" THEN RUN update-itemfg (2).
    END.

    ELSE
    FOR EACH b-eb
        WHERE b-eb.company  EQ eb.company
          AND b-eb.cust-no  EQ eb.cust-no
          AND b-eb.part-no  EQ eb.part-no
          AND b-eb.stock-no EQ ""
          AND ROWID(b-eb)   NE ROWID(eb),
        FIRST b-ef
        WHERE b-ef.company EQ b-eb.company
          AND b-ef.est-no  EQ b-eb.est-no
          AND b-ef.form-no EQ b-eb.form-no:

      IF tb_fgp-die THEN b-eb.die-no = eb.die-no.
      IF tb_est-img THEN b-ef.cad-image = ef.cad-image.
      IF tb_fgp-cad THEN b-eb.cad-no = eb.cad-no.

      IF b-eb.stock-no NE "" THEN RUN update-itemfg (2).
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */
DEF VAR ll AS LOG NO-UNDO.


FIND eb WHERE ROWID(eb) EQ ip-rowid NO-LOCK NO-ERROR.

DO TRANSACTION:
  IF AVAIL eb AND eb.stock-no NE "" THEN
  FIND FIRST itemfg
      WHERE itemfg.company EQ eb.company
        AND itemfg.i-no    EQ eb.stock-no
        AND itemfg.est-no  EQ eb.est-no
      NO-ERROR.

  IF AVAIL itemfg THEN
    ASSIGN
     itemfg.die-no = eb.die-no
     itemfg.cad-no = eb.cad-no.

  ELSE
  FIND itemfg WHERE ROWID(itemfg) EQ ip-rowid NO-ERROR.

  IF AVAIL itemfg AND NOT AVAIL eb THEN DO:
    ll = YES.
    FIND FIRST eb
        WHERE eb.company  EQ itemfg.company
          AND eb.est-no   EQ itemfg.est-no
          AND eb.stock-no EQ itemfg.i-no
        NO-ERROR.

    IF AVAIL eb THEN DO:
      IF ip-upd-die THEN eb.die-no = itemfg.die-no.
      eb.cad-no = itemfg.cad-no.
    END.
  END.
END.

FIND CURRENT eb NO-LOCK NO-ERROR.
FIND CURRENT itemfg NO-LOCK NO-ERROR.

IF AVAIL eb THEN
FIND FIRST ef
    WHERE ef.company EQ eb.company
      AND ef.est-no  EQ eb.est-no
    NO-LOCK NO-ERROR.

IF AVAIL ef THEN
FIND FIRST est
    WHERE est.company EQ ef.company
      AND est.est-no  EQ ef.est-no
    NO-LOCK NO-ERROR.

IF AVAIL est THEN
  ASSIGN
   ll-est = CAN-FIND(FIRST b-eb
                     WHERE b-eb.company  EQ eb.company
                       AND b-eb.est-no   EQ eb.est-no
                       AND ROWID(b-eb)   NE ROWID(eb))
   ll-fgp = (AVAIL itemfg AND
             CAN-FIND(FIRST b-eb
                      WHERE b-eb.company  EQ eb.company
                        AND b-eb.stock-no EQ eb.stock-no
                        AND ROWID(b-eb)   NE ROWID(eb))) OR
            CAN-FIND(FIRST b-eb
                     WHERE b-eb.company  EQ eb.company
                       AND b-eb.cust-no  EQ eb.cust-no
                       AND b-eb.part-no  EQ eb.part-no
                       AND b-eb.stock-no EQ ""
                       AND ROWID(b-eb)   NE ROWID(eb)).

IF ll-est OR ll-fgp THEN DO:
  FRAME {&FRAME-NAME}:TITLE = TRIM(FRAME {&FRAME-NAME}:TITLE) +
                              " " + TRIM(est.est-no).

  IF ll THEN ip-cimage = ef.cad-image.

  ll-combo = est.est-type EQ 4 OR est.est-type EQ 8.

  IF ll-combo THEN DO:
    RUN ce/com/istandem.p (ROWID(est), OUTPUT ll-tandem).
    IF ll-tandem THEN ll-combo = NO.
  END.

  ELSE ll-combo = (est.est-type EQ 2 OR est.est-type EQ 6) AND
                  CAN-FIND(FIRST b-eb
                           WHERE b-eb.company  EQ eb.company
                             AND b-eb.est-no   EQ eb.est-no
                             AND b-eb.form-no  NE 0
                             AND ROWID(b-eb)   NE ROWID(eb)).

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
  DISPLAY tb_est-die tb_fgp-die tb_est-img tb_fgp-img tb_est-cad tb_fgp-cad 
      WITH FRAME D-Dialog.
  ENABLE RECT-26 RECT-27 RECT-28 tb_est-die tb_fgp-die tb_est-img tb_fgp-img 
         tb_est-cad tb_fgp-cad Btn_OK Btn_Cancel 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize D-Dialog 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    DISABLE {&List-1}.

    IF ip-pgm-name = "itemfg" THEN
      tb_est-die:SCREEN-VALUE = STRING(ip-upd-die).

    IF ll-est THEN DO:
      IF ll-combo THEN
        ASSIGN
         tb_est-die:SENSITIVE = ip-die-no NE eb.die-no
         tb_est-img:SENSITIVE = ip-cimage NE ef.cad-image AND
                                CAN-FIND(FIRST b-ef
                                         WHERE b-ef.company EQ ef.company
                                           AND b-ef.est-no  EQ ef.est-no
                                           AND ROWID(b-ef)  NE ROWID(ef)).

      IF ll-combo OR ll-tandem THEN                       
        tb_est-cad:SENSITIVE = ip-cad-no NE eb.cad-no.
    END.

    IF ll-fgp THEN
      ASSIGN
       tb_fgp-die:SENSITIVE = NOT ll-combo              OR
                              ip-die-no NE eb.die-no    OR
                              ll-tandem
       tb_fgp-img:SENSITIVE = NOT ll-combo              OR
                              ip-cimage NE ef.cad-image OR
                              ll-tandem
       tb_fgp-cad:SENSITIVE = NOT ll-combo              OR
                              ip-cad-no NE eb.cad-no    OR
                              ll-tandem.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-itemfg D-Dialog 
PROCEDURE update-itemfg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-int AS INT NO-UNDO.

  DEF BUFFER b-itemfg FOR itemfg.

  FOR EACH b-itemfg
      WHERE b-itemfg.company EQ b-eb.company
        AND b-itemfg.i-no    EQ b-eb.stock-no
        AND b-itemfg.est-no  EQ b-eb.est-no
        AND ROWID(b-itemfg)  NE ROWID(itemfg):
    IF (ip-int EQ 1 AND tb_est-die) OR 
       (ip-int EQ 2 AND tb_fgp-die) THEN b-itemfg.die-no = b-eb.die-no.

    IF (ip-int EQ 1 AND tb_est-cad) OR 
       (ip-int EQ 2 AND tb_fgp-cad) THEN b-itemfg.cad-no = b-eb.cad-no.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

