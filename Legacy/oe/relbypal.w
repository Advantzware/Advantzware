&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF INPUT  PARAM ip-rowid AS ROWID NO-UNDO.
DEF OUTPUT PARAM op-success AS LOG NO-UNDO.

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

DEF VAR li-max-pal AS INT NO-UNDO.

{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES oe-rel

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame oe-rel.ord-no oe-rel.i-no ~
oe-rel.po-no 
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH oe-rel SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH oe-rel SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame oe-rel
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame oe-rel


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi_pallets Btn_OK RECT-8 
&Scoped-Define DISPLAYED-FIELDS oe-rel.ord-no oe-rel.i-no oe-rel.po-no 
&Scoped-define DISPLAYED-TABLES oe-rel
&Scoped-define FIRST-DISPLAYED-TABLE oe-rel
&Scoped-Define DISPLAYED-OBJECTS fi_job-no fi_pallets fi_qty 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE fi_job-no AS CHARACTER FORMAT "x(9)" 
     LABEL "Job#" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1.

DEFINE VARIABLE fi_pallets AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Pallets" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1 NO-UNDO.

DEFINE VARIABLE fi_qty AS INTEGER FORMAT "->>>,>>>,>>9":U INITIAL 0 
     LABEL "Qty" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 59 BY 6.67.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      oe-rel SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     oe-rel.ord-no AT ROW 1.48 COL 17 COLON-ALIGNED
          LABEL "Order#" FORMAT ">>>>>>>>"
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     oe-rel.i-no AT ROW 2.43 COL 17 COLON-ALIGNED
          LABEL "FG Item#"
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     oe-rel.po-no AT ROW 3.38 COL 17 COLON-ALIGNED
          LABEL "Cust PO#"
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     fi_job-no AT ROW 4.33 COL 17 COLON-ALIGNED HELP
          "Enter the Job Number for this release line"
     fi_pallets AT ROW 5.29 COL 17 COLON-ALIGNED HELP
          "Enter # of Pallets for this release"
     fi_qty AT ROW 6.24 COL 17 COLON-ALIGNED HELP
          "Enter # of Pallets for this release"
     Btn_OK AT ROW 8.14 COL 22
     RECT-8 AT ROW 1 COL 1
     SPACE(0.59) SKIP(1.99)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "Create Release by Pallet"
         DEFAULT-BUTTON Btn_OK.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
                                                                        */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fi_job-no IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_qty IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN oe-rel.i-no IN FRAME Dialog-Frame
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN oe-rel.ord-no IN FRAME Dialog-Frame
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN oe-rel.po-no IN FRAME Dialog-Frame
   NO-ENABLE EXP-LABEL                                                  */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "asi.oe-rel"
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Create Release by Pallet */
DO:
  op-success = NO.

  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  RUN new-qty ("OK").

  op-success = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_pallets
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_pallets Dialog-Frame
ON VALUE-CHANGED OF fi_pallets IN FRAME Dialog-Frame /* Pallets */
DO:
  RUN new-qty (IF INT({&self-name}:SCREEN-VALUE) GT li-max-pal THEN "MAIN"
                                                               ELSE "PALLETS").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent. */  
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW. 


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  FIND oe-rel WHERE ROWID(oe-rel) EQ ip-rowid NO-ERROR.
  IF NOT AVAIL oe-rel THEN RETURN.

  FIND FIRST oe-ordl OF oe-rel NO-LOCK NO-ERROR.

  IF AVAIL oe-ordl THEN DO:
    fi_job-no = TRIM(oe-ordl.job-no) + "-" + STRING(oe-ordl.job-no2,"99").
    IF fi_job-no EQ "-00" THEN fi_job-no = "".
  END.

  RUN new-qty ("MAIN").

  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY fi_job-no fi_pallets fi_qty 
      WITH FRAME Dialog-Frame.
  IF AVAILABLE oe-rel THEN 
    DISPLAY oe-rel.ord-no oe-rel.i-no oe-rel.po-no 
      WITH FRAME Dialog-Frame.
  ENABLE fi_pallets Btn_OK RECT-8 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-qty Dialog-Frame 
PROCEDURE new-qty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-process AS CHAR NO-UNDO.

  DEF VAR li-pallets AS INT NO-UNDO.
  DEF VAR ld-paldec AS DEC NO-UNDO.
  DEF VAR li-pal-ent AS INT NO-UNDO.
  DEF VAR li-unit AS INT NO-UNDO.
  DEF VAR li-qty AS INT NO-UNDO.
  DEF VAR li-diff AS INT NO-UNDO.
  DEF VAR li AS INT NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     li-pallets = 0
     li-pal-ent = INT(fi_pallets:SCREEN-VALUE).

    FOR EACH oe-rell
        WHERE oe-rell.company  EQ oe-rel.company
          AND oe-rell.ord-no   EQ oe-rel.ord-no
          AND oe-rell.rel-no   EQ oe-rel.rel-no
          AND oe-rell.b-ord-no EQ oe-rel.b-ord-no
          AND oe-rell.i-no     EQ oe-rel.i-no
          AND oe-rell.line     EQ oe-rel.line
          AND CAN-FIND(FIRST oe-relh
                       WHERE oe-relh.r-no    EQ oe-rell.r-no
                         AND oe-relh.posted  EQ NO
                         AND oe-relh.deleted EQ NO)
        BY oe-rell.rec_key:

      FIND FIRST fg-bin
          WHERE fg-bin.company EQ oe-rell.company
            AND fg-bin.i-no    EQ oe-rell.i-no
            AND fg-bin.job-no  EQ oe-rell.job-no
            AND fg-bin.job-no2 EQ oe-rell.job-no2
            AND fg-bin.loc     EQ oe-rell.loc
            AND fg-bin.loc-bin EQ oe-rell.loc-bin
            AND fg-bin.tag     EQ oe-rell.tag
          NO-LOCK NO-ERROR.

      ASSIGN
       li-unit   = IF AVAIL fg-bin AND
                      fg-bin.cases-unit NE 0 AND fg-bin.case-count NE 0 THEN
                     (fg-bin.cases-unit * fg-bin.case-count)
                   ELSE oe-rell.qty-case
       ld-paldec = ((oe-rell.qty - oe-rell.partial) / li-unit) +
                   INT(oe-rell.partial GT 0).

      {sys/inc/roundup.i ld-paldec}

      ASSIGN
       li-pallets = li-pallets + ld-paldec.
       li-qty     = li-qty + oe-rell.qty.

      IF ip-process NE "MAIN"     AND
         li-pallets GE li-pal-ent THEN DO:

        ASSIGN
         li-diff    = li-pallets - li-pal-ent - INT(oe-rell.partial NE 0)
         li-pallets = li-pal-ent.

        IF ip-process EQ "OK" THEN
          ASSIGN
           oe-rell.qty     = oe-rell.qty - (li-diff * li-unit) - oe-rell.partial
           oe-rell.partial = 0
           oe-rell.cases   = TRUNC(oe-rell.qty / oe-rell.qty-case,0).

        ELSE li-qty = li-qty - (li-diff * li-unit) - oe-rell.partial.

        IF ip-process NE "OK" THEN LEAVE.

        ELSE
        IF oe-rell.qty LE 0 THEN DELETE oe-rell.
      END.
    END.
                 
    IF ip-process EQ "MAIN" THEN DO:
      ASSIGN
       fi_pallets              = li-pallets
       fi_pallets:SCREEN-VALUE = STRING(fi_pallets)
       li-max-pal              = li-pallets
       fi_qty                  = li-qty
       fi_qty:SCREEN-VALUE     = STRING(fi_qty).

      APPLY "entry" TO fi_pallets.
    END.

    ELSE
    IF ip-process NE "OK" THEN fi_qty:SCREEN-VALUE = STRING(li-qty).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

