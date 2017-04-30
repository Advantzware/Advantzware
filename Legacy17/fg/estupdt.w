&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: fg/estupdt.w

  Description: update selected est fields from fg item

  Input Parameters: itemfg rowid, original part no

  Output Parameters: <none>

  Author: Ron Stark

  Created: 9.12.2005
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

&IF DEFINED(UIB_is_Running) EQ 0 &THEN
DEFINE INPUT PARAMETER ipRowID AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER ipPartNo AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opDieNo AS LOG NO-UNDO.
&ELSE
DEFINE VARIABLE ipRowID AS ROWID NO-UNDO.
DEFINE VARIABLE ipPartNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE opDieNo AS LOG NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}

{sys/inc/var.i new shared}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS partNo partDscr1 partDscr2 dieNo plateNo ~
spcNo upcNo proCat Btn_OK 
&Scoped-Define DISPLAYED-OBJECTS partNo partDscr1 partDscr2 dieNo plateNo ~
spcNo upcNo proCat 

/* Custom List Definitions                                              */
/* orderFields,List-2,List-3,List-4,List-5,List-6                       */
&Scoped-define orderFields partNo partDscr1 partDscr2 dieNo plateNo spcNo ~
upcNo proCat 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 41 BY 8.1.

DEFINE VARIABLE dieNo AS LOGICAL INITIAL no 
     LABEL "Die Number" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .81 NO-UNDO.

DEFINE VARIABLE partDscr1 AS LOGICAL INITIAL no 
     LABEL "Item Name" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE partDscr2 AS LOGICAL INITIAL no 
     LABEL "Part Description" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE partNo AS LOGICAL INITIAL no 
     LABEL "Part Number" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .81 NO-UNDO.

DEFINE VARIABLE plateNo AS LOGICAL INITIAL no 
     LABEL "Plate Number" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .81 NO-UNDO.

DEFINE VARIABLE proCat AS LOGICAL INITIAL no 
     LABEL "Category" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .81 NO-UNDO.

DEFINE VARIABLE spcNo AS LOGICAL INITIAL no 
     LABEL "SPC/QC Number" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .81 NO-UNDO.

DEFINE VARIABLE upcNo AS LOGICAL INITIAL no 
     LABEL "UPC Number" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     partNo AT ROW 2.19 COL 11 HELP
          "Select to Update Estimate's Part Number"
     partDscr1 AT ROW 3.14 COL 11 HELP
          "Select to Update Estimate's Part Description 1"
     partDscr2 AT ROW 4.1 COL 11 HELP
          "Select to Update Estimate's Part Description 2"
     dieNo AT ROW 5.05 COL 11 HELP
          "Select to Update Estimate's Die Number"
     plateNo AT ROW 6 COL 11 HELP
          "Select to Update Estimate's Plate Number"
     spcNo AT ROW 6.95 COL 11 HELP
          "Select to Update Estimate's SPC/QC Number"
     upcNo AT ROW 7.91 COL 11 HELP
          "Select to Update Estimate's UPC Number"
     proCat AT ROW 8.86 COL 11 HELP
          "Select to Update Estimate's Category" WIDGET-ID 2
     Btn_OK AT ROW 10.52 COL 15
     "Select Est. Information to Update:" VIEW-AS TEXT
          SIZE 32 BY .62 AT ROW 1.24 COL 3
     RECT-11 AT ROW 1.95 COL 2
     SPACE(0.00) SKIP(1.94)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Update Estimates"
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
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR TOGGLE-BOX dieNo IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX partDscr1 IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX partDscr2 IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX partNo IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX plateNo IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX proCat IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR RECTANGLE RECT-11 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX spcNo IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX upcNo IN FRAME Dialog-Frame
   1                                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Update Estimates */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  DEF BUFFER b-eb FOR eb.


  ASSIGN {&orderFields}
         opDieNo = dieNo.

  FIND itemfg NO-LOCK WHERE ROWID(itemfg) EQ ipRowID NO-ERROR.

  IF AVAILABLE itemfg THEN DO:
   FOR EACH eb NO-LOCK
       WHERE eb.company EQ itemfg.company
         AND eb.cust-no EQ itemfg.cust-no
         AND ((eb.part-no EQ ipPartNo
         AND   eb.stock-no EQ "")
          OR   eb.stock-no EQ itemfg.i-no):
     FIND b-eb WHERE ROWID(b-eb) EQ ROWID(eb) EXCLUSIVE NO-WAIT NO-ERROR.
     IF AVAIL b-eb THEN DO:
       IF partNo THEN b-eb.part-no = itemfg.part-no.
       IF partDscr1 THEN b-eb.part-dscr1 = itemfg.i-name.
       IF partDscr2 THEN b-eb.part-dscr2 = itemfg.part-dscr1.
       IF plateNo THEN b-eb.plate-no = itemfg.plate-no.
       IF spcNo THEN b-eb.spc-no = itemfg.spc-no.
       IF upcNo THEN b-eb.upc-no = itemfg.upc-no.
       IF proCat THEN b-eb.procat = itemfg.procat.
     END.
   END. /* each eb */
  END. /* if avail */
  RUN custom/usrprint.p (v-prgmname,FRAME {&FRAME-NAME}:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

{custom/getcmpny.i}
{custom/getloc.i}

ASSIGN
 cocode = gcompany
 locode = gloc.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  DO WITH FRAME {&FRAME-NAME}:
    {custom/usrprint.i}
  END.
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
  DISPLAY partNo partDscr1 partDscr2 dieNo plateNo spcNo upcNo proCat 
      WITH FRAME Dialog-Frame.
  ENABLE partNo partDscr1 partDscr2 dieNo plateNo spcNo upcNo proCat Btn_OK 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

