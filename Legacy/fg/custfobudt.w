&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: fg/custfobudt.w

  Description: update selected  fields from cust

  Input Parameters: 

  Output Parameters: <none>

  Author: Ron Stark

  Created: 1.13.2019
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

&IF DEFINED(UIB_is_Running) EQ 0 &THEN
DEFINE INPUT PARAMETER ipRowID AS ROWID NO-UNDO.
&ELSE
DEFINE VARIABLE ipRowID AS ROWID NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */

{methods/defines/hndldefs.i}

{custom/gcompany.i}
{custom/gloc.i}

{sys/inc/var.i new shared}

{system/sysconst.i}
{methods/defines/globdefs.i}

DEFINE BUFFER b-prgrms FOR prgrms.

DEFINE VARIABLE v-prgmname   LIKE b-prgrms.prgmname NO-UNDO.
DEFINE VARIABLE period_pos   AS INTEGER NO-UNDO.
DEFINE VARIABLE num-groups   AS INTEGER NO-UNDO.
DEFINE VARIABLE group-ok     AS LOGICAL NO-UNDO.
DEFINE VARIABLE access-close AS LOGICAL NO-UNDO.

IF INDEX(PROGRAM-NAME(1),".uib") NE 0 OR
   INDEX(PROGRAM-NAME(1),".ab")  NE 0 OR
   INDEX(PROGRAM-NAME(1),".ped") NE 0 THEN
    v-prgmname = USERID("ASI") + "..".
ELSE
    ASSIGN
        v-prgmname = SUBSTRING(PROGRAM-NAME(1), R-INDEX(PROGRAM-NAME(1), "/") + 1)
        v-prgmname = SUBSTR(v-prgmname,1,INDEX(v-prgmname,"."))
        .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tb-open_order tb-un_bol tb-un_inv Btn_OK 
&Scoped-Define DISPLAYED-OBJECTS tb-open_order tb-un_bol tb-un_inv 

/* Custom List Definitions                                              */
/* orderFields,List-2,List-3,List-4,List-5,List-6                       */
&Scoped-define orderFields tb-open_order tb-un_bol tb-un_inv 

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
     SIZE 43 BY 5.24.

DEFINE VARIABLE tb-open_order AS LOGICAL INITIAL no 
     LABEL "Open Orders" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.

DEFINE VARIABLE tb-un_bol AS LOGICAL INITIAL no 
     LABEL "Unposted BOL's" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .81 NO-UNDO.

DEFINE VARIABLE tb-un_inv AS LOGICAL INITIAL no 
     LABEL "Unposted Invoices" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     tb-open_order AT ROW 2.52 COL 11
     tb-un_bol AT ROW 3.62 COL 11 HELP
          "Select to Update BOL"
     tb-un_inv AT ROW 4.86 COL 11 HELP
          "Select to Update Invoice "
     Btn_OK AT ROW 7.43 COL 15
     "Select Customer Information to Update:" VIEW-AS TEXT
          SIZE 37 BY .62 AT ROW 1.24 COL 3
     RECT-11 AT ROW 1.95 COL 2
     SPACE(0.99) SKIP(1.66)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Update FOB and Freight"
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

/* SETTINGS FOR RECTANGLE RECT-11 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tb-open_order IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX tb-un_bol IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX tb-un_inv IN FRAME Dialog-Frame
   1                                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Update FOB and Freight */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
 
  ASSIGN {&orderFields}.

  FIND FIRST cust NO-LOCK WHERE ROWID(cust) EQ ipRowID NO-ERROR.

  IF AVAILABLE cust THEN DO:
   
    IF tb-open_order THEN
        FOR EACH oe-ordl NO-LOCK
           WHERE oe-ordl.company EQ cust.company
           AND oe-ordl.cust-no EQ cust.cust-no 
           AND oe-ordl.opened EQ YES :

            {custom/statusMsg.i " 'message  '  + string(oe-ordl.ord-no) "}
           FIND FIRST oe-ord OF oe-ordl EXCLUSIVE-LOCK NO-ERROR .
            IF AVAIL oe-ord THEN
                assign
                oe-ord.frt-pay = cust.frt-pay 
                oe-ord.fob-code = cust.fob-code.
        END. /* each oe-ord */
    IF tb-un_bol THEN
        FOR EACH oe-bolh EXCLUSIVE-LOCK
          WHERE oe-bolh.company EQ cust.company
          AND oe-bolh.cust-no EQ cust.cust-no
          AND oe-bolh.posted  EQ NO  :

          {custom/statusMsg.i " 'message  '  + string(oe-bolh.bol-no) "}

          oe-bolh.frt-pay = cust.frt-pay .
          /*oe-bolh.fob-code = cust.fob-code.*/
        END. /* each oe-bolh */
    IF tb-un_inv THEN
        FOR EACH inv-head EXCLUSIVE-LOCK
          WHERE inv-head.company EQ cust.company
          AND inv-head.cust-no EQ cust.cust-no :

           {custom/statusMsg.i " 'message  '  + string(inv-head.inv-no) "}

          inv-head.frt-pay = cust.frt-pay .
          inv-head.fob-code = cust.fob-code.
        END. /* each oe-bolh */

    RELEASE oe-ord .
    RELEASE oe-bolh.
    RELEASE inv-head .
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
  DISPLAY tb-open_order tb-un_bol tb-un_inv 
      WITH FRAME Dialog-Frame.
  ENABLE tb-open_order tb-un_bol tb-un_inv Btn_OK 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

