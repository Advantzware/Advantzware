&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
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
DEFINE INPUT PARAMETER ipiBeginBol  AS INTEGER NO-UNDO .
DEFINE INPUT PARAMETER ipiEndBol    AS INTEGER NO-UNDO .
DEFINE INPUT PARAMETER ipcBeginCust AS CHARACTER NO-UNDO .
DEFINE INPUT PARAMETER ipcEndCust   AS CHARACTER NO-UNDO .
DEFINE INPUT PARAMETER ipdtBeginDate AS DATE NO-UNDO .
DEFINE INPUT PARAMETER ipdtEndDate  AS DATE NO-UNDO .
/* Local Variable Definitions ---                                       */

{methods/defines/globdefs.i}
DEFINE VARIABLE v-prgmname   LIKE prgrms.prgmname NO-UNDO.
/*{custom/globdefs.i}*/

{oe/rep/oe-lad.i }

{sys/inc/var.i new shared}


assign
 cocode = g_company
 locode = g_loc.

ASSIGN
    v-prgmname = SUBSTRING(PROGRAM-NAME(1), R-INDEX(PROGRAM-NAME(1), "/") + 1)
    v-prgmname = SUBSTR(v-prgmname,1,INDEX(v-prgmname,".")) .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-40 RECT-41 cCarrier cName cTrailer ~
cAdd1 cSerialNo cAdd2 cCity cState cZip cScac cProName cCortons Btn_OK ~
Btn_Cancel cSpecInst 
&Scoped-Define DISPLAYED-OBJECTS cCarrier cName cTrailer cAdd1 cSerialNo ~
cAdd2 cCity cState cZip cScac cProName cCortons cSpecInst 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 cCarrier cName cTrailer cAdd1 cSerialNo cAdd2 cCity ~
cState cZip cScac cProName cCortons cSpecInst 

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

DEFINE VARIABLE cAdd1 AS CHARACTER FORMAT "X(30)":U 
     LABEL "Address 1" 
     VIEW-AS FILL-IN 
     SIZE 39.4 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cAdd2 AS CHARACTER FORMAT "X(30)":U 
     LABEL "Address 2" 
     VIEW-AS FILL-IN 
     SIZE 39.4 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cCarrier AS CHARACTER FORMAT "X(25)":U 
     LABEL "Carrier Name" 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE cCity AS CHARACTER FORMAT "X(20)":U 
     LABEL "City" 
     VIEW-AS FILL-IN 
     SIZE 18.4 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cCortons AS CHARACTER FORMAT "X(30)":U 
     LABEL "Cortons" 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cName AS CHARACTER FORMAT "X(25)":U 
     LABEL "Name" 
     VIEW-AS FILL-IN 
     SIZE 39.4 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cProName AS CHARACTER FORMAT "X(20)":U 
     LABEL "Pro Name" 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cScac AS CHARACTER FORMAT "X(20)":U 
     LABEL "SCAC" 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cSerialNo AS CHARACTER FORMAT "X(20)":U 
     LABEL "Serial Number" 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cSpecInst AS CHARACTER FORMAT "X(200)":U 
     LABEL "Special Instructions" 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 77 BY 1.67
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cState AS CHARACTER FORMAT "X(2)":U 
     VIEW-AS FILL-IN 
     SIZE 6.6 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cTrailer AS CHARACTER FORMAT "X(20)":U 
     LABEL "Trailer Name" 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cZip AS CHARACTER FORMAT "X(10)":U 
     VIEW-AS FILL-IN 
     SIZE 13.4 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-40
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 113 BY 8.81.

DEFINE RECTANGLE RECT-41
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 116 BY 11.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     cCarrier AT ROW 2.19 COL 20 COLON-ALIGNED      
     cTrailer AT ROW 3.33 COL 20 COLON-ALIGNED      
     cSerialNo AT ROW 4.48 COL 20 COLON-ALIGNED       
     cScac AT ROW 5.67 COL 20 COLON-ALIGNED WIDGET-ID 16
     cProName AT ROW 6.91 COL 20 COLON-ALIGNED WIDGET-ID 18        
     cName AT ROW 2.19 COL 71.8 COLON-ALIGNED WIDGET-ID 2
     cAdd1 AT ROW 3.33 COL 71.8 COLON-ALIGNED WIDGET-ID 4
     cAdd2 AT ROW 4.48 COL 71.8 COLON-ALIGNED WIDGET-ID 6
     cCity AT ROW 5.62 COL 71.8 COLON-ALIGNED WIDGET-ID 8
     cState AT ROW 5.62 COL 90.8 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     cZip AT ROW 5.62 COL 97.8 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     cCortons AT ROW 6.91 COL 71.8 COLON-ALIGNED WIDGET-ID 20
     cSpecInst AT ROW 8.14 COL 28.8 COLON-ALIGNED WIDGET-ID 22
     Btn_OK AT ROW 10.76 COL 52.2
     Btn_Cancel AT ROW 10.76 COL 96.4       
     RECT-40 AT ROW 1.48 COL 3
     RECT-41 AT ROW 1 COL 1
     SPACE(0.39) SKIP(0.18)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FGCOLOR 1 FONT 6
         TITLE "More Information for Bol"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


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

/* SETTINGS FOR FILL-IN cAdd1 IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR FILL-IN cAdd2 IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR FILL-IN cCarrier IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR FILL-IN cCity IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR FILL-IN cCortons IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR FILL-IN cName IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR FILL-IN cProName IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR FILL-IN cScac IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR FILL-IN cSerialNo IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR FILL-IN cSpecInst IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR FILL-IN cState IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR FILL-IN cTrailer IN FRAME Dialog-Frame
   1                                                                    */
/* SETTINGS FOR FILL-IN cZip IN FRAME Dialog-Frame
   1                                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* More Information for Bol */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
    
    ASSIGN {&list-1}.
    CREATE tt-temp-report .
     ASSIGN
        tt-temp-report.key-01  = cSerialNo:SCREEN-VALUE 
        tt-temp-report.key-02  = cCortons:SCREEN-VALUE
        /*tt-temp-report.key-03  = cSub:SCREEN-VALUE*/
        tt-temp-report.key-04  = cName:SCREEN-VALUE 
        tt-temp-report.key-05  = cAdd1:SCREEN-VALUE
        tt-temp-report.key-06  = cAdd2:SCREEN-VALUE
        tt-temp-report.key-07  = cCity:SCREEN-VALUE 
        tt-temp-report.key-08  = cState:SCREEN-VALUE
        tt-temp-report.key-09  = cZip:SCREEN-VALUE
        
        tt-temp-report.cCarrier  = cCarrier:SCREEN-VALUE 
        tt-temp-report.cTrailer  = cTrailer:SCREEN-VALUE
        tt-temp-report.cScac     = cScac:SCREEN-VALUE
        tt-temp-report.cProName  = cProName:SCREEN-VALUE 
        tt-temp-report.cSpecInst = cSpecInst:SCREEN-VALUE
        .                     

     RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  
  RUN enable_UI.

  FIND FIRST oe-bolh NO-LOCK
      WHERE oe-bolh.company EQ cocode 
      AND oe-bolh.bol-no  GE ipiBeginBol 
      AND oe-bolh.bol-no  LE ipiEndBol 
      AND oe-bolh.cust-no GE ipcBeginCust 
      AND oe-bolh.cust-no LE ipcEndCust 
      AND oe-bolh.bol-date GE ipdtBeginDate 
      AND oe-bolh.bol-date LE ipdtEndDate
      AND oe-bolh.frt-pay EQ "T" NO-ERROR .

  DO WITH FRAME {&FRAME-NAME}:
      {custom/usrprint.i}
      IF NOT AVAIL oe-bolh THEN do:
          DISABLE cName cAdd1 cAdd2 cCity cState cZip  .
          cName:SCREEN-VALUE = "".
          cAdd1:SCREEN-VALUE = "".
          cAdd2:SCREEN-VALUE = "".
          cCity:SCREEN-VALUE = "".
          cState:SCREEN-VALUE = "".
          cZip:SCREEN-VALUE = "".
      END.
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
  DISPLAY cCarrier cName cTrailer cAdd1 cSerialNo cAdd2 cCity cState cZip cScac 
          cProName cCortons cSpecInst 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-40 RECT-41 cCarrier cName cTrailer cAdd1 cSerialNo cAdd2 cCity 
         cState cZip cScac cProName cCortons Btn_OK Btn_Cancel cSpecInst 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

