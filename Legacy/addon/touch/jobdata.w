&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS s-object 
/*------------------------------------------------------------------------

  File: addon/touch/jobdata.w

  Description: from SMART.W - Template for basic SmartObject

  Author: Ron Stark
  Created: 4.18.2000

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

{touch/touchdef.i}
{custom/shftdefs.i}

/* internal procedures */
{custom/shftproc.i}
{custom/emprate.i}
{methods/defines/globdefs.i}

DEF VAR lv-timer AS INT NO-UNDO. /* clock timer */

{sys/inc/var.i NEW SHARED}

ASSIGN cocode = g_company
       locode = g_loc.

DO TRANSACTION:
   {sys/inc/tspostfg.i}
   {sys/inc/fgrecpt.i}
   {sys/inc/tsqty.i}
   {sys/inc/tsfinish.i}
   {sys/inc/tsdocksec.i}
   {sys/inc/tscomplete.i}
   {sys/inc/tstimeb.i}
   {sys/inc/tsendwash.i}
   {sys/inc/tskey.i}
END.

DEF VAR v-time-clock-off AS LOG  NO-UNDO.

DEF NEW SHARED TEMP-TABLE tt-comp FIELD i-no AS cha 
                              FIELD rcv-qty AS INT
                              FIELD est-no AS cha 
                              FIELD form-no AS INT 
                              FIELD set-qty AS INT .

DEF TEMP-TABLE tt-mach NO-UNDO
    FIELD machine AS CHAR
    INDEX machine machine.

DEF TEMP-TABLE tt-machtran-reckey NO-UNDO
    FIELD rec_key AS CHAR
    FIELD START_date AS DATE
    FIELD START_time AS INT
    FIELD end_date AS DATE
    FIELD end_time AS INT
    FIELD shift AS CHAR.

DEF BUFFER b-tt-machtran-reckey FOR tt-machtran-reckey.

/*{methods/defines/hndldefs.i}            */
{methods/prgsecd3.i "p-tchupd."}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartObject
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 Btn_Close Btn_Hour Btn_Minute ~
Btn_AMPM Btn_Quantity Btn_Waste Btn_complete Btn_ReSet_Time Btn_Set_Time ~
Btn_Accept Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS time-hour time-minute run-qty waste-qty ~
v-completed timerStatus 

/* Custom List Definitions                                              */
/* JOB-DATA-FIELDS,JOB-DATA-BUTTONS,List-3,List-4,List-5,List-6         */
&Scoped-define JOB-DATA-FIELDS time-hour time-minute run-qty waste-qty ~
v-completed 
&Scoped-define JOB-DATA-BUTTONS Btn_Hour Btn_Minute Btn_AMPM Btn_Quantity ~
Btn_Waste Btn_complete Btn_ReSet_Time Btn_Set_Time Btn_Accept Btn_Cancel 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getNumForms s-object
FUNCTION getNumForms RETURNS INTEGER 
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setTimerStatus s-object 
FUNCTION setTimerStatus RETURNS CHARACTER
  (ipStatus AS LOGICAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Accept 
     LABEL "ACCEPT ENTRY" 
     SIZE 41 BY 2.38 TOOLTIP "ACCEPT ENTRY".

DEFINE BUTTON Btn_AMPM 
     LABEL "AM" 
     SIZE 15 BY 1.67.

DEFINE BUTTON Btn_Cancel 
     LABEL "CANCEL" 
     SIZE 40 BY 2.38 TOOLTIP "CANCEL".

DEFINE BUTTON Btn_Close 
     IMAGE-UP FILE "images\exit-au":U
     LABEL "CLOSE" 
     SIZE 10 BY 2.38.

DEFINE BUTTON Btn_complete 
     LABEL "COMPLETE?" 
     SIZE 16 BY 1.67 TOOLTIP "COMPLETE?".

DEFINE BUTTON Btn_Hour 
     LABEL "HOUR" 
     SIZE 16 BY 1.67 TOOLTIP "HOUR".

DEFINE BUTTON Btn_Minute 
     LABEL "MINUTE" 
     SIZE 15 BY 1.67 TOOLTIP "MINUTE".

DEFINE BUTTON Btn_Quantity 
     LABEL "QUANTITY" 
     SIZE 16 BY 1.67 TOOLTIP "QUANTITY".

DEFINE BUTTON Btn_ReSet_Time 
     LABEL "RESET TIME" 
     SIZE 20 BY 2.38 TOOLTIP "RESET TIME".

DEFINE BUTTON Btn_Set_Time 
     LABEL "SET TIME" 
     SIZE 21 BY 2.38 TOOLTIP "SET TIME".

DEFINE BUTTON Btn_Waste 
     LABEL "WASTE" 
     SIZE 16 BY 1.67 TOOLTIP "WASTE".

DEFINE VARIABLE run-qty AS DECIMAL FORMAT "->>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE time-hour AS INTEGER FORMAT ">9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE time-minute AS INTEGER FORMAT ">9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE timerStatus AS CHARACTER FORMAT "X(256)":U INITIAL "Auto Timer On" 
      VIEW-AS TEXT 
     SIZE 19 BY .62 NO-UNDO.

DEFINE VARIABLE v-completed AS LOGICAL FORMAT "yes/no":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE waste-qty AS DECIMAL FORMAT "->>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 124 BY 12.86.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Btn_Close AT ROW 1.71 COL 114
     Btn_Hour AT ROW 1.95 COL 22
     Btn_Minute AT ROW 1.95 COL 48
     Btn_AMPM AT ROW 1.95 COL 73
     time-hour AT ROW 2.19 COL 37 COLON-ALIGNED NO-LABEL
     time-minute AT ROW 2.19 COL 62 COLON-ALIGNED NO-LABEL
     Btn_Quantity AT ROW 4.33 COL 22
     run-qty AT ROW 4.57 COL 37 COLON-ALIGNED NO-LABEL
     Btn_Waste AT ROW 6.48 COL 22
     waste-qty AT ROW 6.71 COL 37 COLON-ALIGNED NO-LABEL
     Btn_complete AT ROW 8.62 COL 22
     v-completed AT ROW 8.86 COL 37 COLON-ALIGNED NO-LABEL
     Btn_ReSet_Time AT ROW 11.24 COL 2
     Btn_Set_Time AT ROW 11.24 COL 22
     Btn_Accept AT ROW 11.24 COL 43
     Btn_Cancel AT ROW 11.24 COL 84
     timerStatus AT ROW 2.43 COL 89 COLON-ALIGNED NO-LABEL
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 7 FGCOLOR 15 FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartObject
   Allow: Basic
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW s-object ASSIGN
         HEIGHT             = 12.95
         WIDTH              = 124.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB s-object 
/* ************************* Included-Libraries *********************** */

{src/adm/method/smart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW s-object
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON Btn_Accept IN FRAME F-Main
   2                                                                    */
ASSIGN 
       Btn_Accept:PRIVATE-DATA IN FRAME F-Main     = 
                "ACCEPT ENTRY".

/* SETTINGS FOR BUTTON Btn_AMPM IN FRAME F-Main
   2                                                                    */
/* SETTINGS FOR BUTTON Btn_Cancel IN FRAME F-Main
   2                                                                    */
ASSIGN 
       Btn_Cancel:PRIVATE-DATA IN FRAME F-Main     = 
                "CANCEL".

/* SETTINGS FOR BUTTON Btn_complete IN FRAME F-Main
   2                                                                    */
ASSIGN 
       Btn_complete:PRIVATE-DATA IN FRAME F-Main     = 
                "Complete".

/* SETTINGS FOR BUTTON Btn_Hour IN FRAME F-Main
   2                                                                    */
ASSIGN 
       Btn_Hour:PRIVATE-DATA IN FRAME F-Main     = 
                "HOUR".

/* SETTINGS FOR BUTTON Btn_Minute IN FRAME F-Main
   2                                                                    */
ASSIGN 
       Btn_Minute:PRIVATE-DATA IN FRAME F-Main     = 
                "MINUTE".

/* SETTINGS FOR BUTTON Btn_Quantity IN FRAME F-Main
   2                                                                    */
ASSIGN 
       Btn_Quantity:PRIVATE-DATA IN FRAME F-Main     = 
                "QUANTITY".

/* SETTINGS FOR BUTTON Btn_ReSet_Time IN FRAME F-Main
   2                                                                    */
ASSIGN 
       Btn_ReSet_Time:PRIVATE-DATA IN FRAME F-Main     = 
                "RESET TIME".

/* SETTINGS FOR BUTTON Btn_Set_Time IN FRAME F-Main
   2                                                                    */
ASSIGN 
       Btn_Set_Time:PRIVATE-DATA IN FRAME F-Main     = 
                "SET TIME".

/* SETTINGS FOR BUTTON Btn_Waste IN FRAME F-Main
   2                                                                    */
ASSIGN 
       Btn_Waste:PRIVATE-DATA IN FRAME F-Main     = 
                "WASTE".

/* SETTINGS FOR FILL-IN run-qty IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN time-hour IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN time-minute IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN timerStatus IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-completed IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN waste-qty IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

