&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: ar-ctrl.w.w

  Description: A/R Control File

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Ron Stark

  Created: 01/12/2000

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

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/getcmpny.i}
{custom/format.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME ar-ctrl

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-15 RECT-16 Btn_Update Btn_Close ~
Btn_Clear 
&Scoped-Define DISPLAYED-FIELDS ar-ctrl.last-inv ar-ctrl.receivables ~
ar-ctrl.sales ar-ctrl.cash-act ar-ctrl.discount ar-ctrl.onac ~
ar-ctrl.freight ar-ctrl.stax ar-ctrl.postUserID ar-ctrl.postStartDtTm ~
ar-ctrl.postType 
&Scoped-define DISPLAYED-TABLES ar-ctrl
&Scoped-define FIRST-DISPLAYED-TABLE ar-ctrl
&Scoped-Define DISPLAYED-OBJECTS cDscrAccRec cDscrSalesAcc cDscrCaseAcc ~
cDscrDiscTaken cDscrChargeAcc cDscrFreightAcc cDscrSalesTaxAcc 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */
&Scoped-define List-1 ar-ctrl.last-inv ar-ctrl.receivables ar-ctrl.sales ~
ar-ctrl.cash-act ar-ctrl.discount ar-ctrl.onac ar-ctrl.freight ar-ctrl.stax ~
ar-ctrl.postUserID ar-ctrl.postStartDtTm ar-ctrl.postType 
&Scoped-define F1 F1 F-2 F-3 F-4 F-5 F-6 F-7 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getAccountDesc C-Win 
FUNCTION getAccountDesc RETURNS CHARACTER
  ( ipcAccount AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Clear 
     LABEL "&Clear Posting" 
     SIZE 15.4 BY 1.14.

DEFINE BUTTON Btn_Close 
     LABEL "&Close" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_Update 
     LABEL "&Update" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE cDscrAccRec AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 47 BY 1
     BGCOLOR 7 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cDscrCaseAcc AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 47 BY 1
     BGCOLOR 7 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cDscrChargeAcc AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 47 BY 1
     BGCOLOR 7 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cDscrDiscTaken AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 47 BY 1
     BGCOLOR 7 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cDscrFreightAcc AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 47 BY 1
     BGCOLOR 7 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cDscrSalesAcc AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 47 BY 1
     BGCOLOR 7 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cDscrSalesTaxAcc AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 47 BY 1
     BGCOLOR 7 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-2 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-3 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-4 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-5 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-6 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F-7 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F1 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE RECTANGLE RECT-15
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 51 BY 1.67.

DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 102 BY 11.67.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME ar-ctrl
     ar-ctrl.last-inv AT ROW 1.24 COL 24 COLON-ALIGNED FORMAT ">>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 
     cDscrAccRec AT ROW 2.38 COL 53.2 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     ar-ctrl.receivables AT ROW 2.48 COL 24.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
          BGCOLOR 15 
     ar-ctrl.sales AT ROW 3.62 COL 24.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
          BGCOLOR 15 
     cDscrSalesAcc AT ROW 3.62 COL 53.2 COLON-ALIGNED NO-LABEL WIDGET-ID 4 NO-TAB-STOP 
     ar-ctrl.cash-act AT ROW 4.81 COL 24.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
          BGCOLOR 15 
     cDscrCaseAcc AT ROW 4.81 COL 53.2 COLON-ALIGNED NO-LABEL WIDGET-ID 6 NO-TAB-STOP 
     ar-ctrl.discount AT ROW 6 COL 24.2 COLON-ALIGNED
          LABEL "Discount Taken"
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
          BGCOLOR 15 
     cDscrDiscTaken AT ROW 6 COL 53.2 COLON-ALIGNED NO-LABEL WIDGET-ID 8 NO-TAB-STOP 
     ar-ctrl.onac AT ROW 7.19 COL 24.2 COLON-ALIGNED
          LABEL "Finance Charge Account"
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
          BGCOLOR 15 
     cDscrChargeAcc AT ROW 7.19 COL 53.2 COLON-ALIGNED NO-LABEL WIDGET-ID 10 NO-TAB-STOP 
     ar-ctrl.freight AT ROW 8.38 COL 24.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
          BGCOLOR 15 
     cDscrFreightAcc AT ROW 8.38 COL 53.2 COLON-ALIGNED NO-LABEL WIDGET-ID 12 NO-TAB-STOP 
     ar-ctrl.stax AT ROW 9.57 COL 24.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
          BGCOLOR 15 
     cDscrSalesTaxAcc AT ROW 9.57 COL 53.2 COLON-ALIGNED NO-LABEL WIDGET-ID 14 NO-TAB-STOP 
     ar-ctrl.postUserID AT ROW 11.05 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
          BGCOLOR 7 FGCOLOR 15  NO-TAB-STOP 
     ar-ctrl.postStartDtTm AT ROW 11.05 COL 40.6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 34.4 BY 1
          BGCOLOR 7 FGCOLOR 15  NO-TAB-STOP 
     ar-ctrl.postType AT ROW 11.05 COL 76.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
          BGCOLOR 7 FGCOLOR 15  NO-TAB-STOP 
     Btn_Update AT ROW 13.24 COL 27 HELP
          "Update/Save System Configurations"
     Btn_Close AT ROW 13.24 COL 43 HELP
          "Cancel Update or Close Window"
     Btn_Clear AT ROW 13.24 COL 59.6 HELP
          "Cancel Update or Close Window" WIDGET-ID 2
     F1 AT ROW 2.43 COL 53 NO-LABEL
     F-2 AT ROW 3.62 COL 53 NO-LABEL
     F-3 AT ROW 4.81 COL 53 NO-LABEL
     F-4 AT ROW 6 COL 53 NO-LABEL
     F-5 AT ROW 7.19 COL 53 NO-LABEL
     F-6 AT ROW 8.38 COL 53 NO-LABEL
     F-7 AT ROW 9.57 COL 53 NO-LABEL
     RECT-15 AT ROW 13.05 COL 26
     RECT-16 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 102.2 BY 14.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "A/R Control"
         HEIGHT             = 14
         WIDTH              = 102.2
         MAX-HEIGHT         = 14.14
         MAX-WIDTH          = 102.2
         VIRTUAL-HEIGHT     = 14.14
         VIRTUAL-WIDTH      = 102.2
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("Graphics\asiicon.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics\asiicon.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME ar-ctrl
   FRAME-NAME                                                           */
ASSIGN 
       Btn_Clear:PRIVATE-DATA IN FRAME ar-ctrl     = 
                "ribbon-button".

ASSIGN 
       Btn_Close:PRIVATE-DATA IN FRAME ar-ctrl     = 
                "ribbon-button".

/* SETTINGS FOR FILL-IN ar-ctrl.cash-act IN FRAME ar-ctrl
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN cDscrAccRec IN FRAME ar-ctrl
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cDscrCaseAcc IN FRAME ar-ctrl
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cDscrChargeAcc IN FRAME ar-ctrl
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cDscrDiscTaken IN FRAME ar-ctrl
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cDscrFreightAcc IN FRAME ar-ctrl
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cDscrSalesAcc IN FRAME ar-ctrl
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cDscrSalesTaxAcc IN FRAME ar-ctrl
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ar-ctrl.discount IN FRAME ar-ctrl
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN F-2 IN FRAME ar-ctrl
   NO-DISPLAY NO-ENABLE ALIGN-L 6                                       */
ASSIGN 
       F-2:HIDDEN IN FRAME ar-ctrl           = TRUE.

/* SETTINGS FOR FILL-IN F-3 IN FRAME ar-ctrl
   NO-DISPLAY NO-ENABLE ALIGN-L 6                                       */
ASSIGN 
       F-3:HIDDEN IN FRAME ar-ctrl           = TRUE.

/* SETTINGS FOR FILL-IN F-4 IN FRAME ar-ctrl
   NO-DISPLAY NO-ENABLE ALIGN-L 6                                       */
ASSIGN 
       F-4:HIDDEN IN FRAME ar-ctrl           = TRUE.

/* SETTINGS FOR FILL-IN F-5 IN FRAME ar-ctrl
   NO-DISPLAY NO-ENABLE ALIGN-L 6                                       */
ASSIGN 
       F-5:HIDDEN IN FRAME ar-ctrl           = TRUE.

/* SETTINGS FOR FILL-IN F-6 IN FRAME ar-ctrl
   NO-DISPLAY NO-ENABLE ALIGN-L 6                                       */
ASSIGN 
       F-6:HIDDEN IN FRAME ar-ctrl           = TRUE.

/* SETTINGS FOR FILL-IN F-7 IN FRAME ar-ctrl
   NO-DISPLAY NO-ENABLE ALIGN-L 6                                       */
ASSIGN 
       F-7:HIDDEN IN FRAME ar-ctrl           = TRUE.

/* SETTINGS FOR FILL-IN F1 IN FRAME ar-ctrl
   NO-DISPLAY NO-ENABLE ALIGN-L 6                                       */
ASSIGN 
       F1:HIDDEN IN FRAME ar-ctrl           = TRUE.

/* SETTINGS FOR FILL-IN ar-ctrl.freight IN FRAME ar-ctrl
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN ar-ctrl.last-inv IN FRAME ar-ctrl
   NO-ENABLE 1 EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN ar-ctrl.onac IN FRAME ar-ctrl
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN ar-ctrl.postStartDtTm IN FRAME ar-ctrl
   NO-ENABLE 1                                                          */
ASSIGN 
       ar-ctrl.postStartDtTm:READ-ONLY IN FRAME ar-ctrl        = TRUE.

/* SETTINGS FOR FILL-IN ar-ctrl.postType IN FRAME ar-ctrl
   NO-ENABLE 1                                                          */
ASSIGN 
       ar-ctrl.postType:READ-ONLY IN FRAME ar-ctrl        = TRUE.

/* SETTINGS FOR FILL-IN ar-ctrl.postUserID IN FRAME ar-ctrl
   NO-ENABLE 1                                                          */
ASSIGN 
       ar-ctrl.postUserID:READ-ONLY IN FRAME ar-ctrl        = TRUE.

/* SETTINGS FOR FILL-IN ar-ctrl.receivables IN FRAME ar-ctrl
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN ar-ctrl.sales IN FRAME ar-ctrl
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN ar-ctrl.stax IN FRAME ar-ctrl
   NO-ENABLE 1                                                          */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME ar-ctrl
/* Query rebuild information for FRAME ar-ctrl
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME ar-ctrl */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* A/R Control */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* A/R Control */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Clear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Clear C-Win
ON CHOOSE OF Btn_Clear IN FRAME ar-ctrl /* Clear Posting */
DO:
  
  DO WITH FRAME {&FRAME-NAME}:
    FIND CURRENT ar-ctrl EXCLUSIVE-LOCK.
    ASSIGN
    ar-ctrl.postStartDtTm = ?
    ar-ctrl.postInProcess = NO
    ar-ctrl.postType = ""
    ar-ctrl.postUserID = ""
    ar-ctrl.postStartDtTm:SCREEN-VALUE = ?    
    ar-ctrl.postType:SCREEN-VALUE = ""
    ar-ctrl.postUserID:SCREEN-VALUE = "".    
    
    FIND CURRENT ar-ctrl NO-LOCK.    
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Close
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Close C-Win
ON CHOOSE OF Btn_Close IN FRAME ar-ctrl /* Close */
DO:
  IF {&SELF-NAME}:LABEL = "&Close" THEN
  APPLY "CLOSE" TO THIS-PROCEDURE.
  ELSE
  DO WITH FRAME {&FRAME-NAME}:
      IF ar-ctrl.receivables:BGCOLOR EQ 16 THEN             
          ASSIGN 
              ar-ctrl.receivables:BGCOLOR = 15
              ar-ctrl.receivables:FGCOLOR = ?
             .
      IF ar-ctrl.sales:BGCOLOR EQ 16 THEN             
          ASSIGN 
              ar-ctrl.sales:BGCOLOR = 15
              ar-ctrl.sales:FGCOLOR = ?
              .
      IF ar-ctrl.cash-act:BGCOLOR EQ 16 THEN             
          ASSIGN 
              ar-ctrl.cash-act:BGCOLOR = 15
              ar-ctrl.cash-act:FGCOLOR = ?
              .
      IF ar-ctrl.discount:BGCOLOR EQ 16 THEN             
          ASSIGN 
              ar-ctrl.discount:BGCOLOR = 15
              ar-ctrl.discount:FGCOLOR = ?
              .
      IF ar-ctrl.onac:BGCOLOR EQ 16 THEN             
          ASSIGN 
              ar-ctrl.onac:BGCOLOR = 15
              ar-ctrl.onac:FGCOLOR = ?
              .
      IF ar-ctrl.freight:BGCOLOR EQ 16 THEN             
          ASSIGN 
              ar-ctrl.freight:BGCOLOR = 15
              ar-ctrl.freight:FGCOLOR = ?
              . 
      IF ar-ctrl.stax:BGCOLOR EQ 16 THEN             
          ASSIGN 
              ar-ctrl.stax:BGCOLOR = 15
              ar-ctrl.stax:FGCOLOR = ?
              .                            
    DISABLE {&LIST-1} WITH FRAME {&FRAME-NAME}.
    ASSIGN
      {&SELF-NAME}:LABEL = "&Close"
      Btn_Update:LABEL = "&Update".
    RUN enable_UI.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Update
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Update C-Win
ON CHOOSE OF Btn_Update IN FRAME ar-ctrl /* Update */
DO:
  IF {&SELF-NAME}:LABEL = "&Update" THEN
  DO WITH FRAME {&FRAME-NAME}:
    ENABLE {&LIST-1}.
    DISPLAY {&F1}.
    ASSIGN
      {&SELF-NAME}:LABEL = "&Save"
      Btn_Close:LABEL = "&Cancel".
    APPLY "ENTRY" TO ar-ctrl.last-inv.
  END.
  ELSE
  DO WITH FRAME {&FRAME-NAME}:
    /* VALIDATION */
    DEF VAR v-avail AS LOG NO-UNDO.
    DEFINE VARIABLE lValid   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    
    RUN validGLAccount(ar-ctrl.receivables:SCREEN-VALUE, OUTPUT lValid, OUTPUT cMessage) NO-ERROR.
    IF NOT lValid THEN DO:
        IF ar-ctrl.receivables:BGCOLOR EQ 16 THEN             
            ASSIGN 
                ar-ctrl.receivables:BGCOLOR = 15
                ar-ctrl.receivables:FGCOLOR = ?
               .
            IF INDEX(cMessage, "Inactive") GT 0 THEN 
                ASSIGN 
                    ar-ctrl.receivables:BGCOLOR = 16
                    ar-ctrl.receivables:FGCOLOR = 15
                    . 
            MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.   
            APPLY "ENTRY" TO ar-ctrl.receivables.
            RETURN NO-APPLY.                              
    END.  
    IF ar-ctrl.receivables:BGCOLOR EQ 16 THEN             
        ASSIGN 
            ar-ctrl.receivables:BGCOLOR = 15
            ar-ctrl.receivables:FGCOLOR = ?
            .
            
    RUN validGLAccount(ar-ctrl.sales:SCREEN-VALUE, OUTPUT lValid, OUTPUT cMessage) NO-ERROR.
    IF NOT lValid THEN DO:
        IF ar-ctrl.sales:BGCOLOR EQ 16 THEN             
            ASSIGN 
                ar-ctrl.sales:BGCOLOR = 15
                ar-ctrl.sales:FGCOLOR = ?
               .
            IF INDEX(cMessage, "Inactive") GT 0 THEN 
                ASSIGN 
                    ar-ctrl.sales:BGCOLOR = 16
                    ar-ctrl.sales:FGCOLOR = 15
                    . 
            MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.   
            APPLY "ENTRY" TO ar-ctrl.sales.
            RETURN NO-APPLY.                              
    END.  
    IF ar-ctrl.sales:BGCOLOR EQ 16 THEN             
        ASSIGN 
            ar-ctrl.sales:BGCOLOR = 15
            ar-ctrl.sales:FGCOLOR = ?
            .         
     
    RUN validGLAccount(ar-ctrl.cash-act:SCREEN-VALUE, OUTPUT lValid, OUTPUT cMessage) NO-ERROR.
    IF NOT lValid THEN DO:
        IF ar-ctrl.cash-act:BGCOLOR EQ 16 THEN             
            ASSIGN 
                ar-ctrl.cash-act:BGCOLOR = 15
                ar-ctrl.cash-act:FGCOLOR = ?
               .
            IF INDEX(cMessage, "Inactive") GT 0 THEN 
                ASSIGN 
                    ar-ctrl.cash-act:BGCOLOR = 16
                    ar-ctrl.cash-act:FGCOLOR = 15
                    . 
            MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.   
            APPLY "ENTRY" TO ar-ctrl.cash-act.
            RETURN NO-APPLY.                              
    END.  
    IF ar-ctrl.cash-act:BGCOLOR EQ 16 THEN             
        ASSIGN 
            ar-ctrl.cash-act:BGCOLOR = 15
            ar-ctrl.cash-act:FGCOLOR = ?
            .  
            
    RUN validGLAccount(ar-ctrl.discount:SCREEN-VALUE, OUTPUT lValid, OUTPUT cMessage) NO-ERROR.
    IF NOT lValid THEN DO:
        IF ar-ctrl.discount:BGCOLOR EQ 16 THEN             
            ASSIGN 
                ar-ctrl.discount:BGCOLOR = 15
                ar-ctrl.discount:FGCOLOR = ?
               .
            IF INDEX(cMessage, "Inactive") GT 0 THEN 
                ASSIGN 
                    ar-ctrl.discount:BGCOLOR = 16
                    ar-ctrl.discount:FGCOLOR = 15
                    . 
            MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.   
            APPLY "ENTRY" TO ar-ctrl.discount.
            RETURN NO-APPLY.                              
    END.  
    IF ar-ctrl.discount:BGCOLOR EQ 16 THEN             
        ASSIGN 
            ar-ctrl.discount:BGCOLOR = 15
            ar-ctrl.discount:FGCOLOR = ?
            .   
            
    RUN validGLAccount(ar-ctrl.onac:SCREEN-VALUE, OUTPUT lValid, OUTPUT cMessage) NO-ERROR.
    IF NOT lValid THEN DO:
        IF ar-ctrl.onac:BGCOLOR EQ 16 THEN             
            ASSIGN 
                ar-ctrl.onac:BGCOLOR = 15
                ar-ctrl.onac:FGCOLOR = ?
               .
            IF INDEX(cMessage, "Inactive") GT 0 THEN 
                ASSIGN 
                    ar-ctrl.onac:BGCOLOR = 16
                    ar-ctrl.onac:FGCOLOR = 15
                    . 
            MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.   
            APPLY "ENTRY" TO ar-ctrl.onac.
            RETURN NO-APPLY.                              
    END.  
    IF ar-ctrl.onac:BGCOLOR EQ 16 THEN             
        ASSIGN 
            ar-ctrl.onac:BGCOLOR = 15
            ar-ctrl.onac:FGCOLOR = ?
            .            
            
    RUN validGLAccount(ar-ctrl.freight:SCREEN-VALUE, OUTPUT lValid, OUTPUT cMessage) NO-ERROR.
    IF NOT lValid THEN DO:
        IF ar-ctrl.freight:BGCOLOR EQ 16 THEN             
            ASSIGN 
                ar-ctrl.freight:BGCOLOR = 15
                ar-ctrl.freight:FGCOLOR = ?
               .
            IF INDEX(cMessage, "Inactive") GT 0 THEN 
                ASSIGN 
                    ar-ctrl.freight:BGCOLOR = 16
                    ar-ctrl.freight:FGCOLOR = 15
                    . 
            MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.   
            APPLY "ENTRY" TO ar-ctrl.freight.
            RETURN NO-APPLY.                              
    END.  
    IF ar-ctrl.freight:BGCOLOR EQ 16 THEN             
        ASSIGN 
            ar-ctrl.freight:BGCOLOR = 15
            ar-ctrl.freight:FGCOLOR = ?
            .            
            
    RUN validGLAccount(ar-ctrl.stax:SCREEN-VALUE, OUTPUT lValid, OUTPUT cMessage) NO-ERROR.
    IF NOT lValid THEN DO:
        IF ar-ctrl.stax:BGCOLOR EQ 16 THEN             
            ASSIGN 
                ar-ctrl.stax:BGCOLOR = 15
                ar-ctrl.stax:FGCOLOR = ?
               .
            IF INDEX(cMessage, "Inactive") GT 0 THEN 
                ASSIGN 
                    ar-ctrl.stax:BGCOLOR = 16
                    ar-ctrl.stax:FGCOLOR = 15
                    . 
            MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.   
            APPLY "ENTRY" TO ar-ctrl.stax.
            RETURN NO-APPLY.                              
    END.  
    IF ar-ctrl.stax:BGCOLOR EQ 16 THEN             
        ASSIGN 
            ar-ctrl.stax:BGCOLOR = 15
            ar-ctrl.stax:FGCOLOR = ?
            .                                                  
       
    DISABLE {&LIST-1}.
    HIDE {&F1} NO-PAUSE.
    ASSIGN
      {&SELF-NAME}:LABEL = "&Update"
      Btn_Close:LABEL = "&Close".
    FIND CURRENT ar-ctrl EXCLUSIVE-LOCK.
    ASSIGN {&LIST-1}.
    FIND CURRENT ar-ctrl NO-LOCK.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-ctrl.cash-act
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-ctrl.cash-act C-Win
ON ENTRY OF ar-ctrl.cash-act IN FRAME ar-ctrl /* Cash Account */
DO:
  /*{custom/actentry.i}  format is not right "-"  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-ctrl.cash-act C-Win
ON LEAVE OF ar-ctrl.cash-act IN FRAME ar-ctrl /* Cash Account */
DO:
  {custom/actleave.i} 
  cDscrCaseAcc:SCREEN-VALUE = getAccountDesc(SELF:SCREEN-VALUE).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-ctrl.discount
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-ctrl.discount C-Win
ON ENTRY OF ar-ctrl.discount IN FRAME ar-ctrl /* Discount Taken */
DO:
  /* {custom/actentry.i}  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-ctrl.discount C-Win
ON LEAVE OF ar-ctrl.discount IN FRAME ar-ctrl /* Discount Taken */
DO:
  {custom/actleave.i}
  cDscrDiscTaken:SCREEN-VALUE = getAccountDesc(SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-ctrl.freight
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-ctrl.freight C-Win
ON ENTRY OF ar-ctrl.freight IN FRAME ar-ctrl /* Freight */
DO:
  /*{custom/actentry.i}  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-ctrl.freight C-Win
ON LEAVE OF ar-ctrl.freight IN FRAME ar-ctrl /* Freight */
DO:
  {custom/actleave.i}
   cDscrFreightAcc:SCREEN-VALUE = getAccountDesc(SELF:SCREEN-VALUE).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-ctrl.onac
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-ctrl.onac C-Win
ON ENTRY OF ar-ctrl.onac IN FRAME ar-ctrl /* Finance Charge Account */
DO:
  /* {custom/actentry.i}  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-ctrl.onac C-Win
ON LEAVE OF ar-ctrl.onac IN FRAME ar-ctrl /* Finance Charge Account */
DO:
  {custom/actleave.i}
  cDscrChargeAcc:SCREEN-VALUE = getAccountDesc(SELF:SCREEN-VALUE).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-ctrl.receivables
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-ctrl.receivables C-Win
ON ENTRY OF ar-ctrl.receivables IN FRAME ar-ctrl /* Accounts Receivable */
DO:
  /*{custom/actentry.i}  format is not right "-" */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-ctrl.receivables C-Win
ON LEAVE OF ar-ctrl.receivables IN FRAME ar-ctrl /* Accounts Receivable */
DO:
  {custom/actleave.i}
  cDscrAccRec:SCREEN-VALUE = getAccountDesc(SELF:SCREEN-VALUE).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-ctrl.sales
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-ctrl.sales C-Win
ON ENTRY OF ar-ctrl.sales IN FRAME ar-ctrl /* Sales Account */
DO:
  /* {custom/actentry.i}  format is not right "-"  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-ctrl.sales C-Win
ON LEAVE OF ar-ctrl.sales IN FRAME ar-ctrl /* Sales Account */
DO:
  {custom/actleave.i}
   cDscrSalesAcc:SCREEN-VALUE = getAccountDesc(SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-ctrl.stax
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-ctrl.stax C-Win
ON ENTRY OF ar-ctrl.stax IN FRAME ar-ctrl /* Sales Tax */
DO:
  /* {custom/actentry.i}  format is not right "-"  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-ctrl.stax C-Win
ON LEAVE OF ar-ctrl.stax IN FRAME ar-ctrl /* Sales Tax */
DO:
  {custom/actleave.i}
   cDscrSalesTaxAcc:SCREEN-VALUE = getAccountDesc(SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.
{sys/inc/f3help.i}
/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN.
  END.

  IF NOT CAN-FIND(FIRST ar-ctrl WHERE ar-ctrl.company EQ gcompany) THEN DO:
    CREATE ar-ctrl.
    ar-ctrl.company = gcompany.
  END.
  FIND FIRST ar-ctrl WHERE ar-ctrl.company EQ gcompany NO-LOCK NO-ERROR.
  
  RUN pDisplayField.

  RUN enable_UI.

  {methods/nowait.i}

  APPLY 'ENTRY':U TO Btn_Update IN FRAME {&FRAME-NAME}.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY cDscrAccRec cDscrSalesAcc cDscrCaseAcc cDscrDiscTaken cDscrChargeAcc 
          cDscrFreightAcc cDscrSalesTaxAcc 
      WITH FRAME ar-ctrl IN WINDOW C-Win.
  IF AVAILABLE ar-ctrl THEN 
    DISPLAY ar-ctrl.last-inv ar-ctrl.receivables ar-ctrl.sales ar-ctrl.cash-act 
          ar-ctrl.discount ar-ctrl.onac ar-ctrl.freight ar-ctrl.stax 
          ar-ctrl.postUserID ar-ctrl.postStartDtTm ar-ctrl.postType 
      WITH FRAME ar-ctrl IN WINDOW C-Win.
  ENABLE RECT-15 RECT-16 Btn_Update Btn_Close Btn_Clear 
      WITH FRAME ar-ctrl IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-ar-ctrl}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDisplayField C-Win 
PROCEDURE pDisplayField :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/    
   IF AVAIL ar-ctrl THEN
   DO:
     ASSIGN
        cDscrAccRec = getAccountDesc(ar-ctrl.receivables)
        cDscrSalesAcc = getAccountDesc(ar-ctrl.sales)
        cDscrCaseAcc = getAccountDesc(ar-ctrl.cash-act)
        cDscrDiscTaken = getAccountDesc(ar-ctrl.discount)
        cDscrChargeAcc = getAccountDesc(ar-ctrl.onac)
        cDscrFreightAcc = getAccountDesc(ar-ctrl.freight)
        cDscrSalesTaxAcc = getAccountDesc(ar-ctrl.stax)
        . 
   END. 
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validGLAccount C-Win
PROCEDURE validGLAccount:
/*------------------------------------------------------------------------------
 Purpose: Check for valid and Active GL Account
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipAccount LIKE account.actnum NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid   AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE hValidate AS HANDLE NO-UNDO.
  
    RUN util/Validate.p PERSISTENT SET hValidate.
    
    DO WITH FRAME {&FRAME-NAME}:
        
        RUN pIsValidGLAccount IN hValidate (
            INPUT  ipAccount, 
            INPUT  YES,             
            INPUT  g_company, 
            OUTPUT oplValid, 
            OUTPUT opcMessage
            ) NO-ERROR.
    END.     

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getAccountDesc C-Win 
FUNCTION getAccountDesc RETURNS CHARACTER
  ( ipcAccount AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEF VAR lcDesc LIKE account.dscr.
    DEF BUFFER bf-account FOR account.

    FIND FIRST bf-account 
        WHERE bf-account.company = ar-ctrl.company
        AND bf-account.actnum = ipcAccount NO-LOCK NO-ERROR.
    IF AVAIL bf-account THEN
        lcDesc = bf-account.dscr.
    ELSE
        lcDesc = "".

    RETURN lcDesc.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

