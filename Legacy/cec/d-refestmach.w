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
def input param machrecid as recid no-undo.
def input param ip-title as cha no-undo.

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

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS lv-qty-1 lv-cost-1 lv-qty-2 lv-cost-2 ~
lv-qty-3 lv-cost-3 lv-qty-4 lv-cost-4 lv-qty-5 lv-cost-5 lv-qty-6 lv-cost-6 ~
lv-qty-7 lv-cost-7 lv-qty-8 lv-cost-8 lv-qty-9 lv-cost-9 lv-qty-10 ~
lv-cost-10 lv-qty-11 lv-cost-11 lv-qty-12 lv-cost-12 lv-qty-13 lv-cost-13 ~
Btn_Done Btn_Cancel RECT-30 
&Scoped-Define DISPLAYED-OBJECTS fi_lab-1 fi_lab-2 lv-qty-1 lv-cost-1 ~
lv-qty-2 lv-cost-2 lv-qty-3 lv-cost-3 lv-qty-4 lv-cost-4 lv-qty-5 lv-cost-5 ~
lv-qty-6 lv-cost-6 lv-qty-7 lv-cost-7 lv-qty-8 lv-cost-8 lv-qty-9 lv-cost-9 ~
lv-qty-10 lv-cost-10 lv-qty-11 lv-cost-11 lv-qty-12 lv-cost-12 lv-qty-13 ~
lv-cost-13 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY DEFAULT 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Done AUTO-GO DEFAULT 
     LABEL "&Done" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE fi_lab-1 AS CHARACTER FORMAT "X(256)":U INITIAL "Quantity" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE fi_lab-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Cost/M" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE lv-cost-1 AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE lv-cost-10 AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE lv-cost-11 AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE lv-cost-12 AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE lv-cost-13 AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE lv-cost-2 AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE lv-cost-3 AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE lv-cost-4 AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE lv-cost-5 AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE lv-cost-6 AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE lv-cost-7 AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE lv-cost-8 AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE lv-cost-9 AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE lv-qty-1 AS DECIMAL FORMAT "->>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE lv-qty-10 AS DECIMAL FORMAT "->>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE lv-qty-11 AS DECIMAL FORMAT "->>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE lv-qty-12 AS DECIMAL FORMAT "->>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE lv-qty-13 AS DECIMAL FORMAT "->>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE lv-qty-2 AS DECIMAL FORMAT "->>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE lv-qty-3 AS DECIMAL FORMAT "->>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE lv-qty-4 AS DECIMAL FORMAT "->>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE lv-qty-5 AS DECIMAL FORMAT "->>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE lv-qty-6 AS DECIMAL FORMAT "->>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE lv-qty-7 AS DECIMAL FORMAT "->>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE lv-qty-8 AS DECIMAL FORMAT "->>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE lv-qty-9 AS DECIMAL FORMAT "->>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-30
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 54 BY 13.81.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     fi_lab-1 AT ROW 1.24 COL 1 COLON-ALIGNED NO-LABEL
     fi_lab-2 AT ROW 1.24 COL 27 COLON-ALIGNED NO-LABEL
     lv-qty-1 AT ROW 2.19 COL 1 COLON-ALIGNED NO-LABEL
     lv-cost-1 AT ROW 2.19 COL 27 COLON-ALIGNED NO-LABEL
     lv-qty-2 AT ROW 3.14 COL 1 COLON-ALIGNED NO-LABEL
     lv-cost-2 AT ROW 3.14 COL 27 COLON-ALIGNED NO-LABEL
     lv-qty-3 AT ROW 4.1 COL 1 COLON-ALIGNED NO-LABEL
     lv-cost-3 AT ROW 4.1 COL 27 COLON-ALIGNED NO-LABEL
     lv-qty-4 AT ROW 5.05 COL 1 COLON-ALIGNED NO-LABEL
     lv-cost-4 AT ROW 5.05 COL 27 COLON-ALIGNED NO-LABEL
     lv-qty-5 AT ROW 6 COL 1 COLON-ALIGNED NO-LABEL
     lv-cost-5 AT ROW 6 COL 27 COLON-ALIGNED NO-LABEL
     lv-qty-6 AT ROW 6.95 COL 1 COLON-ALIGNED NO-LABEL
     lv-cost-6 AT ROW 6.95 COL 27 COLON-ALIGNED NO-LABEL
     lv-qty-7 AT ROW 7.91 COL 1 COLON-ALIGNED NO-LABEL
     lv-cost-7 AT ROW 7.91 COL 27 COLON-ALIGNED NO-LABEL
     lv-qty-8 AT ROW 8.86 COL 1 COLON-ALIGNED NO-LABEL
     lv-cost-8 AT ROW 8.86 COL 27 COLON-ALIGNED NO-LABEL
     lv-qty-9 AT ROW 9.81 COL 1 COLON-ALIGNED NO-LABEL
     lv-cost-9 AT ROW 9.81 COL 27 COLON-ALIGNED NO-LABEL
     lv-qty-10 AT ROW 10.76 COL 1 COLON-ALIGNED NO-LABEL
     lv-cost-10 AT ROW 10.76 COL 27 COLON-ALIGNED NO-LABEL
     lv-qty-11 AT ROW 11.71 COL 1 COLON-ALIGNED NO-LABEL
     lv-cost-11 AT ROW 11.71 COL 27 COLON-ALIGNED NO-LABEL
     lv-qty-12 AT ROW 12.67 COL 1 COLON-ALIGNED NO-LABEL
     lv-cost-12 AT ROW 12.67 COL 27 COLON-ALIGNED NO-LABEL
     lv-qty-13 AT ROW 13.62 COL 1 COLON-ALIGNED NO-LABEL
     lv-cost-13 AT ROW 13.62 COL 27 COLON-ALIGNED NO-LABEL
     Btn_Done AT ROW 15.29 COL 7.8
     Btn_Cancel AT ROW 15.29 COL 34
     RECT-30 AT ROW 1 COL 1
     SPACE(0.00) SKIP(2.09)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "Misc . Detail  -  Qty/Cost"
         DEFAULT-BUTTON Btn_Done CANCEL-BUTTON Btn_Cancel.


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
   L-To-R                                                               */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fi_lab-1 IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_lab-2 IN FRAME D-Dialog
   NO-ENABLE                                                            */
ASSIGN 
       lv-qty-10:AUTO-RESIZE IN FRAME D-Dialog      = TRUE.

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
ON WINDOW-CLOSE OF FRAME D-Dialog /* Misc . Detail  -  Qty/Cost */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done D-Dialog
ON CHOOSE OF Btn_Done IN FRAME D-Dialog /* Done */
DO:
  do with frame {&frame-name} :
     assign lv-qty-1 lv-qty-2 lv-qty-3 lv-qty-4 lv-qty-5
            lv-qty-6 lv-qty-7 lv-qty-8 lv-qty-9 lv-qty-10
            lv-qty-11 lv-qty-12 lv-qty-13
            lv-cost-1 lv-cost-2 lv-cost-3 lv-cost-4 lv-cost-5
            lv-cost-6 lv-cost-7 lv-cost-8 lv-cost-9 lv-cost-10
            lv-cost-11 lv-cost-12 lv-cost-13.

     /*if lv-qty-2 = 0 then lv-qty-1 =  99999999.
     else if lv-qty-3 = 0 then lv-qty-2 =  99999999.
     else if lv-qty-4 = 0 then lv-qty-3 =  99999999.
     else if lv-qty-5 = 0 then lv-qty-4 =  99999999.
     else if lv-qty-6 = 0 then lv-qty-5 =  99999999.
     else if lv-qty-7 = 0 then lv-qty-6 =  99999999.
     else if lv-qty-8 = 0 then lv-qty-7 =  99999999.
     else if lv-qty-9 = 0 then lv-qty-8 =  99999999.
     else if lv-qty-10 = 0 then lv-qty-9 =  99999999.
     else if lv-qty-11 = 0 then lv-qty-10 =  99999999.
     else if lv-qty-12 = 0 then lv-qty-11 =  99999999.
     else if lv-qty-13 = 0 then lv-qty-12 =  99999999.
     else lv-qty-13 = 99999999.
     display lv-qty-1 lv-qty-2 lv-qty-3 lv-qty-4 lv-qty-5
             lv-qty-6 lv-qty-7 lv-qty-8 lv-qty-9 lv-qty-10 
             lv-qty-11 lv-qty-12 lv-qty-13.*/
  end.

  /* Cost must be first to allow write trigger to resequence qty & cost */
  find mach where recid(mach) = machrecid.
  IF ip-title = "M R" THEN DO:
      assign mach.mr-crusiz-cst[1] = lv-cost-1
             mach.mr-crusiz-cst[2] = lv-cost-2
             mach.mr-crusiz-cst[3] = lv-cost-3
             mach.mr-crusiz-cst[4] = lv-cost-4
             mach.mr-crusiz-cst[5] = lv-cost-5
             mach.mr-crusiz-cst[6] = lv-cost-6
             mach.mr-crusiz-cst[7] = lv-cost-7
             mach.mr-crusiz-cst[8] = lv-cost-8
             mach.mr-crusiz-cst[9] = lv-cost-9
             mach.mr-crusiz-cst[10] = lv-cost-10
             mach.mr-crusiz-cst[11] = lv-cost-11
             mach.mr-crusiz-cst[12] = lv-cost-12
             mach.mr-crusiz-cst[13] = lv-cost-13
             mach.mr-crusiz-qty[1] = lv-qty-1
             mach.mr-crusiz-qty[2] = lv-qty-2
             mach.mr-crusiz-qty[3] = lv-qty-3
             mach.mr-crusiz-qty[4] = lv-qty-4
             mach.mr-crusiz-qty[5] = lv-qty-5
             mach.mr-crusiz-qty[6] = lv-qty-6
             mach.mr-crusiz-qty[7] = lv-qty-7
             mach.mr-crusiz-qty[8] = lv-qty-8
             mach.mr-crusiz-qty[9] = lv-qty-9
             mach.mr-crusiz-qty[10] = lv-qty-10
             mach.mr-crusiz-qty[11] = lv-qty-11
             mach.mr-crusiz-qty[12] = lv-qty-12
             mach.mr-crusiz-qty[13] = lv-qty-13
             .
  END.   
  ELSE IF ip-title = "RUN" THEN DO:
      assign mach.run-crusiz-cst[1] = lv-cost-1
             mach.run-crusiz-cst[2] = lv-cost-2
             mach.run-crusiz-cst[3] = lv-cost-3
             mach.run-crusiz-cst[4] = lv-cost-4
             mach.run-crusiz-cst[5] = lv-cost-5
             mach.run-crusiz-cst[6] = lv-cost-6
             mach.run-crusiz-cst[7] = lv-cost-7
             mach.run-crusiz-cst[8] = lv-cost-8
             mach.run-crusiz-cst[9] = lv-cost-9
             mach.run-crusiz-cst[10] = lv-cost-10
             mach.run-crusiz-cst[11] = lv-cost-11
             mach.run-crusiz-cst[12] = lv-cost-12
             mach.run-crusiz-cst[13] = lv-cost-13
             mach.run-crusiz-qty[1] = lv-qty-1
             mach.run-crusiz-qty[2] = lv-qty-2
             mach.run-crusiz-qty[3] = lv-qty-3
             mach.run-crusiz-qty[4] = lv-qty-4
             mach.run-crusiz-qty[5] = lv-qty-5
             mach.run-crusiz-qty[6] = lv-qty-6
             mach.run-crusiz-qty[7] = lv-qty-7
             mach.run-crusiz-qty[8] = lv-qty-8
             mach.run-crusiz-qty[9] = lv-qty-9
             mach.run-crusiz-qty[10] = lv-qty-10
             mach.run-crusiz-qty[11] = lv-qty-11
             mach.run-crusiz-qty[12] = lv-qty-12
             mach.run-crusiz-qty[13] = lv-qty-13
             .
  END.           

  APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-cost-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-cost-1 D-Dialog
ON ENTRY OF lv-cost-1 IN FRAME D-Dialog
DO:
   IF int(lv-qty-1:SCREEN-VALUE) = 0 THEN DO:
      APPLY "tab" TO SELF.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-cost-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-cost-10 D-Dialog
ON ENTRY OF lv-cost-10 IN FRAME D-Dialog
DO:
   IF int(lv-qty-10:SCREEN-VALUE) = 0 THEN DO:
      APPLY "tab" TO SELF.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-cost-11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-cost-11 D-Dialog
ON ENTRY OF lv-cost-11 IN FRAME D-Dialog
DO:
  IF int(lv-qty-11:SCREEN-VALUE) = 0 THEN DO:
      APPLY "tab" TO SELF.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-cost-12
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-cost-12 D-Dialog
ON ENTRY OF lv-cost-12 IN FRAME D-Dialog
DO:
  IF int(lv-qty-12:SCREEN-VALUE) = 0 THEN DO:
      APPLY "tab" TO SELF.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-cost-13
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-cost-13 D-Dialog
ON ENTRY OF lv-cost-13 IN FRAME D-Dialog
DO:
   IF int(lv-qty-13:SCREEN-VALUE) = 0 THEN DO:
      APPLY "tab" TO SELF.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-cost-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-cost-2 D-Dialog
ON ENTRY OF lv-cost-2 IN FRAME D-Dialog
DO:
  IF int(lv-qty-2:SCREEN-VALUE) = 0 THEN DO:
      APPLY "tab" TO SELF.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-cost-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-cost-3 D-Dialog
ON ENTRY OF lv-cost-3 IN FRAME D-Dialog
DO:
  IF int(lv-qty-3:SCREEN-VALUE) = 0 THEN DO:
      APPLY "tab" TO SELF.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-cost-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-cost-4 D-Dialog
ON ENTRY OF lv-cost-4 IN FRAME D-Dialog
DO:
  IF int(lv-qty-4:SCREEN-VALUE) = 0 THEN DO:
      APPLY "tab" TO SELF.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-cost-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-cost-5 D-Dialog
ON ENTRY OF lv-cost-5 IN FRAME D-Dialog
DO:
  IF int(lv-qty-5:SCREEN-VALUE) = 0 THEN DO:
      APPLY "tab" TO SELF.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-cost-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-cost-6 D-Dialog
ON ENTRY OF lv-cost-6 IN FRAME D-Dialog
DO:
  IF int(lv-qty-6:SCREEN-VALUE) = 0 THEN DO:
      APPLY "tab" TO SELF.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-cost-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-cost-7 D-Dialog
ON ENTRY OF lv-cost-7 IN FRAME D-Dialog
DO:
  IF int(lv-qty-7:SCREEN-VALUE) = 0 THEN DO:
      APPLY "tab" TO SELF.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-cost-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-cost-8 D-Dialog
ON ENTRY OF lv-cost-8 IN FRAME D-Dialog
DO:
  IF int(lv-qty-8:SCREEN-VALUE) = 0 THEN DO:
      APPLY "tab" TO SELF.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-cost-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-cost-9 D-Dialog
ON ENTRY OF lv-cost-9 IN FRAME D-Dialog
DO:
  IF int(lv-qty-9:SCREEN-VALUE) = 0 THEN DO:
      APPLY "tab" TO SELF.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty-1 D-Dialog
ON LEAVE OF lv-qty-1 IN FRAME D-Dialog
DO:
    if lastkey <> -1 and int(self:screen-value) = 0 then do:
        lv-cost-1:SCREEN-VALUE = '0'.
    /*    APPLY "entry" TO lv-qty-2.
        return no-apply. */
    END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty-10 D-Dialog
ON LEAVE OF lv-qty-10 IN FRAME D-Dialog
DO:
   if lastkey <> -1 and int(self:screen-value) = 0 then do:
        lv-cost-10:SCREEN-VALUE = '0'.
       /* return no-apply.*/
   END.  

   /*if int(self:screen-value) <> 0 and
      int(lv-qty-10:screen-value) < int(lv-qty-9:screen-value) then return no-apply.*/

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty-11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty-11 D-Dialog
ON LEAVE OF lv-qty-11 IN FRAME D-Dialog
DO:
    if lastkey <> -1 and int(self:screen-value) = 0 then do:
        lv-cost-11:SCREEN-VALUE = '0'.
  /*      APPLY "entry" TO lv-qty-9.
        return no-apply. */
    END.  

    /*if int(self:screen-value) <> 0 and
       int(lv-qty-11:screen-value) < int(lv-qty-10:screen-value) then return no-apply.*/

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty-12
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty-12 D-Dialog
ON LEAVE OF lv-qty-12 IN FRAME D-Dialog
DO:
   if lastkey <> -1 and int(self:screen-value) = 0 then do:
        lv-cost-12:SCREEN-VALUE = '0'.
   /*     APPLY "entry" TO lv-qty-10.
        return no-apply. */
   END. 

   /*if int(self:screen-value) <> 0 and
      int(lv-qty-12:screen-value) < int(lv-qty-11:screen-value) then return no-apply.*/

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty-13
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty-13 D-Dialog
ON LEAVE OF lv-qty-13 IN FRAME D-Dialog
DO:
   if lastkey <> -1 and int(self:screen-value) = 0 then do:
        lv-cost-13:SCREEN-VALUE = '0'.
       /* return no-apply.*/
   END.  

   /*if int(self:screen-value) <> 0 and
      int(lv-qty-13:screen-value) < int(lv-qty-12:screen-value) then return no-apply.*/

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty-2 D-Dialog
ON LEAVE OF lv-qty-2 IN FRAME D-Dialog
DO:
    if lastkey <> -1 and int(self:screen-value) = 0 then do:
        lv-cost-2:SCREEN-VALUE = '0'.
  /*      APPLY "entry" TO lv-qty-3.
        return no-apply. */
    END.  
    
    /*if int(self:screen-value) <> 0 and
      int(lv-qty-2:screen-value) < int(lv-qty-1:screen-value) then return no-apply.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty-3 D-Dialog
ON LEAVE OF lv-qty-3 IN FRAME D-Dialog
DO:
    if lastkey <> -1 and int(self:screen-value) = 0 then do:
        lv-cost-3:SCREEN-VALUE = '0'.
    /*    APPLY "entry" TO lv-qty-4.
        return no-apply. */
    END.  
    
    /*if int(self:screen-value) <> 0 and
      int(lv-qty-3:screen-value) < int(lv-qty-2:screen-value) then return no-apply.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty-4 D-Dialog
ON LEAVE OF lv-qty-4 IN FRAME D-Dialog
DO:
     if lastkey <> -1 and int(self:screen-value) = 0 then do:
        lv-cost-4:SCREEN-VALUE = '0'.
      /*  APPLY "entry" TO lv-qty-5.
        return no-apply. */
    END.   
    
    /*if int(self:screen-value) <> 0 and
      int(lv-qty-4:screen-value) < int(lv-qty-3:screen-value) then return no-apply.*/

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty-5 D-Dialog
ON LEAVE OF lv-qty-5 IN FRAME D-Dialog
DO:
    if lastkey <> -1 and int(self:screen-value) = 0 then do:
        lv-cost-5:SCREEN-VALUE = '0'.
     /*   APPLY "entry" TO lv-qty-6.
        return no-apply. */
    END.   
    
    /*if int(self:screen-value) <> 0 and
      int(lv-qty-5:screen-value) < int(lv-qty-4:screen-value) then return no-apply.*/

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty-6 D-Dialog
ON LEAVE OF lv-qty-6 IN FRAME D-Dialog
DO:
    if lastkey <> -1 and int(self:screen-value) = 0 then do:
        lv-cost-6:SCREEN-VALUE = '0'.
     /*   APPLY "entry" TO lv-qty-7.
        return no-apply. */
    END.   
    
    /*if int(self:screen-value) <> 0 and
      int(lv-qty-6:screen-value) < int(lv-qty-5:screen-value) then return no-apply.*/

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty-7 D-Dialog
ON LEAVE OF lv-qty-7 IN FRAME D-Dialog
DO:
    if lastkey <> -1 and int(self:screen-value) = 0 then do:
        lv-cost-7:SCREEN-VALUE = '0'.
       /* APPLY "entry" TO lv-qty-8.
        return no-apply. */
    END.   
    
    /*if int(self:screen-value) <> 0 and
      int(lv-qty-7:screen-value) < int(lv-qty-6:screen-value) then return no-apply.*/

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty-8 D-Dialog
ON LEAVE OF lv-qty-8 IN FRAME D-Dialog
DO:
    if lastkey <> -1 and int(self:screen-value) = 0 then do:
        lv-cost-8:SCREEN-VALUE = '0'.
  /*      APPLY "entry" TO lv-qty-9.
        return no-apply. */
    END.  

    /*if int(self:screen-value) <> 0 and
       int(lv-qty-8:screen-value) < int(lv-qty-7:screen-value) then return no-apply.*/

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-qty-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-qty-9 D-Dialog
ON LEAVE OF lv-qty-9 IN FRAME D-Dialog
DO:
   if lastkey <> -1 and int(self:screen-value) = 0 then do:
        lv-cost-9:SCREEN-VALUE = '0'.
   /*     APPLY "entry" TO lv-qty-10.
        return no-apply. */
   END.  

   /*if int(self:screen-value) <> 0 and
      int(lv-qty-9:screen-value) < int(lv-qty-8:screen-value) then return no-apply.*/

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */
SESSION:DATA-ENTRY-RETURN = YES.

IF CAN-FIND(mach WHERE RECID(mach) EQ machrecid) THEN DO:
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
  DISPLAY fi_lab-1 fi_lab-2 lv-qty-1 lv-cost-1 lv-qty-2 lv-cost-2 lv-qty-3 
          lv-cost-3 lv-qty-4 lv-cost-4 lv-qty-5 lv-cost-5 lv-qty-6 lv-cost-6 
          lv-qty-7 lv-cost-7 lv-qty-8 lv-cost-8 lv-qty-9 lv-cost-9 lv-qty-10 
          lv-cost-10 lv-qty-11 lv-cost-11 lv-qty-12 lv-cost-12 lv-qty-13 
          lv-cost-13 
      WITH FRAME D-Dialog.
  ENABLE lv-qty-1 lv-cost-1 lv-qty-2 lv-cost-2 lv-qty-3 lv-cost-3 lv-qty-4 
         lv-cost-4 lv-qty-5 lv-cost-5 lv-qty-6 lv-cost-6 lv-qty-7 lv-cost-7 
         lv-qty-8 lv-cost-8 lv-qty-9 lv-cost-9 lv-qty-10 lv-cost-10 lv-qty-11 
         lv-cost-11 lv-qty-12 lv-cost-12 lv-qty-13 lv-cost-13 Btn_Done 
         Btn_Cancel RECT-30 
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
  DEF VAR lv-title AS CHAR NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  FIND mach WHERE RECID(mach) EQ machrecid NO-LOCK.
  IF ip-title = "M R" THEN DO:
  ASSIGN lv-qty-1 = mach.mr-crusiz-qty[1]
         lv-qty-2 = mach.mr-crusiz-qty[2]
         lv-qty-3 = mach.mr-crusiz-qty[3]
         lv-qty-4 = mach.mr-crusiz-qty[4]
         lv-qty-5 = mach.mr-crusiz-qty[5]
         lv-qty-6 = mach.mr-crusiz-qty[6]
         lv-qty-7 = mach.mr-crusiz-qty[7]
         lv-qty-8 = mach.mr-crusiz-qty[8]
         lv-qty-9 = mach.mr-crusiz-qty[9]
         lv-qty-10 = mach.mr-crusiz-qty[10]
         lv-qty-11 = mach.mr-crusiz-qty[11]
         lv-qty-12 = mach.mr-crusiz-qty[12]
         lv-qty-13 = mach.mr-crusiz-qty[13]
         lv-cost-1 = mach.mr-crusiz-cst[1]
         lv-cost-2 = mach.mr-crusiz-cst[2]
         lv-cost-3 = mach.mr-crusiz-cst[3]
         lv-cost-4 = mach.mr-crusiz-cst[4]
         lv-cost-5 = mach.mr-crusiz-cst[5]
         lv-cost-6 = mach.mr-crusiz-cst[6]
         lv-cost-7 = mach.mr-crusiz-cst[7]
         lv-cost-8 = mach.mr-crusiz-cst[8]
         lv-cost-9 = mach.mr-crusiz-cst[9]
         lv-cost-10 = mach.mr-crusiz-cst[10]
         lv-cost-11 = mach.mr-crusiz-cst[11]
         lv-cost-12 = mach.mr-crusiz-cst[12]
         lv-cost-13 = mach.mr-crusiz-cst[13]
         .
  END.   
  ELSE IF ip-title = "RUN" THEN DO:
  ASSIGN lv-cost-1 = mach.run-crusiz-cst[1]
         lv-cost-2 = mach.run-crusiz-cst[2]
         lv-cost-3 = mach.run-crusiz-cst[3]
         lv-cost-4 = mach.run-crusiz-cst[4]
         lv-cost-5 = mach.run-crusiz-cst[5]
         lv-cost-6 = mach.run-crusiz-cst[6]
         lv-cost-7 = mach.run-crusiz-cst[7]
         lv-cost-8 = mach.run-crusiz-cst[8]
         lv-cost-9 = mach.run-crusiz-cst[9]
         lv-cost-10 = mach.run-crusiz-cst[10]
         lv-cost-11 = mach.run-crusiz-cst[11]
         lv-cost-12 = mach.run-crusiz-cst[12]
         lv-cost-13 = mach.run-crusiz-cst[13]
         lv-qty-1 = mach.run-crusiz-qty[1]
         lv-qty-2 = mach.run-crusiz-qty[2]
         lv-qty-3 = mach.run-crusiz-qty[3]
         lv-qty-4 = mach.run-crusiz-qty[4]
         lv-qty-5 = mach.run-crusiz-qty[5]
         lv-qty-6 = mach.run-crusiz-qty[6]
         lv-qty-7 = mach.run-crusiz-qty[7]
         lv-qty-8 = mach.run-crusiz-qty[8]
         lv-qty-9 = mach.run-crusiz-qty[9]
         lv-qty-10 = mach.run-crusiz-qty[10]
         lv-qty-11 = mach.run-crusiz-qty[11]
         lv-qty-12 = mach.run-crusiz-qty[12]
         lv-qty-13 = mach.run-crusiz-qty[13]         
         .
   END.

  DO WITH FRAME {&FRAME-NAME}:
    lv-title = FRAME {&FRAME-NAME}:TITLE.

    
      ASSIGN
       lv-title = "Mach Crew Size -"
       fi_lab-1 = "Feed Square Feet"
       fi_lab-2 = "Crew Size".

    FRAME {&FRAME-NAME}:TITLE = TRIM(lv-title) + "  " + ip-title.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

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

