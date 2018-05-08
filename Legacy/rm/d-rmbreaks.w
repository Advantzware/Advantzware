&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: rm/d-rmbreaks.w
  
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ip-e-item-vend AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER ip-e-item AS ROWID NO-UNDO.

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */




def temp-table tmpfile NO-UNDO
    field cost as DEC DECIMALS 4
    field qty as DEC DECIMALS 4
    field setups as DEC DECIMALS 4.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_update btn-exit 
&Scoped-Define DISPLAYED-OBJECTS fi-run-qty-1 fi-run-cost-1 fi-setups-1 ~
fi-run-qty-2 fi-run-cost-2 fi-setups-2 fi-run-qty-3 fi-run-cost-3 ~
fi-setups-3 fi-run-qty-4 fi-run-cost-4 fi-setups-4 fi-run-qty-5 ~
fi-run-cost-5 fi-setups-5 fi-run-qty-6 fi-run-cost-6 fi-setups-6 ~
fi-run-qty-7 fi-run-cost-7 fi-setups-7 fi-run-qty-8 fi-run-cost-8 ~
fi-setups-8 fi-run-qty-9 fi-run-cost-9 fi-setups-9 fi-run-qty-10 ~
fi-run-cost-10 fi-setups-10 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-6 fi-run-qty-1 fi-run-cost-1 fi-setups-1 fi-run-qty-2 ~
fi-run-cost-2 fi-setups-2 fi-run-qty-3 fi-run-cost-3 fi-setups-3 ~
fi-run-qty-4 fi-run-cost-4 fi-setups-4 fi-run-qty-5 fi-run-cost-5 ~
fi-setups-5 fi-run-qty-6 fi-run-cost-6 fi-setups-6 fi-run-qty-7 ~
fi-run-cost-7 fi-setups-7 fi-run-qty-8 fi-run-cost-8 fi-setups-8 ~
fi-run-qty-9 fi-run-cost-9 fi-setups-9 fi-run-qty-10 fi-run-cost-10 ~
fi-setups-10 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-exit 
     LABEL "Exit" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_Cancel 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_update 
     LABEL "Update" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE fi-run-cost-1 AS DECIMAL FORMAT ">>,>>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-run-cost-10 AS DECIMAL FORMAT ">>,>>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-run-cost-2 AS DECIMAL FORMAT ">>,>>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-run-cost-3 AS DECIMAL FORMAT ">>,>>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-run-cost-4 AS DECIMAL FORMAT ">>,>>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-run-cost-5 AS DECIMAL FORMAT ">>,>>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-run-cost-6 AS DECIMAL FORMAT ">>,>>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-run-cost-7 AS DECIMAL FORMAT ">>,>>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-run-cost-8 AS DECIMAL FORMAT ">>,>>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-run-cost-9 AS DECIMAL FORMAT ">>,>>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-run-qty-1 AS DECIMAL FORMAT ">,>>>,>>9.9<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-run-qty-10 AS DECIMAL FORMAT ">,>>>,>>9.9<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-run-qty-2 AS DECIMAL FORMAT ">,>>>,>>9.9<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-run-qty-3 AS DECIMAL FORMAT ">,>>>,>>9.9<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-run-qty-4 AS DECIMAL FORMAT ">,>>>,>>9.9<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-run-qty-5 AS DECIMAL FORMAT ">,>>>,>>9.9<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-run-qty-6 AS DECIMAL FORMAT ">,>>>,>>9.9<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-run-qty-7 AS DECIMAL FORMAT ">,>>>,>>9.9<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-run-qty-8 AS DECIMAL FORMAT ">,>>>,>>9.9<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-run-qty-9 AS DECIMAL FORMAT ">,>>>,>>9.9<<":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-setups-1 AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-setups-10 AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-setups-2 AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-setups-3 AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-setups-4 AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-setups-5 AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-setups-6 AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-setups-7 AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-setups-8 AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-setups-9 AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     FONT 6 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     fi-run-qty-1 AT ROW 2.76 COL 2.6 NO-LABEL WIDGET-ID 10
     fi-run-cost-1 AT ROW 2.76 COL 20 NO-LABEL WIDGET-ID 32
     fi-setups-1 AT ROW 2.76 COL 37.6 NO-LABEL WIDGET-ID 54
     fi-run-qty-2 AT ROW 3.76 COL 2.6 NO-LABEL WIDGET-ID 12
     fi-run-cost-2 AT ROW 3.76 COL 20 NO-LABEL WIDGET-ID 34
     fi-setups-2 AT ROW 3.76 COL 37.6 NO-LABEL WIDGET-ID 56
     fi-run-qty-3 AT ROW 4.76 COL 2.6 NO-LABEL WIDGET-ID 14
     fi-run-cost-3 AT ROW 4.76 COL 20 NO-LABEL WIDGET-ID 36
     fi-setups-3 AT ROW 4.76 COL 37.6 NO-LABEL WIDGET-ID 58
     fi-run-qty-4 AT ROW 5.76 COL 2.6 NO-LABEL WIDGET-ID 16
     fi-run-cost-4 AT ROW 5.76 COL 20 NO-LABEL WIDGET-ID 38
     fi-setups-4 AT ROW 5.76 COL 37.6 NO-LABEL WIDGET-ID 60
     fi-run-qty-5 AT ROW 6.76 COL 2.6 NO-LABEL WIDGET-ID 18
     fi-run-cost-5 AT ROW 6.76 COL 20 NO-LABEL WIDGET-ID 40
     fi-setups-5 AT ROW 6.76 COL 37.6 NO-LABEL WIDGET-ID 62
     fi-run-qty-6 AT ROW 7.76 COL 2.6 NO-LABEL WIDGET-ID 20
     fi-run-cost-6 AT ROW 7.76 COL 20 NO-LABEL WIDGET-ID 42
     fi-setups-6 AT ROW 7.76 COL 37.6 NO-LABEL WIDGET-ID 64
     fi-run-qty-7 AT ROW 8.76 COL 2.6 NO-LABEL WIDGET-ID 22
     fi-run-cost-7 AT ROW 8.76 COL 20 NO-LABEL WIDGET-ID 44
     fi-setups-7 AT ROW 8.76 COL 37.6 NO-LABEL WIDGET-ID 66
     fi-run-qty-8 AT ROW 9.76 COL 2.6 NO-LABEL WIDGET-ID 24
     fi-run-cost-8 AT ROW 9.76 COL 20 NO-LABEL WIDGET-ID 46
     fi-setups-8 AT ROW 9.76 COL 37.6 NO-LABEL WIDGET-ID 68
     fi-run-qty-9 AT ROW 10.76 COL 2.6 NO-LABEL WIDGET-ID 26
     fi-run-cost-9 AT ROW 10.76 COL 20 NO-LABEL WIDGET-ID 48
     fi-setups-9 AT ROW 10.76 COL 37.6 NO-LABEL WIDGET-ID 70
     fi-run-qty-10 AT ROW 11.76 COL 2.6 NO-LABEL WIDGET-ID 28
     fi-run-cost-10 AT ROW 11.76 COL 20 NO-LABEL WIDGET-ID 50
     fi-setups-10 AT ROW 11.76 COL 37.6 NO-LABEL WIDGET-ID 72
     Btn_update AT ROW 13.52 COL 2.8
     Btn_Cancel AT ROW 13.52 COL 19.2
     btn-exit AT ROW 13.52 COL 35.8 WIDGET-ID 74
     "QTY to" VIEW-AS TEXT
          SIZE 15 BY 1 AT ROW 1.38 COL 3 WIDGET-ID 6
          FONT 6
     "Cost Per" VIEW-AS TEXT
          SIZE 15 BY 1 AT ROW 1.38 COL 20.2 WIDGET-ID 4
          FONT 6
     "Setup $" VIEW-AS TEXT
          SIZE 9.6 BY 1 AT ROW 1.38 COL 38 WIDGET-ID 8
          FONT 6
     SPACE(10.39) SKIP(12.90)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "More R/M Qty. Breaks"
         CANCEL-BUTTON Btn_Cancel WIDGET-ID 100.


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
       FRAME Dialog-Frame:SCROLLABLE       = FALSE.

/* SETTINGS FOR BUTTON Btn_Cancel IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-run-cost-1 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L 6                                                  */
/* SETTINGS FOR FILL-IN fi-run-cost-10 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L 6                                                  */
/* SETTINGS FOR FILL-IN fi-run-cost-2 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L 6                                                  */
/* SETTINGS FOR FILL-IN fi-run-cost-3 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L 6                                                  */
/* SETTINGS FOR FILL-IN fi-run-cost-4 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L 6                                                  */
/* SETTINGS FOR FILL-IN fi-run-cost-5 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L 6                                                  */
/* SETTINGS FOR FILL-IN fi-run-cost-6 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L 6                                                  */
/* SETTINGS FOR FILL-IN fi-run-cost-7 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L 6                                                  */
/* SETTINGS FOR FILL-IN fi-run-cost-8 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L 6                                                  */
/* SETTINGS FOR FILL-IN fi-run-cost-9 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L 6                                                  */
/* SETTINGS FOR FILL-IN fi-run-qty-1 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L 6                                                  */
/* SETTINGS FOR FILL-IN fi-run-qty-10 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L 6                                                  */
/* SETTINGS FOR FILL-IN fi-run-qty-2 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L 6                                                  */
/* SETTINGS FOR FILL-IN fi-run-qty-3 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L 6                                                  */
/* SETTINGS FOR FILL-IN fi-run-qty-4 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L 6                                                  */
/* SETTINGS FOR FILL-IN fi-run-qty-5 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L 6                                                  */
/* SETTINGS FOR FILL-IN fi-run-qty-6 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L 6                                                  */
/* SETTINGS FOR FILL-IN fi-run-qty-7 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L 6                                                  */
/* SETTINGS FOR FILL-IN fi-run-qty-8 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L 6                                                  */
/* SETTINGS FOR FILL-IN fi-run-qty-9 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L 6                                                  */
/* SETTINGS FOR FILL-IN fi-setups-1 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L 6                                                  */
/* SETTINGS FOR FILL-IN fi-setups-10 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L 6                                                  */
/* SETTINGS FOR FILL-IN fi-setups-2 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L 6                                                  */
/* SETTINGS FOR FILL-IN fi-setups-3 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L 6                                                  */
/* SETTINGS FOR FILL-IN fi-setups-4 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L 6                                                  */
/* SETTINGS FOR FILL-IN fi-setups-5 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L 6                                                  */
/* SETTINGS FOR FILL-IN fi-setups-6 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L 6                                                  */
/* SETTINGS FOR FILL-IN fi-setups-7 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L 6                                                  */
/* SETTINGS FOR FILL-IN fi-setups-8 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L 6                                                  */
/* SETTINGS FOR FILL-IN fi-setups-9 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L 6                                                  */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* More R/M Qty. Breaks */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-exit Dialog-Frame
ON CHOOSE OF btn-exit IN FRAME Dialog-Frame /* Exit */
DO:
  APPLY "WINDOW-CLOSE" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Cancel */
DO:
   DO WITH FRAME {&FRAME-NAME}:
      DISABLE {&list-6} btn_cancel.

      ASSIGN
         fi-run-qty-1:SCREEN-VALUE = STRING(fi-run-qty-1)
         fi-run-qty-2:SCREEN-VALUE = STRING(fi-run-qty-2)
         fi-run-qty-3:SCREEN-VALUE = STRING(fi-run-qty-3)
         fi-run-qty-4:SCREEN-VALUE = STRING(fi-run-qty-4)
         fi-run-qty-5:SCREEN-VALUE = STRING(fi-run-qty-5)
         fi-run-qty-6:SCREEN-VALUE = STRING(fi-run-qty-6)
         fi-run-qty-7:SCREEN-VALUE = STRING(fi-run-qty-7)
         fi-run-qty-8:SCREEN-VALUE = STRING(fi-run-qty-8)
         fi-run-qty-9:SCREEN-VALUE = STRING(fi-run-qty-9)
         fi-run-qty-10:SCREEN-VALUE = STRING(fi-run-qty-10)
         fi-run-cost-1:SCREEN-VALUE = STRING(fi-run-cost-1)
         fi-run-cost-2:SCREEN-VALUE = STRING(fi-run-cost-2)
         fi-run-cost-3:SCREEN-VALUE = STRING(fi-run-cost-3)
         fi-run-cost-4:SCREEN-VALUE = STRING(fi-run-cost-4)
         fi-run-cost-5:SCREEN-VALUE = STRING(fi-run-cost-5)
         fi-run-cost-6:SCREEN-VALUE = STRING(fi-run-cost-6)
         fi-run-cost-7:SCREEN-VALUE = STRING(fi-run-cost-7)
         fi-run-cost-8:SCREEN-VALUE = STRING(fi-run-cost-8)
         fi-run-cost-9:SCREEN-VALUE = STRING(fi-run-cost-9)
         fi-run-cost-10:SCREEN-VALUE = STRING(fi-run-cost-10)
         fi-setups-1:SCREEN-VALUE = STRING(fi-setups-1)
         fi-setups-2:SCREEN-VALUE = STRING(fi-setups-2)
         fi-setups-3:SCREEN-VALUE = STRING(fi-setups-3)
         fi-setups-4:SCREEN-VALUE = STRING(fi-setups-4)
         fi-setups-5:SCREEN-VALUE = STRING(fi-setups-5)
         fi-setups-6:SCREEN-VALUE = STRING(fi-setups-6)
         fi-setups-7:SCREEN-VALUE = STRING(fi-setups-7)
         fi-setups-8:SCREEN-VALUE = STRING(fi-setups-8)
         fi-setups-9:SCREEN-VALUE = STRING(fi-setups-9)
         fi-setups-10:SCREEN-VALUE = STRING(fi-setups-10)
         btn_update:LABEL = "Update".
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_update
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_update Dialog-Frame
ON CHOOSE OF Btn_update IN FRAME Dialog-Frame /* Update */
DO:
   DEF VAR i AS INT NO-UNDO.
   DEF VAR v-count AS INT NO-UNDO.
   DEF VAR v-update-9 AS LOG INIT TRUE NO-UNDO.

   DO WITH FRAME {&FRAME-NAME}:

      IF btn_update:LABEL = "Update" THEN
      DO:
         ENABLE {&list-6} btn_cancel.
         btn_update:LABEL = "Save".
         APPLY "ENTRY" TO fi-run-qty-1 IN FRAME {&FRAME-NAME}.
      END.
      ELSE /*Save*/
      DO:
         EMPTY TEMP-TABLE tmpfile.

         DISABLE {&list-6} btn_cancel.
         ASSIGN
            btn_update:LABEL = "Update"
            fi-run-qty-1
            fi-run-qty-2
            fi-run-qty-3
            fi-run-qty-4
            fi-run-qty-5
            fi-run-qty-6
            fi-run-qty-7
            fi-run-qty-8
            fi-run-qty-9
            fi-run-qty-10
            fi-run-cost-1
            fi-run-cost-2
            fi-run-cost-3
            fi-run-cost-4
            fi-run-cost-5
            fi-run-cost-6
            fi-run-cost-7
            fi-run-cost-8
            fi-run-cost-9
            fi-run-cost-10
            fi-setups-1
            fi-setups-2
            fi-setups-3
            fi-setups-4
            fi-setups-5
            fi-setups-6
            fi-setups-7
            fi-setups-8
            fi-setups-9
            fi-setups-10.

         

         FIND current e-item-vend EXCLUSIVE NO-ERROR.
         
         IF AVAIL e-item-vend THEN
         DO:
            do i = 1 to 10:
               create tmpfile.
           
               IF i EQ 1 THEN
                  assign
                     tmpfile.qty = fi-run-qty-1
                     tmpfile.cost = fi-run-cost-1
                     tmpfile.setups = fi-setups-1.
               ELSE
               IF i EQ 2 THEN
                  assign
                     tmpfile.qty = fi-run-qty-2
                     tmpfile.cost = fi-run-cost-2
                     tmpfile.setups = fi-setups-2.
               ELSE
               IF i EQ 3 THEN
                  assign
                     tmpfile.qty = fi-run-qty-3
                     tmpfile.cost = fi-run-cost-3
                     tmpfile.setups = fi-setups-3.
               ELSE
               IF i EQ 4 THEN
                  assign
                     tmpfile.qty = fi-run-qty-4
                     tmpfile.cost = fi-run-cost-4
                     tmpfile.setups = fi-setups-4.
               ELSE
               IF i EQ 5 THEN
                  assign
                     tmpfile.qty = fi-run-qty-5
                     tmpfile.cost = fi-run-cost-5
                     tmpfile.setups = fi-setups-5.
               ELSE
               IF i EQ 6 THEN
                  assign
                     tmpfile.qty = fi-run-qty-6
                     tmpfile.cost = fi-run-cost-6
                     tmpfile.setups = fi-setups-6.
               ELSE
               IF i EQ 7 THEN
                  assign
                     tmpfile.qty = fi-run-qty-7
                     tmpfile.cost = fi-run-cost-7
                     tmpfile.setups = fi-setups-7.
               ELSE
               IF i EQ 8 THEN
                  assign
                     tmpfile.qty = fi-run-qty-8
                     tmpfile.cost = fi-run-cost-8
                     tmpfile.setups = fi-setups-8.
               ELSE
               IF i EQ 9 THEN
                  assign
                     tmpfile.qty = fi-run-qty-9
                     tmpfile.cost = fi-run-cost-9
                     tmpfile.setups = fi-setups-9.
               ELSE
               IF i EQ 10 THEN
                  assign
                     tmpfile.qty = fi-run-qty-10
                     tmpfile.cost = fi-run-cost-10
                     tmpfile.setups = fi-setups-10.
           
               RELEASE tmpfile.
           
               ASSIGN
                  e-item-vend.runQtyXtra[i] = 0
                  e-item-vend.runCostXtra[i] = 0
                  e-item-vend.setupsXtra[i] = 0.
            end.
           
            i = 1.
            
            FOR EACH tmpfile WHERE tmpfile.qty GT 0 BREAK BY tmpfile.qty:
            
               ASSIGN
                  e-item-vend.runQtyXtra[i] = tmpfile.qty
                  e-item-vend.runCostXtra[i] = tmpfile.cost
                  e-item-vend.setupsXtra[i] = tmpfile.setups.

               IF LAST(tmpfile.qty) THEN
               DO:
                  DO v-count = 1 TO 10:
                     IF e-item-vend.run-qty[v-count] GT tmpfile.qty THEN
                     DO:
                        v-update-9 = FALSE.
                        LEAVE.
                     END.
                  END.

                  IF v-update-9 THEN
                     e-item-vend.runQtyXtra[i] = 9999999.9.
               END.

               i = i + 1.
            END.

            IF e-item-vend.vend-no EQ "" THEN
            DO:

               FIND current e-item-vend EXCLUSIVE NO-ERROR.
               IF AVAIL e-item THEN
               DO:
                  ASSIGN
                     i = 1
                     e-item.run-qty = 0
                     e-item.run-cost = 0.


                  FOR EACH tmpfile WHERE tmpfile.qty GT 0 BREAK BY tmpfile.qty:
                      ASSIGN
                         e-item.run-qty[i] = tmpfile.qty
                         e-item.run-cost[i] = tmpfile.cost.

                      IF LAST(tmpfile.qty) THEN
                         e-item.run-qty[i] = 9999999.9.               
                      i = i + 1.
                  END.
               END.
            END.
           
            ASSIGN
            e-item-vend.updated-id[1] = USERID("NOSWEAT")
            e-item-vend.updated-date[1] = TODAY.
            FIND CURRENT e-item-vend NO-LOCK.
            FIND CURRENT e-item NO-LOCK.

            
                        
            btn_update:LABEL = "Update".

            RUN init-proc.
         END.
      END.
   END.
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

   FIND e-item-vend WHERE ROWID(e-item-vend) EQ ip-e-item-vend NO-LOCK NO-ERROR.
   FIND e-item WHERE ROWID(e-item) EQ ip-e-item NO-LOCK NO-ERROR.

   IF NOT AVAIL e-item-vend OR NOT AVAIL e-item THEN LEAVE.

   RUN init-proc.

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
  DISPLAY fi-run-qty-1 fi-run-cost-1 fi-setups-1 fi-run-qty-2 fi-run-cost-2 
          fi-setups-2 fi-run-qty-3 fi-run-cost-3 fi-setups-3 fi-run-qty-4 
          fi-run-cost-4 fi-setups-4 fi-run-qty-5 fi-run-cost-5 fi-setups-5 
          fi-run-qty-6 fi-run-cost-6 fi-setups-6 fi-run-qty-7 fi-run-cost-7 
          fi-setups-7 fi-run-qty-8 fi-run-cost-8 fi-setups-8 fi-run-qty-9 
          fi-run-cost-9 fi-setups-9 fi-run-qty-10 fi-run-cost-10 fi-setups-10 
      WITH FRAME Dialog-Frame.
  ENABLE Btn_update btn-exit 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE init-proc Dialog-Frame 
PROCEDURE init-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   
   DO WITH FRAME {&FRAME-NAME}:
   
      ASSIGN
         fi-run-qty-1 = e-item-vend.runQtyXtra[1]
         fi-run-qty-2 = e-item-vend.runQtyXtra[2]
         fi-run-qty-3 = e-item-vend.runQtyXtra[3]
         fi-run-qty-4 = e-item-vend.runQtyXtra[4]
         fi-run-qty-5 = e-item-vend.runQtyXtra[5]
         fi-run-qty-6 = e-item-vend.runQtyXtra[6]
         fi-run-qty-7 = e-item-vend.runQtyXtra[7]
         fi-run-qty-8 = e-item-vend.runQtyXtra[8]
         fi-run-qty-9 = e-item-vend.runQtyXtra[9]
         fi-run-qty-10 = e-item-vend.runQtyXtra[10]
         fi-run-cost-1 = e-item-vend.runCostXtra[1]
         fi-run-cost-2 = e-item-vend.runCostXtra[2]
         fi-run-cost-3 = e-item-vend.runCostXtra[3]
         fi-run-cost-4 = e-item-vend.runCostXtra[4]
         fi-run-cost-5 = e-item-vend.runCostXtra[5]
         fi-run-cost-6 = e-item-vend.runCostXtra[6]
         fi-run-cost-7 = e-item-vend.runCostXtra[7]
         fi-run-cost-8 = e-item-vend.runCostXtra[8]
         fi-run-cost-9 = e-item-vend.runCostXtra[9]
         fi-run-cost-10 = e-item-vend.runCostXtra[10]
         fi-setups-1 = e-item-vend.setupsXtra[1]
         fi-setups-2 = e-item-vend.setupsXtra[2]
         fi-setups-3 = e-item-vend.setupsXtra[3]
         fi-setups-4 = e-item-vend.setupsXtra[4]
         fi-setups-5 = e-item-vend.setupsXtra[5]
         fi-setups-6 = e-item-vend.setupsXtra[6]
         fi-setups-7 = e-item-vend.setupsXtra[7]
         fi-setups-8 = e-item-vend.setupsXtra[8]
         fi-setups-9 = e-item-vend.setupsXtra[9]
         fi-setups-10 = e-item-vend.setupsXtra[10].

     DISPLAY fi-run-qty-1 fi-run-qty-2 fi-run-qty-3 fi-run-qty-4 fi-run-qty-5
            fi-run-qty-6 fi-run-qty-7 fi-run-qty-8 fi-run-qty-9 fi-run-qty-10
            fi-run-cost-1 fi-run-cost-2 fi-run-cost-3 fi-run-cost-4 fi-run-cost-5
            fi-run-cost-6 fi-run-cost-7 fi-run-cost-8 fi-run-cost-9 fi-run-cost-10
            fi-setups-1 fi-setups-2 fi-setups-3 fi-setups-4 fi-setups-5 fi-setups-6
            fi-setups-7 fi-setups-8 fi-setups-9 fi-setups-10 WITH FRAME {&FRAME-NAME}.
  END. 


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

