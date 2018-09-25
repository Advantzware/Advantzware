&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: oerep\b-trksch.w

  Description: Truck Release Selection

  Author: Eric Panchenko

  Created: April 3, 2007
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

{oerep/tt-truck-stop.i}

&SCOPED-DEFINE yellowColumnsName truck-run
&SCOPED-DEFINE cellColumnDat truck-run

DEF VAR lv-error AS LOG NO-UNDO.
DEF VAR v-tot-msf AS DECI NO-UNDO.
DEF VAR v-tot-weight AS DECI NO-UNDO.
DEF VAR v-col-move as log init yes no-undo.

DEFINE BUFFER b-tt-report FOR tt-report.

{custom/globdefs.i}
{sys/inc/VAR.i "new shared" }
ASSIGN cocode = g_company
       locode = g_loc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-report

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 tt-report.carrier tt-report.truck-code tt-report.truck-dscr tt-report.order-no tt-report.load-no tt-report.ship-date tt-report.stop-no tt-report.cust-name tt-report.cust-no tt-report.item-no tt-report.rel-no tt-report.bol-no tt-report.city tt-report.state tt-report.zip tt-report.ship-no tt-report.deliv-zone tt-report.pallets tt-report.no-units tt-report.tot-units tt-report.msf tt-report.tot-msf tt-report.weight tt-report.tot-weight tt-report.release-type   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2 tt-report.carrier tt-report.truck-code tt-report.stop-no tt-report.load-no tt-report.ship-date tt-report.pallets   
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-2 tt-report
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-2 tt-report
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH tt-report USE-INDEX tt-trk-stop-3 NO-LOCK ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME} FOR EACH tt-report USE-INDEX tt-trk-stop-3 NO-LOCK ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 tt-report
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 tt-report


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-D-Dialog ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-2 bUpdate bDelete bExcel bExit ~
btn-move btCopy 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON bCancel 
     LABEL "&Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON bDelete 
     LABEL "&Delete" 
     SIZE 15 BY 1.14.

DEFINE BUTTON bExcel 
     LABEL "Print To Excel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON bExit 
     LABEL "E&xit" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btCopy 
     LABEL "Copy" 
     SIZE 13 BY 1.14.

DEFINE BUTTON btn-move 
     LABEL "Move Col." 
     SIZE 15 BY 1.14.

DEFINE BUTTON bUpdate 
     LABEL "&Update" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE fi_sortby AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      tt-report SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 D-Dialog _FREEFORM
  QUERY BROWSE-2 DISPLAY
      tt-report.carrier COLUMN-LABEL "Carrier" WIDTH 10 LABEL-BGCOLOR 14
   tt-report.truck-code COLUMN-LABEL "Trailer#" WIDTH 20 LABEL-BGCOLOR 14
   tt-report.truck-dscr COLUMN-LABEL "Descrp" WIDTH 23 LABEL-BGCOLOR 14
   tt-report.order-no COLUMN-LABEL "Order #" LABEL-BGCOLOR 14
   tt-report.load-no     COLUMN-LABEL "Load #" WIDTH 10 LABEL-BGCOLOR 14
   tt-report.ship-date COLUMN-LABEL "Ship Date" FORMAT "99/99/99" LABEL-BGCOLOR 14 WIDTH 13
   tt-report.stop-no COLUMN-LABEL "Stop" LABEL-BGCOLOR 14 WIDTH 7
   tt-report.cust-name COLUMN-LABEL "Cust Name" WIDTH 28 LABEL-BGCOLOR 14
   tt-report.cust-no COLUMN-LABEL "Cust #" WIDTH 11 LABEL-BGCOLOR 14
   tt-report.item-no  COLUMN-LABEL "FG Item #" WIDTH 21 LABEL-BGCOLOR 14
   tt-report.rel-no COLUMN-LABEL "Rel #" WIDTH 11 FORMAT ">>>>>>>>>>" LABEL-BGCOLOR 14
   tt-report.bol-no COLUMN-LABEL "BOL #" FORMAT ">>>>>>>>" LABEL-BGCOLOR 14
   tt-report.city    COLUMN-LABEL "City" WIDTH 14 LABEL-BGCOLOR 14
   tt-report.state   COLUMN-LABEL "St" WIDTH 4 LABEL-BGCOLOR 14
   tt-report.zip     COLUMN-LABEL "Zip"   WIDTH 10 LABEL-BGCOLOR 14
   tt-report.ship-no COLUMN-LABEL "Ship To" WIDTH 14 LABEL-BGCOLOR 14
   tt-report.deliv-zone COLUMN-LABEL "Zone" LABEL-BGCOLOR 14
   tt-report.pallets COLUMN-LABEL "Pallets" FORM "->>>>>>9" LABEL-BGCOLOR 14 WIDTH 10
   tt-report.no-units COLUMN-LABEL "Units" FORM "->>>>>>9" LABEL-BGCOLOR 14 WIDTH 10
   tt-report.tot-units COLUMN-LABEL "Tot Load" FORM "->>>>>>>9" LABEL-BGCOLOR 14
   tt-report.msf COLUMN-LABEL "MSF" FORM "->>>>9.9999" LABEL-BGCOLOR 14 WIDTH 15
   tt-report.tot-msf COLUMN-LABEL "Total MSF" FORM "->>>>>9.9999" LABEL-BGCOLOR 14 WIDTH 15
   tt-report.weight COLUMN-LABEL "Weight" FORM "->>>>>>9.99" LABEL-BGCOLOR 14 WIDTH 14
   tt-report.tot-weight COLUMN-LABEL "Tot Wght" FORM "->>>>>>>9.99" LABEL-BGCOLOR 14 WIDTH 16
   tt-report.release-type COLUMN-LABEL "Type" FORMAT "X(1)" LABEL-BGCOLOR 14
   ENABLE tt-report.carrier tt-report.truck-code tt-report.stop-no tt-report.load-no tt-report.ship-date tt-report.pallets
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 260 BY 15.95
         BGCOLOR 8 FONT 2 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     BROWSE-2 AT ROW 1 COL 2
     bUpdate AT ROW 17.91 COL 57.8
     bCancel AT ROW 17.91 COL 77.2
     bDelete AT ROW 17.91 COL 96.6 WIDGET-ID 4
     bExcel AT ROW 17.91 COL 116
     bExit AT ROW 17.91 COL 135.4
     btn-move AT ROW 17.91 COL 154.8 WIDGET-ID 2
     btCopy AT ROW 17.91 COL 174.2 WIDGET-ID 6
     fi_sortby AT ROW 19.57 COL 166 COLON-ALIGNED NO-LABEL
     SPACE(35.39) SKIP(0.42)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Truck Run Selection".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Design Page: 2
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{custom/yellowcolumns.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB BROWSE-2 1 D-Dialog */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON bCancel IN FRAME D-Dialog
   NO-ENABLE                                                            */
ASSIGN 
       BROWSE-2:NUM-LOCKED-COLUMNS IN FRAME D-Dialog     = 5
       BROWSE-2:ALLOW-COLUMN-SEARCHING IN FRAME D-Dialog = TRUE.

/* SETTINGS FOR FILL-IN fi_sortby IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       fi_sortby:HIDDEN IN FRAME D-Dialog           = TRUE
       fi_sortby:READ-ONLY IN FRAME D-Dialog        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-report USE-INDEX tt-trk-stop-3 NO-LOCK ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Truck Run Selection */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bCancel D-Dialog
ON CHOOSE OF bCancel IN FRAME D-Dialog /* Cancel */
DO:
   IF BROWSE {&browse-name}:NUM-SELECTED-ROWS NE 0 THEN
   DO:
      ASSIGN tt-report.carrier:SCREEN-VALUE
             IN BROWSE {&browse-name} = tt-report.old-carrier
             tt-report.truck-code:SCREEN-VALUE
             IN BROWSE {&browse-name} =  tt-report.old-truck-code
             tt-report.truck-dscr:SCREEN-VALUE
             IN BROWSE {&browse-name} =  tt-report.old-truck-dscr
             tt-report.stop-no:SCREEN-VALUE
             IN BROWSE {&browse-name} = STRING(tt-report.old-stop-no)
             tt-report.load-no:SCREEN-VALUE
             IN BROWSE {&browse-name} = tt-report.old-load-no
             tt-report.ship-date:SCREEN-VALUE
             IN BROWSE {&browse-name} = STRING(tt-report.old-ship-date).
     
      RUN set-read-only(INPUT YES).

      APPLY "ENTRY" TO bUpdate IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bDelete D-Dialog
ON CHOOSE OF bDelete IN FRAME D-Dialog /* Delete */
DO:
   DEF VAR v-ans AS LOG NO-UNDO.
   DEF VAR v-index AS INT NO-UNDO.

   IF AVAIL tt-report THEN
   DO:
      MESSAGE "Delete Selected Truck Run Lines?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE v-ans.

      IF v-ans THEN
      DO:
         DO v-index = 1 TO browse-2:NUM-SELECTED-ROWS:

            browse-2:FETCH-SELECTED-ROW(v-index).

            IF tt-report.link-no EQ 0 AND tt-report.oe-rel-r-no NE 0 THEN
               FIND FIRST truck-run-print WHERE
                    truck-run-print.company EQ tt-report.company AND
                    truck-run-print.oe-rel-r-no EQ tt-report.oe-rel-r-no AND
                    truck-run-print.rec_key = tt-report.truck-print-key
                    EXCLUSIVE-LOCK NO-ERROR.
           
            ELSE IF tt-report.link-no EQ 0 THEN
               FIND FIRST truck-run-print WHERE
                    truck-run-print.company EQ tt-report.company AND
                    truck-run-print.ord-no  EQ tt-report.order-no AND
                    truck-run-print.i-no  EQ tt-report.item-no AND
                    truck-run-print.line  EQ tt-report.line-no AND
                    truck-run-print.rel-no EQ tt-report.rel-no-internal AND
                    truck-run-print.b-ord-no EQ tt-report.b-ord-no AND
                    truck-run-print.po-no  EQ tt-report.po-no AND
                    truck-run-print.rec_key = tt-report.truck-print-key
                    EXCLUSIVE-LOCK NO-ERROR.
           
            ELSE
               FIND FIRST truck-run-print WHERE
                    truck-run-print.company EQ tt-report.company AND
                    truck-run-print.link-no EQ tt-report.link-no AND
                    truck-run-print.ord-no  EQ tt-report.order-no AND
                    truck-run-print.rel-no EQ tt-report.rel-no-internal AND
                    truck-run-print.b-ord-no EQ tt-report.b-ord-no AND
                    truck-run-print.i-no  EQ tt-report.item-no AND
                    truck-run-print.line  EQ tt-report.line-no AND
                    truck-run-print.po-no  EQ tt-report.po-no AND
                    truck-run-print.rec_key = tt-report.truck-print-key
                    EXCLUSIVE-LOCK NO-ERROR.
           
            IF AVAIL truck-run-print THEN
               DELETE truck-run-print.
           
            DELETE tt-report.

         END. /*end of selected rows loop*/

         RUN calc-tot-wgt-proc.

         CLOSE QUERY browse-2.
         OPEN QUERY browse-2 FOR EACH tt-report USE-INDEX tt-trk-stop-3 NO-LOCK.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bExcel D-Dialog
ON CHOOSE OF bExcel IN FRAME D-Dialog /* Print To Excel */
DO:
  RUN oerep\trkschxl.p(INPUT YES,
                       INPUT 55).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bExit D-Dialog
ON CHOOSE OF bExit IN FRAME D-Dialog /* Exit */
DO:
  APPLY 'GO':U TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&Scoped-define SELF-NAME BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 D-Dialog
ON HELP OF BROWSE-2 IN FRAME D-Dialog
DO:
   DEF VAR lv-focus AS WIDGET-HANDLE NO-UNDO.
   DEF VAR char-val AS CHAR NO-UNDO.

   lv-focus = FOCUS.
   CASE lv-focus:NAME:

      WHEN "carrier" THEN
      DO:
         run windows/l-carrie.w (cocode,locode, focus:screen-value, output char-val).
         if char-val NE "" AND entry(1,char-val) NE FOCUS:SCREEN-VALUE IN FRAME {&FRAME-NAME} THEN
            focus:screen-value = entry(1,char-val).
      END.
      WHEN "truck-code" THEN
      DO:
         run browsers/l-truck2.w (cocode,locode,tt-report.carrier:SCREEN-VALUE IN BROWSE {&browse-name},lv-focus:screen-value, output char-val).
         if char-val <> "" THEN
         DO:
            assign lv-focus:screen-value = entry(1,char-val).
            FIND FIRST truck WHERE
                 truck.company = cocode AND
                 truck.loc = locode AND
                 truck.carrier = tt-report.carrier:SCREEN-VALUE IN BROWSE {&browse-name} AND
                 truck.truck-code = lv-focus:screen-value
                 NO-LOCK NO-ERROR.

            IF AVAIL truck THEN
            DO:
               tt-report.truck-dscr:SCREEN-VALUE IN BROWSE {&browse-name} = truck.truck-desc.
               RELEASE truck.
            END.
         END.
      END.
   END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 D-Dialog
ON MOUSE-SELECT-CLICK OF BROWSE-2 IN FRAME D-Dialog
DO:
  IF bUpdate:LABEL = "&Save" THEN
     APPLY "CHOOSE" TO bUpdate IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 D-Dialog
ON START-SEARCH OF BROWSE-2 IN FRAME D-Dialog
DO:
  RUN startSearch.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btCopy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCopy D-Dialog
ON CHOOSE OF btCopy IN FRAME D-Dialog /* Copy */
DO:
  DEF VAR v-ans AS LOG NO-UNDO.
  DEF VAR v-index AS INT NO-UNDO.
  DEF VAR v-next-seq AS INT NO-UNDO.
  DEF VAR v-truck-run-rowid AS ROWID NO-UNDO.

  DEF BUFFER bf-tt-report FOR tt-report.
  IF AVAIL tt-report THEN
  DO:
     IF browse-2:NUM-SELECTED-ROWS > 1 THEN DO:
         MESSAGE "Only one row can be selected for copy.~
Please select one row and try again."
             VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
     END.
     MESSAGE "Copy Selected Truck Run Lines?" tt-report.order-no
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE v-ans.

     IF v-ans THEN
     DO:
        DO v-index = 1 TO browse-2:NUM-SELECTED-ROWS:

           browse-2:FETCH-SELECTED-ROW(v-index).
           v-next-seq = 0.
           FOR LAST bf-tt-report WHERE bf-tt-report.company = tt-report.company
               AND bf-tt-report.order-no EQ tt-report.order-no
               AND bf-tt-report.b-ord-no   EQ tt-report.b-ord-no
               AND bf-tt-report.item-no  EQ tt-report.item-no
               AND bf-tt-report.line-no  EQ tt-report.line-no
               AND bf-tt-report.rel-no EQ tt-report.rel-no
               AND bf-tt-report.b-ord-no EQ tt-report.b-ord-no
               AND bf-tt-report.po-no  EQ tt-report.po-no
               BY bf-tt-report.load-no.
               v-next-seq = integer(bf-tt-report.load-no).
           END.
           v-next-seq = v-next-seq + 1.

           CREATE bf-tt-report.
           BUFFER-COPY tt-report TO bf-tt-report.
           bf-tt-report.load-no = STRING(v-next-seq).

           RUN add-truck-run (INPUT rowid(bf-tt-report), INPUT YES, OUTPUT v-truck-run-rowid).


        END. /*end of selected rows loop*/

        RUN calc-tot-wgt-proc.
        
        CLOSE QUERY browse-2.
        OPEN QUERY browse-2 FOR EACH tt-report USE-INDEX tt-trk-stop-3 NO-LOCK.
        RUN local-initialize.
        LEAVE.
     END.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-move
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-move D-Dialog
ON CHOOSE OF btn-move IN FRAME D-Dialog /* Move Col. */
DO:
   DO WITH FRAME {&FRAME-NAME}:
     ASSIGN
        Browse-2:COLUMN-MOVABLE = v-col-move
        Browse-2:COLUMN-RESIZABLE = v-col-move
        v-col-move = NOT v-col-move
        btn-move:LABEL = IF v-col-move = NO THEN "Sort Col." ELSE "Move Col.".
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bUpdate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bUpdate D-Dialog
ON CHOOSE OF bUpdate IN FRAME D-Dialog /* Update */
DO:
  DEF VAR op-error AS LOG NO-UNDO.

  IF AVAIL tt-report THEN
  DO:
     IF bUpdate:LABEL = "&Update" THEN
     DO:
        RUN set-read-only(INPUT NO).
        APPLY "ENTRY" TO tt-report.pallets IN BROWSE {&browse-name}.
     END.
     ELSE
     DO:
        RUN update-record(OUTPUT op-error).

        IF NOT op-error THEN
        DO:
           RUN set-read-only(INPUT YES).
           APPLY "ENTRY" TO browse-2 IN FRAME {&FRAME-NAME}.
           bUpdate:LABEL = "&Update".
        END.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */
SESSION:DATA-ENTRY-RETURN = YES.

{methods/browsers/setCellColumns.i}

ON 'leave':U OF tt-report.carrier IN BROWSE {&browse-name} DO:
   IF LASTKEY NE -1 AND
      tt-report.carrier:SCREEN-VALUE IN BROWSE {&browse-name} NE "" AND
      NOT CAN-FIND(FIRST carrier WHERE
          carrier.company EQ cocode AND
          carrier.loc EQ locode AND
          carrier.carrier EQ tt-report.carrier:SCREEN-VALUE IN BROWSE {&browse-name}) THEN
          DO:
            MESSAGE "Invalid Carrier."
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            RETURN NO-APPLY.
          END.
END.

ON 'leave':U OF tt-report.truck-code IN BROWSE {&browse-name} DO:
  IF LASTKEY NE -1 THEN
  DO:
     IF tt-report.truck-code:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN
     DO:
        FIND FIRST truck WHERE
             truck.company = cocode AND
             truck.loc = locode AND
             truck.carrier = tt-report.carrier:SCREEN-VALUE IN BROWSE {&browse-name} AND
             truck.truck-code = tt-report.truck-code:SCREEN-VALUE IN BROWSE {&browse-name}
             NO-LOCK NO-ERROR.
       
        IF NOT AVAIL truck THEN
        DO:
           MESSAGE "Invalid Truck Code."
               VIEW-AS ALERT-BOX ERROR BUTTONS OK.
           RETURN NO-APPLY.
        END.
        ELSE
        DO:
           tt-report.truck-dscr:SCREEN-VALUE = truck.truck-desc.
           RELEASE truck.
        END.
     END.
     ELSE
        tt-report.truck-dscr:SCREEN-VALUE = "".
  END.
END.

ON 'leave':U OF tt-report.LOAD-NO IN BROWSE {&browse-name} DO:

   IF INT(tt-report.stop-no:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN
   DO:
      FIND LAST b-tt-report WHERE
           b-tt-report.carrier = tt-report.carrier:SCREEN-VALUE IN BROWSE {&browse-name} AND
           b-tt-report.truck-code = tt-report.truck-code:SCREEN-VALUE IN BROWSE {&browse-name} AND
           b-tt-report.ship-date = DATE(tt-report.ship-date:SCREEN-VALUE IN BROWSE {&browse-name}) AND
           b-tt-report.load-no = tt-report.load-no:SCREEN-VALUE IN BROWSE {&browse-name} AND
           b-tt-report.unique-no NE tt-report.unique-no
           NO-ERROR.
     
      IF AVAIL b-tt-report THEN
      DO:
         tt-report.stop-no:SCREEN-VALUE = STRING(b-tt-report.stop-no + 1).
         RELEASE b-tt-report.
      END.
   END.
END.

ON 'leave':U OF tt-report.stop-no IN BROWSE {&browse-name} DO:

   IF LASTKEY EQ KEYCODE("tab") THEN
   DO:
      APPLY "CHOOSE":U TO bUpdate IN FRAME {&FRAME-NAME}.
      browse-2:SELECT-FOCUSED-ROW().
      RETURN NO-APPLY.
   END.
END.

ON 'return':U OF tt-report.carrier,tt-report.truck-code,tt-report.stop-no,tt-report.load-no
   IN BROWSE {&browse-name} DO:
   APPLY "CHOOSE":U TO bUpdate IN FRAME {&FRAME-NAME}.
   browse-2:SELECT-FOCUSED-ROW().
   RETURN NO-APPLY.
END.

RUN set-read-only(INPUT YES).

RUN setCellColumns.

APPLY "entry" TO BROWSE {&browse-name}.

{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-truck-run D-Dialog 
PROCEDURE add-truck-run :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER tt-report-rowid AS ROWID.
DEF INPUT PARAMETER always-new AS LOG.
DEF OUTPUT PARAMETER truck-run-rowid AS ROWID.
DEF BUFFER bf-tt-report FOR tt-report.

  FIND bf-tt-report WHERE ROWID(bf-tt-report) = tt-report-rowid.
  /* FIND truck-run-print WHERE truck-run-print.rec_key = bf-tt-report.truck-print-key 
      NO-ERROR. */
      IF bf-tt-report.link-no EQ 0 AND bf-tt-report.oe-rel-r-no NE 0 THEN
         FIND FIRST truck-run-print WHERE
              truck-run-print.company EQ bf-tt-report.company AND
              truck-run-print.oe-rel-r-no EQ bf-tt-report.oe-rel-r-no AND
              bf-tt-report.truck-print-key = truck-run-print.rec_key
              EXCLUSIVE-LOCK NO-ERROR.

      ELSE IF bf-tt-report.link-no EQ 0 THEN
         FIND FIRST truck-run-print WHERE
              truck-run-print.company EQ bf-tt-report.company AND
              truck-run-print.ord-no  EQ bf-tt-report.order-no AND
              truck-run-print.i-no  EQ bf-tt-report.item-no AND
              truck-run-print.line  EQ bf-tt-report.line-no AND
              truck-run-print.rel-no EQ bf-tt-report.rel-no-internal AND
              truck-run-print.b-ord-no EQ bf-tt-report.b-ord-no AND
              truck-run-print.po-no  EQ bf-tt-report.po-no AND
              bf-tt-report.truck-print-key = truck-run-print.rec_key
              EXCLUSIVE-LOCK NO-ERROR.
     
      ELSE
         FIND FIRST truck-run-print WHERE
              truck-run-print.company EQ bf-tt-report.company AND
              truck-run-print.link-no EQ bf-tt-report.link-no AND
              truck-run-print.ord-no  EQ bf-tt-report.order-no AND
              truck-run-print.rel-no EQ bf-tt-report.rel-no-internal AND
              truck-run-print.b-ord-no EQ bf-tt-report.b-ord-no AND
              truck-run-print.i-no  EQ bf-tt-report.item-no AND
              truck-run-print.line  EQ bf-tt-report.line-no AND
              truck-run-print.po-no  EQ bf-tt-report.po-no AND
             bf-tt-report.truck-print-key = truck-run-print.rec_key
              EXCLUSIVE-LOCK NO-ERROR.      

      IF NOT AVAIL truck-run-print OR always-new THEN DO:
         CREATE truck-run-print.
         ASSIGN truck-run-print.company = bf-tt-report.company 
                truck-run-print.ord-no  = bf-tt-report.order-no 
                truck-run-print.i-no  = bf-tt-report.item-no
                truck-run-print.line  = bf-tt-report.line-no
                truck-run-print.rel-no = bf-tt-report.rel-no-internal
                truck-run-print.b-ord-no = bf-tt-report.b-ord-no
                truck-run-print.po-no = bf-tt-report.po-no
                truck-run-print.oe-rel-r-no = bf-tt-report.oe-rel-r-no
                truck-run-print.link-no = bf-tt-report.link-no
                truck-run-print.oe-rel-r-no = bf-tt-report.oe-rel-r-no
                truck-run-print.spare-char-2 = string(bf-tt-report.is-orig)
                bf-tt-report.truck-print-key = truck-run-print.rec_key.
         IF always-new = NO THEN
           truck-run-print.spare-char-1 = bf-tt-report.rec_key.

      END.
      
      ASSIGN
         truck-run-print.carrier = bf-tt-report.carrier
         truck-run-print.truck-code  = bf-tt-report.truck-code
         truck-run-print.load-no = bf-tt-report.load-no
         truck-run-print.stop-no = bf-tt-report.stop-no
         truck-run-print.ship-date = bf-tt-report.ship-date.
      truck-run-rowid = ROWID(truck-run-print).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-tot-wgt-proc D-Dialog 
PROCEDURE calc-tot-wgt-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bf-tt-report FOR tt-report.
  DEF BUFFER bf2-tt-report FOR tt-report.

  FOR EACH bf2-tt-report:

      ASSIGN
         bf2-tt-report.tot-msf = 0
         bf2-tt-report.tot-weight = 0
         bf2-tt-report.tot-units = 0.

      IF bf2-tt-report.carrier NE "" AND bf2-tt-report.truck-code NE "" THEN
      FOR EACH bf-tt-report WHERE
          bf-tt-report.carrier    = bf2-tt-report.carrier AND
          bf-tt-report.truck-code = bf2-tt-report.truck-code AND
          bf-tt-report.load-no = bf2-tt-report.load-no AND
          bf-tt-report.ship-date = bf2-tt-report.ship-date:

          ASSIGN
           bf2-tt-report.tot-msf = bf2-tt-report.tot-msf + bf-tt-report.msf
           bf2-tt-report.tot-weight = bf2-tt-report.tot-weight + bf-tt-report.weight
           bf2-tt-report.tot-units = bf2-tt-report.tot-units + bf-tt-report.pallets.
      END.
  END.
  
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
  ENABLE BROWSE-2 bUpdate bDelete bExcel bExit btn-move btCopy 
      WITH FRAME D-Dialog.
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
  RUN calc-tot-wgt-proc.
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

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-report"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-read-only D-Dialog 
PROCEDURE set-read-only :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-log AS LOG NO-UNDO.
  
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      tt-report.pallets:READ-ONLY IN BROWSE {&browse-name} = ip-log
      tt-report.carrier:READ-ONLY IN BROWSE {&browse-name} = ip-log
      tt-report.truck-code:read-only IN BROWSE {&browse-name} = ip-log
      tt-report.stop-no:read-only    IN BROWSE {&browse-name} = ip-log
      tt-report.load-no:READ-ONLY    IN BROWSE {&browse-name} = ip-log
      tt-report.ship-date:READ-ONLY  IN BROWSE {&browse-name} = ip-log
      bCancel:SENSITIVE = NOT ip-log
      bUpdate:LABEL = IF ip-log THEN "&Update" ELSE "&Save".
    
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-bol D-Dialog 
PROCEDURE update-bol :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FIND FIRST oe-bolh WHERE
        oe-bolh.company  EQ cocode AND
        oe-bolh.bol-no EQ tt-report.bol-no
        EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

   IF AVAIL oe-bolh THEN
   DO:
      ASSIGN
         oe-bolh.carrier = tt-report.carrier
         oe-bolh.trailer = tt-report.truck-code
         oe-bolh.ship-date = tt-report.ship-date.
      FIND CURRENT oe-bolh NO-LOCK.
      RELEASE oe-bolh.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-record D-Dialog 
PROCEDURE update-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF OUTPUT PARAMETER op-error AS LOG NO-UNDO.
 DEF VAR v-warn AS LOG NO-UNDO.
 DEF VAR v-rowid AS ROWID NO-UNDO.
 DEF VAR v-row AS INT NO-UNDO.
 DEF VAR v-truck-run-rowid AS ROWID.
 DO WITH FRAME {&FRAME-NAME}:
    
   ASSIGN
     v-rowid = ROWID(tt-report)
     v-row   = browse-2:FOCUSED-ROW.

   IF INT(tt-report.stop-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0 THEN
   DO:
      IF tt-report.carrier:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" THEN
      DO:
         MESSAGE "Carrier Cannot Be Blank."
             VIEW-AS ALERT-BOX ERROR BUTTONS OK.

         op-error = YES.
         APPLY "ENTRY":U TO tt-report.carrier IN BROWSE {&browse-name}.
         RETURN NO-APPLY.
      END.

      IF tt-report.truck-code:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" THEN
      DO:
         MESSAGE "Truck Code Cannot Be Blank."
             VIEW-AS ALERT-BOX ERROR BUTTONS OK.

         op-error = YES.
         APPLY "ENTRY":U TO tt-report.truck-code IN BROWSE {&browse-name}.
         RETURN NO-APPLY.
      END.

      IF NOT op-error AND
         tt-report.load-no:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" THEN
         DO:
            MESSAGE "Load # Cannot Be Blank."
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            op-error = YES.
            APPLY "ENTRY":U TO tt-report.load-no IN BROWSE {&browse-name}.
            RETURN NO-APPLY.
         END.

      IF NOT op-error AND
         DATE(tt-report.ship-date:SCREEN-VALUE IN BROWSE {&browse-name}) EQ ? THEN
         DO:
            MESSAGE "Ship Date Cannot Be Blank."
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            op-error = YES.
            APPLY "ENTRY":U TO tt-report.ship-date IN BROWSE {&browse-name}.
            RETURN NO-APPLY.
         END.
   END.

   IF tt-report.carrier:SCREEN-VALUE IN BROWSE {&browse-name} NE "" AND
      NOT CAN-FIND(FIRST carrier WHERE
      carrier.company EQ cocode AND
      carrier.loc EQ locode AND
      carrier.carrier EQ tt-report.carrier:SCREEN-VALUE IN BROWSE {&browse-name}) THEN
      DO:
        MESSAGE "Invalid Carrier."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        op-error = YES.
        APPLY "ENTRY":U TO tt-report.carrier IN BROWSE {&browse-name}.
        RETURN NO-APPLY.
      END.

   IF tt-report.truck-code:SCREEN-VALUE IN BROWSE {&browse-name} NE "" THEN
   DO:
      FIND FIRST truck WHERE
           truck.company = cocode AND
           truck.loc     = locode AND
           truck.carrier = tt-report.carrier:SCREEN-VALUE IN BROWSE {&browse-name} AND
           truck.truck-code = tt-report.truck-code:SCREEN-VALUE IN BROWSE {&browse-name}
           NO-LOCK NO-ERROR.
     
      IF NOT AVAIL truck THEN
      DO:
         MESSAGE "Invalid Truck Code."
             VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         op-error = YES.
         APPLY "ENTRY":U TO tt-report.truck-code IN BROWSE {&browse-name}.
         RETURN NO-APPLY.
      END.
      ELSE
      DO:
         tt-report.truck-dscr:SCREEN-VALUE = truck.truck-desc.
         RELEASE truck.
      END.
   END.
   ELSE
      tt-report.truck-dscr:SCREEN-VALUE = "".
   
   IF NOT op-error THEN
   DO:       
      RUN add-truck-run (INPUT rowid(tt-report), INPUT NO, OUTPUT v-truck-run-rowid).
      FIND truck-run-print WHERE rowid(truck-run-print) = v-truck-run-rowid
          EXCLUSIVE-LOCK.

      ASSIGN
         truck-run-print.carrier = tt-report.carrier:SCREEN-VALUE IN BROWSE {&browse-name}
         truck-run-print.truck-code  = tt-report.truck-code:SCREEN-VALUE IN BROWSE {&browse-name}
         truck-run-print.load-no = tt-report.load-no:SCREEN-VALUE IN BROWSE {&browse-name}
         truck-run-print.stop-no = INT(tt-report.stop-no:SCREEN-VALUE IN BROWSE {&browse-name})
         truck-run-print.ship-date = DATE(tt-report.ship-date:SCREEN-VALUE IN BROWSE {&browse-name})
         truck-run-print.spare-int-1 = int(tt-report.pallets:SCREEN-VALUE IN BROWSE {&browse-name}).

      FIND CURRENT truck-run-print NO-LOCK.
      RELEASE truck-run-print.
   END.
   
   ASSIGN tt-report.carrier    = tt-report.carrier:SCREEN-VALUE IN BROWSE {&browse-name}
          tt-report.truck-code = tt-report.truck-code:SCREEN-VALUE IN BROWSE {&browse-name}
          tt-report.truck-dscr = tt-report.truck-dscr:SCREEN-VALUE IN BROWSE {&browse-name}
          tt-report.stop-no = INT(tt-report.stop-no:SCREEN-VALUE IN BROWSE {&browse-name})
          tt-report.load-no = tt-report.load-no:SCREEN-VALUE IN BROWSE {&browse-name}
          tt-report.ship-date = DATE(tt-report.ship-date:SCREEN-VALUE IN BROWSE {&browse-name})
          tt-report.old-truck-code = tt-report.truck-code
          tt-report.old-stop-no = tt-report.stop-no
          tt-report.old-load-no = tt-report.old-load-no
          tt-report.old-truck-dscr = tt-report.truck-dscr
          tt-report.old-ship-date = tt-report.ship-date
          tt-report.no-units = INT(tt-report.no-units:SCREEN-VALUE IN BROWSE {&browse-name})
          tt-report.msf =  DEC(tt-report.msf:SCREEN-VALUE IN BROWSE {&browse-name})
          tt-report.weight = DEC(tt-report.weight:SCREEN-VALUE IN BROWSE {&browse-name}).
  END.

  RUN calc-tot-wgt-proc.

  FIND FIRST truck WHERE
       truck.company = tt-report.company AND
       truck.loc = locode AND
       truck.carrier = tt-report.carrier AND
       truck.truck-code = tt-report.truck-code
       NO-LOCK NO-ERROR.
  
  IF AVAIL truck THEN 
  DO: 
      IF truck.weight-limit < tt-report.tot-weight THEN
      DO:
         MESSAGE "Total Weight is greater than Truck Weight Limit of " + STRING(truck.weight-limit) + "."
            VIEW-AS ALERT-BOX WARNING.
         v-warn = YES.
      END.
  END.

  IF v-warn = NO THEN
  DO:
    FIND FIRST reftable WHERE
         reftable.reftable EQ "msf-limit" AND
         reftable.company  EQ tt-report.company AND
         reftable.loc      EQ locode AND 
         reftable.CODE     EQ tt-report.carrier AND
         reftable.code2    EQ tt-report.truck-code
         NO-LOCK NO-ERROR.
     
    IF AVAIL reftable AND reftable.val[1] < tt-report.tot-msf THEN
       MESSAGE "Total MSF is greater than Truck MSF Limit of " + STRING(reftable.val[1]) + "."
          VIEW-AS ALERT-BOX WARNING.
  END.
  IF AVAIL truck AND truck.max-units < tt-report.tot-units THEN
     MESSAGE "Total Units is greater than Truck Max Units of " + STRING(truck.max-units) + "."
          VIEW-AS ALERT-BOX WARNING.

  IF INDEX("AB",tt-report.key-06) GT 0 THEN
     RUN update-release.
  ELSE
  IF INDEX("PCZ",tt-report.key-06) GT 0 AND tt-report.bol-no NE 0 THEN
     RUN update-bol.

  CLOSE QUERY browse-2.
  OPEN QUERY browse-2 FOR EACH tt-report USE-INDEX tt-trk-stop-3 NO-LOCK.
 
  browse-2:SET-REPOSITIONED-ROW(v-row).

  REPOSITION browse-2 TO ROWID v-rowid NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-release D-Dialog 
PROCEDURE update-release :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FIND FIRST oe-relh WHERE
        oe-relh.company  EQ cocode AND
        oe-relh.release# EQ tt-report.rel-no
        EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

   IF AVAIL oe-relh THEN
   DO:
      ASSIGN
         oe-relh.carrier = tt-report.carrier
         oe-relh.trailer = tt-report.truck-code
         oe-relh.rel-date = tt-report.ship-date.
         
      FIND CURRENT oe-relh NO-LOCK.
      RELEASE oe-relh.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

