&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: jcrep\v-wiptag.w

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

{custom/globdefs.i}

&SCOPED-DEFINE post-enable post-enable

{custom/gcompany.i}
{custom/gloc.i}
{sys/inc/var.i new shared}

   assign cocode = g_company
       locode = g_loc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES wiptag
&Scoped-define FIRST-EXTERNAL-TABLE wiptag


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR wiptag.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS wiptag.sts wiptag.rm-i-no machine-code wiptag.rm-i-name ~
wiptag.cust-no wiptag.fg-i-name wiptag.fg-i-no wiptag.rm-whs wiptag.rm-bin ~
wiptag.wip-warehouse wiptag.wip-rm-bin wiptag.shift wiptag.job-no ~
wiptag.job-no2 wiptag.form-no wiptag.blank-no wiptag.n-out wiptag.num-up ~
wiptag.pallet-count wiptag.rm-tag-no wiptag.spare-char-1 wiptag.tag-date ~
wiptag.completed 
&Scoped-define ENABLED-TABLES wiptag
&Scoped-define FIRST-ENABLED-TABLE wiptag
&Scoped-Define DISPLAYED-FIELDS wiptag.tag-no wiptag.sts wiptag.rm-i-no ~
wiptag.rm-i-name wiptag.cust-no wiptag.fg-i-name wiptag.fg-i-no ~
wiptag.rm-whs wiptag.rm-bin wiptag.wip-warehouse wiptag.wip-rm-bin ~
wiptag.shift wiptag.job-no wiptag.job-no2 wiptag.form-no wiptag.blank-no ~
wiptag.n-out wiptag.num-up wiptag.pallet-count wiptag.crt-userid ~
wiptag.upd-userid wiptag.rm-tag-no wiptag.crt-date wiptag.upd-date ~
wiptag.spare-char-1 wiptag.tag-date wiptag.completed 
&Scoped-define DISPLAYED-TABLES wiptag
&Scoped-define FIRST-DISPLAYED-TABLE wiptag
&Scoped-Define DISPLAYED-OBJECTS machine-code machine-dscr v-tagtime ~
v-tagtime-upd v-tagtime-2 fi_sht-wid fi_sht-len 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,textFields,F1 */
&Scoped-define ADM-CREATE-FIELDS wiptag.tag-no 
&Scoped-define ADM-ASSIGN-FIELDS fi_sht-wid fi_sht-len 
&Scoped-define DISPLAY-FIELD fi_sht-wid fi_sht-len 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
/**************************
</EXECUTING-CODE> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE fi_sht-len AS DECIMAL FORMAT ">>9.9999":U INITIAL 0 
     LABEL "Sht Length" 
     VIEW-AS FILL-IN 
     SIZE 11.6 BY 1 NO-UNDO.

DEFINE VARIABLE fi_sht-wid AS DECIMAL FORMAT ">>9.9999":U INITIAL 0 
     LABEL "Sht Width" 
     VIEW-AS FILL-IN 
     SIZE 11.6 BY 1 NO-UNDO.

DEFINE VARIABLE machine-code AS CHARACTER FORMAT "x(6)" 
     LABEL "Machine" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1.

DEFINE VARIABLE machine-dscr AS CHARACTER FORMAT "x(15)" 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1.

DEFINE VARIABLE v-tagtime AS CHARACTER FORMAT "X(256)":U 
     LABEL "Created Time" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE v-tagtime-2 AS CHARACTER FORMAT "99:99:99":U 
     LABEL "Tag Time" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE v-tagtime-upd AS CHARACTER FORMAT "X(256)":U 
     LABEL "Updated Time" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     wiptag.tag-no AT ROW 1.48 COL 15 COLON-ALIGNED HELP
          "" WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 43 BY 1
     wiptag.sts AT ROW 1.48 COL 103.6 COLON-ALIGNED HELP
          "" WIDGET-ID 40
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEMS "","Printed","Received","On Hand","Bill of Lading","Invoiced","Completed","Deleted","Issued","Transferred" 
          DROP-DOWN
          SIZE 23 BY 1
     machine-code AT ROW 2.67 COL 15.2 COLON-ALIGNED WIDGET-ID 2
     machine-dscr AT ROW 2.67 COL 35.2 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     wiptag.rm-i-no AT ROW 3.86 COL 15 COLON-ALIGNED WIDGET-ID 20
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     wiptag.rm-i-name AT ROW 3.86 COL 41.4 COLON-ALIGNED NO-LABEL WIDGET-ID 18
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     wiptag.cust-no AT ROW 3.86 COL 103.4 COLON-ALIGNED HELP
          "" WIDGET-ID 4
          LABEL "Customer"
          VIEW-AS FILL-IN 
          SIZE 19.6 BY 1
     wiptag.fg-i-name AT ROW 5 COL 41.4 COLON-ALIGNED NO-LABEL WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
     wiptag.fg-i-no AT ROW 5.05 COL 15 COLON-ALIGNED WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     wiptag.rm-whs AT ROW 6.05 COL 15 COLON-ALIGNED HELP
          "" WIDGET-ID 32
          LABEL "RM Whs."
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     wiptag.rm-bin AT ROW 6.05 COL 41.4 COLON-ALIGNED HELP
          "" WIDGET-ID 30
          LABEL "RM Bin"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     wiptag.wip-warehouse AT ROW 6.05 COL 73.2 COLON-ALIGNED HELP
          "" WIDGET-ID 36
          LABEL "WIP Whs."
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     wiptag.wip-rm-bin AT ROW 6.05 COL 98.8 COLON-ALIGNED HELP
          "" WIDGET-ID 34
          LABEL "WIP Bin"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     wiptag.shift AT ROW 6.05 COL 127 COLON-ALIGNED WIDGET-ID 44
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     wiptag.job-no AT ROW 7.24 COL 15 COLON-ALIGNED HELP
          "Job Number." WIDGET-ID 6
          LABEL "Job Number"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     wiptag.job-no2 AT ROW 7.24 COL 27.4 COLON-ALIGNED NO-LABEL WIDGET-ID 16
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     wiptag.form-no AT ROW 7.24 COL 41.6 COLON-ALIGNED WIDGET-ID 14
          LABEL "Form #"
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     wiptag.blank-no AT ROW 7.24 COL 59 COLON-ALIGNED WIDGET-ID 8
          LABEL "Blank #"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     wiptag.n-out AT ROW 7.24 COL 73.2 COLON-ALIGNED WIDGET-ID 24
          LABEL "# Out"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     wiptag.num-up AT ROW 7.24 COL 86.2 COLON-ALIGNED WIDGET-ID 26
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
     wiptag.pallet-count AT ROW 9 COL 23.6 COLON-ALIGNED WIDGET-ID 28
          LABEL "Qty(Pallet Count)"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     wiptag.crt-userid AT ROW 9 COL 60.8 COLON-ALIGNED HELP
          "" WIDGET-ID 52
          LABEL "Created User"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     wiptag.upd-userid AT ROW 9 COL 99.6 COLON-ALIGNED HELP
          "" WIDGET-ID 54
          LABEL "Updated User"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     wiptag.rm-tag-no AT ROW 10 COL 15 COLON-ALIGNED HELP
          "" WIDGET-ID 22
          LABEL "RM Tag#"
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     wiptag.crt-date AT ROW 10 COL 60.8 COLON-ALIGNED HELP
          "" WIDGET-ID 56
          LABEL "Created Date"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     wiptag.upd-date AT ROW 10 COL 99.6 COLON-ALIGNED HELP
          "" WIDGET-ID 48
          LABEL "Updated Date"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     wiptag.spare-char-1 AT ROW 11.24 COL 15 COLON-ALIGNED HELP
          "" WIDGET-ID 84
          LABEL "RM Tag#" FORMAT "x(20)"
          VIEW-AS FILL-IN 
          SIZE 26 BY 1 TOOLTIP "Second RM Tag used for WIP Tag quantity."
     wiptag.tag-date AT ROW 12.33 COL 15 COLON-ALIGNED WIDGET-ID 60
          LABEL "Tag Date"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     v-tagtime AT ROW 12.33 COL 60.8 COLON-ALIGNED WIDGET-ID 46
     v-tagtime-upd AT ROW 12.33 COL 99.6 COLON-ALIGNED WIDGET-ID 50
     v-tagtime-2 AT ROW 13.33 COL 15 COLON-ALIGNED WIDGET-ID 64
     fi_sht-wid AT ROW 13.33 COL 45.6 COLON-ALIGNED WIDGET-ID 82
     fi_sht-len AT ROW 13.33 COL 72.8 COLON-ALIGNED WIDGET-ID 80
     wiptag.completed AT ROW 13.33 COL 99.6 COLON-ALIGNED WIDGET-ID 58
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: asi.wiptag
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
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
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 15.24
         WIDTH              = 138.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{methods/template/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN wiptag.blank-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN wiptag.crt-date IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-HELP                                         */
/* SETTINGS FOR FILL-IN wiptag.crt-userid IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-HELP                                         */
/* SETTINGS FOR FILL-IN wiptag.cust-no IN FRAME F-Main
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN wiptag.fg-i-name IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN fi_sht-len IN FRAME F-Main
   NO-ENABLE 2 4                                                        */
/* SETTINGS FOR FILL-IN fi_sht-wid IN FRAME F-Main
   NO-ENABLE 2 4                                                        */
/* SETTINGS FOR FILL-IN wiptag.form-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN wiptag.job-no IN FRAME F-Main
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN wiptag.job-no2 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN wiptag.n-out IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN wiptag.pallet-count IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN wiptag.rm-bin IN FRAME F-Main
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN wiptag.rm-i-name IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN wiptag.rm-tag-no IN FRAME F-Main
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN wiptag.rm-whs IN FRAME F-Main
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN wiptag.spare-char-1 IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR COMBO-BOX wiptag.sts IN FRAME F-Main
   EXP-FORMAT EXP-HELP                                                  */
/* SETTINGS FOR FILL-IN wiptag.tag-date IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN wiptag.tag-no IN FRAME F-Main
   NO-ENABLE 1 EXP-HELP                                                 */
/* SETTINGS FOR FILL-IN wiptag.upd-date IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-HELP                                         */
/* SETTINGS FOR FILL-IN wiptag.upd-userid IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-HELP                                         */
/* SETTINGS FOR FILL-IN v-tagtime IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-tagtime-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-tagtime-upd IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN wiptag.wip-rm-bin IN FRAME F-Main
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN wiptag.wip-warehouse IN FRAME F-Main
   EXP-LABEL EXP-HELP                                                   */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME machine-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL machine-code V-table-Win
ON HELP OF machine-code IN FRAME F-Main /* Machine */
DO:
   DEF VAR char-val AS cha NO-UNDO.

   RUN windows/l-mach.w (g_company, g_loc, FOCUS:SCREEN-VALUE, OUTPUT char-val).
   FOCUS:SCREEN-VALUE = ENTRY(1,char-val).

   FIND  FIRST mach where mach.company = g_company AND
    mach.loc = g_loc AND mach.m-code = FOCUS:SCREEN-VALUE NO-LOCK NO-ERROR.
 IF AVAIL mach THEN ASSIGN
    machine-dscr:SCREEN-VALUE IN FRAME {&FRAME-NAME} = mach.m-dscr.
 ELSE ASSIGN 
   machine-dscr:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME wiptag.tag-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wiptag.tag-no V-table-Win
ON HELP OF wiptag.tag-no IN FRAME F-Main /* Machine */
DO:
   DEF VAR char-val AS cha NO-UNDO.
   DEF VAR v-rec-val AS cha NO-UNDO.
   RUN windows/l-wptagst.w (g_company, FOCUS:SCREEN-VALUE,"Printed", OUTPUT char-val, OUTPUT v-rec-val).
        IF char-val <> "" THEN wiptag.tag-no:SCREEN-VALUE = ENTRY(1,char-val).
        return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL machine-code V-table-Win
ON LEAVE OF machine-code IN FRAME F-Main /* Machine */
DO:
   IF LASTKEY <> -1  THEN DO:
      RUN valid-machine-code NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL machine-code V-table-Win
ON RETURN OF machine-code IN FRAME F-Main /* Machine */
DO:
   
   /* RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"tableio-source", OUTPUT char-hdl).
    RUN do-save IN WIDGET-HANDLE(char-hdl).
    RETURN NO-APPLY.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME wiptag.wip-rm-bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wiptag.wip-rm-bin V-table-Win
ON HELP OF wiptag.wip-rm-bin IN FRAME F-Main /* WIP Bin */
DO:
   DEF VAR char-val AS CHAR NO-UNDO.

   DO WITH FRAME {&FRAME-NAME}:
      run rm/l-wipbin.w (cocode,wiptag.wip-warehouse:SCREEN-VALUE, output char-val).
      if char-val <> "" then
         wiptag.wip-rm-bin:screen-value = entry(1,char-val).
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wiptag.wip-rm-bin V-table-Win
ON LEAVE OF wiptag.wip-rm-bin IN FRAME F-Main /* WIP Bin */
DO:
   DO WITH FRAME {&FRAME-NAME}:
   
     IF wiptag.wip-rm-bin:SCREEN-VALUE NE "" THEN
      DO:
         IF NOT CAN-FIND(FIRST wip-bin WHERE
            wip-bin.company EQ cocode AND
            wip-bin.loc     EQ wiptag.wip-warehouse:SCREEN-VALUE AND
            wip-bin.loc-bin EQ wiptag.wip-rm-bin:SCREEN-VALUE) THEN
            DO:
               MESSAGE "Invalid WIP Bin."
                   VIEW-AS ALERT-BOX ERROR BUTTONS OK.
               APPLY "entry" TO wiptag.wip-rm-bin .
               RETURN NO-APPLY.
            END.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME wiptag.wip-warehouse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wiptag.wip-warehouse V-table-Win
ON HELP OF wiptag.wip-warehouse IN FRAME F-Main /* WIP Whs */
DO:
   DEF VAR char-val AS CHAR NO-UNDO.
   DO WITH FRAME {&FRAME-NAME}:
      run rm/l-loc.w (cocode,wiptag.wip-warehouse:screen-value, output char-val).
      if char-val <> "" THEN
         wiptag.wip-warehouse:screen-value = entry(1,char-val).
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wiptag.wip-warehouse V-table-Win
ON LEAVE OF wiptag.wip-warehouse IN FRAME F-Main /* WIP Whs */
DO:
   DO WITH FRAME {&FRAME-NAME}:
   
  
   IF wiptag.wip-warehouse:SCREEN-VALUE NE "" AND
      NOT CAN-FIND(FIRST loc WHERE
      loc.company EQ cocode AND
      loc.loc     EQ wiptag.wip-warehouse:SCREEN-VALUE) THEN
      DO:
         MESSAGE "Invalid WIP Warehouse."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "entry" TO wiptag.wip-warehouse.
         RETURN NO-APPLY.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define SELF-NAME wiptag.rm-bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wiptag.rm-bin V-table-Win
ON HELP OF wiptag.rm-bin IN FRAME F-Main /* WIP Bin */
DO:
   DEF VAR char-val AS CHAR NO-UNDO.

   DO WITH FRAME {&FRAME-NAME}:
      run rm/l-wipbin.w (cocode,wiptag.rm-whs:SCREEN-VALUE, output char-val).
      if char-val <> "" then
         wiptag.rm-bin:screen-value = entry(1,char-val).
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wiptag.rm-bin V-table-Win
ON LEAVE OF wiptag.rm-bin IN FRAME F-Main /* WIP Bin */
DO:
   DO WITH FRAME {&FRAME-NAME}:
   
     IF wiptag.rm-bin:SCREEN-VALUE NE "" THEN
      DO:
         IF NOT CAN-FIND(FIRST wip-bin WHERE
            wip-bin.company EQ cocode AND
            wip-bin.loc     EQ wiptag.rm-whs:SCREEN-VALUE AND
            wip-bin.loc-bin EQ wiptag.rm-bin:SCREEN-VALUE) THEN
            DO:
               MESSAGE "Invalid RM Bin."
                   VIEW-AS ALERT-BOX ERROR BUTTONS OK.
               APPLY "entry" TO wiptag.rm-bin .
               RETURN NO-APPLY.
            END.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME wiptag.wiptag.rm-whs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wiptag.rm-whs V-table-Win
ON HELP OF wiptag.rm-whs IN FRAME F-Main /* WIP Whs */
DO:
   DEF VAR char-val AS CHAR NO-UNDO.
   DO WITH FRAME {&FRAME-NAME}:
      run rm/l-loc.w (cocode,wiptag.rm-whs:screen-value, output char-val).
      if char-val <> "" THEN
         wiptag.rm-whs:screen-value = entry(1,char-val).
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wiptag.rm-whs V-table-Win
ON LEAVE OF wiptag.rm-whs IN FRAME F-Main /* WIP Whs */
DO:
   DO WITH FRAME {&FRAME-NAME}:
   
  
   IF wiptag.rm-whs:SCREEN-VALUE NE "" AND
      NOT CAN-FIND(FIRST loc WHERE
      loc.company EQ cocode AND
      loc.loc     EQ wiptag.rm-whs:SCREEN-VALUE) THEN
      DO:
         MESSAGE "Invalid RM Warehouse."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "entry" TO wiptag.rm-whs.
         RETURN NO-APPLY.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME wiptag.cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wiptag.cust-no V-table-Win
ON HELP OF wiptag.cust-no IN FRAME F-Main /* customer# */
DO:
  DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.
  DEFINE VARIABLE help-recid AS RECID NO-UNDO.
  def var look-recid as recid no-undo.

 RUN windows/l-custact.w (cocode, wiptag.cust-no:SCREEN-VALUE, OUTPUT char-val, OUTPUT look-recid).
              FIND cust WHERE RECID(cust) EQ look-recid NO-LOCK NO-ERROR.
              IF AVAIL cust THEN DO:
               wiptag.cust-no:SCREEN-VALUE = cust.cust-no.
              END.
             APPLY "entry" TO wiptag.fg-i-no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wiptag.cust-no V-table-Win
ON LEAVE OF wiptag.cust-no IN FRAME F-Main /* Customer# */
DO:
   DO WITH FRAME {&FRAME-NAME}:
   
  
   IF wiptag.cust-no:SCREEN-VALUE NE "" AND
      NOT CAN-FIND(FIRST cust WHERE
      cust.company EQ cocode AND
      cust.cust-no    EQ wiptag.cust-no:SCREEN-VALUE) THEN
      DO:
         MESSAGE "Invalid Customer Try Help."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "entry" TO wiptag.cust-no.
         RETURN NO-APPLY.
      END.
          APPLY "entry" TO wiptag.fg-i-no.
   END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME wiptag.rm-i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wiptag.rm-i-no V-table-Win
ON HELP OF wiptag.rm-i-no IN FRAME F-Main /* Job# */
DO:
  DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.
  DEFINE VARIABLE help-recid AS RECID NO-UNDO.
  run windows/l-itmRE.w (cocode,"","","R",wiptag.rm-i-no:SCREEN-VALUE, output char-val,OUTPUT help-recid).
               if char-val <> "" AND ENTRY(1,char-val) NE FOCUS:SCREEN-VALUE then do :
                  wiptag.rm-i-no:SCREEN-VALUE  = ENTRY(1,char-val).
                  /*RUN new-i-no.*/
               END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME wiptag.job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wiptag.job-no V-table-Win
ON HELP OF wiptag.job-no IN FRAME F-Main /* Job# */
DO:
  DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.
  DEFINE VARIABLE help-recid AS RECID NO-UNDO.
  run windows/l-openjobs.w (cocode,wiptag.job-no:screen-value, 
                            output char-val, OUTPUT help-recid).
  IF NUM-ENTRIES(char-val) > 1 THEN
     DO WITH FRAME {&FRAME-NAME}:
        ASSIGN wiptag.job-no:SCREEN-VALUE  = ENTRY(1,char-val)
               wiptag.job-no2:SCREEN-VALUE = ENTRY(2,char-val).
        APPLY "entry" TO wiptag.job-no2.
     END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wiptag.job-no V-table-Win
ON LEAVE OF wiptag.job-no IN FRAME F-Main /* Job# */
DO:
   DO WITH FRAME {&FRAME-NAME}:
      wiptag.job-no:SCREEN-VALUE = FILL(" ",6 - LENGTH(TRIM(wiptag.job-no:SCREEN-VALUE))) +
                                wiptag.job-no:SCREEN-VALUE.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-item V-table-Win 
PROCEDURE add-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN dispatch ('add-record').
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win  _ADM-ROW-AVAILABLE
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

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "wiptag"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "wiptag"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win  _DEFAULT-DISABLE
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
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getMachine V-table-Win 
PROCEDURE getMachine :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-machine AS cha .

  op-machine = IF AVAIL wiptag-mch THEN wiptag-mch.m-code ELSE "".


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getTag# V-table-Win 
PROCEDURE getTag# :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-tag# AS cha .

  op-tag# = IF AVAIL wiptag THEN wiptag.tag-no ELSE "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
  
     wiptag.tag-time = (INT(SUBSTRING(v-tagtime-2:SCREEN-VALUE,1,2)) * 3600)
                     + (INT(SUBSTRING(v-tagtime-2:SCREEN-VALUE,4,2)) * 60)
                     + (INT(SUBSTRING(v-tagtime-2:SCREEN-VALUE,7,2))).

     RUN reftable-values(INPUT NO).
  END.
  IF adm-new-record THEN DO:
     CREATE wiptag-mch.
      ASSIGN wiptag-mch.company = wiptag.company
             wiptag-mch.tag-no = wiptag.tag-no
             wiptag-mch.m-code = machine-code:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
  END.
  ELSE 
     FIND FIRST  wiptag-mch WHERE  wiptag-mch.company = wiptag.company 
                              AND   wiptag-mch.tag-no = wiptag.tag-no EXCLUSIVE-LOCK NO-ERROR.
     IF AVAIL wiptag-mch THEN
        ASSIGN
             wiptag-mch.m-code = machine-code:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-cancel-record V-table-Win 
PROCEDURE local-cancel-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DISABLE v-tagtime-2 fi_sht-wid fi_sht-len WITH FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
  
  ASSIGN
     wiptag.company = g_company
     fi_sht-wid:SCREEN-VALUE = "0"
     fi_sht-len:SCREEN-VALUE = "0".
     machine-code:SCREEN-VALUE = "".
     machine-dscr:SCREEN-VALUE = "" .
      
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .
  DEF VAR char-hdl AS cha NO-UNDO.
  DEF VAR lMachineCode AS cha NO-UNDO.

  /* Code placed here will execute AFTER standard behavior.    */
  
  IF AVAIL wiptag THEN
  DO:
     ASSIGN
        wiptag.sts:SCREEN-VALUE IN FRAME {&FRAME-NAME} = wiptag.sts
        v-tagtime:SCREEN-VALUE = STRING(wiptag.crt-time,"hh:mm:ss")
        v-tagtime-upd:SCREEN-VALUE = STRING(wiptag.upd-time,"hh:mm:ss")
        v-tagtime-2:SCREEN-VALUE = STRING(wiptag.tag-time,"hh:mm:ss").

     IF NOT adm-new-record THEN
        RUN reftable-values (YES).
  END.
  IF NOT adm-new-record THEN do:
    RUN get-link-handle IN adm-broker-hdl
       (THIS-PROCEDURE,'record-source':U,OUTPUT char-hdl).
   IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN DO:
        RUN getMachine IN WIDGET-HANDLE(char-hdl) (OUTPUT lMachineCode).
   END.
     machine-code:SCREEN-VALUE IN FRAME {&FRAME-NAME} = lMachineCode. 
     FIND FIRST mach WHERE mach.m-code = lMachineCode 
                       AND mach.company = cocode NO-LOCK NO-ERROR.
      IF AVAIL mach THEN ASSIGN
         machine-dscr:SCREEN-VALUE IN FRAME {&FRAME-NAME} = mach.m-dscr.
  END.
  
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN local-display-fields.
  DISABLE v-tagtime-2 fi_sht-wid fi_sht-len WITH FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reftable-values V-table-Win 
PROCEDURE reftable-values :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAM ip-display AS LOG NO-UNDO.
 
    DO WITH FRAME {&FRAME-NAME}:
  
       IF AVAIL wiptag THEN DO:
          FIND FIRST reftable WHERE
               reftable.reftable = "WIPLEN" AND
               reftable.company = wiptag.company AND
               reftable.CODE = wiptag.tag-no
               USE-INDEX CODE
               NO-ERROR.
         
          IF NOT AVAIL reftable THEN
          DO:
             CREATE reftable.
             ASSIGN
                reftable.reftable = "WIPLEN"
                reftable.company = wiptag.company
                reftable.CODE = wiptag.tag-no.
          END.
         
          IF ip-display THEN
             ASSIGN
                fi_sht-wid:SCREEN-VALUE = STRING(reftable.val[1])
                fi_sht-len:SCREEN-VALUE = STRING(reftable.val[2]).
          ELSE
             ASSIGN
                reftable.val[1] = DEC(fi_sht-wid:SCREEN-VALUE)
                reftable.val[2] = DEC(fi_sht-len:SCREEN-VALUE).
      
          FIND CURRENT reftable NO-LOCK.
       END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "wiptag"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/vstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-machine-code V-table-Win 
PROCEDURE valid-machine-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 IF NOT CAN-FIND(FIRST mach where mach.company = g_company AND
    mach.loc = g_loc AND mach.m-code = machine-code:SCREEN-VALUE IN FRAME {&FRAME-NAME}
    ) THEN DO:
     MESSAGE "Invalid Machine. Try help. "
         VIEW-AS ALERT-BOX ERROR.
     APPLY 'entry' TO machine-code.
     RETURN ERROR.
 END.

 FIND  FIRST mach where mach.company = g_company AND
    mach.loc = g_loc AND mach.m-code = machine-code:SCREEN-VALUE IN FRAME {&FRAME-NAME} NO-LOCK NO-ERROR.
 IF AVAIL mach THEN ASSIGN
    machine-dscr:SCREEN-VALUE IN FRAME {&FRAME-NAME} = mach.m-dscr.
 ELSE ASSIGN 
   machine-dscr:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" .


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
