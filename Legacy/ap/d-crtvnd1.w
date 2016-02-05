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

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES vend

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame vend.type vend.vend-no ~
vend.active vend.contact vend.name vend.buyer vend.add1 vend.fax-area ~
vend.fax vend.area-code vend.phone vend.add2 vend.city vend.state vend.zip ~
vend.country vend.Postal vend.actnum vend.actdscr vend.curr-code ~
vend.tax-gr vend.code-1099 vend.an-edi-vend vend.remit vend.terms ~
vend.r-add1 vend.disc-% vend.r-add2 vend.r-city vend.r-state vend.r-zip ~
vend.r-country vend.r-postal vend.fob-code vend.disc-days vend.frt-pay ~
vend.check-memo vend.carrier 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame vend.type vend.active ~
vend.contact vend.name vend.buyer vend.add1 vend.fax-area vend.fax ~
vend.area-code vend.phone vend.add2 vend.city vend.state vend.zip ~
vend.country vend.Postal vend.actnum vend.curr-code vend.tax-gr ~
vend.code-1099 vend.an-edi-vend vend.remit vend.terms vend.r-add1 ~
vend.disc-% vend.r-add2 vend.r-city vend.r-state vend.r-zip vend.r-country ~
vend.r-postal vend.fob-code vend.disc-days vend.frt-pay vend.check-memo ~
vend.carrier 
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame vend
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame vend
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH vend SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame vend
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame vend


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS vend.type vend.active vend.contact vend.name ~
vend.buyer vend.add1 vend.fax-area vend.fax vend.area-code vend.phone ~
vend.add2 vend.city vend.state vend.zip vend.country vend.Postal ~
vend.actnum vend.curr-code vend.tax-gr vend.code-1099 vend.an-edi-vend ~
vend.remit vend.terms vend.r-add1 vend.disc-% vend.r-add2 vend.r-city ~
vend.r-state vend.r-zip vend.r-country vend.r-postal vend.fob-code ~
vend.disc-days vend.frt-pay vend.check-memo vend.carrier 
&Scoped-define ENABLED-TABLES vend
&Scoped-define FIRST-ENABLED-TABLE vend
&Scoped-define DISPLAYED-TABLES vend
&Scoped-define FIRST-DISPLAYED-TABLE vend
&Scoped-Define ENABLED-OBJECTS carrier_dscr Btn_OK Btn_Cancel RECT-1 ~
RECT-29 RECT-30 RECT-31 
&Scoped-Define DISPLAYED-FIELDS vend.type vend.vend-no vend.active ~
vend.contact vend.name vend.buyer vend.add1 vend.fax-area vend.fax ~
vend.area-code vend.phone vend.add2 vend.city vend.state vend.zip ~
vend.country vend.Postal vend.actnum vend.actdscr vend.curr-code ~
vend.tax-gr vend.code-1099 vend.an-edi-vend vend.remit vend.terms ~
vend.r-add1 vend.disc-% vend.r-add2 vend.r-city vend.r-state vend.r-zip ~
vend.r-country vend.r-postal vend.fob-code vend.disc-days vend.frt-pay ~
vend.check-memo vend.carrier 
&Scoped-Define DISPLAYED-OBJECTS ventype_Dscr buyer_buyer-n terms_dscr ~
carrier_dscr 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 vend.vend-no 
&Scoped-define List-4 vend.type vend.buyer vend.terms 

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

DEFINE VARIABLE buyer_buyer-n AS CHARACTER FORMAT "x(20)" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1
     BGCOLOR 7 FGCOLOR 15 FONT 4.

DEFINE VARIABLE carrier_dscr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE terms_dscr AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1
     BGCOLOR 7 FGCOLOR 15 FONT 4.

DEFINE VARIABLE ventype_Dscr AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1
     BGCOLOR 7 FGCOLOR 15 FONT 4.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 148 BY 16.91.

DEFINE RECTANGLE RECT-29
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 65 BY 8.57.

DEFINE RECTANGLE RECT-30
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 77 BY 3.81.

DEFINE RECTANGLE RECT-31
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 77 BY 4.76.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      vend SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     vend.type AT ROW 1.95 COL 84 COLON-ALIGNED
          LABEL "Type"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
          BGCOLOR 15 FONT 4
     ventype_Dscr AT ROW 1.95 COL 97 COLON-ALIGNED NO-LABEL
     vend.vend-no AT ROW 2.19 COL 11 COLON-ALIGNED
          LABEL "Vendor"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
          BGCOLOR 15 FONT 4
     vend.active AT ROW 2.19 COL 40 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Active", "A":U,
"Inactive", "I":U
          SIZE 29 BY .71
     vend.contact AT ROW 2.95 COL 84 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 40 BY 1
          BGCOLOR 15 FONT 4
     vend.name AT ROW 3.14 COL 11 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
          BGCOLOR 15 FONT 4
     vend.buyer AT ROW 3.91 COL 84 COLON-ALIGNED
          LABEL "Buyer"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
          BGCOLOR 15 FONT 4
     buyer_buyer-n AT ROW 3.95 COL 97 COLON-ALIGNED NO-LABEL
     vend.add1 AT ROW 4.14 COL 11 COLON-ALIGNED
          LABEL "Address"
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
          BGCOLOR 15 FONT 4
     vend.fax-area AT ROW 4.95 COL 115 COLON-ALIGNED
          LABEL "Fax" FORMAT "(xxx)"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
          BGCOLOR 15 FONT 4
     vend.fax AT ROW 4.95 COL 123 COLON-ALIGNED NO-LABEL FORMAT "xxx-xxxx"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 4
     vend.area-code AT ROW 5 COL 84 COLON-ALIGNED
          LABEL "Phone" FORMAT "(xxx)"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
          BGCOLOR 15 FONT 4
     vend.phone AT ROW 5 COL 92 COLON-ALIGNED NO-LABEL FORMAT "xxx-xxxx"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
          BGCOLOR 15 FONT 4
     vend.add2 AT ROW 5.1 COL 13 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
          BGCOLOR 15 FONT 4
     vend.city AT ROW 6.05 COL 11 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21.2 BY 1
          BGCOLOR 15 FONT 4
     vend.state AT ROW 6.05 COL 33 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
          BGCOLOR 15 FONT 4
     vend.zip AT ROW 6.05 COL 40 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 4
     vend.country AT ROW 7 COL 11 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 4
     vend.Postal AT ROW 7 COL 44 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
          BGCOLOR 15 FONT 4
     vend.actnum AT ROW 7.91 COL 75 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 30 BY 1
          BGCOLOR 15 FONT 4
     vend.actdscr AT ROW 7.91 COL 103 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 41 BY 1
          BGCOLOR 7 FGCOLOR 0 
     vend.curr-code AT ROW 9.57 COL 93 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
          BGCOLOR 15 
     vend.tax-gr AT ROW 10.52 COL 93 COLON-ALIGNED
          LABEL "Tax"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
          BGCOLOR 15 
     vend.code-1099 AT ROW 10.52 COL 116 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
          BGCOLOR 15 FONT 4
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME Dialog-Frame
     vend.an-edi-vend AT ROW 10.52 COL 129
          LABEL "EDI"
          VIEW-AS TOGGLE-BOX
          SIZE 11 BY 1.05
     vend.remit AT ROW 11 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
          BGCOLOR 15 FONT 4
     vend.terms AT ROW 11.71 COL 93 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
          BGCOLOR 15 FONT 4
     terms_dscr AT ROW 11.71 COL 104 COLON-ALIGNED NO-LABEL
     vend.r-add1 AT ROW 11.95 COL 13 COLON-ALIGNED
          LABEL "Address"
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
          BGCOLOR 15 FONT 4
     vend.disc-% AT ROW 12.67 COL 93 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
          BGCOLOR 15 FONT 4
     vend.r-add2 AT ROW 12.91 COL 13 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
          BGCOLOR 15 FONT 4
     vend.r-city AT ROW 13.86 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21.2 BY 1
          BGCOLOR 15 FONT 4
     vend.r-state AT ROW 13.86 COL 35 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
          BGCOLOR 15 FONT 4
     vend.r-zip AT ROW 13.86 COL 42 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 4
     vend.r-country AT ROW 14.81 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 4
     vend.r-postal AT ROW 14.81 COL 46 COLON-ALIGNED
          LABEL "Postal Code"
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
          BGCOLOR 15 FONT 4
     vend.fob-code AT ROW 14.81 COL 133 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Dest", "Dest":U,
"Orig", "Orig":U,
"None", ""
          SIZE 12 BY 2.62
     vend.disc-days AT ROW 15.29 COL 118 COLON-ALIGNED
          LABEL "Lead Time Days"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
          BGCOLOR 15 FONT 4
     vend.frt-pay AT ROW 15.52 COL 86 COLON-ALIGNED
          LABEL "Freight Pay" FORMAT "X"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
          BGCOLOR 15 
     vend.check-memo AT ROW 16.24 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
          BGCOLOR 8 FGCOLOR 9 
     vend.carrier AT ROW 16.48 COL 86 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     carrier_dscr AT ROW 16.48 COL 96 COLON-ALIGNED NO-LABEL
     Btn_OK AT ROW 18.62 COL 35
     Btn_Cancel AT ROW 18.86 COL 93
     RECT-1 AT ROW 1.24 COL 1
     RECT-29 AT ROW 9.1 COL 3
     RECT-30 AT ROW 13.86 COL 70
     RECT-31 AT ROW 9.1 COL 70
     "Status:" VIEW-AS TEXT
          SIZE 9 BY .76 AT ROW 2.19 COL 31
     "Default G/L#" VIEW-AS TEXT
          SIZE 17 BY .71 AT ROW 6.95 COL 75
          FGCOLOR 9 FONT 6
     "Remit to Address" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 9.57 COL 6
          FGCOLOR 9 FONT 6
     "Shipping Information" VIEW-AS TEXT
          SIZE 25 BY .62 AT ROW 14.1 COL 73
          FGCOLOR 9 FONT 6
     "FOB:" VIEW-AS TEXT
          SIZE 7 BY .76 AT ROW 14.1 COL 135
     SPACE(14.59) SKIP(5.75)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "Vendor Maintenance"
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
                                                                        */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN vend.actdscr IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN vend.actnum IN FRAME Dialog-Frame
   ALIGN-L EXP-LABEL                                                    */
/* SETTINGS FOR FILL-IN vend.add1 IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN vend.add2 IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* SETTINGS FOR TOGGLE-BOX vend.an-edi-vend IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN vend.area-code IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN vend.buyer IN FRAME Dialog-Frame
   4 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN buyer_buyer-n IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN vend.disc-days IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN vend.fax IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN vend.fax-area IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN vend.frt-pay IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN vend.phone IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN vend.r-add1 IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN vend.r-postal IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN vend.tax-gr IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN vend.terms IN FRAME Dialog-Frame
   4                                                                    */
/* SETTINGS FOR FILL-IN terms_dscr IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN vend.type IN FRAME Dialog-Frame
   4 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN vend.vend-no IN FRAME Dialog-Frame
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN ventype_Dscr IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "ASI.vend"
     _Options          = "SHARE-LOCK"
     _Query            is OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Vendor Maintenance */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend.actnum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend.actnum Dialog-Frame
ON LEAVE OF vend.actnum IN FRAME Dialog-Frame /* Account Number */
DO:
  if lastkey ne -1 then do:
    run valid-actnum.
    if not v-avail then return no-apply.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend.add1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend.add1 Dialog-Frame
ON LEAVE OF vend.add1 IN FRAME Dialog-Frame /* Address */
DO:
  if adm-new-record and vend.r-add1:screen-value eq "" then
    vend.r-add1:screen-value = {&self-name}:screen-value.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend.add2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend.add2 Dialog-Frame
ON LEAVE OF vend.add2 IN FRAME Dialog-Frame /* address line 2 */
DO:
  if adm-new-record and vend.r-add2:screen-value eq "" then
    vend.r-add2:screen-value = {&self-name}:screen-value.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend.buyer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend.buyer Dialog-Frame
ON LEAVE OF vend.buyer IN FRAME Dialog-Frame /* Buyer */
DO:
  if lastkey ne -1 then do:
    run valid-buyer.
    if not v-avail then return no-apply.
  end.
  
  {methods/dispflds.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend.city
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend.city Dialog-Frame
ON LEAVE OF vend.city IN FRAME Dialog-Frame /* City */
DO:
  if adm-new-record and vend.r-city:screen-value eq "" then
    vend.r-city:screen-value = {&self-name}:screen-value.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend.country
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend.country Dialog-Frame
ON LEAVE OF vend.country IN FRAME Dialog-Frame /* Country */
DO:
  if adm-new-record and vend.r-country:screen-value eq "" then
    vend.r-country:screen-value = {&self-name}:screen-value.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend.frt-pay
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend.frt-pay Dialog-Frame
ON ENTRY OF vend.frt-pay IN FRAME Dialog-Frame /* Freight Pay */
DO:
  run display-frt-pay. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend.name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend.name Dialog-Frame
ON LEAVE OF vend.name IN FRAME Dialog-Frame /* Name */
DO:
  if adm-new-record and vend.remit:screen-value eq "" then
    vend.remit:screen-value = {&self-name}:screen-value.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend.Postal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend.Postal Dialog-Frame
ON LEAVE OF vend.Postal IN FRAME Dialog-Frame /* Postal Code */
DO:
  if adm-new-record and vend.r-postal:screen-value eq "" then
    vend.r-postal:screen-value = {&self-name}:screen-value.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend.r-state
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend.r-state Dialog-Frame
ON LEAVE OF vend.r-state IN FRAME Dialog-Frame /* State */
DO:
  if lastkey ne -1 then do:
    run valid-r-state.
    if not v-avail then return no-apply.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend.state
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend.state Dialog-Frame
ON LEAVE OF vend.state IN FRAME Dialog-Frame /* State */
DO:
  if lastkey ne -1 then do:
    run valid-state.
    if not v-avail then return no-apply.
    
    if adm-new-record and vend.r-state:screen-value eq "" then
      vend.r-state:screen-value = {&self-name}:screen-value.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend.tax-gr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend.tax-gr Dialog-Frame
ON LEAVE OF vend.tax-gr IN FRAME Dialog-Frame /* Tax */
DO:
  if lastkey ne -1 then do:
    run valid-stax.
    if not v-avail then return no-apply.
  end. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend.terms
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend.terms Dialog-Frame
ON LEAVE OF vend.terms IN FRAME Dialog-Frame /* Terms */
DO:
  if lastkey ne -1 then do:
    run valid-terms.
    if not v-avail then return no-apply.
  end.
  
  {methods/dispflds.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend.type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend.type Dialog-Frame
ON LEAVE OF vend.type IN FRAME Dialog-Frame /* Type */
DO:
  if lastkey ne -1 then do:
    run valid-vendtype.
    if not v-avail then return no-apply.
  end.
  
  {methods/dispflds.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vend.zip
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vend.zip Dialog-Frame
ON LEAVE OF vend.zip IN FRAME Dialog-Frame /* Zip */
DO:
  if adm-new-record and trim(vend.r-zip:screen-value) eq "-" then
    vend.r-zip:screen-value = {&self-name}:screen-value.
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

  {&OPEN-QUERY-Dialog-Frame}
  GET FIRST Dialog-Frame.
  DISPLAY ventype_Dscr buyer_buyer-n terms_dscr carrier_dscr 
      WITH FRAME Dialog-Frame.
  IF AVAILABLE vend THEN 
    DISPLAY vend.type vend.vend-no vend.active vend.contact vend.name vend.buyer 
          vend.add1 vend.fax-area vend.fax vend.area-code vend.phone vend.add2 
          vend.city vend.state vend.zip vend.country vend.Postal vend.actnum 
          vend.actdscr vend.curr-code vend.tax-gr vend.code-1099 
          vend.an-edi-vend vend.remit vend.terms vend.r-add1 vend.disc-% 
          vend.r-add2 vend.r-city vend.r-state vend.r-zip vend.r-country 
          vend.r-postal vend.fob-code vend.disc-days vend.frt-pay 
          vend.check-memo vend.carrier 
      WITH FRAME Dialog-Frame.
  ENABLE vend.type vend.active vend.contact vend.name vend.buyer vend.add1 
         vend.fax-area vend.fax vend.area-code vend.phone vend.add2 vend.city 
         vend.state vend.zip vend.country vend.Postal vend.actnum 
         vend.curr-code vend.tax-gr vend.code-1099 vend.an-edi-vend vend.remit 
         vend.terms vend.r-add1 vend.disc-% vend.r-add2 vend.r-city 
         vend.r-state vend.r-zip vend.r-country vend.r-postal vend.fob-code 
         vend.disc-days vend.frt-pay vend.check-memo vend.carrier carrier_dscr 
         Btn_OK Btn_Cancel RECT-1 RECT-29 RECT-30 RECT-31 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

