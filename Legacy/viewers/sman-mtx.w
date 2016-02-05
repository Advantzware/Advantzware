&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File:

  Description: from VIEWER.W - Template for SmartViewer Objects

  Input Parameters:
      <none>

  Output Parameters:
      <none>

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
def buffer b-mtx for sman-mtx.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES sman-mtx sman
&Scoped-define FIRST-EXTERNAL-TABLE sman-mtx


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR sman-mtx, sman.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS sman-mtx.type-comm sman-mtx.procat[1] ~
sman-mtx.comm[1] sman-mtx.commbasis[1] sman-mtx.procat[2] sman-mtx.comm[2] ~
sman-mtx.commbasis[2] sman-mtx.procat[3] sman-mtx.comm[3] ~
sman-mtx.commbasis[3] sman-mtx.procat[4] sman-mtx.comm[4] ~
sman-mtx.commbasis[4] sman-mtx.procat[5] sman-mtx.comm[5] ~
sman-mtx.commbasis[5] sman-mtx.procat[6] sman-mtx.comm[6] ~
sman-mtx.commbasis[6] sman-mtx.procat[7] sman-mtx.comm[7] ~
sman-mtx.commbasis[7] sman-mtx.procat[8] sman-mtx.comm[8] ~
sman-mtx.commbasis[8] sman-mtx.procat[9] sman-mtx.comm[9] ~
sman-mtx.commbasis[9] sman-mtx.procat[10] sman-mtx.comm[10] ~
sman-mtx.commbasis[10] 
&Scoped-define ENABLED-TABLES sman-mtx
&Scoped-define FIRST-ENABLED-TABLE sman-mtx
&Scoped-Define ENABLED-OBJECTS RECT-29 
&Scoped-Define DISPLAYED-FIELDS sman.sman sman.sname sman.commbasis ~
sman.territory sman.scomm sman-mtx.custype sman-mtx.type-comm ~
sman-mtx.procat[1] sman-mtx.dscr[1] sman-mtx.comm[1] sman-mtx.commbasis[1] ~
sman-mtx.procat[2] sman-mtx.dscr[2] sman-mtx.comm[2] sman-mtx.commbasis[2] ~
sman-mtx.procat[3] sman-mtx.dscr[3] sman-mtx.comm[3] sman-mtx.commbasis[3] ~
sman-mtx.procat[4] sman-mtx.dscr[4] sman-mtx.comm[4] sman-mtx.commbasis[4] ~
sman-mtx.procat[5] sman-mtx.dscr[5] sman-mtx.comm[5] sman-mtx.commbasis[5] ~
sman-mtx.procat[6] sman-mtx.dscr[6] sman-mtx.comm[6] sman-mtx.commbasis[6] ~
sman-mtx.procat[7] sman-mtx.dscr[7] sman-mtx.comm[7] sman-mtx.commbasis[7] ~
sman-mtx.procat[8] sman-mtx.dscr[8] sman-mtx.comm[8] sman-mtx.commbasis[8] ~
sman-mtx.procat[9] sman-mtx.dscr[9] sman-mtx.comm[9] sman-mtx.commbasis[9] ~
sman-mtx.procat[10] sman-mtx.dscr[10] sman-mtx.comm[10] ~
sman-mtx.commbasis[10] 
&Scoped-define DISPLAYED-TABLES sman sman-mtx
&Scoped-define FIRST-DISPLAYED-TABLE sman
&Scoped-define SECOND-DISPLAYED-TABLE sman-mtx
&Scoped-Define DISPLAYED-OBJECTS terr_dscr custtype_dscr 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-CREATE-FIELDS sman-mtx.custype 
&Scoped-define ADM-ASSIGN-FIELDS sman-mtx.dscr[1] sman-mtx.dscr[2] ~
sman-mtx.dscr[3] sman-mtx.dscr[4] sman-mtx.dscr[5] sman-mtx.dscr[6] ~
sman-mtx.dscr[7] sman-mtx.dscr[8] sman-mtx.dscr[9] sman-mtx.dscr[10] 

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
DEFINE VARIABLE custtype_dscr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE terr_dscr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-29
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 103 BY 17.14.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     sman.sman AT ROW 1.48 COL 20.6 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     sman.sname AT ROW 1.48 COL 31 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     sman.commbasis AT ROW 1.48 COL 60 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Gross Profit", "G":U,
"Selling Price", "S":U
          SIZE 37.8 BY .81
     sman.territory AT ROW 2.43 COL 20.6 COLON-ALIGNED
          LABEL "Territory"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     terr_dscr AT ROW 2.43 COL 31 COLON-ALIGNED NO-LABEL
     sman.scomm AT ROW 2.43 COL 74 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
     sman-mtx.custype AT ROW 3.86 COL 21 COLON-ALIGNED
          LABEL "Customer Type"
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     custtype_dscr AT ROW 3.86 COL 34 COLON-ALIGNED NO-LABEL
     sman-mtx.type-comm AT ROW 4.81 COL 24 COLON-ALIGNED
          LABEL "Default Commission"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     sman-mtx.procat[1] AT ROW 7.67 COL 3 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     sman-mtx.dscr[1] AT ROW 7.67 COL 17 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     sman-mtx.comm[1] AT ROW 7.67 COL 51 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     sman-mtx.commbasis[1] AT ROW 7.67 COL 65 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Gross Profit", "G":U,
"Selling Price", "S":U
          SIZE 36 BY .81
     sman-mtx.procat[2] AT ROW 8.67 COL 3 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     sman-mtx.dscr[2] AT ROW 8.67 COL 17 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     sman-mtx.comm[2] AT ROW 8.67 COL 51 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     sman-mtx.commbasis[2] AT ROW 8.67 COL 65 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Gross Profit", "G":U,
"Selling Price", "S":U
          SIZE 36 BY .81
     sman-mtx.procat[3] AT ROW 9.57 COL 3 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     sman-mtx.dscr[3] AT ROW 9.57 COL 17 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     sman-mtx.comm[3] AT ROW 9.57 COL 51 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     sman-mtx.commbasis[3] AT ROW 9.57 COL 65 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Gross Profit", "G":U,
"Selling Price", "S":U
          SIZE 36 BY .81
     sman-mtx.procat[4] AT ROW 10.52 COL 3 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     sman-mtx.dscr[4] AT ROW 10.52 COL 17 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     sman-mtx.comm[4] AT ROW 10.52 COL 51 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     sman-mtx.commbasis[4] AT ROW 10.52 COL 65 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Gross Profit", "G":U,
"Selling Price", "S":U
          SIZE 36 BY .81
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     sman-mtx.procat[5] AT ROW 11.52 COL 3 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     sman-mtx.dscr[5] AT ROW 11.52 COL 17 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     sman-mtx.comm[5] AT ROW 11.52 COL 51 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     sman-mtx.commbasis[5] AT ROW 11.71 COL 65 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Gross Profit", "G":U,
"Selling Price", "S":U
          SIZE 36 BY .81
     sman-mtx.procat[6] AT ROW 12.52 COL 3 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     sman-mtx.dscr[6] AT ROW 12.52 COL 17 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     sman-mtx.comm[6] AT ROW 12.52 COL 51 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     sman-mtx.commbasis[6] AT ROW 12.71 COL 65 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Gross Profit", "G":U,
"Selling Price", "S":U
          SIZE 36 BY .81
     sman-mtx.procat[7] AT ROW 13.62 COL 3 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     sman-mtx.dscr[7] AT ROW 13.62 COL 17 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     sman-mtx.comm[7] AT ROW 13.62 COL 51 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     sman-mtx.commbasis[7] AT ROW 13.81 COL 65 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Gross Profit", "G":U,
"Selling Price", "S":U
          SIZE 36 BY .81
     sman-mtx.procat[8] AT ROW 14.62 COL 3 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     sman-mtx.dscr[8] AT ROW 14.62 COL 17 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     sman-mtx.comm[8] AT ROW 14.62 COL 51 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     sman-mtx.commbasis[8] AT ROW 14.62 COL 65 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Gross Profit", "G":U,
"Selling Price", "S":U
          SIZE 36 BY .81
     sman-mtx.procat[9] AT ROW 15.48 COL 3 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     sman-mtx.dscr[9] AT ROW 15.48 COL 17 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     sman-mtx.comm[9] AT ROW 15.48 COL 51 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     sman-mtx.commbasis[9] AT ROW 15.67 COL 65 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Gross Profit", "G":U,
"Selling Price", "S":U
          SIZE 36 BY .81
     sman-mtx.procat[10] AT ROW 16.48 COL 3 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     sman-mtx.dscr[10] AT ROW 16.48 COL 17 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     sman-mtx.comm[10] AT ROW 16.48 COL 51 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     sman-mtx.commbasis[10] AT ROW 16.67 COL 65 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Gross Profit", "G":U,
"Selling Price", "S":U
          SIZE 36 BY .81
     "Category" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 6.95 COL 6
     "Product" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 6.24 COL 6
     "Commision %" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 6.71 COL 53
     "%" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 2.67 COL 86
     "%" VIEW-AS TEXT
          SIZE 2 BY .62 AT ROW 5.05 COL 37
          FONT 6
     "Description" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 6.71 COL 24
     RECT-29 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.sman-mtx,ASI.sman
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
         HEIGHT             = 19.48
         WIDTH              = 164.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR RADIO-SET sman.commbasis IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN custtype_dscr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN sman-mtx.custype IN FRAME F-Main
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN sman-mtx.dscr[10] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN sman-mtx.dscr[1] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN sman-mtx.dscr[2] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN sman-mtx.dscr[3] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN sman-mtx.dscr[4] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN sman-mtx.dscr[5] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN sman-mtx.dscr[6] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN sman-mtx.dscr[7] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN sman-mtx.dscr[8] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN sman-mtx.dscr[9] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN sman.scomm IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN sman.sman IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN sman.sname IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN sman.territory IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN terr_dscr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN sman-mtx.type-comm IN FRAME F-Main
   EXP-LABEL                                                            */
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

&Scoped-define SELF-NAME F-Main
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main V-table-Win
ON HELP OF FRAME F-Main
DO:
   def var char-val as cha no-undo.
    case focus:name :
         when 'procat' then do: 
              run windows/l-fgcat.w (sman.company, focus:screen-value in frame {&frame-name}, output char-val).
              if char-val <> "" then do:
                 assign focus:screen-value = caps(entry(1,char-val)).
                 case focus:index :
                      when 1 then sman-mtx.dscr[1]:screen-value = entry(2,char-val).
                      when 2 then sman-mtx.dscr[2]:screen-value = entry(2,char-val).
                      when 3 then sman-mtx.dscr[3]:screen-value = entry(2,char-val).
                      when 4 then sman-mtx.dscr[4]:screen-value = entry(2,char-val).
                      when 5 then sman-mtx.dscr[5]:screen-value = entry(2,char-val).
                      when 6 then sman-mtx.dscr[6]:screen-value = entry(2,char-val).
                      when 7 then sman-mtx.dscr[7]:screen-value = entry(2,char-val).
                      when 8 then sman-mtx.dscr[8]:screen-value = entry(2,char-val).
                      when 9 then sman-mtx.dscr[9]:screen-value = entry(2,char-val).
                      when 10 then sman-mtx.dscr[10]:screen-value = entry(2,char-val).
                 end case.
              end.   
         end.
    end case.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sman-mtx.procat[10]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sman-mtx.procat[10] V-table-Win
ON LEAVE OF sman-mtx.procat[10] IN FRAME F-Main /* Product Category[10] */
DO:
   def var temp# as cha no-undo.
   def var x as int no-undo.
   if lastkey <> -1 and self:screen-value ne "" then do:
      temp# = self:screen-value.
      for each b-mtx where b-mtx.company = sman-mtx.company
                     and b-mtx.sman = sman-mtx.sman
                     and b-mtx.custype = sman-mtx.custype:screen-value
                 no-lock:
                   do x = 1 to 10:
                     if (b-mtx.procat[x] = temp# and x <> self:index)
                     or (b-mtx.procat[x] = temp# and x = self:index and
                         b-mtx.custype-no <> sman-mtx.custype-no) then
                     do:
                        message "Duplicate category entered.".
                      return no-apply.
                     end.
                   end.
                 end.
    
      if (self:screen-value = sman-mtx.procat[2]:screen-value ) or
         (self:screen-value = sman-mtx.procat[3]:screen-value ) or
         (self:screen-value = sman-mtx.procat[4]:screen-value ) or
         (self:screen-value = sman-mtx.procat[5]:screen-value ) or
         (self:screen-value = sman-mtx.procat[6]:screen-value ) or
         (self:screen-value = sman-mtx.procat[7]:screen-value ) or
         (self:screen-value = sman-mtx.procat[8]:screen-value ) or
         (self:screen-value = sman-mtx.procat[9]:screen-value ) or
         (self:screen-value = sman-mtx.procat[1]:screen-value ) 
      then do:
           message "Duplicate category entered."  view-as alert-box error.
           return no-apply.      
      end.
    end.             
    find first fgcat where fgcat.company = sman.company and
                           fgcat.procat = self:screen-value
                           no-lock no-error.
    sman-mtx.dscr[10]:screen-value = if avail fgcat then fgcat.dscr else "".                       

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sman-mtx.procat[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sman-mtx.procat[1] V-table-Win
ON LEAVE OF sman-mtx.procat[1] IN FRAME F-Main /* Product Category[1] */
DO:
   def var temp# as cha no-undo.
   def var x as int no-undo.
   if lastkey <> -1 and self:screen-value ne "" then do:
      temp# = self:screen-value.      
      for each b-mtx where b-mtx.company = sman-mtx.company
                     and b-mtx.sman = sman-mtx.sman
                     and b-mtx.custype = sman-mtx.custype:screen-value
                 no-lock:
                   do x = 1 to 10:
                     if (b-mtx.procat[x] = temp# and x <> self:index)
                     or (b-mtx.procat[x] = temp# and x = self:index and
                         b-mtx.custype-no <> sman-mtx.custype-no) then
                     do:
                        message "Duplicate category entered.".
                        return no-apply.
                     end.  /* do */
             
                   end.    /* x */
                   
      end. /* for each */
      if (self:screen-value = sman-mtx.procat[2]:screen-value ) or
         (self:screen-value = sman-mtx.procat[3]:screen-value ) or
         (self:screen-value = sman-mtx.procat[4]:screen-value ) or
         (self:screen-value = sman-mtx.procat[5]:screen-value ) or
         (self:screen-value = sman-mtx.procat[6]:screen-value ) or
         (self:screen-value = sman-mtx.procat[7]:screen-value ) or
         (self:screen-value = sman-mtx.procat[8]:screen-value ) or
         (self:screen-value = sman-mtx.procat[9]:screen-value ) or
         (self:screen-value = sman-mtx.procat[10]:screen-value ) 
      then do:
           message "Duplicate category entered."  view-as alert-box error.
           return no-apply.      
      end.
                     
      find first fgcat where fgcat.company = sman.company and
                           fgcat.procat = self:screen-value
                           no-lock no-error.
      IF NOT AVAIL fgcat THEN DO: 
         MESSAGE "Invalid product category. Try help." VIEW-AS ALERT-BOX ERROR.
         RETURN NO-APPLY.
      END.
      sman-mtx.dscr[1]:screen-value = if avail fgcat then fgcat.dscr else "".                       
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sman-mtx.procat[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sman-mtx.procat[2] V-table-Win
ON LEAVE OF sman-mtx.procat[2] IN FRAME F-Main /* Product Category[2] */
DO:
  def var temp# as cha no-undo.
   def var x as int no-undo.
   if lastkey <> -1 and self:screen-value ne "" then do:
      temp# = self:screen-value.
      for each b-mtx where b-mtx.company = sman-mtx.company
                     and b-mtx.sman = sman-mtx.sman
                     and b-mtx.custype = sman-mtx.custype:screen-value
                 no-lock:
                   do x = 1 to 10:
                     if (b-mtx.procat[x] = temp# and x <> self:index)
                     or (b-mtx.procat[x] = temp# and x = self:index and
                         b-mtx.custype-no <> sman-mtx.custype-no) then
                     do:
                        message "Duplicate category entered.".
                        return no-apply.
                     end.  /* do */
                   end.    /* x */
                   
                 end. /* for each */
                 
    
    if (self:screen-value = sman-mtx.procat[1]:screen-value ) or
         (self:screen-value = sman-mtx.procat[3]:screen-value ) or
         (self:screen-value = sman-mtx.procat[4]:screen-value ) or
         (self:screen-value = sman-mtx.procat[5]:screen-value ) or
         (self:screen-value = sman-mtx.procat[6]:screen-value ) or
         (self:screen-value = sman-mtx.procat[7]:screen-value ) or
         (self:screen-value = sman-mtx.procat[8]:screen-value ) or
         (self:screen-value = sman-mtx.procat[9]:screen-value ) or
         (self:screen-value = sman-mtx.procat[10]:screen-value ) 
      then do:
           message "Duplicate category entered."  view-as alert-box error.
           return no-apply.      
      end.
               
    find first fgcat where fgcat.company = sman.company and
                           fgcat.procat = self:screen-value
                           no-lock no-error.
    IF NOT AVAIL fgcat THEN DO:
       MESSAGE "Invalid product category. Try help." VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
    END.
    sman-mtx.dscr[2]:screen-value = if avail fgcat then fgcat.dscr else "".
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sman-mtx.procat[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sman-mtx.procat[3] V-table-Win
ON LEAVE OF sman-mtx.procat[3] IN FRAME F-Main /* Product Category[3] */
DO:
   def var temp# as cha no-undo.
   def var x as int no-undo.
   if lastkey <> -1 and self:screen-value ne "" then do:
      temp# = self:screen-value.
      for each b-mtx where b-mtx.company = sman-mtx.company
                     and b-mtx.sman = sman-mtx.sman
                     and b-mtx.custype = sman-mtx.custype:screen-value
                 no-lock:
                   do x = 1 to 10:
                     if (b-mtx.procat[x] = temp# and x <> self:index)
                     or (b-mtx.procat[x] = temp# and x = self:index and
                         b-mtx.custype-no <> sman-mtx.custype-no) then
                     do:
                        message "Duplicate category entered.".
                      return no-apply.
                     end.
                   end.
                 end.
 
      if (self:screen-value = sman-mtx.procat[1]:screen-value ) or
         (self:screen-value = sman-mtx.procat[2]:screen-value ) or
         (self:screen-value = sman-mtx.procat[4]:screen-value ) or
         (self:screen-value = sman-mtx.procat[5]:screen-value ) or
         (self:screen-value = sman-mtx.procat[6]:screen-value ) or
         (self:screen-value = sman-mtx.procat[7]:screen-value ) or
         (self:screen-value = sman-mtx.procat[8]:screen-value ) or
         (self:screen-value = sman-mtx.procat[9]:screen-value ) or
         (self:screen-value = sman-mtx.procat[10]:screen-value ) 
      then do:
           message "Duplicate category entered."  view-as alert-box error.
           return no-apply.      
      end.
      find first fgcat where fgcat.company = sman.company and
                           fgcat.procat = self:screen-value
                           no-lock no-error.
      IF NOT AVAIL fgcat THEN DO:
         MESSAGE "Invalid product category. Try help." VIEW-AS ALERT-BOX ERROR.
         RETURN NO-APPLY.
      END.
      sman-mtx.dscr[3]:screen-value = if avail fgcat then fgcat.dscr else "".                       
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sman-mtx.procat[4]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sman-mtx.procat[4] V-table-Win
ON LEAVE OF sman-mtx.procat[4] IN FRAME F-Main /* Product Category[4] */
DO:
   def var temp# as cha no-undo.
   def var x as int no-undo.
   if lastkey <> -1 and self:screen-value ne "" then do:
      temp# = self:screen-value.
      for each b-mtx where b-mtx.company = sman-mtx.company
                     and b-mtx.sman = sman-mtx.sman
                     and b-mtx.custype = sman-mtx.custype:screen-value
                 no-lock:
                   do x = 1 to 10:
                     if (b-mtx.procat[x] = temp# and x <> self:index)
                     or (b-mtx.procat[x] = temp# and x = self:index and
                         b-mtx.custype-no <> sman-mtx.custype-no) then
                     do:
                        message "Duplicate category entered.".
                      return no-apply.
                     end.
                   end.
                 end.
      if (self:screen-value = sman-mtx.procat[1]:screen-value ) or
         (self:screen-value = sman-mtx.procat[2]:screen-value ) or
         (self:screen-value = sman-mtx.procat[3]:screen-value ) or
         (self:screen-value = sman-mtx.procat[5]:screen-value ) or
         (self:screen-value = sman-mtx.procat[6]:screen-value ) or
         (self:screen-value = sman-mtx.procat[7]:screen-value ) or
         (self:screen-value = sman-mtx.procat[8]:screen-value ) or
         (self:screen-value = sman-mtx.procat[9]:screen-value ) or
         (self:screen-value = sman-mtx.procat[10]:screen-value ) 
      then do:
           message "Duplicate category entered."  view-as alert-box error.
           return no-apply.      
      end.
      
      find first fgcat where fgcat.company = sman.company and
                           fgcat.procat = self:screen-value
                           no-lock no-error.
      IF NOT AVAIL fgcat THEN DO:
         MESSAGE "Invalid product category. Try help." VIEW-AS ALERT-BOX ERROR.
         RETURN NO-APPLY.
      END.
      sman-mtx.dscr[4]:screen-value = if avail fgcat then fgcat.dscr else "".                       
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sman-mtx.procat[5]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sman-mtx.procat[5] V-table-Win
ON LEAVE OF sman-mtx.procat[5] IN FRAME F-Main /* Product Category[5] */
DO:
   def var temp# as cha no-undo.
   def var x as int no-undo.
   if lastkey <> -1 and self:screen-value ne "" then do:
      temp# = self:screen-value.
      for each b-mtx where b-mtx.company = sman-mtx.company
                     and b-mtx.sman = sman-mtx.sman
                     and b-mtx.custype = sman-mtx.custype:screen-value
                 no-lock:
                   do x = 1 to 10:
                     if (b-mtx.procat[x] = temp# and x <> self:index)
                     or (b-mtx.procat[x] = temp# and x = self:index and
                         b-mtx.custype-no <> sman-mtx.custype-no) then
                     do:
                        message "Duplicate category entered.".
                      return no-apply.
                     end.
                   end.
                 end.
   
       if (self:screen-value = sman-mtx.procat[1]:screen-value ) or
         (self:screen-value = sman-mtx.procat[2]:screen-value ) or
         (self:screen-value = sman-mtx.procat[3]:screen-value ) or
         (self:screen-value = sman-mtx.procat[4]:screen-value ) or
         (self:screen-value = sman-mtx.procat[6]:screen-value ) or
         (self:screen-value = sman-mtx.procat[7]:screen-value ) or
         (self:screen-value = sman-mtx.procat[8]:screen-value ) or
         (self:screen-value = sman-mtx.procat[9]:screen-value ) or
         (self:screen-value = sman-mtx.procat[10]:screen-value ) 
       then do:
           message "Duplicate category entered."  view-as alert-box error.
           return no-apply.      
       end.
 
       find first fgcat where fgcat.company = sman.company and
                           fgcat.procat = self:screen-value
                           no-lock no-error.
       IF NOT AVAIL fgcat THEN DO:
          MESSAGE "Invalid product category. Try help." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
       END.
       sman-mtx.dscr[5]:screen-value = if avail fgcat then fgcat.dscr else "".                       
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sman-mtx.procat[6]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sman-mtx.procat[6] V-table-Win
ON LEAVE OF sman-mtx.procat[6] IN FRAME F-Main /* Product Category[6] */
DO:
   def var temp# as cha no-undo.
   def var x as int no-undo.
   if lastkey <> -1 and self:screen-value ne "" then do:
      temp# = self:screen-value.
      for each b-mtx where b-mtx.company = sman-mtx.company
                     and b-mtx.sman = sman-mtx.sman
                     and b-mtx.custype = sman-mtx.custype:screen-value
                 no-lock:
                   do x = 1 to 10:
                     if (b-mtx.procat[x] = temp# and x <> self:index)
                     or (b-mtx.procat[x] = temp# and x = self:index and
                         b-mtx.custype-no <> sman-mtx.custype-no) then
                     do:
                        message "Duplicate category entered.".
                      return no-apply.
                     end.
                   end.
                 end.
      if (self:screen-value = sman-mtx.procat[1]:screen-value ) or
         (self:screen-value = sman-mtx.procat[2]:screen-value ) or
         (self:screen-value = sman-mtx.procat[3]:screen-value ) or
         (self:screen-value = sman-mtx.procat[4]:screen-value ) or
         (self:screen-value = sman-mtx.procat[5]:screen-value ) or
         (self:screen-value = sman-mtx.procat[7]:screen-value ) or
         (self:screen-value = sman-mtx.procat[8]:screen-value ) or
         (self:screen-value = sman-mtx.procat[9]:screen-value ) or
          (self:screen-value = sman-mtx.procat[10]:screen-value ) 
      then do:
           message "Duplicate category entered."  view-as alert-box error.
           return no-apply.      
      end.
    
      find first fgcat where fgcat.company = sman.company and
                           fgcat.procat = self:screen-value
                           no-lock no-error.
      IF NOT AVAIL fgcat THEN DO:
         MESSAGE "Invalid product category. Try help." VIEW-AS ALERT-BOX ERROR.
         RETURN NO-APPLY.
      END.
      sman-mtx.dscr[6]:screen-value = if avail fgcat then fgcat.dscr else "".                       
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sman-mtx.procat[7]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sman-mtx.procat[7] V-table-Win
ON LEAVE OF sman-mtx.procat[7] IN FRAME F-Main /* Product Category[7] */
DO:
   def var temp# as cha no-undo.
   def var x as int no-undo.
   if lastkey <> -1 and self:screen-value ne "" then do:
      temp# = self:screen-value.
      for each b-mtx where b-mtx.company = sman-mtx.company
                     and b-mtx.sman = sman-mtx.sman
                     and b-mtx.custype = sman-mtx.custype:screen-value
                 no-lock:
                   do x = 1 to 10:
                     if (b-mtx.procat[x] = temp# and x <> self:index)
                     or (b-mtx.procat[x] = temp# and x = self:index and
                         b-mtx.custype-no <> sman-mtx.custype-no) then
                     do:
                        message "Duplicate category entered.".
                      return no-apply.
                     end.
                   end.
                 end.
                    
      if (self:screen-value = sman-mtx.procat[1]:screen-value ) or
         (self:screen-value = sman-mtx.procat[2]:screen-value ) or
         (self:screen-value = sman-mtx.procat[3]:screen-value ) or
         (self:screen-value = sman-mtx.procat[4]:screen-value ) or
         (self:screen-value = sman-mtx.procat[5]:screen-value ) or
         (self:screen-value = sman-mtx.procat[6]:screen-value ) or
         (self:screen-value = sman-mtx.procat[8]:screen-value ) or
         (self:screen-value = sman-mtx.procat[9]:screen-value ) or
         (self:screen-value = sman-mtx.procat[10]:screen-value ) 
      then do:
           message "Duplicate category entered."  view-as alert-box error.
           return no-apply.      
      end.
      find first fgcat where fgcat.company = sman.company and
                           fgcat.procat = self:screen-value
                           no-lock no-error.
      IF NOT AVAIL fgcat THEN DO:
         MESSAGE "Invalid product category. Try help." VIEW-AS ALERT-BOX ERROR.
         RETURN NO-APPLY.
      END.
      sman-mtx.dscr[7]:screen-value = if avail fgcat then fgcat.dscr else "".                       
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sman-mtx.procat[8]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sman-mtx.procat[8] V-table-Win
ON LEAVE OF sman-mtx.procat[8] IN FRAME F-Main /* Product Category[8] */
DO:
   def var temp# as cha no-undo.
   def var x as int no-undo.
   if lastkey <> -1 and self:screen-value ne "" then do:
      temp# = self:screen-value.
      for each b-mtx where b-mtx.company = sman-mtx.company
                     and b-mtx.sman = sman-mtx.sman
                     and b-mtx.custype = sman-mtx.custype:screen-value
                 no-lock:
                   do x = 1 to 10:
                     if (b-mtx.procat[x] = temp# and x <> self:index)
                     or (b-mtx.procat[x] = temp# and x = self:index and
                         b-mtx.custype-no <> sman-mtx.custype-no) then
                     do:
                        message "Duplicate category entered.".
                      return no-apply.
                     end.
                   end.
                 end.
                  
    if (self:screen-value = sman-mtx.procat[1]:screen-value ) or
         (self:screen-value = sman-mtx.procat[2]:screen-value ) or
         (self:screen-value = sman-mtx.procat[3]:screen-value ) or
         (self:screen-value = sman-mtx.procat[4]:screen-value ) or
         (self:screen-value = sman-mtx.procat[5]:screen-value ) or
         (self:screen-value = sman-mtx.procat[6]:screen-value ) or
         (self:screen-value = sman-mtx.procat[7]:screen-value ) or
         (self:screen-value = sman-mtx.procat[9]:screen-value ) or
         (self:screen-value = sman-mtx.procat[10]:screen-value ) 
      then do:
           message "Duplicate category entered."  view-as alert-box error.
           return no-apply.      
      end.
      
      find first fgcat where fgcat.company = sman.company and
                           fgcat.procat = self:screen-value
                           no-lock no-error.
      IF NOT AVAIL fgcat THEN DO:
         MESSAGE "Invalid product category. Try help." VIEW-AS ALERT-BOX ERROR.
         RETURN NO-APPLY.
      END.
      sman-mtx.dscr[8]:screen-value = if avail fgcat then fgcat.dscr else "".                       
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sman-mtx.procat[9]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sman-mtx.procat[9] V-table-Win
ON LEAVE OF sman-mtx.procat[9] IN FRAME F-Main /* Product Category[9] */
DO:
   def var temp# as cha no-undo.
   def var x as int no-undo.
   if lastkey <> -1 and self:screen-value ne "" then do:
      temp# = self:screen-value.
      for each b-mtx where b-mtx.company = sman-mtx.company
                     and b-mtx.sman = sman-mtx.sman
                     and b-mtx.custype = sman-mtx.custype:screen-value
                 no-lock:
                   do x = 1 to 10:
                     if (b-mtx.procat[x] = temp# and x <> self:index)
                     or (b-mtx.procat[x] = temp# and x = self:index and
                         b-mtx.custype-no <> sman-mtx.custype-no) then
                     do:
                        message "Duplicate category entered.".
                      return no-apply.
                     end.
                   end.
                 end.
    
      if (self:screen-value = sman-mtx.procat[1]:screen-value ) or
         (self:screen-value = sman-mtx.procat[2]:screen-value ) or
         (self:screen-value = sman-mtx.procat[3]:screen-value ) or
         (self:screen-value = sman-mtx.procat[4]:screen-value ) or
         (self:screen-value = sman-mtx.procat[5]:screen-value ) or
         (self:screen-value = sman-mtx.procat[6]:screen-value ) or
         (self:screen-value = sman-mtx.procat[7]:screen-value ) or
         (self:screen-value = sman-mtx.procat[8]:screen-value ) or
         (self:screen-value = sman-mtx.procat[10]:screen-value ) 
      then do:
           message "Duplicate category entered."  view-as alert-box error.
           return no-apply.      
      end.
                
      find first fgcat where fgcat.company = sman.company and
                           fgcat.procat = self:screen-value
                           no-lock no-error.
      IF NOT AVAIL fgcat THEN DO:
         MESSAGE "Invalid product category. Try help." VIEW-AS ALERT-BOX ERROR.
         RETURN NO-APPLY.
      END.
      sman-mtx.dscr[9]:screen-value = if avail fgcat then fgcat.dscr else "".                       
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
session:data-entry-return = yes.
{sys/inc/f3help.i}
  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  {src/adm/template/row-list.i "sman-mtx"}
  {src/adm/template/row-list.i "sman"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "sman-mtx"}
  {src/adm/template/row-find.i "sman"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def buffer sman-mtx-tmp for sman-mtx.
  
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  if adm-adding-record then do:
     find last sman-mtx-tmp where sman-mtx-tmp.company = sman.company and
                                  sman-mtx-tmp.sman = sman.sman and
                                  sman-mtx-tmp.custype = sman-mtx.custype
                                  no-lock no-error.
     sman-mtx.custype-no = if avail sman-mtx-tmp then sman-mtx-tmp.custype-no + 1
                           else 1.
  end.
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
  assign sman-mtx.company = sman.company
         sman-mtx.sman = sman.sman.
                  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def var char-hdl as cha no-undo.
  /* Code placed here will execute PRIOR to standard behavior. */
  if not avail sman-mtx then return.
  find first custype where custype.custype = sman-mtx.custype no-lock no-error.
  custtype_dscr:screen-value in frame {&frame-name} = if avail custype then custype.dscr else "".
  find first terr where terr.terr = sman.territory no-lock no-error.
  terr_dscr:screen-value = if avail terr then terr.dscr else "".
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  run get-link-handle in adm-broker-hdl (this-procedure, "tableio-source", output char-hdl).
  if valid-handle(widget-handle(char-hdl)) then
      run set-buttons in widget-handle(char-hdl) ("initial").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def var temp# as cha no-undo.
  def var i as int no-undo. 
  def var x as int no-undo.
  
  /* Code placed here will execute PRIOR to standard behavior. */
  do with frame {&frame-name} i = 1 to 10:
     if input sman-mtx.procat[i] ne "" then do:
         temp# = input sman-mtx.procat[i].
         for each b-mtx where b-mtx.company = sman-mtx.company
                     and b-mtx.sman = sman-mtx.sman
                     and b-mtx.custype = sman-mtx.custype:screen-value 
                 no-lock:
                   do x = 1 to 10:
                     if (b-mtx.procat[x] = temp# and x <> i)
                     or (b-mtx.procat[x] = temp# and x = i and
                         b-mtx.custype-no <> sman-mtx.custype-no
                         /*recid(b-mtx) <> recid(sman-mtx)*/ ) then
                     do:
                       message "Duplicate category entered." view-as alert-box error.
                       case i :
                          when 1 then apply "entry" to sman-mtx.procat[1].
                          when 2 then apply "entry" to sman-mtx.procat[2].
                          when 3 then apply "entry" to sman-mtx.procat[3].
                          when 4 then apply "entry" to sman-mtx.procat[4].
                          when 5 then apply "entry" to sman-mtx.procat[5].
                          when 6 then apply "entry" to sman-mtx.procat[6].
                          when 7 then apply "entry" to sman-mtx.procat[7].
                          when 8 then apply "entry" to sman-mtx.procat[8].
                          when 9 then apply "entry" to sman-mtx.procat[9].
                          when 10 then apply "entry" to sman-mtx.procat[10].
                     end case.
                     return no-apply.
                     end.
                   end.  /* x */
         end.      
         find first fgcat where fgcat.company = sman.company and
                           fgcat.procat = temp#
                           no-lock no-error.
         IF NOT AVAIL fgcat THEN DO:
            MESSAGE "Invalid product category. Try help." VIEW-AS ALERT-BOX ERROR.
            CASE i :
                WHEN 1 THEN APPLY "entry" TO sman-mtx.procat[1].
                WHEN 2 THEN APPLY "entry" TO sman-mtx.procat[2].
                WHEN 3 THEN APPLY "entry" TO sman-mtx.procat[3].
                WHEN 4 THEN APPLY "entry" TO sman-mtx.procat[4].
                WHEN 5 THEN APPLY "entry" TO sman-mtx.procat[5].
                WHEN 6 THEN APPLY "entry" TO sman-mtx.procat[6].
                WHEN 7 THEN APPLY "entry" TO sman-mtx.procat[7].
                WHEN 8 THEN APPLY "entry" TO sman-mtx.procat[8].
                WHEN 9 THEN APPLY "entry" TO sman-mtx.procat[9].
                WHEN 10 THEN APPLY "entry" TO sman-mtx.procat[10].
            END.
            RETURN error.
         END.
     end.  /* if input <> "" */
  end.  /* do i */
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

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
  {src/adm/template/snd-list.i "sman-mtx"}
  {src/adm/template/snd-list.i "sman"}

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

