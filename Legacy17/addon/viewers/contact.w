&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          emptrack         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS B-table-Win
{Advantzware\WinKit\admViewersUsing.i} /* added by script _admViewers.p */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/contact.w

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
def var ll-new-record as log no-undo.
{custom/gcompany.i}
{methods/defines/contact.i &NEW="NEW"}

&IF DEFINED(UIB_is_Running) NE 0 &THEN
&Scoped-define NEW NEW GLOBAL
&ENDIF

DEFINE {&NEW} SHARED VARIABLE g_lookup-var AS CHARACTER NO-UNDO.

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
&Scoped-define EXTERNAL-TABLES contact
&Scoped-define FIRST-EXTERNAL-TABLE contact


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR contact.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS contact.sirname contact.first-name ~
contact.middle-initial contact.last-name contact.contact-title ~
contact.cust-no contact.ship-id contact.contact-loc contact.type ~
contact.maillist contact.access-code contact.phone contact.extension ~
contact.cell-phone contact.fax contact.email contact.website 
&Scoped-define ENABLED-TABLES contact
&Scoped-define FIRST-ENABLED-TABLE contact
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-3 RECT-4 RECT-5 RECT-6 
&Scoped-Define DISPLAYED-FIELDS contact.sirname contact.first-name ~
contact.middle-initial contact.last-name contact.contact-title ~
contact.cust-no contact.ship-id contact.contact-loc contact.type ~
contact.maillist contact.access-code contact.phone contact.extension ~
contact.cell-phone contact.fax contact.email contact.website 
&Scoped-define DISPLAYED-TABLES contact
&Scoped-define FIRST-DISPLAYED-TABLE contact
&Scoped-Define DISPLAYED-OBJECTS contact_sman contact_cust-name ~
contact_addr1 contact_addr2 contact_city contact_state contact_zip ~
contact_country contact_county contact_territory F1 F-2 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-CREATE-FIELDS contact.contact-loc 
&Scoped-define DISPLAY-FIELD contact.contact-loc 
&Scoped-define F1 F1 F-2 

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
DEFINE VARIABLE contact_addr1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Address" 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE contact_addr2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Address" 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE contact_city AS CHARACTER FORMAT "X(256)":U 
     LABEL "City/St/Zip" 
     VIEW-AS FILL-IN 
     SIZE 19.6 BY 1
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE contact_country AS CHARACTER FORMAT "X(256)":U 
     LABEL "Country" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE contact_county AS CHARACTER FORMAT "X(256)":U 
     LABEL "County" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE contact_cust-name AS CHARACTER FORMAT "X(256)":U 
     LABEL "Company" 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE contact_sman AS CHARACTER FORMAT "X(256)":U 
     LABEL "SalesRep" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE contact_state AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE contact_territory AS CHARACTER FORMAT "X(3)":U 
     LABEL "Territory" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE contact_zip AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE F-2 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE F1 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 145 BY 17.14.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 108 BY 3.57.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 108 BY 2.62.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 141 BY 3.57.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 69 BY 6.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     contact.sirname AT ROW 1.48 COL 19 COLON-ALIGNED
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEMS "","Mr.","Mrs.","Ms.","Dr." 
          DROP-DOWN-LIST
          SIZE 12 BY 1
     contact.first-name AT ROW 2.43 COL 19 COLON-ALIGNED
          LABEL "Contact Name"
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
          BGCOLOR 15 FONT 4
     contact.middle-initial AT ROW 2.43 COL 60 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
          BGCOLOR 15 FONT 4
     contact.last-name AT ROW 2.43 COL 68 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
          BGCOLOR 15 FONT 4
     contact.contact-title AT ROW 3.38 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 62 BY 1
          BGCOLOR 15 FONT 4
     contact.cust-no AT ROW 5.29 COL 19 COLON-ALIGNED
          LABEL "Customer"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
          BGCOLOR 15 FONT 4
     contact.ship-id AT ROW 6.24 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
          BGCOLOR 15 FONT 4
     contact.contact-loc AT ROW 5.29 COL 52 COLON-ALIGNED
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEMS "","C","S" 
          DROP-DOWN-LIST
          SIZE 9 BY 1 TOOLTIP "'C'ustomer, 'S'hipto or ' ' for Neithor"
     contact.type AT ROW 6.24 COL 52 COLON-ALIGNED FORMAT "X(8)"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
          BGCOLOR 15 FONT 4
     contact_sman AT ROW 5.29 COL 85 COLON-ALIGNED
     contact.maillist AT ROW 6.24 COL 85 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
          BGCOLOR 15 FONT 4
     contact.access-code AT ROW 8.14 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 4
     contact.phone AT ROW 9.1 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 4
     contact.extension AT ROW 9.1 COL 52 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
          BGCOLOR 15 FONT 4
     contact.cell-phone AT ROW 10.05 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 4
     contact.fax AT ROW 8.14 COL 77 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 4
     contact.email AT ROW 9.1 COL 77 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 62 BY 1
          BGCOLOR 15 FONT 4
     contact.website AT ROW 10.05 COL 77 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 62 BY 1
          BGCOLOR 15 FONT 4
     contact_cust-name AT ROW 11.95 COL 19 COLON-ALIGNED
     contact_addr1 AT ROW 12.91 COL 19 COLON-ALIGNED
     contact_addr2 AT ROW 13.86 COL 19 COLON-ALIGNED
     contact_city AT ROW 14.81 COL 19 COLON-ALIGNED
     contact_state AT ROW 14.81 COL 41 COLON-ALIGNED NO-LABEL
     contact_zip AT ROW 14.81 COL 50 COLON-ALIGNED NO-LABEL
     contact_country AT ROW 15.76 COL 19 COLON-ALIGNED
     contact_county AT ROW 16.71 COL 19 COLON-ALIGNED
     contact_territory AT ROW 16.71 COL 50 COLON-ALIGNED
     F1 AT ROW 5.29 COL 36 NO-LABEL
     F-2 AT ROW 6.24 COL 34 NO-LABEL
     RECT-1 AT ROW 1 COL 1
     RECT-3 AT ROW 1.24 COL 2
     RECT-4 AT ROW 5.05 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     RECT-5 AT ROW 7.91 COL 2
     RECT-6 AT ROW 11.71 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: contact
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
         HEIGHT             = 17.71
         WIDTH              = 145.
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
   NOT-VISIBLE Size-to-Fit Custom                                       */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR COMBO-BOX contact.contact-loc IN FRAME F-Main
   1 4                                                                  */
/* SETTINGS FOR FILL-IN contact_addr1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN contact_addr2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN contact_city IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN contact_country IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN contact_county IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN contact_cust-name IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN contact_sman IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN contact_state IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN contact_territory IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN contact_zip IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN contact.cust-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN F-2 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F-2:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN F1 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F1:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN contact.first-name IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN contact.type IN FRAME F-Main
   EXP-FORMAT                                                           */
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
   def var lv-handle as widget-handle no-undo.
   def var char-val as cha no-undo.

   case focus:name :
        when "contact-title" then do:
             run windows/l-title.w (focus:screen-value, output char-val).
             if char-val <> "" then 
                assign focus:screen-value = entry(1,char-val)
                       .
             return no-apply.          
        end.
        when "contact_sman" then do:
             run windows/l-sman.w (gcompany, output char-val).
             if char-val <> "" then 
                assign focus:screen-value = entry(1,char-val)
                       .
             return no-apply.          
        end.

        otherwise do:
           lv-handle = focus:handle.
           run applhelp.p.

           if g_lookup-var <> "" then do:
              lv-handle:screen-value = g_lookup-var.
              if lv-handle:name = "cust-no" then do:
                 find cust where cust.company = gcompany and
                              cust.cust-no = lv-handle:screen-value 
                              no-lock no-error.
              end.    
           end.   /* g_lookup-var <> "" */

        end.   
   end case.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME contact.contact-loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contact.contact-loc V-table-Win
ON LEAVE OF contact.contact-loc IN FRAME F-Main /* Location */
DO:
  {&methods/lValidateError.i YES}
  if SELF:SCREEN-VALUE eq "C" and contact.cust-no:SCREEN-VALUE eq "" then
  do:
    message "Location May no be 'C' when Customer is Blank" 
       view-as alert-box error.
    SELF:SCREEN-VALUE = "".
    APPLY 'ENTRY' TO SELF.
    RETURN NO-APPLY.
  end.

  if SELF:SCREEN-VALUE eq "S" and contact.ship-id:SCREEN-VALUE eq "" then
  do:
    message "Location May no be 'S' when ShipID is Blank" 
       view-as alert-box error.
    SELF:SCREEN-VALUE = "".
    APPLY 'ENTRY' TO SELF.
    RETURN NO-APPLY.
  end.

  if SELF:SCREEN-VALUE eq ? and contact.cust-no:screen-value eq "" then
    enable  contact_sman
            contact_cust-name
            contact_addr1 
            contact_addr2
            contact_city
            contact_state
            contact_zip
            contact_country
            contact_county
            contact_territory
     WITH FRAME {&FRAME-NAME}.
  else
    disable contact_sman
            contact_cust-name
            contact_addr1 
            contact_addr2
            contact_city
            contact_state
            contact_zip
            contact_country
            contact_county
            contact_territory
     WITH FRAME {&FRAME-NAME}.

  if SELF:SCREEN-VALUE eq ? and contact.cust-no:screen-value ne "" then    
  do:
    message "Location May no be ' ' when Customer or ShipID Non-Blank." 
       view-as alert-box error.
    SELF:SCREEN-VALUE = "".
    APPLY 'ENTRY' TO SELF.
    RETURN NO-APPLY.
  END.

  {methods/dispflds.i}
  {&methods/lValidateError.i NO}

 END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contact.contact-loc V-table-Win
ON VALUE-CHANGED OF contact.contact-loc IN FRAME F-Main /* Location */
DO:
  {&methods/lValidateError.i YES}
  if SELF:SCREEN-VALUE eq "C" and contact.cust-no:SCREEN-VALUE eq "" then
  do:
    message "Location May no be 'C' when Customer is Blank" 
       view-as alert-box error.
    SELF:SCREEN-VALUE = "".
    APPLY 'ENTRY' TO SELF.
    RETURN NO-APPLY.
  end.

  if SELF:SCREEN-VALUE eq "S" and contact.ship-id:SCREEN-VALUE eq "" then
  do:
    message "Location May no be 'S' when ShipID is Blank" 
       view-as alert-box error.
    SELF:SCREEN-VALUE = "".
    APPLY 'ENTRY' TO SELF.
    RETURN NO-APPLY.
  end.

  if SELF:SCREEN-VALUE eq ? and contact.cust-no:screen-value eq "" then
  do:
    if contact.cust-name eq "" then contact_cust-name = "".
    if contact.addr1     eq "" then contact_addr1 = "".
    if contact.addr2     eq "" then contact_addr2 = "".
    if contact.city      eq "" then contact_city = "".
    if contact.state     eq "" then contact_state = "".
    if contact.zip       eq "" then contact_zip = "".
    if contact.country   eq "" then contact_country = "".
    if contact.territory   eq "" then contact_territory = "".

    assign  contact_cust-name:fgcolor = ?
            contact_addr1:fgcolor = ? 
            contact_addr2:fgcolor = ?
            contact_city:fgcolor = ?
            contact_state:fgcolor = ?
            contact_zip:fgcolor = ?
            contact_country:fgcolor = ?
            contact_county:fgcolor = ?
            contact_territory:fgcolor = ?
            contact_sman:fgcolor = ?
            contact_cust-name:bgcolor = 15
            contact_addr1:bgcolor = 15
            contact_addr2:bgcolor = 15
            contact_city:bgcolor = 15
            contact_state:bgcolor = 15
            contact_zip:bgcolor = 15
            contact_country:bgcolor = 15
            contact_county:bgcolor = 15
            contact_territory:bgcolor = 15
            contact_sman:bgcolor = 15
            .


    enable  contact_sman
            contact_cust-name
            contact_addr1 
            contact_addr2
            contact_city
            contact_state
            contact_zip
            contact_country
            contact_county
            contact_territory
     WITH FRAME {&FRAME-NAME}.
  end.
  else do:
    assign  contact_cust-name:fgcolor = ?
            contact_addr1:fgcolor = ? 
            contact_addr2:fgcolor = ?
            contact_city:fgcolor = ?
            contact_state:fgcolor = ?
            contact_zip:fgcolor = ?
            contact_country:fgcolor = ?
            contact_county:fgcolor = ?
            contact_territory:fgcolor = ?
            contact_sman:fgcolor = ?
            contact_cust-name:bgcolor = 7
            contact_addr1:bgcolor = 7
            contact_addr2:bgcolor = 7
            contact_city:bgcolor = 7
            contact_state:bgcolor = 7
            contact_zip:bgcolor = 7
            contact_country:bgcolor = 7
            contact_county:bgcolor = 7
            contact_territory:bgcolor = 7
            contact_sman:bgcolor = 7
            .

    disable contact_cust-name
            contact_addr1 
            contact_addr2
            contact_city
            contact_state
            contact_zip
            contact_country
            contact_county
            contact_territory
            contact_sman
     WITH FRAME {&FRAME-NAME}.
  end.
  if SELF:SCREEN-VALUE eq ? and contact.cust-no:screen-value ne "" then    
  do:
    message "Location May no be ' ' when Customer or ShipID Non-Blank." 
       view-as alert-box error.
    SELF:SCREEN-VALUE = "".
    APPLY 'ENTRY' TO SELF.
    RETURN NO-APPLY.
  END.

  {methods/dispflds.i}
  {&methods/lValidateError.i NO}

END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME contact.cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contact.cust-no V-table-Win
ON LEAVE OF contact.cust-no IN FRAME F-Main /* Customer */
DO:
  {&methods/lValidateError.i YES}
  if SELF:SCREEN-VALUE ne "" then
  do:
    {methods/entryerr.i
      &can-find="FIRST cust WHERE cust.company = gcompany
                              AND cust.cust-no = SELF:SCREEN-VALUE"
      &error-message="Invalid Customer Number"}

    FIND FIRST cust WHERE cust.company eq gcompany
                      AND cust.cust-no eq contact.cust-no:screen-value
                    NO-LOCK NO-ERROR.
    IF avail cust and contact.phone:screen-value eq "   -   -    " then                
      assign contact.phone:screen-value = cust.area + cust.phone.
    IF avail cust and contact.fax:screen-value eq "   -   -    " then
      assign contact.fax:screen-value = cust.fax.

    if avail cust then contact_sman:screen-value in frame {&frame-name} = cust.sman.  
    enable contact.contact-loc WITH FRAME {&FRAME-NAME}.

    disable contact_sman
            contact_cust-name
            contact_addr1 
            contact_addr2
            contact_city
            contact_state
            contact_zip
            contact_country
            contact_county
            contact_territory
     WITH FRAME {&FRAME-NAME}.

  end.
  {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME contact.first-name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contact.first-name V-table-Win
ON LEAVE OF contact.first-name IN FRAME F-Main /* Contact Name */
DO:
  {&methods/lValidateError.i YES}
  correct-error = false.
  if contact.first-name:SCREEN-VALUE in frame {&frame-name} eq '' then
    assign correct-error = true.

  {methods/entryerr.i
            &error-message="First Name CANNOT be Blank"}
  {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME contact.last-name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contact.last-name V-table-Win
ON LEAVE OF contact.last-name IN FRAME F-Main /* Last Name */
DO:
  {&methods/lValidateError.i YES}
  if contact.last-name:SCREEN-VALUE in frame {&frame-name} eq '' then
    assign correct-error = true.

  {methods/entryerr.i
            &error-message="Last Name CANNOT be Blank"}
  {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME contact.maillist
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contact.maillist V-table-Win
ON LEAVE OF contact.maillist IN FRAME F-Main /* Mail List */
DO:
  {&methods/lValidateError.i YES}
  if  SELF:SCREEN-VALUE eq "yes" and 
     (contact_addr1:screen-value in frame {&frame-name} eq '' or
      contact_city:screen-value  in frame {&frame-name} eq '' or 
      contact_state:screen-value in frame {&frame-name} eq '' or
      contact_zip:screen-value   in frame {&frame-name} eq '') then
    message "Remember to Complete the Contact Address Information Below." view-as alert-box.
    {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME contact.ship-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contact.ship-id V-table-Win
ON ENTRY OF contact.ship-id IN FRAME F-Main /* Shipto ID */
DO:
  ASSIGN
    s-cust-no = contact.cust-no:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL contact.ship-id V-table-Win
ON LEAVE OF contact.ship-id IN FRAME F-Main /* Shipto ID */
DO:
  {&methods/lValidateError.i YES}
  if SELF:SCREEN-VALUE ne "" then
  do:
    {methods/entryerr.i
      &can-find="FIRST shipto WHERE shipto.company = gcompany
                                AND shipto.cust-no = s-cust-no
                                AND shipto.ship-id = SELF:SCREEN-VALUE"
      &error-message="Invalid Shipto Number"}

    assign contact.contact-loc:screen-value = "S".

    if contact.cust-no:screen-value ne contact.ship-id:screen-value then
      disable contact.contact-loc with frame {&frame-name}.
    else
      enable contact.contact-loc with frame {&frame-name}.
    APPLY 'VALUE-CHANGED' TO contact.contact-loc.
  end.
  {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  {custom/getcmpny.i}


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
  {src/adm/template/row-list.i "contact"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "contact"}

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

  if contact.first-name:SCREEN-VALUE in frame {&frame-name} eq '' or
     contact.last-name:SCREEN-VALUE  in frame {&frame-name} eq '' then
  do:
    message "First and Last Name CANNOT be Blank.  Please ReEnter."
          view-as alert-box.
    undo, retry.
  end.

  if  contact.maillist:SCREEN-VALUE eq "yes" and 
     (contact_addr1:screen-value in frame {&frame-name} eq '' or
      contact_city:screen-value  in frame {&frame-name} eq '' or 
      contact_state:screen-value in frame {&frame-name} eq '' or
      contact_zip:screen-value   in frame {&frame-name} eq '') then
  do:
    message "Address CANNOT be Blank when Mailing is YES.  Please ReEnter"
          view-as alert-box.
    enable  contact_addr1 
            contact_addr2
            contact_city
            contact_state
            contact_zip
            contact_country
            contact_county
            contact_territory
     WITH FRAME {&FRAME-NAME}. 
     undo, retry.
  end.

  /* Code placed here will execute PRIOR to standard behavior. */
  assign frame {&frame-name} contact_cust-name
         frame {&frame-name} contact_addr1
         frame {&frame-name} contact_addr2
         frame {&frame-name} contact_city
         frame {&frame-name} contact_state
         frame {&frame-name} contact_zip
         frame {&frame-name} contact_country
         frame {&frame-name} contact_county
         frame {&frame-name} contact_territory
         frame {&frame-name} contact_sman
         .

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  {methods/viewers/assign/contact.i}
  ll-new-record = no. 
  session:set-wait-state("general").
  for each mailcont where mailcont.contact-rec = recid(contact) :
      assign mailcont.first-name = contact.first-name
             mailcont.last-name = contact.last-name
             mailcont.contact-title = contact.contact-title.
  end.
  session:set-wait-state("").
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
  ll-new-record = yes.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  {methods/viewers/create/contact.i}

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

  if avail contact and not ll-new-record then
     assign contact_addr1 = contact.addr1
            contact_addr2 = contact.addr2
            contact_city = contact.city
            contact_state = contact.state
            contact_zip = contact.zip
            contact_country = contact.country
            contact_county = contact.county
            contact_territory = contact.territory
            contact_sman = contact.sman
            contact_cust-name = contact.cust-name
            contact.contact-loc:screen-value in frame {&frame-name} = 
                      if contact.contact-loc = ? or contact.contact-loc = " "
                      then " " else contact.contact-loc
            .

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

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
  {src/adm/template/snd-list.i "contact"}

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

