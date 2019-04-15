&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: util\updcust#.w

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
{custom/gloc.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

assign
 cocode = gcompany
 locode = gloc.

def var v-process as log no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 begin_cust end_cust btn-process ~
btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_cust end_cust 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
     LABEL "Ca&ncel" 
     SIZE 18 BY 1.14.

DEFINE BUTTON btn-process 
     LABEL "&Start Process" 
     SIZE 18 BY 1.14.

DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)":U 
     LABEL "Old Customer#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE end_cust AS CHARACTER FORMAT "x(8)":U 
     LABEL "New Customer#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 9.52.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     begin_cust AT ROW 7.67 COL 37 COLON-ALIGNED
     end_cust AT ROW 10.52 COL 37 COLON-ALIGNED
     btn-process AT ROW 15.76 COL 21
     btn-cancel AT ROW 15.76 COL 53
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 5.29 COL 5
     "" VIEW-AS TEXT
          SIZE 2.2 BY .95 AT ROW 1.95 COL 88
          BGCOLOR 11 
     RECT-17 AT ROW 4.81 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.6 BY 17.52.

DEFINE FRAME FRAME-B
     "" VIEW-AS TEXT
          SIZE 6.2 BY .95 AT ROW 2.91 COL 82.8
          BGCOLOR 11 
     "" VIEW-AS TEXT
          SIZE 88.8 BY .95 AT ROW 1 COL 1
          BGCOLOR 11 
     "" VIEW-AS TEXT
          SIZE 3 BY .95 AT ROW 1.95 COL 1
          BGCOLOR 11 
     "This process may take hours.  Please let the process complete!" VIEW-AS TEXT
          SIZE 76 BY .95 AT ROW 2.91 COL 8
          BGCOLOR 11 FGCOLOR 12 FONT 5
     "You MUST perform a database backup before running this procedure!" VIEW-AS TEXT
          SIZE 84 BY .95 AT ROW 1.95 COL 4
          BGCOLOR 11 FGCOLOR 12 FONT 5
     "" VIEW-AS TEXT
          SIZE 7 BY .95 AT ROW 2.91 COL 1
          BGCOLOR 11 
     "" VIEW-AS TEXT
          SIZE 88.8 BY .95 AT ROW 3.76 COL 1
          BGCOLOR 11 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.2 BY 3.81
         BGCOLOR 11 .


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
         TITLE              = "Update Customer Number"
         HEIGHT             = 17.71
         WIDTH              = 90.2
         MAX-HEIGHT         = 19.76
         MAX-WIDTH          = 98.2
         VIRTUAL-HEIGHT     = 19.76
         VIRTUAL-WIDTH      = 98.2
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
/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
ASSIGN
       btn-cancel:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


ASSIGN
       btn-process:PRIVATE-DATA IN FRAME FRAME-A     = 
                "ribbon-button".


/* SETTINGS FOR FRAME FRAME-B
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Update Customer Number */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Update Customer Number */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust C-Win
ON LEAVE OF begin_cust IN FRAME FRAME-A /* Old Customer# */
DO:
  assign {&self-name}.

  {&self-name}:screen-value = caps({&self-name}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME FRAME-A /* Cancel */
DO:
    apply "close" to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CHOOSE OF btn-process IN FRAME FRAME-A /* Start Process */
DO:
  if not can-find(first cust
                  where cust.company eq cocode
                    and cust.cust-no eq begin_cust) then do:
    message "You must enter a valid customer number" view-as alert-box error.
    apply "entry" to begin_cust.
    return no-apply.
  end.

  IF begin_cust EQ end_cust THEN
  DO:
    MESSAGE "Old and New Customer #s are the same.  Cannot Process."
        VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    apply "entry" to end_cust.
    return no-apply.
  END.

  if can-find(first cust
              where cust.company eq cocode
                and cust.cust-no eq end_cust) then do:
    v-process = no.

    message "The new Cust# already exists, merge old Cust# into new Cust#?"
            view-as alert-box question button yes-no update v-process.

    if not v-process then return no-apply.
  end.

  v-process  = no.

  message "Are you sure you want change customer number" trim(caps(begin_cust))
          "to" trim(caps(end_cust)) + "?"       
          view-as alert-box question button yes-no update v-process.

  if v-process then run run-process.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME end_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL end_cust C-Win
ON LEAVE OF end_cust IN FRAME FRAME-A /* New Customer# */
DO:
  assign {&self-name}.

  {&self-name}:screen-value = caps({&self-name}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3helpw.i}
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

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
     RETURN .
  END.

  RUN enable_UI.
  apply "entry" to begin_cust.
  {methods/nowait.i}
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
  DISPLAY begin_cust end_cust 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE RECT-17 begin_cust end_cust btn-process btn-cancel 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW FRAME FRAME-B IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
/* ------------------------------------------------ util/updcust#.p 05/01 JLF */
/*  Update Customer with a new cust-no                                        */
/* -------------------------------------------------------------------------- */

def buffer b-cust for cust.
def buffer b-ship for shipto.
def buffer b-sold for soldto.
DEF BUFFER b-sold2 FOR soldto.

def var v-cust      like cust.cust-no.
def var v-new-cust  like cust.cust-no.
def var v-char      as   char.
DEF VAR old-rec-key AS CHAR NO-UNDO.

DEF BUFFER bf-cust FOR cust.  
DEF BUFFER bf-cust-new FOR cust. 

assign
 v-cust     = begin_cust
 v-new-cust = end_cust.

session:set-wait-state("General").

find first cust no-lock
    where cust.company eq cocode
      and cust.cust-no eq v-cust
    no-error.

for each ar-cash
    where ar-cash.company eq cocode
      and ar-cash.cust-no eq v-cust
    use-index ar-cash

    transaction:

  DISPLAY ar-cash.check-no FORMAT "9999999999" WITH DOWN.

  for each ar-cashl where ar-cashl.c-no eq ar-cash.c-no use-index c-no:

    ar-cashl.cust-no = v-new-cust.
  end.

  ar-cash.cust-no = v-new-cust.
end.

for each ar-inv
    where ar-inv.company eq cocode
      and ar-inv.cust-no eq v-cust
    use-index ar-inv

    transaction:

  DISPLAY ar-inv.inv-no WITH DOWN.

  for each ar-invl where ar-invl.x-no eq ar-inv.x-no use-index x-no:

    ar-invl.cust-no = v-new-cust.
  end.

  ar-inv.cust-no = v-new-cust.
end.

for each ar-ledger
    where ar-ledger.company eq cocode
      and ar-ledger.cust-no eq v-cust
    use-index ar-ledger

    transaction:

  DISPLAY ar-ledger.ref-num WITH DOWN.

  ar-ledger.cust-no = v-new-cust.
end.

for each job-set
    where job-set.company eq cocode
      and job-set.cust-no eq v-cust
    transaction:

  DISPLAY job-set.job-no + "-" + STRING(job-set.job-no2,"99") WITH DOWN.

  job-set.cust-no = v-new-cust.
end.

for each wiptag
    where wiptag.company eq cocode
      and wiptag.cust-no eq v-cust
    transaction:

  DISPLAY wiptag.tag-no WITH DOWN.
  wiptag.cust-no = v-new-cust.
end.

for each quoteitm
    where quoteitm.company eq cocode
      and quoteitm.cust-no eq v-cust
    transaction:

  DISPLAY quoteitm.q-no WITH DOWN.
  quoteitm.cust-no = v-new-cust.
end.


v-char = "".

find first eb
    where eb.company eq cocode
    use-index cust no-lock no-error.

do while avail eb:
  v-char = eb.loc.

  for each eb
      where eb.company eq cocode
        and eb.loc     eq v-char
        and eb.cust-no eq v-cust
      use-index cust

      transaction:

    DISPLAY eb.est-no FORMAT "x(10)" WITH DOWN.

    for each probeit
        where probeit.company eq cocode
          and probeit.est-no  eq eb.est-no
          and probeit.cust-no eq v-cust:

      probeit.cust-no = v-new-cust.
    end.

    for each quotehd
        where quotehd.company eq cocode
          and quotehd.est-no  eq eb.est-no
          and quotehd.cust-no eq v-cust
        use-index cust2:

      quotehd.cust-no = v-new-cust.

      IF AVAIL cust THEN
        ASSIGN
         quotehd.billto[1] = cust.name
         quotehd.billto[2] = cust.addr[1]
         quotehd.billto[3] = cust.addr[2]
         quotehd.billto[4] = cust.city + ", " + cust.state + "  " + cust.zip.
    end.

    eb.cust-no = v-new-cust.
  end.

  RELEASE eb.

  find first eb
      where eb.company eq cocode
        and eb.loc     gt v-char
      use-index cust no-lock no-error.
end.

for each EDIVTran
    where EDIVTran.company eq cocode
      and EDIVTran.cust    eq v-cust

    transaction:

  EDIVTran.cust = v-new-cust.
end.

for each EDMast where EDMast.cust eq v-cust

    transaction:

  EDMast.cust = v-new-cust.
end.

for each EDPD where EDPD.cust eq v-cust

    transaction:

   EDPD.cust = v-new-cust.
end.

for each EDPOTran where EDPOTran.cust eq v-cust

    transaction:

  EDPOTran.cust = v-new-cust.
end.

for each EDShipto where EDShipto.cust eq v-cust
    use-index ByCustShip

    transaction:

  EDShipto.cust = v-new-cust.
end.

for each fg-rcpts
    where fg-rcpts.company eq cocode
      and fg-rcpts.cust-no eq v-cust
    use-index cust-no

    transaction:

  DISPLAY fg-rcpts.r-no LABEL "FG Rcpt Seq#" WITH DOWN.

  fg-rcpts.cust-no = v-new-cust.
end.

for each inv-head
    where inv-head.company eq cocode
      and inv-head.cust-no eq v-cust
    use-index cust

    transaction:

  DISPLAY inv-head.inv-no
          inv-head.r-no LABEL "Seq#" WITH DOWN.

  for each inv-line  where inv-line.r-no eq inv-head.r-no use-index r-no:

    inv-line.cust-no = v-new-cust.
  end.

  inv-head.cust-no = v-new-cust.
end.

for each itemfg
    where itemfg.company eq cocode
      and itemfg.cust-no eq v-cust
    use-index customer

    transaction:

  DISPLAY itemfg.i-no WITH DOWN.

  for each itemfgdtl
      where itemfgdtl.company eq cocode
        and itemfgdtl.i-no    eq itemfg.i-no
      use-index pi-itemfgdtl:

    itemfgdtl.cust-no = v-new-cust.
  end.

  itemfg.cust-no = v-new-cust.
end.

for each job-hdr
    where job-hdr.company eq cocode
      and job-hdr.cust-no eq v-cust
    use-index cust-idx

    transaction:

  DISPLAY job-hdr.job-no job-hdr.job-no2 WITH DOWN.

  for each fg-act
      where fg-act.company eq cocode
        and fg-act.job     eq job-hdr.job
        and fg-act.job-no  eq job-hdr.job-no
        and fg-act.job-no2 eq job-hdr.job-no2
        and fg-act.cust-no eq v-cust
      use-index job-idx:

    fg-act.cust-no = v-new-cust.
  end.

  for each fg-hist
      where fg-hist.company eq cocode
        and fg-hist.i-no    eq job-hdr.i-no
        and fg-hist.job-no  eq job-hdr.job-no
        and fg-hist.cust-no eq v-cust
      use-index ino:

    fg-hist.cust-no = v-new-cust.
  end.

  job-hdr.cust-no = v-new-cust.
end.

for each cust-markup
    where cust-markup.company eq cocode
      and cust-markup.cust-no eq v-cust
    use-index cust-style

    transaction:

  DISPLAY cust-markup.style cust-markup.procat WITH DOWN.

  cust-markup.cust-no = v-new-cust.
end.

for each oe-bolh
    where oe-bolh.company eq cocode
      and oe-bolh.cust-no eq v-cust
    use-index cust

    transaction:

  DISPLAY oe-bolh.bol-no WITH DOWN.

  for each oe-ship
      where oe-ship.company eq cocode
        and oe-ship.bol-no  eq string(oe-bolh.bol-no,"99999999")
        and oe-ship.cust-no eq v-cust
      use-index pi-oe-ship:

    oe-ship.cust-no = v-new-cust.
  end.

  for each oe-boll
      where oe-boll.company eq cocode
        and oe-boll.b-no    eq oe-bolh.b-no:

    oe-boll.cust-no = v-new-cust.
  end.

  oe-bolh.cust-no = v-new-cust.
end.

DISABLE TRIGGERS FOR LOAD OF oe-ord.
DISABLE TRIGGERS FOR LOAD OF oe-ordl.
DISABLE TRIGGERS FOR LOAD OF oe-rel.

for each oe-ord
    where oe-ord.company eq cocode
      and oe-ord.cust-no eq v-cust
    transaction:

    FIND FIRST bf-cust WHERE
          bf-cust.company EQ oe-ord.company AND
          bf-cust.cust-no EQ v-cust
          NO-LOCK NO-ERROR.

    old-rec-key = bf-cust.rec_key .

    DISPLAY oe-ord.ord-no WITH DOWN.

    for each oe-ordl
        where oe-ordl.company eq cocode
          and oe-ordl.ord-no  eq oe-ord.ord-no:

      oe-ordl.cust-no = v-new-cust.
    end.

    for each oe-rel
        where oe-rel.company eq cocode
          and oe-rel.ord-no  eq oe-ord.ord-no:

      oe-rel.cust-no = v-new-cust.
    end.

    oe-ord.cust-no = v-new-cust.

    FIND FIRST bf-cust-new WHERE
            bf-cust-new.company EQ oe-ord.company AND
            bf-cust-new.cust-no EQ v-new-cust
            NO-LOCK NO-ERROR.

       IF AVAIL bf-cust-new THEN
         FOR EACH attach WHERE
           attach.company = oe-ord.company and
           attach.rec_key = old-rec-key EXCLUSIVE-LOCK:

           attach.rec_key = bf-cust-new.rec_key .
         END.

    find first b-cust
        where b-cust.company eq cocode
          and b-cust.cust-no eq v-new-cust
        no-lock no-error.

    IF AVAIL b-cust THEN
    DO:
       ASSIGN
          oe-ord.cust-name = cust.name
          oe-ord.addr[1]   = cust.addr[1]
          oe-ord.addr[2]   = cust.addr[2]
          oe-ord.city      = cust.city
          oe-ord.state     = cust.state
          oe-ord.zip       = cust.zip.
       RELEASE b-cust.
    END.

end.

FIND FIRST bf-cust-new WHERE
    bf-cust-new.company EQ cocode AND
    bf-cust-new.cust-no EQ v-new-cust
    NO-LOCK NO-ERROR.

 IF AVAIL bf-cust-new THEN do:
     FIND CURRENT cust NO-ERROR.
     FOR EACH attach WHERE
         attach.company = cocode and
         attach.rec_key = cust.rec_key EXCLUSIVE-LOCK:
         attach.rec_key = bf-cust-new.rec_key .
     END.

     FOR EACH phone WHERE 
         phone.table_rec_key = cust.rec_key EXCLUSIVE-LOCK :
         phone.table_rec_key =  bf-cust-new.rec_key .
         IF NOT CAN-FIND (FIRST reftable NO-LOCK
                          WHERE reftable.rec_key = phone.table_rec_key
                          AND reftable.CODE    = STRING (phone.rec_key)) 
         THEN DO:
                CREATE reftable.
                ASSIGN reftable.rec_key   = STRING (phone.table_rec_key)
                       reftable.CODE      = STRING (phone.rec_key).
         END. /* not avail reftable */
     END.
 END.

for each oe-relh
    where oe-relh.company eq cocode
      and oe-relh.cust-no eq v-cust

    transaction:

  DISPLAY oe-relh.release# WITH DOWN.

  for each oe-rell
      where oe-rell.company eq cocode
        and oe-rell.r-no    eq oe-relh.r-no
      USE-INDEX r-no:

    oe-rell.cust-no = v-new-cust.
  end.

  oe-relh.cust-no = v-new-cust.
end.

v-char = "".

find first oe-prmtx
    where oe-prmtx.company eq cocode
    use-index custitem no-lock no-error.

do while avail oe-prmtx:
  v-char = oe-prmtx.custype.

  for each oe-prmtx
      where oe-prmtx.company eq cocode
        and oe-prmtx.custype eq v-char
        and oe-prmtx.cust-no eq v-cust
      use-index custitem

      transaction:

    oe-prmtx.cust-no = v-new-cust.
  end.

  RELEASE oe-prmtx.

  find first oe-prmtx
      where oe-prmtx.company eq cocode
        and oe-prmtx.custype gt v-char
      use-index custitem no-lock no-error.
end.

do i = 1 to 4:
  for each oe-reth
      where oe-reth.company eq cocode
        and oe-reth.posted  eq (i ge 3)
        and oe-reth.applied eq (i modulo 2 eq 0)
        and oe-reth.cust-no eq v-cust
      use-index posted

      transaction:

    oe-reth.cust-no = v-new-cust.
  end.
end.

for each pdh where pdh.cust eq v-cust

    transaction:

  pdh.cust = v-new-cust.
end.

for each po-ord
    where po-ord.company eq cocode
      and po-ord.cust-no eq v-cust

    transaction:

  DISPLAY po-ord.po-no WITH DOWN.

  for each po-ordl
      where po-ordl.company eq cocode
        and po-ordl.po-no   eq po-ord.po-no
        and po-ordl.cust-no eq v-cust
      use-index cust-no:

    po-ordl.cust-no = v-new-cust.
  end.

  po-ord.cust-no = v-new-cust.
end.

for each po-ordl
    where po-ordl.company eq cocode
      and po-ordl.cust-no eq v-cust
    use-index cust

    transaction:

  po-ordl.cust-no = v-new-cust.
end.

for each shipto 
    where shipto.company eq cocode
      and shipto.cust-no eq v-cust
    use-index ship-id:

  DISPLAY shipto.ship-id WITH DOWN.

  find last b-ship
      where b-ship.company eq cocode
        and b-ship.cust-no eq v-new-cust
      use-index ship-no no-lock no-error.

  i = (if avail b-ship then b-ship.ship-no else 0) + 1.

  find first b-ship
      where b-ship.company eq cocode
        and b-ship.cust-no eq v-new-cust
        and b-ship.ship-id eq (if shipto.ship-id eq v-cust then v-new-cust
                               else shipto.ship-id)
      use-index ship-id no-lock no-error.

  find first eb
      where eb.company eq cocode
      use-index cust no-lock no-error.

  do while avail eb:
    v-char = eb.loc.

    for each eb
        where eb.company eq cocode
          and eb.loc     eq v-char
          and eb.cust-no eq v-new-cust
          and eb.ship-id eq shipto.ship-id
        use-index cust

        transaction:

      if avail b-ship then eb.ship-id = trim(string(i,">>>>>>>9")).
      else
      if shipto.ship-id eq v-cust then eb.ship-id = v-new-cust.

      ASSIGN
       eb.ship-no      = i
       eb.ship-name    = shipto.ship-name
       eb.ship-addr[1] = shipto.ship-addr[1]
       eb.ship-addr[2] = shipto.ship-addr[2]
       eb.ship-city    = shipto.ship-city
       eb.ship-state   = shipto.ship-state
       eb.ship-zip     = shipto.ship-zip.
    end.

    RELEASE eb.

    find first eb
        where eb.company eq cocode
          and eb.loc     gt v-char
        use-index cust no-lock no-error.
  end.

  for each quotehd
      where quotehd.company eq cocode
        and quotehd.cust-no eq v-new-cust
        and quotehd.ship-id eq shipto.ship-id

      transaction:

    if avail b-ship then quotehd.ship-id = trim(string(i,">>>>>>>9")).
    else
    if shipto.ship-id eq v-cust then quotehd.ship-id = v-new-cust.

    ASSIGN
     quotehd.shipto[1] = shipto.ship-name
     quotehd.shipto[2] = shipto.ship-addr[1]
     quotehd.shipto[3] = shipto.ship-addr[2]
     quotehd.shipto[4] = shipto.ship-city + ", " + shipto.ship-state +
                         "  " + shipto.ship-zip.
  end.

  for each oe-rel
      where oe-rel.company eq cocode
        and oe-rel.cust-no eq v-new-cust
        and oe-rel.ship-id eq shipto.ship-id

      transaction:

    if avail b-ship then oe-rel.ship-id = trim(string(i,">>>>>>>9")).
    else
    if shipto.ship-id eq v-cust then oe-rel.ship-id = v-new-cust.

    oe-rel.ship-no = i.
  end.

  for each oe-relh
      where oe-relh.company eq cocode
        /*and oe-relh.posted  eq no*/
        and oe-relh.cust-no eq v-new-cust
        and oe-relh.ship-id eq shipto.ship-id
      use-index post
      transaction:

    if avail b-ship then oe-relh.ship-id = trim(string(i,">>>>>>>9")).
    else
    if shipto.ship-id eq v-cust then oe-relh.ship-id = v-new-cust.

    oe-relh.ship-no = i.
  end.

  for each oe-bolh
      where oe-bolh.company eq cocode
        /*and oe-bolh.posted  eq no*/
        and oe-bolh.cust-no eq v-new-cust
        and oe-bolh.ship-id eq shipto.ship-id
      use-index post

      transaction:

    if avail b-ship then oe-bolh.ship-id = trim(string(i,">>>>>>>9")).
    else
    if shipto.ship-id eq v-cust then oe-bolh.ship-id = v-new-cust.

    oe-bolh.ship-no = i.
  end.

  for each ar-inv
      where ar-inv.company eq cocode
        /*and ar-inv.posted  eq no*/ /* ticket - 24071*/ 
        and ar-inv.cust-no eq v-new-cust
        and ar-inv.ship-id eq shipto.ship-id
      use-index posted

      transaction:

    if avail b-ship then ar-inv.ship-id = trim(string(i,">>>>>>>9")).
    else
    if shipto.ship-id eq v-cust then ar-inv.ship-id = v-new-cust.
  end.

  for each oe-ord
      where oe-ord.company eq cocode
        and oe-ord.cust-no eq v-new-cust
        and oe-ord.ship-id eq shipto.ship-id
      
      transaction:
   
     if avail b-ship then oe-ord.ship-id = trim(string(i,">>>>>>>9")).
     else
     if shipto.ship-id eq v-cust then oe-ord.ship-id = v-new-cust.
                                      
  end.

  do transaction:
    if avail b-ship then shipto.ship-id = trim(string(i,">>>>>>>9")).
    else
    if shipto.ship-id eq v-cust then shipto.ship-id = v-new-cust.

    assign
     shipto.cust-no = v-new-cust
     shipto.ship-no = i.
  end.
end.

for each soldto 
    where soldto.company eq cocode
      and soldto.cust-no eq v-cust
    use-index sold-id:

  DISPLAY soldto.sold-id WITH DOWN.

  find last b-sold
      where b-sold.company eq cocode
        and b-sold.cust-no eq v-new-cust
      use-index sold-no no-lock no-error.

  i = (if avail b-sold then b-sold.sold-no else 0) + 1.

  find first b-sold
      where b-sold.company eq cocode
        and b-sold.cust-no eq v-new-cust
        and b-sold.sold-id eq (if soldto.sold-id eq v-cust then v-new-cust
                               else soldto.sold-id)
      use-index sold-id no-lock no-error.

  for each quotehd
      where quotehd.company eq cocode
        and quotehd.cust-no eq v-new-cust
        and quotehd.sold-id eq soldto.sold-id

      transaction:

    if avail b-sold then quotehd.sold-id = trim(string(i,">>>>>>>9")).
    else
    if soldto.sold-id eq v-cust then quotehd.sold-id = v-new-cust.

    ASSIGN
     quotehd.soldto[1] = soldto.sold-name
     quotehd.soldto[2] = soldto.sold-addr[1]
     quotehd.soldto[3] = soldto.sold-addr[2]
     quotehd.soldto[4] = soldto.sold-city + ", " + soldto.sold-state +
                         "  " + soldto.sold-zip.
  end.

  for each oe-ord
      where oe-ord.company eq cocode
        and oe-ord.cust-no eq v-new-cust
        and oe-ord.sold-id eq soldto.sold-id

      transaction:

    if avail b-sold then
    DO:
       oe-ord.sold-id = trim(string(i,">>>>>>>9")).

       FOR EACH b-sold2 NO-LOCK
          WHERE b-sold2.company EQ oe-ord.company
            AND b-sold2.cust-no EQ v-new-cust
            AND b-sold2.sold-id EQ oe-ord.sold-id:

           ASSIGN
              oe-ord.sold-name    = b-sold2.sold-name
              oe-ord.sold-addr[1] = b-sold2.sold-addr[1]
              oe-ord.sold-addr[2] = b-sold2.sold-addr[2]
              oe-ord.sold-city    = b-sold2.sold-city
              oe-ord.sold-state   = b-sold2.sold-state
              oe-ord.sold-zip     = b-sold2.sold-zip.
       END.

    END.
    else
    if soldto.sold-id eq v-cust then
    DO:
       oe-ord.sold-id = v-new-cust.

       FOR EACH b-sold2 NO-LOCK
          WHERE b-sold2.company EQ oe-ord.company
            AND b-sold2.cust-no EQ v-new-cust
            AND b-sold2.sold-id EQ oe-ord.sold-id:

           ASSIGN
              oe-ord.sold-name    = b-sold2.sold-name
              oe-ord.sold-addr[1] = b-sold2.sold-addr[1]
              oe-ord.sold-addr[2] = b-sold2.sold-addr[2]
              oe-ord.sold-city    = b-sold2.sold-city
              oe-ord.sold-state   = b-sold2.sold-state
              oe-ord.sold-zip     = b-sold2.sold-zip.
       END.

    END.

    oe-ord.sold-no = i.
  end.

  for each oe-rel
      where oe-rel.company eq cocode
        and oe-rel.cust-no eq v-new-cust
        and oe-rel.sold-no eq soldto.sold-no

      transaction:

    oe-rel.sold-no = i.
  end.

  for each oe-bolh
      where oe-bolh.company eq cocode
        /*and oe-bolh.posted  eq no*/
        and oe-bolh.cust-no eq v-new-cust
        and oe-bolh.sold-id eq soldto.sold-id
      use-index post

      transaction:

    if avail b-sold then oe-bolh.sold-id = trim(string(i,">>>>>>>9")).
    else
    if soldto.sold-id eq v-cust then oe-bolh.sold-id = v-new-cust.

    oe-bolh.sold-no = i.
  end.

  for each inv-head
      where inv-head.company eq cocode
        and inv-head.cust-no eq v-new-cust
        and inv-head.sold-no eq soldto.sold-id

      transaction:

    if avail b-sold then inv-head.sold-no = trim(string(i,">>>>>>>9")).
    else
    if soldto.sold-id eq v-cust then inv-head.sold-no = v-new-cust.
  end.

  for each ar-inv
      where ar-inv.company eq cocode
        /*and ar-inv.posted  eq no*/ /* ticket 24071*/
        and ar-inv.cust-no eq v-new-cust
        and ar-inv.sold-id eq soldto.sold-id
      use-index posted

      transaction:

    if avail b-sold then ar-inv.sold-id = trim(string(i,">>>>>>>9")).
    else
    if soldto.sold-id eq v-cust then ar-inv.sold-id = v-new-cust.

    ar-inv.sold-no = i.
  end.

  do transaction:
    if avail b-sold then soldto.sold-id = trim(string(i,">>>>>>>9")).
    else
    if soldto.sold-id eq v-cust then soldto.sold-id = v-new-cust.

    assign
     soldto.cust-no = v-new-cust
     soldto.sold-no = i.
  end.
end.

do transaction:
  FIND CURRENT cust NO-ERROR.

  if avail cust then do:
    find first b-cust
        where b-cust.company eq cocode
          and b-cust.cust-no eq v-new-cust
        no-lock no-error.                   
    if avail b-cust then do:
      for each notes where notes.rec_key eq cust.rec_key:
        notes.rec_key = b-cust.rec_key.
      end.
      delete cust.
      IF b-cust.ACTIVE EQ "" THEN do:
          FIND CURRENT b-cust EXCLUSIVE-LOCK NO-ERROR.
          ASSIGN b-cust.ACTIVE = "A" .
      END.
    end. /* avail b-cust*/
    ELSE do:
         cust.cust-no = v-new-cust.
         IF cust.ACTIVE EQ "" THEN
          ASSIGN cust.ACTIVE = "A" .
    END. /* else do */
  end.
end.

session:set-wait-state("").

message trim(c-win:title) + " Process Complete..." view-as alert-box.

apply "close" to this-procedure.

/* end ---------------------------------- copr. 2001  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

