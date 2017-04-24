/*------------------------------------------------------------------------------
  Purpose:     
  meters:  <none>
  Notes:   not used anywhere else but oerep/r-BOLtag.w in procedure run-report
           moved here b/c of size in appBuilder section editor
------------------------------------------------------------------------------ */

DEF BUFFER b-itemfg FOR itemfg.
DEF BUFFER b-w-ord FOR w-ord.

DEF VAR lv-ord-no LIKE oe-ord.ord-no NO-UNDO.

DEF VAR ll AS LOG NO-UNDO.
DEF VAR li AS INT NO-UNDO.

DEF VAR v-b-word-created AS LOG NO-UNDO.
DEF VAR lv-message AS cha NO-UNDO.
DEF VAR op-warning AS LOG NO-UNDO.
DEF VAR var-display-warning AS LOG NO-UNDO.

DEF VAR v-icnt AS INT NO-UNDO.

DEF VAR v-ordlist AS CHAR NO-UNDO.

SESSION:SET-WAIT-STATE ("general").

ASSIGN
  v-fbol-no[1]   = begin_bolno
  v-fbol-no[2]   = end_bolno  
  form#          = begin_form
  copy_count     = begin_labels
  form_fid       = begin_filename
  v-ordlist      = "".
  
EMPTY TEMP-TABLE w-ord.
EMPTY TEMP-TABLE tt-tag.
EMPTY TEMP-TABLE w-file.
EMPTY TEMP-TABLE ttblJob.  
EMPTY TEMP-TABLE w-file.

ASSIGN v-bol-list = TRIM(v-bol-list).

IF v-bol-list <> "" AND 
  (ASC(SUBSTR(v-bol-list,LENGTH(v-bol-list),1)) = 10 OR
   ASC(SUBSTR(v-bol-list,LENGTH(v-bol-list),1)) = 13 )
  THEN v-bol-list = substring(v-bol-list,1,LENGTH(v-bol-list) - 1).

IF v-bol-list NE "" THEN DO:

  DO i = 1 TO NUM-ENTRIES(v-bol-list):
      
    IF ENTRY(i,v-bol-list) EQ "" THEN NEXT.

    FOR EACH oe-boll NO-LOCK
      WHERE oe-boll.company EQ cocode
        AND oe-boll.bol-no  EQ INT(ENTRY(i,v-bol-list))
       BREAK BY oe-boll.ord-no:
     
       IF FIRST-OF(oe-boll.ord-no) THEN DO:
         FIND FIRST oe-ord NO-LOCK
           WHERE oe-ord.company EQ oe-boll.company
             AND oe-ord.ord-no EQ oe-boll.ord-no NO-ERROR.
         IF AVAIL oe-ord 
           THEN ASSIGN v-ordlist = v-ordlist  +  "," +  STRING(oe-ord.ord-no).
         
       END.
    END. /* FOR EACH */

    DO v-icnt = 1 TO NUM-ENTRIES(v-ordlist).
     
     ASSIGN lv-ord-no = INT(ENTRY(v-icnt,v-ordlist)) NO-ERROR.

     IF NOT ERROR-STATUS:ERROR AND 
        lv-ord-no NE 0 
       THEN RUN temp-ord (lv-ord-no,
                          INT(ENTRY(i,v-bol-list)),
                          INT(ENTRY(i,v-bol-list))
                          ).
    END.

  END.
  
END. /* v-bol-list */
ELSE
IF v-fbol-no[1] NE 0 OR 
   v-fbol-no[2] NE 0 
  THEN DO:
   FOR EACH oe-boll NO-LOCK
     WHERE oe-boll.company EQ cocode
       AND oe-boll.bol-no  GE v-fbol-no[1]
       AND oe-boll.bol-no  LE v-fbol-no[2]
      BREAK BY oe-boll.ord-no:
     
      IF FIRST-OF(oe-boll.ord-no) THEN DO:

        FIND FIRST oe-ord NO-LOCK
          WHERE oe-ord.company EQ oe-boll.company
            AND oe-ord.ord-no EQ oe-boll.ord-no NO-ERROR.
        IF AVAIL oe-ord 
         THEN ASSIGN v-ordlist = v-ordlist  +  "," +  STRING(oe-ord.ord-no).     
      END. /* LAST-OF */
   END. /* FOR EACH */

   DO i = 1 TO NUM-ENTRIES(v-ordlist).
     
     ASSIGN lv-ord-no = INT(ENTRY(i,v-ordlist)) NO-ERROR.

     IF NOT ERROR-STATUS:ERROR AND 
        lv-ord-no NE 0 
       THEN RUN temp-ord (lv-ord-no,
                          v-fbol-no[1],
                          v-fbol-no[2]
                          ).
   END.

END. /* IF v-fbol-no */

FOR EACH w-file:
   
   ASSIGN 
     v-fbol-no[1] = w-file.w-fbol
     v-fbol-no[2] = w-file.w-tbol.   

  RUN from-ord (w-file.w-key).

END. /* FOR EACH w-file */

FOR EACH w-ord,
 FIRST itemfg NO-LOCK
  WHERE itemfg.company EQ cocode
    AND itemfg.i-no    EQ w-ord.i-no
    AND itemfg.isaset  EQ YES
  BY w-ord.bol-no:

  IF w-ord.dont-run-set EQ NO THEN DO:
     
     RUN fg/fullset.p (ROWID(itemfg)).

     ASSIGN v-b-word-created = NO.

     FOR EACH tt-fg-set NO-LOCK
       WHERE tt-fg-set.part-no <> w-ord.i-no,
      FIRST b-itemfg NO-LOCK
       WHERE b-itemfg.company EQ cocode
         AND b-itemfg.i-no    EQ tt-fg-set.part-no:
         

         CREATE b-w-ord.
         BUFFER-COPY w-ord TO b-w-ord
         ASSIGN
           b-w-ord.i-no    = tt-fg-set.part-no
           b-w-ord.i-name  = b-itemfg.i-name
           b-w-ord.ord-qty = w-ord.ord-qty * tt-fg-set.part-qty-dec
           b-w-ord.box-len = b-itemfg.l-score[50]
           b-w-ord.box-wid = b-itemfg.w-score[50]
           b-w-ord.box-dep = b-itemfg.d-score[50].

         ASSIGN v-b-word-created = YES.

         FIND FIRST est NO-LOCK
           WHERE est.company EQ cocode
             AND est.est-no  EQ w-ord.est-no NO-ERROR.

         RELEASE eb.
         
         IF AVAIL est 
           THEN
            FIND FIRST eb NO-LOCK 
              WHERE eb.company EQ cocode
                AND eb.est-no EQ w-ord.est-no                       
                AND eb.stock-no EQ tt-fg-set.part-no NO-ERROR.
            IF AVAIL eb 
              THEN
               ASSIGN
                 b-w-ord.flute      = eb.flute
                 b-w-ord.test       = eb.test
                 b-w-ord.pcs        = eb.cas-cnt
                 b-w-ord.bundle     = eb.cas-pal
                 b-w-ord.total-unit = b-w-ord.pcs * b-w-ord.bundle
                 b-w-ord.cas-no     = eb.cas-no.

         ASSIGN b-w-ord.total-tags = ((b-w-ord.ord-qty / 
                                       b-w-ord.total-unit) + 
                                       .49) +
                                       (IF LOOKUP(v-loadtag,"SSLABEL,CentBox") 
                                        > 0  THEN 0 ELSE 1) .
     END.  /* each tt-fg-set */

     IF v-b-word-created AND itemfg.alloc THEN DELETE w-ord.

  END.   /* if dont-run-set */
END.  /* each w-ord */

ASSIGN
  str-tit  = coname + " - " + loname
  str-tit2 = "DOWNLOAD LOADTAG DATA"
  x        = (56 - LENGTH(str-tit)) / 2 
  str-tit  = FILL(" ",x) + str-tit
  x        = (56 - LENGTH(str-tit2)) / 2
  str-tit2 = FILL(" ",x) + str-tit2.

RUN final-update.
RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

SESSION:SET-WAIT-STATE ("").

FIND FIRST w-ord NO-LOCK NO-ERROR.
IF NOT AVAIL w-ord
 THEN ASSIGN var-display-warning = TRUE.

IF var-display-warning 
  THEN
   MESSAGE 
      "Job does not contain an order number, hence data such as " + 
      "PO# will not print."
    VIEW-AS ALERT-BOX WARNING BUTTONS OK.

ASSIGN choice = NO.

RUN oerep/d-BOLtg.w .


FIND FIRST w-ord  NO-ERROR.

ASSIGN lv-message = "Are you Sure you Want to Create BOLtag File? ".
IF AVAIL w-ord AND
  choice
  THEN
   MESSAGE 
     lv-message
    VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO 
    UPDATE choice.
   IF NOT choice THEN RETURN ERROR.


SESSION:SET-WAIT-STATE ("general").

{sys/inc/print1.i}
{sys/inc/outprint.i value(lines-per-page)} 


VIEW FRAME r-top.
VIEW FRAME top.

IF v-out = "" 
  THEN v-out = "c:~\BOL~\label~\BOLtag.txt".
  ELSE DO:
    IF SUBSTR(v-out,LENGTH(v-out),1) = "/" OR
       SUBSTR(v-out,LENGTH(v-out),1) = "\" 
      THEN .
      ELSE ASSIGN v-out = v-out + "\".
        
      ASSIGN v-out = v-out + "BOLtag.txt".
  END.

IF choice THEN DO:

   ASSIGN begin_filename = v-out
          begin_filename:SCREEN-VALUE = begin_filename.


   DISPLAY begin_filename:SCREEN-VALUE.

  IF OPSYS eq "UNIX" and v-loadtag ne "TRIAD" 
    THEN DO:

     MESSAGE 
       "Unable to Create Loadtag File for Non MSDos Platform.".
       PAUSE.
     RETURN.
  END.


  RUN create-text-file.

END.

SESSION:SET-WAIT-STATE ("").
