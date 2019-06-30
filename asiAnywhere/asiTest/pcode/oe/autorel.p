/* --------------------------------------------------- oe/autorel.p 11/96 JLF */
/* order entry - AUTO RELEASE ALL ORDER LINES                                 */
/* -------------------------------------------------------------------------- */
def new shared var relh-recid as recid no-undo.

{sys/inc/var.i shared}

def shared buffer xoe-ord  for oe-ord.
def shared buffer xoe-ordl for oe-ordl.

def shared var v-auto as log no-undo.

def var v-auto-def as log no-undo.
def var v-stat as char no-undo.
def var v-first as log no-undo.
def var v-ship like oe-rel.ship-id no-undo.
def var v-date like oe-rel.rel-date init ? no-undo.
def shared var fil_id as recid no-undo.
def var choice as log no-undo.

DEF WORKFILE w-rel FIELD w-rowid AS ROWID.
PROCEDURE mail EXTERNAL "xpMail.dll" :
   DEF INPUT PARAM mailTo AS CHAR.
   DEF INPUT PARAM mailsubject AS CHAR.
   DEF INPUT PARAM mailText AS CHAR.
   DEF INPUT PARAM mailFiles AS CHAR.
   DEF INPUT PARAM mailDialog AS LONG.
   DEF OUTPUT PARAM retCode AS LONG.
END.
{oe/chkordl.i NEW}
{oe/relemail.i NEW}

find first oe-ctrl where oe-ctrl.company eq cocode no-lock.

find xoe-ordl where recid(xoe-ordl) eq fil_id no-lock no-error.
if not avail xoe-ordl then return.

find xoe-ord where xoe-ord.company = cocode and xoe-ord.ord-no = xoe-ordl.ord-no
                no-lock no-error.
find first eb where eb.company = xoe-ordl.company
                and eb.est-no eq xoe-ordl.est-no no-lock no-error.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "AUTOREL"
    no-lock no-error.
if not avail sys-ctrl then do transaction:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name = "AUTOREL"
   sys-ctrl.descrip = "Auto Release Default"
   sys-ctrl.log-fld = no.
  MESSAGE sys-ctrl.descrip
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE sys-ctrl.log-fld.
end.
assign
 v-auto     = sys-ctrl.log-fld
 v-auto-def = sys-ctrl.int-fld eq 1.

if v-auto then do on endkey undo, return:
  FOR EACH oe-rel
      WHERE oe-rel.company EQ xoe-ord.company
        AND oe-rel.ord-no  EQ xoe-ord.ord-no
        AND oe-rel.link-no EQ 0
        AND oe-rel.qty     gt 0
      NO-LOCK,
      FIRST xoe-ordl OF oe-rel NO-LOCK:
      
    RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT v-stat).
       
    if index("AB",v-stat) eq 0 then do:
      choice = yes.
      leave.
    end.
    
    release oe-rel.
  end.
  /*      
  v-auto = v-auto-def.
  if avail oe-rel then  message "Release all items on order?" view-as alert-box question
                    button yes-no update v-auto.
  else v-auto = no.
  */
end.

if v-auto and xoe-ord.stat eq "H" and oe-ctrl.p-pick eq no then do:
   message "Can not release items for customers on Credit Hold." view-as alert-box
           error.
   v-auto = no.
end.

if v-auto then do:
  FOR EACH w-rel:
    DELETE w-rel.
  END.

  FOR EACH oe-rel
      WHERE oe-rel.company EQ xoe-ord.company
        AND oe-rel.ord-no  EQ xoe-ord.ord-no
        AND oe-rel.link-no EQ 0
        AND oe-rel.qty     GT 0
      NO-LOCK,
      first xoe-ordl of oe-rel no-lock
      break by oe-rel.rel-date
            by oe-rel.ship-id:

    if first-of(oe-rel.ship-id) then v-first = yes.

    RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT v-stat).

    IF INDEX("AB",v-stat) EQ 0 THEN DO:
      IF v-first THEN DO:
        ASSIGN
         v-first = NO
         choice  = YES.
        MESSAGE "Create Release for Date-" + TRIM(STRING(oe-rel.rel-date)) +
               " and ShipID-" +  TRIM(oe-rel.ship-id) + " ?"
              VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE choice.
      END.

      IF choice THEN DO:
        CREATE w-rel.
        w-rowid = ROWID(oe-rel).
      END.
    END.

    /*if index("AB",v-stat) gt 0                      or
       (oe-rel.rel-date eq v-date and v-date ne ?   and
        oe-rel.ship-id  eq v-ship and v-ship ne "") then choice = no.

    else
    if v-first then
    do on endkey undo, retry:
       assign v-first = no
              choice  = yes
              v-date  = ?
              v-ship  = "".
       message "Create Release for Date-" + trim(string(oe-rel.rel-date)) +
               " and ShipID-" +  trim(oe-rel.ship-id) + " ?"
              view-as alert-box question button yes-no update choice.
    end.

    if choice then do:
      if avail eb and oe-rel.ship-id eq "" then
        assign
         oe-rel.ship-id      = eb.ship-id
         oe-rel.ship-addr[1] = eb.ship-addr[1]
         oe-rel.ship-addr[2] = eb.ship-addr[2]
         oe-rel.ship-city    = eb.ship-city
         oe-rel.ship-state   = eb.ship-state
         oe-rel.ship-zip     = eb.ship-zip.
         
      run oe/actrel.p (recid(oe-rel)).
    end.
    
    else 
    if index("AB",v-stat) LE 0 THEN
      assign
       v-date = oe-rel.rel-date
       v-ship = oe-rel.ship-id.*/
  end.

  FOR EACH w-rel,
      FIRST oe-rel WHERE ROWID(oe-rel) EQ w-rowid NO-LOCK
      BREAK BY oe-rel.rel-date
            BY oe-rel.ship-id:

    IF AVAIL eb AND oe-rel.ship-id EQ "" THEN
      ASSIGN
       oe-rel.ship-id      = eb.ship-id
       oe-rel.ship-addr[1] = eb.ship-addr[1]
       oe-rel.ship-addr[2] = eb.ship-addr[2]
       oe-rel.ship-city    = eb.ship-city
       oe-rel.ship-state   = eb.ship-state
       oe-rel.ship-zip     = eb.ship-zip. 
         
    v-auto = NOT FIRST-OF(oe-rel.ship-id).

    RUN oe/actrel.p (RECID(oe-rel)).

  END.
  MESSAGE "mail2".
  RUN send-email-proc.
end.
PROCEDURE send-email-proc:
   DEF VAR ls-to-list AS cha NO-UNDO.
   DEF VAR lv-mailto AS cha NO-UNDO.
   DEF VAR lv-mailsubject AS cha NO-UNDO.
   DEF VAR lv-mailbody AS cha NO-UNDO.
   DEF VAR retcode AS INT NO-UNDO.
   DEF VAR v-prgmname LIKE prgrms.prgmname NO-UNDO.
   
   v-prgmname = "actrel.".
MESSAGE "mail3".
   FOR EACH tt-email,
       FIRST cust WHERE
             cust.company = cocode AND
             cust.cust-no = tt-email.cust-no AND
             cust.active = "E"
             NO-LOCK
             BREAK BY tt-email.ord-no:

       IF FIRST-OF(tt-email.ord-no) THEN
          lv-mailbody = "Order Number " +  STRING(tt-email.ord-no)
                      + " has been released." + CHR(10).

       lv-mailbody = lv-mailbody 
                   + "Item: " + STRING(tt-email.i-no,"X(15)")
                   + " Qty: " + STRING(tt-email.rel-qty,"->>,>>>,>>9")
                   + " Date: " + STRING(tt-email.rel-date,"99/99/99")
                   + " PO#: "  + STRING(tt-email.po-no,"X(15)") + CHR(10).
       
       IF LAST-OF(tt-email.ord-no) THEN do:
           
           {custom/emailList.i &recKey=cust.rec_key &emailList=ls-to-list}

           IF ls-to-list NE '' THEN DO:

             ASSIGN lv-mailto = "To:" + ls-to-list
                    lv-mailsubject = "Release Generated".
             /*RUN mail(lv-mailto,lv-mailsubject,lv-mailbody,"",1,OUTPUT retcode).*/
           END.
       END. /* last-of(tt-email.cust-no) */
   END.
END.

/* end ---------------------------------- copr. 1996  advanced software, inc. */





