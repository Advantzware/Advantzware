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
DEF VAR v#ofItemsReleased AS INT no-undo.
DEF VAR iocPrompt AS CHAR NO-UNDO.
DEFINE VARIABLE lAbort AS LOGICAL     NO-UNDO.
DEFINE VARIABLE iNumOrderLines AS INTEGER     NO-UNDO.
DEFINE VARIABLE iNum-w-rel AS INTEGER NO-UNDO.
DEFINE VARIABLE cRelLogData AS CHAR NO-UNDO.
DEFINE STREAM sRelErrorLog.

DEF BUFFER bf-oe-rel FOR oe-rel.

DEF TEMP-TABLE w-rel NO-UNDO FIELD w-rowid AS ROWID.

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

DO TRANSACTION:
  {sys/inc/addrelse.i}
END.
find first oe-ctrl where oe-ctrl.company eq cocode no-lock.

find xoe-ordl where recid(xoe-ordl) eq fil_id no-lock no-error.
if not avail xoe-ordl then do: 
    return. 
END.

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
        AND oe-rel.tot-qty     gt 0
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

if v-auto and (xoe-ord.stat eq "H" OR xoe-ord.priceHold) and oe-ctrl.p-pick eq no then do:
   message "Can not release items for customers on Credit Hold or Price Hold." view-as alert-box
           error.
   v-auto = no.
end.

if v-auto then do:
  EMPTY TEMP-TABLE w-rel.
  choice  = YES.
  FOR EACH oe-rel
      WHERE oe-rel.company EQ xoe-ord.company
        AND oe-rel.ord-no  EQ xoe-ord.ord-no
        AND oe-rel.link-no EQ 0
        AND oe-rel.tot-qty     GT 0
      NO-LOCK,
      first xoe-ordl of oe-rel no-lock
      break by oe-rel.rel-date
            by oe-rel.ship-id:
    
    if first-of(oe-rel.ship-id) then v-first = yes.

    RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT v-stat).
    cRelLogData = cRelLogData + string(oe-rel.r-no) + "|" + oe-rel.stat + "|" + v-stat + "|" + oe-rel.i-no + "," .
  

DEF VAR ip-view-type AS CHAR NO-UNDO.
DEF VAR ip-parms     AS CHAR NO-UNDO.
DEF VAR op-values    AS CHAR NO-UNDO.
DEF VAR lcMergeMessage AS CHAR NO-UNDO.
DEF VAR llPrompt AS LOG NO-UNDO.
DEF VAR llMergeNew AS LOG NO-UNDO.


    IF INDEX("AB",v-stat) EQ 0 THEN DO:
      IF v-first THEN DO:
        ASSIGN
         v-first = NO
         choice = YES.

lcMergeMessage = "Create Release for Date-" + TRIM(STRING(oe-rel.rel-date)) + 
                " and ShipID-" +  TRIM(oe-rel.ship-id) + " ?" .
/*         MESSAGE "Create Release for Date-" + TRIM(STRING(oe-rel.rel-date)) + */
/*                " and ShipID-" +  TRIM(oe-rel.ship-id) + " ?"                 */
/*               VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE choice.        */
.
         
            ip-parms = 
          "type=literal,name=fi4,row=3,col=18,enable=false,width=58,scrval=" + lcMergeMessage + ",FORMAT=X(58)"
          + "|type=toggle,name=tg1,row=4,col=18,enable=true,width=32,label=Prompt to merge each item?"
          + "|type=toggle,name=tg2,row=5,col=18,enable=true,width=38,label=Merge items with existing release?"
          /* + "|type=literal,name=fi5,row=4,col=22,enable=false,scrval=Prompt to merge each item?,width=28,format=x(28)"  */
          + "|type=image,image=webspeed\images\question.gif,name=im1,row=3,col=4,enable=true " 
          + "|type=win,name=fi3,enable=true,label=Question,FORMAT=X(30),height=11".
    
        RUN custom/d-prompt.w (INPUT "yes-no", ip-parms, "", OUTPUT op-values).
    
        DO i = 1 TO NUM-ENTRIES(op-values) BY 2.
            IF ENTRY(i, op-values) EQ "default" THEN
              choice = LOGICAL(ENTRY(i + 1, op-values)) NO-ERROR.
            IF ENTRY(i, op-values) EQ "tg1" THEN
              llPrompt = LOGICAL(ENTRY(i + 1, op-values)) NO-ERROR.
            IF ENTRY(i, op-values) EQ "tg2" THEN
              llMergeNew = LOGICAL(ENTRY(i + 1, op-values)) NO-ERROR.            
        END.
        iocPrompt = (IF llPrompt THEN "ALWAYS" ELSE "NEVER").
        /* If they check of merge to new release only, then set iocPrompt to FIRST and use that to  */
        /* make sure it doesn't merge on the first time through, then change iocPrompt back to 'ALWAYS' */
        IF NOT llPrompt AND NOT llMergeNew THEN
            iocPrompt = "FIRST".                                                                       

      END. /*v-first*/

      IF choice THEN DO:
        CREATE w-rel.
        w-rowid = ROWID(oe-rel).
        iNum-w-rel = iNum-w-rel + 1.
      END.
    END. /*v-stat ne A or B*/

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

  v#ofItemsReleased = 0.
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

    RUN oe/actrel.p (RECID(oe-rel), INPUT-OUTPUT iocPrompt).
    FIND bf-oe-rel WHERE ROWID(bf-oe-rel) EQ ROWID(oe-rel) NO-LOCK NO-ERROR.
    IF AVAIL bf-oe-rel AND bf-oe-rel.rel-no GT 0 THEN
        v#ofItemsReleased = v#ofItemsReleased + 1.
  END.

  RUN send-email-proc.
end.

IF addrelse-cha = "No Tags" THEN DO:
    iNumOrderLines = 0.
    RUN CountOrderLines(BUFFER xoe-ord, 
                        OUTPUT iNumOrderLines).
    IF iNumOrderLines GT v#ofItemsReleased THEN DO:
        RUN LogError.
        lAbort = NO.
        RUN PromptForIncompleteRelease(INPUT iNumOrderlines,
                                       INPUT v#ofItemsReleased, 
                                       INPUT xoe-ord.ord-no,
                                       OUTPUT lAbort).
        IF lAbort THEN DO:
            RUN DeleteIncompleteRelease.
        END.
    END.
    ELSE
        MESSAGE "Total # of items released: " v#ofItemsReleased
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

PROCEDURE send-email-proc:
   DEF VAR ls-to-list AS cha NO-UNDO.
   DEF VAR lv-mailto AS cha NO-UNDO.
   DEF VAR lv-mailsubject AS cha NO-UNDO.
   DEF VAR lv-mailbody AS cha NO-UNDO.
   DEF VAR retcode AS INT NO-UNDO.
   DEF VAR v-prgmname LIKE prgrms.prgmname NO-UNDO.
   
   v-prgmname = "actrel.".

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
             RUN mail(lv-mailto,lv-mailsubject,lv-mailbody,"",1,OUTPUT retcode).
           END.
       END. /* last-of(tt-email.cust-no) */
   END.
END.

PROCEDURE CountOrderLines:
    DEFINE PARAMETER BUFFER ipbf-oe-ord FOR oe-ord.
    DEFINE OUTPUT PARAMETER opiCount AS INTEGER NO-UNDO.

    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
    DEFINE BUFFER bf-itemfg FOR itemfg.
    
    opiCount = 0.
    FOR EACH bf-oe-ordl OF ipbf-oe-ord 
        NO-LOCK:
      FIND FIRST bf-itemfg 
                     WHERE bf-itemfg.company EQ bf-oe-ordl.company
                       AND bf-itemfg.i-no    EQ bf-oe-ordl.i-no
                     NO-LOCK NO-ERROR.
      /* Don't count unassembled set header */
      IF bf-itemfg.alloc = YES THEN
        NEXT.
      opiCount = opiCount + 1.
    END.
   
END PROCEDURE.

PROCEDURE DeleteIncompleteRelease:

    DEFINE BUFFER bf-oe-rel FOR oe-rel.
    DEFINE BUFFER bf-oe-relh FOR oe-relh.
    DEFINE BUFFER bf-oe-rell FOR oe-rell.
    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
    
    FOR EACH w-rel,
        FIRST bf-oe-rel WHERE ROWID(bf-oe-rel) EQ w-rowid EXCLUSIVE-LOCK:

        /* oe-rell has link to oe-rel, so use that to get oe-relh */
        FIND FIRST bf-oe-rell
            WHERE bf-oe-rell.company EQ bf-oe-rel.company
              AND bf-oe-rell.ord-no  EQ bf-oe-rel.ord-no      
              AND bf-oe-rell.i-no    EQ bf-oe-rel.i-no
              AND bf-oe-rell.line    EQ bf-oe-rel.line
              AND bf-oe-rell.link-no EQ bf-oe-rel.r-no
            NO-LOCK NO-ERROR.
        IF AVAIL bf-oe-rell THEN
            FIND FIRST bf-oe-relh WHERE bf-oe-relh.r-no EQ bf-oe-rell.r-no NO-ERROR.
        IF AVAIL bf-oe-relh THEN DO:
            FOR EACH bf-oe-rell
                WHERE bf-oe-rell.company EQ bf-oe-rel.company
                  AND bf-oe-rell.ord-no  EQ bf-oe-rel.ord-no            
                  AND bf-oe-rell.i-no    EQ bf-oe-rel.i-no
                  AND bf-oe-rell.line    EQ bf-oe-rel.line
                  AND bf-oe-rell.link-no EQ bf-oe-rel.r-no:

                DELETE bf-oe-rell.

            END. /* Each oe-rell */

            FIND FIRST bf-oe-rell
                WHERE bf-oe-rell.company EQ bf-oe-relh.company
                  AND bf-oe-rell.r-no    EQ bf-oe-relh.r-no
                USE-INDEX r-no NO-LOCK NO-ERROR.
            IF NOT AVAIL bf-oe-rell THEN DO:
                bf-oe-relh.posted = NO.
                DELETE bf-oe-relh.
            END. /* no more oe-rell's for this oe-relh */
            
            RELEASE bf-oe-relh.
        END. /* If avail oe-relh */

        bf-oe-rel.link-no = 0.
       
        /*update release qty on orderline*/
        FIND FIRST bf-oe-ordl
            WHERE bf-oe-ordl.company EQ bf-oe-rel.company
              AND bf-oe-ordl.ord-no  EQ bf-oe-rel.ord-no      
              AND bf-oe-ordl.i-no    EQ bf-oe-rel.i-no
            EXCLUSIVE-LOCK NO-ERROR.
        IF AVAIL bf-oe-ordl THEN DO:
            bf-oe-ordl.t-rel-qty = bf-oe-ordl.t-rel-qty - bf-oe-rel.qty.
            RELEASE bf-oe-ordl.
        END.
    END. /*each w-rel*/

END PROCEDURE.

PROCEDURE PromptForIncompleteRelease:
    DEFINE INPUT PARAMETER  ipiOrderCount AS INTEGER     NO-UNDO.
    DEFINE INPUT PARAMETER ipiReleaseCount AS INTEGER     NO-UNDO.
    DEFINE INPUT PARAMETER ipiOrderNumber AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplAbort AS LOGICAL     NO-UNDO.

    MESSAGE "WARNING: Order " ipiOrderNumber " has " ipiOrderCount " items, but only " ipiReleaseCount " were released.  Would you like to abort the release creation and try again?" SKIP(1)
        "Hit YES to delete the incomplete release and try again." SKIP
        "Hit NO to continue with incomplete release." SKIP
        VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO UPDATE oplAbort.


END PROCEDURE.
PROCEDURE LogError:
    DEFINE VARIABLE lRelErrorLog AS LOG NO-UNDO.

    lRelErrorLog = SEARCH('logs/RelError.log') NE ?.
    IF lRelErrorLog THEN DO:
        OUTPUT STREAM sRelErrorLog TO VALUE('logs/RelError.' +
            STRING(TODAY,'99999999') + '.' + STRING(TIME) + '.log').
        PUT STREAM sRelErrorLog UNFORMATTED 
            cRelLogData SKIP
            iNumOrderLines " lines in order" SKIP
            v#ofItemsReleased " lines released" SKIP
            iNum-w-rel " lines in w-rel".
        OUTPUT STREAM sRelErrorLog CLOSE.
    END.
END PROCEDURE.

/* end ---------------------------------- copr. 1996  advanced software, inc. */

