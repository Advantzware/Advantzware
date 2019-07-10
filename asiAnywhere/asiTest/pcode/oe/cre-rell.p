/* -------------------------------------------------- oe/cre-rell.p 01/98 JLF */
/* order entry - Create actual releases from planned release line             */
/* -------------------------------------------------------------------------- */

def input parameter in-recid as recid.

{sys/inc/var.i shared}

{oe/d-selbin.i NEW}

def shared var out-recid as recid no-undo.
def shared var relh-recid as recid no-undo.

def var v-whse like oe-rell.loc no-undo.
def var v-loc-bin like oe-rell.loc-bin no-undo.
def var v-fgfile as log no-undo.
DEF VAR v-none AS LOG INIT YES NO-UNDO.
DEF VAR lv-all-or-one AS cha NO-UNDO.
DEF VAR lv-rowids AS CHAR NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR ll AS LOG NO-UNDO.
DEF VAR ll-bin-tag AS LOG NO-UNDO.
DEF VAR lv-job-no AS CHAR FORMAT "x(9)" NO-UNDO.
DEF VAR lv-selected-value AS cha NO-UNDO. /*all,one,notag*/
DEF BUFFER b-reftable FOR reftable.
DEF SHARED VAR ARowid AS RECID NO-UNDO.

{sys/inc/addrelse.i}
FIND oe-rel WHERE RECID(oe-rel) EQ ARowid NO-LOCK NO-ERROR.
DO TRANSACTION:
  DEF VAR relmerge-log AS LOG NO-UNDO.
DEF VAR relmerge-int AS INT NO-UNDO.
DEF VAR relmerge-chr AS CHAR NO-UNDO.
FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ oe-rel.company AND
     sys-ctrl.name    EQ "relmerge"
    NO-LOCK NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company = oe-rel.company
   sys-ctrl.name    = "RELMERGE"
   sys-ctrl.descrip = "When creating actual releases, prompt to merge into printed release?"
   sys-ctrl.int-fld = 1.
  MESSAGE sys-ctrl.descrip
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE sys-ctrl.log-fld.
end.
assign
 relmerge-log = sys-ctrl.log-fld
 relmerge-int = sys-ctrl.int-fld
 relmerge-chr = sys-ctrl.char-fld.



  find first sys-ctrl
      where sys-ctrl.company eq oe-rel.company
        and sys-ctrl.name    eq "BOLWHSE"
      no-lock no-error.
  if not avail sys-ctrl then do:
    create sys-ctrl.
    assign
     sys-ctrl.company = oe-rel.company
     sys-ctrl.name    = "BOLWHSE"
     sys-ctrl.descrip = "Default Warehouse for Adding Release/BOL"
     sys-ctrl.log-fld = no.
    message "System control record NOT found. " sys-ctrl.descrip
    update sys-ctrl.char-fld.
  end.
  if avail sys-ctrl then v-whse = sys-ctrl.char-fld.

  find first sys-ctrl
      where sys-ctrl.company eq oe-rel.company
        and sys-ctrl.name    eq "BOLPRINT"
      no-lock no-error.
  if not avail sys-ctrl then do:
    create sys-ctrl.
    assign
     sys-ctrl.company = oe-rel.company
     sys-ctrl.name    = "BOLPRINT"
     sys-ctrl.descrip = "Print Bill of Lading Headers on Plain Paper"
     sys-ctrl.log-fld = no.
    message "System control record NOT found. " sys-ctrl.descrip
            update sys-ctrl.char-fld.
  end.
  if avail sys-ctrl then v-loc-bin = sys-ctrl.char-fld.

  find first sys-ctrl
      where sys-ctrl.company eq oe-rel.company
        and sys-ctrl.name    eq "AUTOPOST"
      no-lock no-error.
  v-fgfile = avail sys-ctrl and sys-ctrl.char-fld eq "FGFILE".
END.

/** If Shipping From Bill Of Lading Then Set Ship Code = B
    Or If Shipping From Finiished Goods Then Set Ship Code = I **/
find first oe-ctrl where oe-ctrl.company eq oe-rel.company no-lock no-error.

FIND oe-rel WHERE RECID(oe-rel) EQ in-recid NO-LOCK.

FIND oe-relh WHERE RECID(oe-relh) EQ out-recid NO-LOCK.
IF relh-recid NE ? THEN
FIND oe-relh WHERE RECID(oe-relh) EQ relh-recid NO-LOCK.

/** Find last actual release for this order number and add 1 to
    the get the next release. **/
/* === rel-no logic moved to line (oe-rell) ========*/
DEF BUFFER bf-rell FOR oe-rell .
DEF VAR li-nxt-rel-no AS INT NO-UNDO.
for each bf-rell
    where bf-rell.company eq oe-rel.company
      and bf-rell.ord-no  eq oe-rel.ord-no no-lock 
      by bf-rell.rel-no desc:
    
      li-nxt-rel-no =  bf-rell.rel-no.
      leave.  
end.
li-nxt-rel-no = li-nxt-rel-no + 1.
/*========== */

DO TRANSACTION:
  FIND CURRENT oe-relh EXCLUSIVE.
  FIND CURRENT oe-rel EXCLUSIVE.
  ASSIGN
   oe-relh.printed = NO
   oe-rel.rel-no   = li-nxt-rel-no.
   oe-rel.b-ord-no = oe-relh.b-ord-no.
  FIND CURRENT oe-relh NO-LOCK.
  FIND CURRENT oe-rel NO-LOCK.
END.

find first oe-ordl
    where oe-ordl.company eq oe-rel.company
      and oe-ordl.ord-no  eq oe-rel.ord-no
      and oe-ordl.i-no    eq oe-rel.i-no
      and oe-ordl.line    eq oe-rel.line
    no-lock.

find first itemfg
    where itemfg.company eq oe-rel.company
      and itemfg.i-no    eq oe-rel.i-no
    no-lock no-error.
    
ll-bin-tag = AVAIL oe-ordl             AND
             addrelse-cha EQ "Bin/Tag" AND
             CAN-FIND(FIRST fg-bin
                      WHERE fg-bin.company EQ oe-rel.company
                        AND fg-bin.i-no    EQ oe-ordl.i-no
                        AND fg-bin.qty     GT 0).

/*IF ll-bin-tag THEN
    MESSAGE "Do you want to select bins and tags?"
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE ll-bin-tag.*/
    
IF ll-bin-tag THEN DO:
  ASSIGN
   ll        = NO
   lv-job-no = TRIM(oe-ordl.job-no) + "-" + STRING(oe-ordl.job-no2,"99").

  IF lv-job-no EQ "-00" THEN lv-job-no = "".

  IF oe-rel.qty > 0 THEN  /*task# 09200502*/
    /* RUN oe/d-relbin.w (oe-rel.ord-no,oe-rel.i-no,oe-rel.po-no,lv-job-no,oe-rel.qty,OUTPUT lv-selected-value).
      ELSE */
      lv-selected-value = "NoTag".

  ASSIGN
   lv-all-or-one = /*STRING(ll,"ALL/ONE")*/ lv-selected-value  /*all,one,notag*/
   ll-bin-tag    = lv-all-or-one NE "ONE" OR
                   CAN-FIND(FIRST fg-bin
                            WHERE fg-bin.company EQ oe-rel.company
                              AND fg-bin.i-no    EQ oe-ordl.i-no
                              AND fg-bin.job-no  EQ oe-ordl.job-no
                              AND fg-bin.job-no2 EQ oe-ordl.job-no2
                              AND fg-bin.qty     GT 0).
END.
   
IF AVAIL itemfg             AND
   oe-rel.qty GT 0          AND
   NOT ll-bin-tag           AND
   v-whse NE "SHIPTO"       AND
   (itemfg.i-code EQ "S" OR
    oe-ordl.job-no EQ "" OR
    v-whse EQ "FIFO")       THEN 
  RUN oe/fifoloop.p (ROWID(oe-rel), lv-selected-value EQ "notag", OUTPUT v-none).

IF v-none THEN DO TRANSACTION:
  FIND FIRST reftable
      WHERE reftable.reftable EQ "oe-rel.s-code"
        AND reftable.company  EQ STRING(oe-rel.r-no,"9999999999")
      NO-LOCK NO-ERROR.

  CREATE oe-rell.
  ASSIGN
   out-recid       = RECID(oe-rell)
   oe-rell.company = oe-rel.company
   oe-rell.r-no    = oe-relh.r-no
   oe-rell.rel-no  = li-nxt-rel-no
   oe-rell.loc     = oe-rel.loc
   oe-rell.ord-no  = oe-rel.ord-no
   oe-rell.qty     = oe-rel.qty
   oe-rell.i-no    = oe-rel.i-no
   oe-rell.job-no  = oe-ordl.job-no
   oe-rell.job-no2 = oe-ordl.job-no2
   oe-rell.po-no   = oe-rel.po-no
   oe-rell.line    = oe-rel.line
   oe-rell.printed = no
   oe-rell.posted  = no
   oe-rell.deleted = no
   /** Set link to the planned releases **/
   oe-rell.link-no = oe-rel.r-no
   oe-rell.s-code  = IF AVAIL reftable THEN reftable.code ELSE
                     IF oe-ordl.is-a-component THEN "S" ELSE
                     if avail oe-ctrl and oe-ctrl.ship-from then "B" else "I".

  RELEASE reftable.

  FIND FIRST b-reftable NO-LOCK
      WHERE b-reftable.reftable EQ "oe-rel.lot-no"
        AND b-reftable.company  EQ STRING(oe-rel.r-no,"9999999999")
      NO-ERROR.

  IF AVAIL b-reftable THEN DO:
    CREATE reftable.
    ASSIGN
     reftable.reftable = "oe-rell.lot-no"
     reftable.rec_key  = oe-rell.rec_key
     reftable.code     = b-reftable.code.
    RELEASE reftable.
    RELEASE b-reftable.
  END.

  FIND FIRST b-reftable NO-LOCK
      WHERE b-reftable.reftable EQ "oe-rel.sell-price"
        AND b-reftable.company  EQ STRING(oe-rel.r-no,"9999999999")
      NO-ERROR.

  IF AVAIL b-reftable THEN DO:
    CREATE reftable.
    ASSIGN
     reftable.reftable = "oe-rell.sell-price"
     reftable.rec_key  = oe-rell.rec_key
     reftable.val[1]   = b-reftable.val[1].
    RELEASE reftable.
    RELEASE b-reftable.
  END.

  if v-whse eq "SHIPTO" then do:
    find first shipto
      where shipto.company eq oe-rel.company
        and shipto.cust-no eq oe-rel.cust-no
        and shipto.ship-no eq oe-rel.ship-no
      use-index ship-no no-lock no-error.
    if avail shipto then 
      assign
       oe-rell.loc     = shipto.loc
       oe-rell.loc-bin = shipto.loc-bin.
  end.
  
  else do:
    find first fg-bin
        where fg-bin.company eq oe-rel.company
          and fg-bin.job-no  eq oe-ordl.job-no
          and fg-bin.job-no2 eq oe-ordl.job-no2
          and fg-bin.i-no    eq oe-rell.i-no
          and fg-bin.qty     ge oe-rell.qty
        use-index job no-lock no-error.
      
    if not avail fg-bin then
    find first fg-bin
        where fg-bin.company eq oe-rel.company
          and fg-bin.job-no  eq oe-ordl.job-no
          and fg-bin.job-no2 eq oe-ordl.job-no2
          and fg-bin.i-no    eq oe-rell.i-no
          and fg-bin.qty     gt 0
        use-index job no-lock no-error.
        
    if not avail fg-bin then
    find first fg-bin
        where fg-bin.company eq oe-rel.company
          and fg-bin.job-no  eq oe-ordl.job-no
          and fg-bin.job-no2 eq oe-ordl.job-no2
          and fg-bin.i-no    eq oe-rell.i-no
        use-index job no-lock no-error.
        
    if not avail fg-bin and oe-ordl.job-no eq "" then
    find first fg-bin
        where fg-bin.company eq oe-rel.company
          and fg-bin.i-no    eq oe-rell.i-no
          and fg-bin.ord-no  eq oe-rel.ord-no
          and fg-bin.qty     gt 0
        use-index co-ino no-lock no-error.
        
    if not avail fg-bin and oe-ordl.job-no eq "" then
    find first fg-bin
        where fg-bin.company eq oe-rel.company
          and fg-bin.i-no    eq oe-rell.i-no
          and fg-bin.ord-no  eq oe-rel.ord-no
        use-index co-ino no-lock no-error.
        
    if not avail fg-bin and oe-ordl.job-no eq "" then
    find first fg-bin
        where fg-bin.company eq oe-rel.company
          and fg-bin.i-no    eq oe-rell.i-no
          and fg-bin.qty     gt 0
        use-index co-ino no-lock no-error.
        
    if not avail fg-bin and oe-ordl.job-no eq "" then
    find first fg-bin
        where fg-bin.company eq oe-rel.company
          and fg-bin.i-no    eq oe-rell.i-no
        use-index co-ino no-lock no-error.
        
    if avail fg-bin then 
      assign
       oe-rell.loc      = fg-bin.loc
       oe-rell.loc-bin  = fg-bin.loc-bin
       oe-rell.tag      = fg-bin.tag
       oe-rell.job-no   = fg-bin.job-no
       oe-rell.job-no2  = fg-bin.job-no2
       oe-rell.qty-case = fg-bin.case-count.
                           
    else 
    if v-fgfile then do:
      find first itemfg
        where itemfg.company eq oe-rel.company
          and itemfg.i-no    eq oe-rell.i-no
        no-lock no-error.
      assign
       oe-rell.loc     = itemfg.def-loc
       oe-rell.loc-bin = itemfg.def-loc-bin.
    end.
  end.
  
  if oe-rell.loc eq "" or oe-rell.loc-bin eq "" then do:
    find first itemfg
      where itemfg.company eq oe-rel.company
        and itemfg.i-no    eq oe-rell.i-no
      no-lock no-error.
    if avail itemfg then
      assign
       oe-rell.loc     = itemfg.def-loc
       oe-rell.loc-bin = itemfg.def-loc-bin.
    if oe-rell.loc eq "" or oe-rell.loc-bin eq "" then do:
      find first cust where cust.company eq oe-rel.company
                        and cust.active  eq "X" 
                      no-lock no-error.
      if avail cust then do:
        find first shipto where shipto.company eq oe-rel.company
                            and shipto.cust-no eq cust.cust-no
                          no-lock no-error.
        if avail shipto then
          assign   
           oe-rell.loc     = shipto.loc
           oe-rell.loc-bin = shipto.loc-bin.
      end.            
    end.
  end.
  if oe-rell.loc-bin eq "" then oe-rell.loc-bin = v-loc-bin.

  if oe-rell.qty-case eq 0 then
    oe-rell.qty-case = if avail itemfg and itemfg.case-count gt 0
                       then itemfg.case-count
                       else
                       if oe-ordl.cas-cnt gt 0 then oe-ordl.cas-cnt
                       else 1.

  ASSIGN
   oe-rell.cases    = TRUNC((oe-rell.qty - oe-rell.partial) /
                            oe-rell.qty-case,0)
   oe-rell.partial  = oe-rell.qty - (oe-rell.cases * oe-rell.qty-case). 

  IF oe-rell.qty LT 0 OR lv-selected-value = "NoTag" THEN oe-rell.tag = "".
  
  RUN oe/rel-stat-upd.p (ROWID(oe-rell)).
end.

IF ll-bin-tag THEN DO TRANSACTION:
 /* IF lv-selected-value NE "Notag" THEN DO:
      
    IF lv-selected-value NE "Cancel" THEN
      RUN oe/d-selbin.w (4, ROWID(oe-rell), lv-all-or-one, oe-rell.i-no,
                         OUTPUT lv-rowids).

    IF lv-rowids = "" THEN DO:
      FIND CURRENT oe-rell NO-ERROR.
      DELETE oe-rell.
    END.
  END. */

  FIND CURRENT oe-rell NO-LOCK NO-ERROR.
  IF NOT AVAIL oe-rell THEN
  FIND FIRST oe-rell
      WHERE oe-rell.company EQ oe-relh.company
        AND oe-rell.r-no    EQ oe-relh.r-no
      USE-INDEX r-no NO-LOCK NO-ERROR.
  out-recid = IF AVAIL oe-rell THEN RECID(oe-rell) ELSE ?.

  IF out-recid EQ ? THEN DO:
    FIND CURRENT oe-relh NO-ERROR.
    IF AVAIL oe-relh THEN DELETE oe-relh.
  END.
END.

/* end ---------------------------------- copr. 1998  advanced software, inc. */

