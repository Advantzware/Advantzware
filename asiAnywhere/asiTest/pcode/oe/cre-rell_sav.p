/* -------------------------------------------------- oe/cre-rell.p 01/98 JLF */
/* order entry - Create actual releases from planned release line             */
/* -------------------------------------------------------------------------- */

def input parameter in-recid as recid.

{sys/inc/var.i shared}

{oe/d-selbin.i NEW}

def shared var out-recid as recid no-undo.
def shared var relh-recid as recid no-undo.
DEF SHARED VAR ARowid AS RECID NO-UNDO.
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


{sys/inc/addrelse.i}

{sys/inc/relmerge.i}

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "BOLWHSE"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "BOLWHSE"
   sys-ctrl.descrip = "Default Warehouse for Adding Release/BOL"
   sys-ctrl.log-fld = no.
  message "System control record NOT found. " sys-ctrl.descrip
  update sys-ctrl.char-fld.
end.
if avail sys-ctrl then v-whse = sys-ctrl.char-fld.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "BOLPRINT"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "BOLPRINT"
   sys-ctrl.descrip = "Print Bill of Lading Headers on Plain Paper"
   sys-ctrl.log-fld = no.
  message "System control record NOT found. " sys-ctrl.descrip
          update sys-ctrl.char-fld.
end.
if avail sys-ctrl then v-loc-bin = sys-ctrl.char-fld.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "AUTOPOST"
    no-lock no-error.
v-fgfile = avail sys-ctrl and sys-ctrl.char-fld eq "FGFILE".

/** If Shipping From Bill Of Lading Then Set Ship Code = B
    Or If Shipping From Finiished Goods Then Set Ship Code = I **/
find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

find oe-rel where recid(oe-rel) eq in-recid.

find oe-relh where recid(oe-relh) eq out-recid.
IF relh-recid <> ? THEN find oe-relh where recid(oe-relh) eq relh-recid.
/** Find last actual release for this order number and add 1 to
    the get the next release. **/
/* === rel-no logic moved to line (oe-rell) ========*/
DEF BUFFER bf-rell FOR oe-rell .
DEF VAR li-nxt-rel-no AS INT NO-UNDO.
for each bf-rell
    where bf-rell.company eq cocode
      and bf-rell.ord-no  eq oe-rel.ord-no no-lock 
      by bf-rell.rel-no desc:
    
      li-nxt-rel-no =  bf-rell.rel-no.
      leave.  
end.
li-nxt-rel-no = li-nxt-rel-no + 1.
/*========== */
assign
 oe-relh.printed = NO
 oe-rel.rel-no   = li-nxt-rel-no.
 oe-rel.b-ord-no = oe-relh.b-ord-no.

find first oe-ordl
    where oe-ordl.company eq cocode
      and oe-ordl.ord-no  eq oe-rel.ord-no
      and oe-ordl.i-no    eq oe-rel.i-no
      and oe-ordl.line    eq oe-rel.line
    no-lock.

find first itemfg
    where itemfg.company eq cocode
      and itemfg.i-no    eq oe-rel.i-no
    no-lock no-error.
    
ll-bin-tag = AVAIL oe-ordl             AND
             addrelse-cha EQ "Bin/Tag" AND
             CAN-FIND(FIRST fg-bin
                      WHERE fg-bin.company EQ cocode
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

  IF relmerge-int NE 0 OR TRIM(oe-ordl.job-no) EQ "" THEN
    MESSAGE "Order#: "   + TRIM(STRING(oe-rel.ord-no,">>>>>>>>>>")) SKIP
            "FG Item#: " + TRIM(oe-rel.i-no)                        SKIP
            "PO#: "      + TRIM(oe-rel.po-no)                       SKIP
            "Job#: "     + TRIM(lv-job-no)                          SKIP
            "Qty: "      + TRIM(STRING(oe-rel.qty,"->>>,>>>,>>9"))  SKIP(1)
    
            "Select Bins for All Jobs?"
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE ll.

  ASSIGN
   lv-all-or-one = STRING(ll,"ALL/ONE")
   ll-bin-tag    = lv-all-or-one EQ "ALL" OR
                   CAN-FIND(FIRST fg-bin
                            WHERE fg-bin.company EQ cocode
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
  RUN oe/fifoloop.p (ROWID(oe-rel), OUTPUT v-none).

if v-none then do:
  create oe-rell.
  assign
   out-recid       = recid(oe-rell)
   oe-rell.company = oe-rel.company
   oe-rell.r-no    = oe-relh.r-no
   oe-rell.rel-no  = li-nxt-rel-no
   oe-rell.loc     = locode
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
   oe-rell.s-code  = IF oe-ordl.is-a-component THEN "S" ELSE
                     if avail oe-ctrl and oe-ctrl.ship-from then "B" else "I".

  if v-whse eq "SHIPTO" then do:
    find first shipto
      where shipto.company eq cocode
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
        where fg-bin.company eq cocode
          and fg-bin.job-no  eq oe-ordl.job-no
          and fg-bin.job-no2 eq oe-ordl.job-no2
          and fg-bin.i-no    eq oe-rell.i-no
          and fg-bin.qty     ge oe-rell.qty
        use-index job no-lock no-error.
      
    if not avail fg-bin then
    find first fg-bin
        where fg-bin.company eq cocode
          and fg-bin.job-no  eq oe-ordl.job-no
          and fg-bin.job-no2 eq oe-ordl.job-no2
          and fg-bin.i-no    eq oe-rell.i-no
          and fg-bin.qty     gt 0
        use-index job no-lock no-error.
        
    if not avail fg-bin then
    find first fg-bin
        where fg-bin.company eq cocode
          and fg-bin.job-no  eq oe-ordl.job-no
          and fg-bin.job-no2 eq oe-ordl.job-no2
          and fg-bin.i-no    eq oe-rell.i-no
        use-index job no-lock no-error.
        
    if not avail fg-bin and oe-ordl.job-no eq "" then
    find first fg-bin
        where fg-bin.company eq cocode
          and fg-bin.i-no    eq oe-rell.i-no
          and fg-bin.ord-no  eq oe-rel.ord-no
          and fg-bin.qty     gt 0
        use-index co-ino no-lock no-error.
        
    if not avail fg-bin and oe-ordl.job-no eq "" then
    find first fg-bin
        where fg-bin.company eq cocode
          and fg-bin.i-no    eq oe-rell.i-no
          and fg-bin.ord-no  eq oe-rel.ord-no
        use-index co-ino no-lock no-error.
        
    if not avail fg-bin and oe-ordl.job-no eq "" then
    find first fg-bin
        where fg-bin.company eq cocode
          and fg-bin.i-no    eq oe-rell.i-no
          and fg-bin.qty     gt 0
        use-index co-ino no-lock no-error.
        
    if not avail fg-bin and oe-ordl.job-no eq "" then
    find first fg-bin
        where fg-bin.company eq cocode
          and fg-bin.i-no    eq oe-rell.i-no
        use-index co-ino no-lock no-error.
        
    if avail fg-bin then
      assign
       oe-rell.loc      = fg-bin.loc
       oe-rell.loc-bin  = fg-bin.loc-bin
       oe-rell.tag      = fg-bin.tag
       oe-rell.job-no   = fg-bin.job-no
       oe-rell.job-no2  = fg-bin.job-no2
       oe-rell.qty-case = fg-bin.case-count
       oe-rell.partial  = fg-bin.partial-count.
                           
    else 
    if v-fgfile then do:
      find first itemfg
        where itemfg.company eq cocode
          and itemfg.i-no    eq oe-rell.i-no
        no-lock no-error.
      assign
       oe-rell.loc     = itemfg.def-loc
       oe-rell.loc-bin = itemfg.def-loc-bin.
    end.
  end.
  
  if oe-rell.loc eq "" or oe-rell.loc-bin eq "" then do:
    find first itemfg
      where itemfg.company eq cocode
        and itemfg.i-no    eq oe-rell.i-no
      no-lock no-error.
    if avail itemfg then
      assign
       oe-rell.loc     = itemfg.def-loc
       oe-rell.loc-bin = itemfg.def-loc-bin.
    if oe-rell.loc eq "" or oe-rell.loc-bin eq "" then do:
      find first cust where cust.company eq cocode
                        and cust.active  eq "X" 
                      no-lock no-error.
      if avail cust then do:
        find first shipto where shipto.company eq cocode
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

  IF oe-rell.qty LT 0 THEN oe-rell.tag = "".
end.

IF ll-bin-tag THEN DO:
  RUN oe/d-selbin.w (4, ROWID(oe-rell), lv-all-or-one, oe-rell.i-no,
                     OUTPUT lv-rowids).

  FIND CURRENT oe-rell NO-LOCK NO-ERROR.
  IF NOT AVAIL oe-rell THEN
  FIND FIRST oe-rell OF oe-relh NO-LOCK NO-ERROR.
  out-recid = IF AVAIL oe-rell THEN RECID(oe-rell) ELSE ?.

  IF out-recid EQ ? THEN DO:
    FIND CURRENT oe-relh EXCLUSIVE NO-ERROR.
    IF AVAIL oe-relh THEN DELETE oe-relh.
  END.
END.

/* end ---------------------------------- copr. 1998  advanced software, inc. */
