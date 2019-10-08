DEF VAR cocode AS CHAR.
DEF VAR out-recid AS RECID.
DEF VAR locode AS CHAR.

/* util/fixrelstat.p */
/* release status problem for invoiced orders is related to an incorrect
   link-no in oe-rell */
DEF VAR ll-create-oe-rell AS LOG NO-UNDO.
DEF VAR v-company LIKE oe-rel.company NO-UNDO.
DEF VAR v-ord-no LIKE oe-rel.ord-no NO-UNDO.
DEF VAR v-status LIKE oe-rel.stat NO-UNDO.
DEF VAR v-correction AS LOG NO-UNDO.
DISABLE TRIGGERS FOR LOAD OF oe-rell.

v-correction = NO.
UPDATE v-company LABEL "Company"
       v-ord-no LABEL "Order Number" 
       v-status LABEL "Incorrect Status"
       WITH 1 COL TITLE "Enter values and press 'F2' ".
FIND FIRST oe-rel WHERE oe-rel.company = v-company
                    AND oe-rel.ord-no  = v-ord-no
                    AND oe-rel.stat    = v-status
                  NO-LOCK NO-ERROR.
IF NOT AVAIL oe-rel THEN DO:
    MESSAGE "No match found, exiting..."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
END.
cocode = v-company.

for each oe-rel where oe-rel.company = v-company
                  AND oe-rel.ord-no = v-ord-no 
                  AND oe-rel.stat   = v-status
                NO-LOCK.
   locode = oe-rel.loc.
   for each oe-rell WHERE oe-rell.company  EQ oe-rel.company
     /*  AND oe-rell.r-no     EQ oe-rel.link-no */
        AND oe-rell.ord-no   EQ oe-rel.ord-no
        AND oe-rell.rel-no   EQ oe-rel.rel-no 
        AND oe-rell.b-ord-no EQ oe-rel.b-ord-no 
        AND oe-rell.i-no     EQ oe-rel.i-no
        AND oe-rell.line     EQ oe-rel.line 
        AND oe-rell.po-no    EQ oe-rel.po-no
        AND CAN-FIND(FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no) 

      USE-INDEX r-no /* NO-ERROR */.
      
      if oe-rell.link-no ne oe-rel.r-no then
        ASSIGN oe-rell.link-no = oe-rel.r-no v-correction = TRUE. 

   end.

END.
/* No correction made - oe-rell not found, try creating it */
IF v-correction = FALSE THEN DO:
    for each oe-rel where oe-rel.company = v-company
                      AND oe-rel.ord-no = v-ord-no 
                      AND oe-rel.stat   = v-status
                    EXCLUSIVE.
        RUN create-oe-rell.
    END.
END.

HIDE ALL NO-PAUSE.

IF v-correction THEN
    MESSAGE "Correction was made for this order number."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
ELSE
    MESSAGE "No correction was needed for this order number."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.


PROCEDURE create-oe-rell :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
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
DEF VAR v-s-code AS CHAR NO-UNDO.
DEF BUFFER b-reftable FOR reftable.
DEF BUFFER bf-oe-rel FOR oe-rel.

IF NOT AVAIL oe-rel THEN
    RETURN.

FIND FIRST oe-relh WHERE oe-relh.company   = oe-rel.company
                     AND oe-relh.cust-no   = oe-rel.cust-no
    /*
                     AND oe-relh.ship-no   = oe-rel.ship-no
                     AND oe-relh.ship-id   = oe-rel.ship-id */
                     AND oe-relh.rel-date  = oe-rel.rel-date
                   NO-LOCK NO-ERROR.
IF NOT AVAIL oe-relh THEN DO:
    RUN oe/cre-relh.p (INPUT RECID(oe-rel)).
    FIND FIRST oe-relh WHERE oe-relh.company   = oe-rel.company
                     AND oe-relh.cust-no   = oe-rel.cust-no
    /*
                     AND oe-relh.ship-no   = oe-rel.ship-no
                     AND oe-relh.ship-id   = oe-rel.ship-id */
                     AND oe-relh.rel-date  = oe-rel.rel-date
                   NO-LOCK NO-ERROR.

END.
  


/* -------------------------------------------------- oe/cre-rell.p 01/98 JLF */
/* order entry - Create actual releases from planned release line             */
/* -------------------------------------------------------------------------- */


/* {sys/inc/var.i shared} */

/* {oe/d-selbin.i NEW} */




{sys/inc/addrelse.i}

DO TRANSACTION:
  {sys/inc/relmerge.i}

  find first sys-ctrl
      where sys-ctrl.company eq cocode
        and sys-ctrl.name    eq "BOLWHSE"
      no-lock no-error.
  if not avail sys-ctrl then DO:
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
END.

find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

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
    
IF ll-bin-tag THEN DO:
  ASSIGN
   ll        = NO
   lv-job-no = TRIM(oe-ordl.job-no) + "-" + STRING(oe-ordl.job-no2,"99").

  IF lv-job-no EQ "-00" THEN lv-job-no = "".

  
  v-s-code  = IF oe-rel.s-code <> "" THEN oe-rel.s-code ELSE
                    IF oe-ordl.is-a-component THEN "S" ELSE
                    if avail oe-ctrl and oe-ctrl.ship-from then "B" else "I".
 lv-selected-value = "NoTag".
  
  ASSIGN
   lv-all-or-one = /*STRING(ll,"ALL/ONE")*/ lv-selected-value  /*all,one,notag*/
   ll-bin-tag    = lv-all-or-one NE "ONE" OR
                   CAN-FIND(FIRST fg-bin
                            WHERE fg-bin.company EQ cocode
                              AND fg-bin.i-no    EQ oe-ordl.i-no
                              AND fg-bin.job-no  EQ oe-ordl.job-no
                              AND fg-bin.job-no2 EQ oe-ordl.job-no2
                              AND fg-bin.qty     GT 0).
END.
   
IF v-none THEN DO TRANSACTION:
  
  v-correction = YES.
  CREATE oe-rell.
  ASSIGN
   out-recid       = RECID(oe-rell)
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
   oe-rell.lot-no  = oe-rel.lot-no
   oe-rell.frt-pay = oe-rel.frt-pay
   oe-rell.fob-code = oe-rel.fob-code
   oe-rell.printed = no
   oe-rell.posted  = no
   oe-rell.deleted = no
   oe-rell.loc     = oe-rel.spare-char-1
   /** Set link to the planned releases **/
   oe-rell.link-no = oe-rel.r-no
   oe-rell.s-code  = IF oe-rel.s-code <> "" THEN oe-rel.s-code ELSE
                     IF oe-ordl.is-a-component THEN "S" ELSE
                     if avail oe-ctrl and oe-ctrl.ship-from then "B" else "I".
   FIND bf-oe-rel WHERE RECID(bf-oe-rel) = RECID(oe-rel) EXCLUSIVE-LOCK.
   bf-oe-rel.link-no = oe-rell.r-no.
   RELEASE bf-oe-rel.  

    ASSIGN
       oe-rell.sell-price = oe-rel.sell-price
       oe-rell.zeroPrice = oe-rel.zeroPrice.

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
        
    if avail fg-bin then DO:
      assign
       oe-rell.loc      = fg-bin.loc
       oe-rell.loc-bin  = fg-bin.loc-bin
       oe-rell.tag      = fg-bin.tag
       oe-rell.job-no   = fg-bin.job-no
       oe-rell.job-no2  = fg-bin.job-no2
       oe-rell.qty-case = fg-bin.case-count.
       
    END.
                           
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

  IF oe-rell.qty LT 0 OR lv-selected-value = "NoTag" THEN oe-rell.tag = "".
  
  RUN oe/rel-stat-upd.p (ROWID(oe-rell)).
end. /* transaction */

IF avail(oe-rell) AND oe-rell.s-code = "I" THEN DO TRANSACTION:
  ASSIGN 
    oe-rell.loc-bin = ""
    oe-rell.job-no  = ""
    oe-rell.job-no2 = 0
    .
END.

DO TRANSACTION:  
  FIND CURRENT oe-rel EXCLUSIVE.
  RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT oe-rel.stat).
  FIND CURRENT oe-rel NO-LOCK.
END.

/* end ---------------------------------- copr. 1998  advanced software, inc. */



END PROCEDURE.


