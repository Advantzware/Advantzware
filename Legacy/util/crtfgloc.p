DEF VAR i AS INT.
DEF VAR j AS INT.


{custom/gcompany.i}
{custom/gloc.i}

/* {sys/inc/var.i NEW SHARED} */
{custom/globdefs.i}
/* Local Variable Definitions ---                                       */
DEF VAR cocode AS CHAR.
DEF VAR locode AS CHAR.
/*def var gcompany as char.
def var gloc as char. */
/*def var g_company as char.
def var g_loc as char. */

DEF VAR c1 AS INT NO-UNDO.
DEF VAR c2 AS INT NO-UNDO.

DEF VAR h_wmessage AS HANDLE.
DEF TEMP-TABLE tt-itemfg-loc LIKE itemfg-loc.


MESSAGE "Press YES to set up inventory records by location."
VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO-CANCEL
UPDATE ll AS LOG.

IF ll EQ YES THEN DO:
  
  
  
  FIND FIRST usercomp WHERE usercomp.user_id = USERID('NOSWEAT') AND
  usercomp.loc = '' AND
  usercomp.company_default = YES NO-LOCK NO-ERROR.
  g_company = IF AVAIL usercomp THEN usercomp.company ELSE "001".
  
  
  FIND FIRST usercomp WHERE usercomp.user_id = USERID("NOSWEAT") AND
  usercomp.company = g_company AND
  usercomp.loc NE "" AND
  usercomp.loc_default = YES
  NO-LOCK NO-ERROR.
  g_loc = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN".
  cocode = g_company.
  locode = g_loc.
  gcompany = g_company.
  gloc = g_loc.
  c1 = 100.
  PAUSE BEFORE-HIDE.
  /* ******************************************************************** */
  /* Populate location for each release record                            */
  /* ******************************************************************** */
  
  /****************************/
  FOR EACH company,
    EACH oe-ord WHERE oe-ord.company = company.company
    AND oe-ord.opened NO-LOCK:
    
    FOR EACH oe-rel WHERE oe-rel.company = oe-ord.company
      AND oe-rel.ord-no = oe-ord.ord-no NO-LOCK.
      i = i + 1.
      IF i GT 99 THEN DO:
        i = 1.
        j = j + 100.
        STATUS DEFAULT STRING(j).
      END.
      
      FIND FIRST tt-itemfg-loc WHERE
      tt-itemfg-loc.company = oe-ord.company AND
      tt-itemfg-loc.i-no    = oe-rel.i-no AND
      tt-itemfg-loc.loc     = oe-ord.loc
      NO-LOCK NO-ERROR.
      IF NOT avail tt-itemfg-loc THEN DO:
        CREATE tt-itemfg-loc.
        ASSIGN
        tt-itemfg-loc.company = oe-ord.company
        tt-itemfg-loc.i-no    = oe-rel.i-no
        tt-itemfg-loc.loc     = oe-ord.loc.
      END.
      
    END. /* each oe-rel */
    
    i = i + 1.
    IF i GT 99 THEN DO:
      i = 1.
      j = j + 100.
      STATUS DEFAULT STRING(j).
    END.
  END. /* each oe-ord */
  
  FOR EACH fg-bin WHERE fg-bin.qty GT 0 NO-LOCK:
    i = i + 1.
    IF i GT 99 THEN DO:
      i = 1.
      j = j + 100.
      STATUS DEFAULT STRING(j).
    END.
    FIND FIRST tt-itemfg-loc WHERE
    tt-itemfg-loc.company = fg-bin.company AND
    tt-itemfg-loc.i-no    = fg-bin.i-no AND
    tt-itemfg-loc.loc     = fg-bin.loc
    NO-LOCK NO-ERROR.
    IF NOT avail tt-itemfg-loc THEN DO:
      
      
      CREATE tt-itemfg-loc.
      ASSIGN
      tt-itemfg-loc.company = fg-bin.company
      tt-itemfg-loc.i-no    = fg-bin.i-no
      tt-itemfg-loc.loc     = fg-bin.loc.
    END.
  END.  /* each fg-bin */
  
  FOR EACH itemfg NO-LOCK:
    FIND FIRST tt-itemfg-loc WHERE
    tt-itemfg-loc.company = itemfg.company AND
    tt-itemfg-loc.i-no    = itemfg.i-no AND
    tt-itemfg-loc.loc     = gloc
    NO-LOCK NO-ERROR.
    IF NOT avail tt-itemfg-loc THEN DO:
      i = i + 1.
      IF i GT 99 THEN DO:
        i = 1.
        j = j + 100.
        STATUS DEFAULT STRING(j).
      END.
      CREATE tt-itemfg-loc.
      ASSIGN
      tt-itemfg-loc.company = itemfg.company
      tt-itemfg-loc.i-no    = itemfg.i-no
      tt-itemfg-loc.loc     = gloc.
    END.
  END. /* each itemfg */
  
  DISP j.
  i = 0.
  FOR EACH tt-itemfg-loc NO-LOCK.
    FIND FIRST itemfg-loc WHERE
    itemfg-loc.company = tt-itemfg-loc.company AND
    itemfg-loc.i-no    = tt-itemfg-loc.i-no AND
    itemfg-loc.loc     = tt-itemfg-loc.loc
    NO-LOCK NO-ERROR.
    i = i + 1.
    IF i GT 99 THEN DO:
      i = 1.
      j = j + 100.
      STATUS DEFAULT STRING(j).
    END.
    IF NOT avail itemfg-loc THEN DO:
      CREATE itemfg-loc.
      BUFFER-COPY tt-itemfg-loc TO itemfg-loc.
    END.
    
  END. /* each tt-itemfg-loc */
  DISP i.
  
  
  FOR EACH company,
    EACH oe-ord WHERE oe-ord.company = company.company
    AND oe-ord.opened NO-LOCK:
    
    FOR EACH oe-rel WHERE oe-rel.company = oe-ord.company
      AND oe-rel.ord-no = oe-ord.ord-no EXCLUSIVE-LOCK.
      i = i + 1.
      IF i GT 99 THEN DO:
        i = 1.
        j = j + 100.
        STATUS DEFAULT STRING(j).
      END.
      
      
      FIND FIRST shipto WHERE shipto.company EQ oe-rel.company
      AND shipto.cust-no EQ oe-ord.cust-no
      AND shipto.ship-id EQ oe-rel.ship-id
      NO-LOCK NO-ERROR.
      IF avail shipto AND shipto.loc GT "" THEN DO:
        oe-rel.spare-char-1 = shipto.loc.
        
      END.
      ELSE
      oe-rel.spare-char-1 = oe-ord.loc.
    END. /* each oe-rel */
    
    i = i + 1.
    IF i GT 99 THEN DO:
      i = 1.
      j = j + 100.
      STATUS DEFAULT STRING(j).
    END.
  END. /* each oe-ord */
  
  /************************************/
  
  
  FOR EACH itemfg NO-LOCK:
    
    c1 = c1 + 1.
    IF c1 GT 500 THEN DO:
      /*      PAUSE 1. */
      /* RUN process-message IN h_wmessage (INPUT "Phase III Item:" + itemfg.i-no ) . */
      c1 = 0.
      PAUSE 1.
      DISP itemfg.i-no.
      
    END.
    RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT locode).
    /* --------------------------------------------- sys/inc/fg-reset.i 08/99 JLF */
    
    DEF BUFFER b-itemfg FOR itemfg.
    DEF BUFFER b-itemfg-loc FOR itemfg-loc.
    
    
    DEF VAR v-hld-qty   AS DEC.
    DEF VAR v-part-qty  AS DEC.
    DEF VAR v-fin-qty   AS DEC.
    DEF VAR v-fstat     LIKE oe-ord.stat INIT "".
    DEF VAR v-tstat     LIKE v-fstat.
    
    RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT "").
    
    /*    {sys/inc/oereordr.i} */
    
    IF itemfg.q-avail EQ 0 AND itemfg.q-ono EQ 0 AND itemfg.q-onh = 0 THEN
    NEXT.
    
    FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ itemfg.company NO-LOCK.
    IF itemfg.est-no GT "" THEN
    FIND FIRST eb WHERE eb.company EQ itemfg.company
    AND eb.est-no  EQ itemfg.est-no
    AND eb.stock-no EQ itemfg.i-no
    NO-LOCK NO-ERROR.
    
    FOR EACH itemfg-loc
      WHERE itemfg-loc.company EQ itemfg.company
      AND itemfg-loc.i-no    EQ itemfg.i-no
      EXCLUSIVE-LOCK:
      
      ASSIGN
      itemfg-loc.q-onh   = 0
      itemfg-loc.q-ono   = 0
      itemfg-loc.q-alloc = 0
      itemfg-loc.q-back  = 0.
      /************************
      /* Set component FGs with the same loc as the current itemfg-loc */
      for each fg-set
      where fg-set.company eq itemfg.company
      and fg-set.part-no eq itemfg.i-no
      no-lock,
      first b-itemfg
      where b-itemfg.company eq itemfg.company
      and b-itemfg.i-no    eq fg-set.set-no
      and b-itemfg.isaset  eq YES
      NO-LOCK,
      first b-itemfg-loc
      where b-itemfg-loc.company eq itemfg.company
      and b-itemfg-loc.i-no    eq fg-set.set-no
      AND b-itemfg-loc.loc     EQ itemfg-loc.loc
      no-lock:
      
      /* 06111209 */
      {sys/inc/part-qty.i v-part-qty fg-set}
      v-part-qty =
      (IF AVAIL eb AND eb.spare-char-2 = "Y" THEN 1.0 ELSE v-part-qty).
      
      assign
      itemfg-loc.q-ono   = itemfg-loc.q-ono   + (b-itemfg-loc.q-ono   * v-part-qty)
      itemfg-loc.q-alloc = itemfg-loc.q-alloc + (b-itemfg-loc.q-alloc * v-part-qty).
      
      end.
      ************************************/
    END.
    
    
    
    
    FOR EACH itemfg-loc WHERE itemfg-loc.company = itemfg.company
      AND itemfg-loc.i-no EQ itemfg.i-no
      EXCLUSIVE-LOCK:
      
      /*** itemfg.q-onh ***/
      FOR EACH fg-bin
        WHERE fg-bin.company EQ itemfg-loc.company
        AND fg-bin.i-no    EQ itemfg-loc.i-no
        AND fg-bin.loc     EQ itemfg-loc.loc
        NO-LOCK:
        
        itemfg-loc.q-onh = itemfg-loc.q-onh + fg-bin.qty.
      END.
      
      /*** itemfg.q-ono from jobs and purchase orders***/
      RUN fg/calcqool.p (ROWID(itemfg), itemfg-loc.loc, OUTPUT itemfg-loc.q-ono).
      
      /*** itemfg.q-alloc & itemfg.q-back from customer orders ***/
      RUN fg/calcqabl.p (ROWID(itemfg), itemfg-loc.loc, OUTPUT itemfg-loc.q-alloc, OUTPUT itemfg-loc.q-back).
      
      itemfg-loc.q-avail = itemfg-loc.q-onh +
      itemfg-loc.q-ono -
      itemfg-loc.q-alloc.
    END.
    
    /* end ---------------------------------- copr. 1999  advanced software, inc. */
    
    
  END. /* each itemfg */
END. /* if ll */

MESSAGE "Done!"
VIEW-AS ALERT-BOX INFO BUTTONS OK.
/* if choose OK */
PAUSE 0 BEFORE-HIDE.





