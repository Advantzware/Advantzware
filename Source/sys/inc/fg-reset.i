/* --------------------------------------------- sys/inc/fg-reset.i 08/99 JLF */
/* itemfg reset                                                               */
/* -------------------------------------------------------------------------- */

    DEF INPUT PARAMETER rec-id AS RECID.

    {sys/inc/var.i shared}

    DEF BUFFER b-itemfg FOR itemfg.
    DEF BUFFER b-itemfg-loc FOR itemfg-loc.

    DEF VAR v-hld-qty AS DEC.
    DEF VAR v-part-qty AS DEC.
    DEF VAR v-fin-qty AS DEC.
    DEF VAR v-fstat LIKE oe-ord.stat INIT "".
    DEF VAR v-tstat LIKE v-fstat.

    {sys/inc/oereordr.i}

    FIND itemfg WHERE
        RECID(itemfg) EQ rec-id.
    
    FIND FIRST oe-ctrl NO-LOCK WHERE 
        oe-ctrl.company EQ itemfg.company.

    ASSIGN
        {1}q-onh   = 0
        {1}q-ono   = 0
        {1}q-alloc = 0
        {1}q-back  = 0.

    IF itemfg.est-no GT "" THEN FIND FIRST eb NO-LOCK WHERE 
        eb.company EQ itemfg.company AND 
        eb.est-no  EQ itemfg.est-no AND 
        eb.stock-no EQ itemfg.i-no
        NO-ERROR.

    FOR EACH itemfg-loc EXCLUSIVE WHERE 
        itemfg-loc.company EQ itemfg.company AND 
        itemfg-loc.i-no    EQ itemfg.i-no:
        
        ASSIGN
            itemfg-loc.q-onh   = 0
            itemfg-loc.q-ono   = 0
            itemfg-loc.q-alloc = 0
            itemfg-loc.q-back  = 0.                   

        /* Set component FGs with the same loc as the current itemfg-loc */
        FOR EACH fg-set NO-LOCK WHERE
            fg-set.company eq itemfg.company AND
            fg-set.part-no eq itemfg.i-no,
            FIRST b-itemfg NO-LOCK WHERE 
                b-itemfg.company eq itemfg.company AND 
                b-itemfg.i-no    eq fg-set.set-no AND
                b-itemfg.isaset  eq YES,
            FIRST b-itemfg-loc NO-LOCK WHERE
                b-itemfg-loc.company eq itemfg.company AND
                b-itemfg-loc.i-no    eq fg-set.set-no AND 
                b-itemfg-loc.loc     EQ itemfg-loc.loc:

            /* 06111209 */
            {sys/inc/part-qty.i v-part-qty fg-set}
            ASSIGN
                v-part-qty = (IF AVAIL eb AND eb.spare-char-2 = "Y" THEN 1.0 ELSE v-part-qty)
                itemfg-loc.q-ono   = itemfg-loc.q-ono   + (b-itemfg-loc.q-ono   * v-part-qty)
                itemfg-loc.q-alloc = itemfg-loc.q-alloc + (b-itemfg-loc.q-alloc * v-part-qty).
        END.
    END.

    FOR EACH fg-set NO-LOCK WHERE
        fg-set.company eq itemfg.company AND
        fg-set.part-no eq itemfg.i-no,
        FIRST b-itemfg NO-LOCK WHERE
            b-itemfg.company eq itemfg.company AND
            b-itemfg.i-no    eq fg-set.set-no AND
            b-itemfg.isaset  eq YES:
  
        /* 06111209 */
        {sys/inc/part-qty.i v-part-qty fg-set}
        ASSIGN
            v-part-qty = (IF AVAIL eb AND eb.spare-char-2 = "Y" THEN 1.0 ELSE v-part-qty)
            {1}q-ono   = {1}q-ono   + (b-itemfg.q-ono   * v-part-qty)
            {1}q-alloc = {1}q-alloc + (b-itemfg.q-alloc * v-part-qty).
    END.

    /*** itemfg.q-onh ***/
    FOR EACH fg-bin NO-LOCK WHERE
        fg-bin.company EQ itemfg.company AND
        fg-bin.i-no    EQ itemfg.i-no:

        {1}q-onh = {1}q-onh + fg-bin.qty.
    END.

    /*** itemfg.q-ono from jobs and purchase orders***/
    RUN fg/calcqono.p (ROWID(itemfg), OUTPUT {1}q-ono).

    /*** itemfg.q-alloc & itemfg.q-back from customer orders ***/
    RUN fg/calcqa&b.p (ROWID(itemfg), OUTPUT {1}q-alloc, OUTPUT {1}q-back).

    ASSIGN
        {1}q-avail = {1}q-onh +
                     (IF oereordr-cha EQ "XOnOrder" THEN 0 ELSE {1}q-ono) -
                     {1}q-alloc.

    FOR EACH itemfg-loc EXCLUSIVE WHERE 
        itemfg-loc.company = itemfg.company AND 
        itemfg-loc.i-no EQ itemfg.i-no:
    
        /*** itemfg.q-onh ***/
        FOR EACH fg-bin NO-LOCK WHERE
            fg-bin.company eq itemfg-loc.company AND
            fg-bin.i-no    eq itemfg-loc.i-no AND
            fg-bin.loc     EQ itemfg-loc.loc:
            ASSIGN    
                itemfg-loc.q-onh = itemfg-loc.q-onh + fg-bin.qty.
        END.
    
        /*** itemfg.q-ono from jobs and purchase orders***/
        RUN fg/calcqool.p (ROWID(itemfg), itemfg-loc.loc, OUTPUT itemfg-loc.q-ono).
    
        /*** itemfg.q-alloc & itemfg.q-back from customer orders ***/
        RUN fg/calcqabl.p (ROWID(itemfg), itemfg-loc.loc, OUTPUT itemfg-loc.q-alloc, OUTPUT itemfg-loc.q-back).
    
        ASSIGN
            itemfg-loc.q-avail = itemfg-loc.q-onh +
                                (IF oereordr-cha EQ "XOnOrder" THEN 0 ELSE itemfg-loc.q-ono) -
                                itemfg-loc.q-alloc.
    END.

    /* This section needed because q-alloc was not being calculated */
    FOR EACH fg-set NO-LOCK WHERE
        fg-set.company eq itemfg.company AND
        fg-set.part-no eq itemfg.i-no,
        FIRST b-itemfg NO-LOCK WHERE
            b-itemfg.company eq itemfg.company AND
            b-itemfg.i-no    eq fg-set.set-no AND
            b-itemfg.isaset  eq YES,
        EACH b-itemfg-loc NO-LOCK WHERE
            b-itemfg-loc.company EQ b-itemfg.company AND 
            b-itemfg-loc.i-no    EQ b-itemfg.i-no,
            FIRST itemfg-loc EXCLUSIVE WHERE 
                itemfg-loc.company EQ b-itemfg.company AND 
                itemfg-loc.i-no    EQ itemfg.i-no AND 
                itemfg-loc.loc     EQ b-itemfg-loc.loc:
        ASSIGN
            itemfg-loc.q-alloc = 0.
        /* 06111209 */
        {sys/inc/part-qty.i v-part-qty fg-set}
        ASSIGN
            v-part-qty = (IF AVAIL eb AND eb.spare-char-2 = "Y" THEN 1.0 ELSE v-part-qty)
            itemfg-loc.q-alloc = itemfg-loc.q-alloc + (b-itemfg-loc.q-alloc * v-part-qty).
    END.

    
