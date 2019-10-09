
    DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

    {sys/inc/var.i SHARED}
    {fg/fullset.i}

    DEF WORKFILE tt-fg-set2 LIKE tt-fg-set.

    DEF VAR v-part-qty-dec LIKE tt-fg-set.part-qty-dec NO-UNDO.
    DEF VAR v-i-no LIKE itemfg.i-no NO-UNDO.

    FOR EACH tt-fg-set:
        DELETE tt-fg-set.
    END.

    FIND itemfg NO-LOCK WHERE 
        ROWID(itemfg) EQ ip-rowid
        NO-ERROR.

    IF AVAIL itemfg THEN DO:
        FIND FIRST fg-set NO-LOCK WHERE 
            fg-set.company EQ cocode AND 
            fg-set.set-no  EQ itemfg.i-no AND 
            fg-set.part-no EQ itemfg.i-no
            NO-ERROR.

        IF AVAIL fg-set THEN DO:
            {sys/inc/part-qty.i v-part-qty-dec fg-set}
        END.

        CREATE tt-fg-set.
        ASSIGN
            tt-fg-set.part-no      = itemfg.i-no
            tt-fg-set.qtyPerSet     = IF AVAIL fg-set THEN fg-set.qtyPerSet ELSE 1
            tt-fg-set.part-qty-dec = IF AVAIL fg-set THEN v-part-qty-dec ELSE 1
            tt-fg-set.isaset       = NOT AVAIL fg-set
            tt-fg-set.alloc        = NOT AVAIL fg-set.
    END.

    DO WHILE CAN-FIND(FIRST tt-fg-set WHERE 
                        tt-fg-set.set-no NE tt-fg-set.part-no AND 
                        tt-fg-set.isaset AND tt-fg-set.alloc):
        FOR EACH tt-fg-set WHERE 
            tt-fg-set.isaset AND tt-fg-set.alloc:
            FOR EACH fg-set NO-LOCK WHERE 
                fg-set.company EQ cocode AND 
                fg-set.set-no  EQ tt-fg-set.part-no AND 
                fg-set.set-no  NE fg-set.part-no,
                FIRST itemfg NO-LOCK WHERE 
                    itemfg.company EQ cocode AND 
                    itemfg.i-no    EQ fg-set.part-no:

                {sys/inc/part-qty.i v-part-qty-dec fg-set}

                CREATE tt-fg-set2.
                BUFFER-COPY fg-set TO tt-fg-set2
                ASSIGN
                    tt-fg-set2.qtyPerSet     = tt-fg-set.qtyPerSet * fg-set.qtyPerSet
                    tt-fg-set2.part-qty-dec = tt-fg-set.part-qty-dec * v-part-qty-dec
                    tt-fg-set2.isaset       = itemfg.isaset
                    tt-fg-set2.alloc        = itemfg.alloc.
            END.
            DELETE tt-fg-set.
        END.

        FOR EACH tt-fg-set2:
            CREATE tt-fg-set.
            BUFFER-COPY tt-fg-set2 TO tt-fg-set.
            DELETE tt-fg-set2.
        END.
    END.

    FOR EACH tt-fg-set 
        BREAK BY tt-fg-set.part-no:
        IF FIRST-OF(tt-fg-set.part-no) THEN ASSIGN
            v-part-qty-dec = 0.
        ASSIGN
            v-part-qty-dec = v-part-qty-dec + tt-fg-set.part-qty-dec.
        IF LAST-OF(tt-fg-set.part-no) THEN ASSIGN
            tt-fg-set.part-qty-dec = v-part-qty-dec.
        ELSE 
            DELETE tt-fg-set.
    END.
