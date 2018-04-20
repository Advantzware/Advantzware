/* -------------------------------------------------- fg/fg-reset.p 11/97 JLF */
/* itemfg reset                                                               */
/* -------------------------------------------------------------------------- */

    {sys/inc/fg-reset.i "itemfg."}

    {fg/fullset.i NEW}

    IF itemfg.isaset THEN DO:
        RUN fg/fullset.p (ROWID(itemfg)).

        FOR EACH tt-fg-set,
            FIRST b-itemfg NO-LOCK WHERE 
                b-itemfg.company EQ cocode AND 
                b-itemfg.i-no    EQ tt-fg-set.part-no:

            RUN fg/fg-rst2.p (RECID(b-itemfg)).
        END.
    END.

