
DEF {1} SHARED WORKFILE tt-fg-set LIKE fg-set
    FIELD isaset LIKE itemfg.isaset
    FIELD alloc  LIKE itemfg.alloc
    FIELD part-qty-dec AS DEC.
