
    FOR EACH reftable NO-LOCK
        WHERE reftable.reftable EQ "oe-relh.can-print"
          AND reftable.company  EQ lv-foreachr,
        FIRST oe-relh WHERE oe-relh.rec_key EQ reftable.rec_key
