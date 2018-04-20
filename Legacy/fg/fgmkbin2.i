
    IF INDEX("RACT",fg-rcpth.rita-code) GT 0 THEN DO:
        IF fg-rdtlh.qty-case GT 0
        AND (fg-bin.case-count LE 0 OR fg-bin.qty EQ 0 OR INDEX("AC",fg-rcpth.rita-code) GT 0) THEN ASSIGN
            fg-bin.case-count = fg-rdtlh.qty-case.
        IF fg-rdtlh.units-pallet GT 0
        AND (fg-bin.units-pallet LE 0 OR fg-bin.qty EQ 0 OR INDEX("AC",fg-rcpth.rita-code) GT 0) THEN ASSIGN
            fg-bin.units-pallet = fg-rdtlh.units-pallet.
        IF fg-rdtlh.stacks-unit GT 0 
        AND (fg-bin.cases-unit LE 0 OR fg-bin.qty EQ 0 OR INDEX("AC",fg-rcpth.rita-code) GT 0) THEN ASSIGN
            fg-bin.cases-unit = fg-rdtlh.stacks-unit.
    END.

    IF fg-rdtlh.qty-case EQ 0 THEN ASSIGN
        fg-rdtlh.qty-case = fg-bin.case-count.

    IF fg-rcpth.trans-date LT 11/01/2005 
    AND fg-rdtlh.rita-code EQ "S"
    AND fg-rdtlh.cases EQ 0
    AND fg-rdtlh.qty NE 0 THEN ASSIGN
        fg-rdtlh.cases = TRUNC(fg-rdtlh.qty / fg-rdtlh.qty-case,0).

    {fg/fg-mkbin.i}

    IF fg-bin.qty EQ 0 THEN ASSIGN
        fg-bin.cases         = 0
        fg-bin.partial-count = 0
        fg-bin.partial-total = 0.
    ELSE DO:
        IF fg-bin.case-count GT 0 THEN DO:
            li = TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0).
            IF fg-bin.partial-count LT 0
            AND fg-bin.qty GT fg-bin.partial-count * -1 THEN ASSIGN
                li = TRUNC(fg-bin.qty / fg-bin.case-count,0).
            ASSIGN
                fg-bin.partial-count = fg-bin.qty - (li * fg-bin.case-count).
        END.

        IF fg-bin.case-count LE 0
        OR (fg-bin.qty LT fg-bin.case-count AND fg-bin.qty GT 0) THEN ASSIGN
            fg-bin.case-count    = fg-bin.qty
            fg-bin.partial-count = 0
            fg-bin.partial-total = 0.
    END.
