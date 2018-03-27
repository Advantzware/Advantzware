/* r-aragedDefs.i - used in aoa/BL/r-araged.p & aoa/BL/agedtot.p */

/* Cash Receipt By SalesRep Name.rpa */
{aoa/tempTable/ttAgedReceivables.i}
/* Aged Receivables Totals.rpa */
{aoa/tempTable/ttAgedReceivablesTotals.i}

{sys/ref/CustList.i NEW}

DEFINE TEMP-TABLE tt-cust NO-UNDO
    FIELD curr-code LIKE cust.curr-code
    FIELD sorter    LIKE cust.cust-no
    FIELD row-id    AS   ROWID
        INDEX tt-cust curr-code sorter
        .


DEFINE TEMP-TABLE tt-factored
    FIELD company LIKE itemfg.company
    FIELD i-no    LIKE itemfg.i-no
    FIELD x-no    LIKE ar-invl.x-no
        INDEX i1 i-no
        INDEX i2 x-no
        .

DEFINE TEMP-TABLE tt-inv NO-UNDO
    FIELD sorter LIKE ar-inv.inv-no
    FIELD inv-no LIKE ar-inv.inv-no
    FIELD row-id AS   ROWID
        INDEX tt-inv sorter inv-no
        .
