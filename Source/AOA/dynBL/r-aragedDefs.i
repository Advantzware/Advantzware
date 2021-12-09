/* r-aragedDefs.i - used in AOA/dynBL/r-araged.p & AOA/dynBL/agedtot.p */

/* Cash Receipt By SalesRep Name.rpa */
{AOA/tempTable/ttAgedReceivables.i}
/* Aged Receivables Totals.rpa */
{AOA/tempTable/ttAgedReceivablesTotals.i}

{sys/ref/CustList.i NEW}

DEFINE TEMP-TABLE tt-cust NO-UNDO
    FIELD curr-code LIKE cust.curr-code
    FIELD sorter    LIKE cust.cust-no
    FIELD classID   AS   INTEGER
    FIELD row-id    AS   ROWID
        INDEX tt-cust curr-code sorter
        .
DEFINE TEMP-TABLE tt-inv NO-UNDO
    FIELD sorter LIKE ar-inv.inv-no
    FIELD inv-no LIKE ar-inv.inv-no
    FIELD row-id AS   ROWID
        INDEX tt-inv sorter inv-no
        .
