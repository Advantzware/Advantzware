/* fgXMLDefs.i */

DEFINE TEMP-TABLE FGReceiptRow LIKE fg-rctd
    FIELD tableRowid AS ROWID.

DEFINE TEMP-TABLE tt-rcpth LIKE fg-rcpth.
DEFINE TEMP-TABLE tt-rdtlh LIKE fg-rdtlh.

DEFINE TEMP-TABLE w-fg-rctd NO-UNDO LIKE fg-rctd
    FIELD row-id   AS ROWID
    FIELD has-rec  AS LOGICAL INITIAL NO
    FIELD invoiced AS LOGICAL INITIAL NO
    .

DEFINE TEMP-TABLE tt-email NO-UNDO
    FIELD tt-recid   AS RECID
    FIELD job-no   LIKE job-hdr.job-no
    FIELD job-no2  LIKE job-hdr.job-no2
    FIELD i-no     LIKE itemfg.i-no
    FIELD qty        AS INTEGER
    FIELD cust-no    AS CHARACTER
        INDEX tt-cust IS PRIMARY cust-no DESCENDING
    .

{pc/pcprdd4u.i NEW}
{fg/invrecpt.i NEW}
{jc/jcgl-sh.i  NEW}
{fg/fullset.i  NEW}
{fg/fg-post3.i NEW}

DEFINE VARIABLE cFGPostGL  AS CHARACTER NO-UNDO.                        
DEFINE VARIABLE dtPostDate AS DATE      NO-UNDO INITIAL TODAY.
