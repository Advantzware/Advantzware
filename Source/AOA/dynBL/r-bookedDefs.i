/* AOA/dynBL/r-bookedDefs.i - used in AOA/dynBL/r-booked.p & AOA/dynBL/recappc.p */
/*            used in AOA/dynBL/r-booked.p & AOA/dynBL/recappc.p */

{sys/ref/CustList.i NEW}

DEFINE TEMP-TABLE w-data NO-UNDO
    FIELD ord-no     LIKE oe-ord.ord-no
    FIELD line       LIKE oe-ordl.line
    FIELD sman         AS CHARACTER 
    FIELD item-n     LIKE itemfg.i-name 
    FIELD proCat     LIKE itemfg.proCat 
    FIELD qty        LIKE oe-ordl.qty   
    FIELD sqft       LIKE itemfg.t-sqft 
    FIELD t-sqft     LIKE itemfg.t-sqft 
    FIELD t-tons       AS DECIMAL         
    FIELD price      LIKE oe-ordl.price 
    FIELD revenue    LIKE oe-ordl.t-price 
    FIELD misc         AS LOGICAL
    FIELD cost         AS DECIMAL
    FIELD comm         AS DECIMAL 
    FIELD margin       AS DECIMAL
    FIELD shp-qty    LIKE oe-ordl.ship-qty  
    FIELD cShip-from LIKE oe-rel.spare-char-1
    . 

DEFINE TEMP-TABLE tt-report NO-UNDO LIKE report
    FIELD inv-no       AS   INTEGER 
    FIELD chk-inv      AS   LOGICAL INITIAL YES
    FIELD q-onh        LIKE itemfg.q-onh
    FIELD q-shp        LIKE itemfg.q-onh
    FIELD q-rel        LIKE itemfg.q-onh
    FIELD q-wip        LIKE itemfg.q-onh
    FIELD q-avl        LIKE itemfg.q-onh
    FIELD po-no        LIKE oe-ord.po-no
    FIELD inv          AS   LOGICAL
    FIELD cad-no       LIKE itemfg.cad-no
    FIELD row-id       AS   ROWID 
    FIELD due-date     LIKE oe-ordl.req-date
    FIELD unit-count   LIKE eb.cas-cnt
    FIELD units-pallet LIKE eb.cas-pal
    .
