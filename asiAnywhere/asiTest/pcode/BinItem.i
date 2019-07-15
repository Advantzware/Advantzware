
/*------------------------------------------------------------------------
    File        BinItem.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Order ItemJoB Detail

    Author(s)   : Jyoti Bajaj
    Created     : Aug 27 2007
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttItemJob NO-UNDO 
    BEFORE-TABLE beforeItemJob
    FIELD job-no-disp as char
    FIELD job-no like job-hdr.job-no
    FIELD job-no2 like job-hdr.job-no2
    FIELD i-no like job-hdr.i-no
    FIELD i-name like itemfg.i-name
    FIELD cust-no LIKE fg-bin.cust-no
    FIELD cust like itemfg.cust-no
    FIELD j-no like job-hdr.j-no
    FIELD loc like fg-bin.loc
    FIELD loc-bin like fg-bin.loc-bin
    FIELD tag like fg-bin.tag
    FIELD cases AS INT
    FIELD case-count like fg-bin.case-count
    FIELD cases-unit like fg-bin.cases-unit
    FIELD qty as int format "->>>,>>9"
    FIELD std-tot-cost like  job-hdr.std-tot-cost
    FIELD std-mat-cost like  job-hdr.std-mat-cost
    FIELD std-lab-cost like  job-hdr.std-lab-cost
    FIELD std-var-cost like  job-hdr.std-var-cost
    FIELD std-fix-cost like  job-hdr.std-fix-cost
    FIELD last-cost like fg-bin.last-cost
    FIELD sell-uom like itemfg.sell-uom
    FIELD partial-count LIKE fg-bin.partial-count
    FIELD q-ono LIKE itemfg.q-ono
    FIELD q-onh LIKE itemfg.q-onh
    FIELD q-alloc LIKE itemfg.q-alloc
    FIELD q-back LIKE itemfg.q-back
    FIELD q-avail LIKE itemfg.q-avail
    FIELD ord-level LIKE itemfg.ord-level
    FIELD ord-min LIKE itemfg.ord-min
    FIELD ord-max LIKE itemfg.ord-max
    FIELD bol-qty AS DECIMAL FORMAT "-9999999999.99"
    FIELD rel-qty AS DECIMAL FORMAT "-9999999999.99"
    FIELD avl-qty AS DECIMAL FORMAT "-9999999999.99"
. 
DEFINE DATASET dsItemJob FOR ttItemJob .

DEFINE QUERY q-ItemJobQuery FOR ttItemJob.

DEFINE DATA-SOURCE src-ItemJob  FOR QUERY q-ItemJobQuery.

BUFFER ttItemJob :ATTACH-DATA-SOURCE(DATA-SOURCE src-ItemJob  :HANDLE).
