
/*------------------------------------------------------------------------
    File        : fgpostBatch.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Sat Sep 22 14:33:33 EDT 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE w-fg-rctd NO-UNDO LIKE fg-rctd
    FIELD row-id      AS ROWID
    FIELD has-rec     AS LOG       INIT NO
    FIELD invoiced    AS LOG       INIT NO
    FIELD old-tag     AS CHARACTER
    FIELD ret-loc     AS CHARACTER
    FIELD ret-loc-bin AS CHARACTER.
    
DEFINE TEMP-TABLE tt-fgemail NO-UNDO
    FIELD i-no      LIKE itemfg.i-no
    FIELD po-no     LIKE oe-ordl.po-no
    FIELD ord-no    LIKE oe-ordl.ord-no
    FIELD qty-rec   AS DEC
    FIELD recipient AS CHAR.

DEFINE TEMP-TABLE tt-email NO-UNDO 
    FIELD tt-recid AS RECID
    FIELD job-no   LIKE job-hdr.job-no
    FIELD job-no2  LIKE job-hdr.job-no2
    FIELD i-no     LIKE itemfg.i-no
    FIELD qty      AS INTEGER
    FIELD cust-no  AS cha
    INDEX tt-cust IS PRIMARY cust-no DESCENDING .
    
DEFINE TEMP-TABLE tt-inv LIKE w-inv.   


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
