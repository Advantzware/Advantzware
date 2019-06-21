


DEFINE VARIABLE fi_cust-part AS CHARACTER FORMAT "X(256)" NO-UNDO.
DEFINE TEMP-TABLE ttViewPo NO-UNDO
BEFORE-TABLE beforeViewPo
        FIELD po-no       AS INTEGER FORMAT ">>>>>9"
        FIELD po-date     AS DATE    FORMAT "99/99/9999" 
        FIELD TYPE        AS CHAR    FORMAT "x"
        FIELD stat        AS CHAR    FORMAT "x"
        FIELD vend-no     AS CHAR    FORMAT "x(8)" 
        FIELD vend-name   AS CHAR    FORMAT "x(30)"  
        FIELD Vaddr1      AS CHAR    FORMAT "x(30)"   
        FIELD Vaddr2      AS CHAR    FORMAT "x(30)"   
        FIELD Vcity       AS CHAR    FORMAT "x(16)"   
        FIELD Vstate      AS CHAR    FORMAT "x(2)"    
        FIELD Vzip        AS CHAR    FORMAT "xxxxx-xxxx" INITIAL "00000-0000"
        FIELD VArea-Code  AS CHAR    FORMAT "(999)" 
        FIELD VPhone      AS CHAR    FORMAT "999-9999" 
        FIELD ship-id     AS CHAR    FORMAT "x(8)"  
        FIELD ship-name   AS CHAR    FORMAT "x(30)"  
        FIELD SAddr1      AS CHAR    FORMAT "x(30)"  
        FIELD SAddr2      AS CHAR    FORMAT "x(30)"  
        FIELD SCity       AS CHAR    FORMAT "x(16)"      
        FIELD SState      AS CHAR    FORMAT "x(2)"       
        FIELD SZip        AS CHAR    FORMAT "xxxxx-xxxx" 
        FIELD buyer       AS CHAR    FORMAT "x(10)"
        FIELD contact     AS CHAR    FORMAT "x(25)"
        FIELD due-date    AS DATE    FORMAT "99/99/9999" 
        FIELD last-date   AS DATE    FORMAT "99/99/9999"
        FIELD under-pct   AS DECIMAL FORMAT "->>9.99%"
        FIELD over-pct    AS DECIMAL FORMAT "->>9.99%"
        FIELD carrier     AS CHAR    FORMAT "x(5)"
        FIELD tax-gr      AS CHAR    FORMAT "x(3)"
        FIELD terms       AS CHAR    FORMAT "x(5)"
        FIELD frt-pay     AS CHAR    FORMAT "x(1)"
        FIELD fob-code    AS CHAR    FORMAT "x(5)"
        FIELD t-freight   AS DECIMAL FORMAT "->>,>>9.99"
        FIELD tax         AS DECIMAL FORMAT "->,>>9.99"
        FIELD t-cost      AS DECIMAL FORMAT "->,>>>,>>9.99<<"
        FIELD i-no        AS CHAR    FORMAT "x(15)"
        FIELD i-name      AS CHAR    FORMAT "x(30)"
        FIELD cust-no     AS CHAR    FORMAT "x(8)"
        FIELD part        AS CHAR
        FIELD due-date1   AS DATE    FORMAT "99/99/9999" 
        FIELD ord-qty     AS DECIMAL FORMAT "->>>,>>>,>>9.9<<<<<"
        FIELD cost        AS DECIMAL FORMAT "->,>>>,>>9.99<<<<<"
        FIELD tot-cost    AS DECIMAL FORMAT "->>>,>>9.99<<"
        FIELD job-no      AS CHAR    FORMAT "x(6)"   
        FIELD job-no2     AS INTEGER FORMAT "99"
        FIELD s-num       AS INTEGER FORMAT "99"
    .
DEFINE DATASET dsViewPo FOR ttViewPo .
DEFINE QUERY q-ViewPoQuery FOR ttViewPo.
DEFINE DATA-SOURCE src-ViewPo  FOR QUERY q-ViewPoQuery.
BUFFER ttViewPo :ATTACH-DATA-SOURCE(DATA-SOURCE src-ViewPo  :HANDLE).



/*********************************/

PROCEDURE local-display-fields :
  fi_cust-part = "".

  IF AVAIL po-ordl THEN DO:
    
    IF NOT po-ordl.item-type THEN
    FIND FIRST itemfg
        WHERE itemfg.company EQ po-ordl.company
          AND itemfg.i-no    EQ po-ordl.i-no
        NO-LOCK NO-ERROR.

    IF AVAIL itemfg THEN fi_cust-part = itemfg.part-no.

    ELSE DO:
      FIND FIRST item
          WHERE item.company EQ po-ordl.company
            AND item.i-no    EQ po-ordl.i-no
          NO-LOCK NO-ERROR.
      IF AVAIL ITEM THEN
      FOR EACH job-hdr
          WHERE job-hdr.company EQ po-ordl.company
            AND job-hdr.job-no  EQ po-ordl.job-no
            AND job-hdr.job-no2 EQ po-ordl.job-no2
          NO-LOCK
          BY job-hdr.frm DESC:
        fi_cust-part = job-hdr.i-no.
        IF job-hdr.frm EQ po-ordl.s-num THEN LEAVE.
      END.
    END.
  END.


END PROCEDURE.

