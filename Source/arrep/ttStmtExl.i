/* -------------------------------------------------- sys/inc/ttStmtExl.i 10/04/2020  */
/*                                                                            */
/* vars & constants include file                                              */
/*                                                                            */
/* -------------------------------------------------------------------------- */

DEF {1} shared temp-table tt-inv no-undo
  field inv-date as date
  field sort-fld as char
  field trans-date as date
  field inv-no like ar-inv.inv-no FORMAT ">>>>>>9"
  field type as char format 'x(4)'
  field description as char format 'x(15)'
  field amount  as dec format '->>,>>>,>>>.99'
  FIELD inv-amt LIKE ar-inv.gross
  FIELD cust-no AS CHAR
  FIELD po-no LIKE ar-invl.po-no
  FIELD bol-no AS CHAR 
  FIELD old-day AS INT
  FIELD check-no AS CHARACTER 
  FIELD charge AS DECIMAL 
  FIELD credits AS DECIMAL 
  index tt-inv cust-no inv-date sort-fld trans-date.

DEF {1} shared TEMP-TABLE tt-cust-excel NO-UNDO
    FIELD cust-no AS CHAR
    FIELD contact AS CHAR
    FIELD addr    AS CHAR EXTENT 5
    FIELD aged    AS DEC EXTENT 5
    FIELD terms   AS CHARACTER 
    INDEX excel cust-no ASC.


/* end ---------------------------------- copr. 2001  advanced software, inc. */

