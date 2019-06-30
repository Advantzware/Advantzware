/*------------------------------------------------------------------------
    File        ViewRfq.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Request for Quote Detail

    Author(s)   : Jyoti Bajaj
    Created     : march 06 2008
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttViewRfq NO-UNDO
    FIELD rfq_no LIKE rfq.rfq-no
     FIELD req_date LIKE rfq.req-date
     FIELD due_date LIKE rfq.due-date
     FIELD cust_no LIKE rfq.cust-no
     FIELD ship_name LIKE rfq.ship-name
     FIELD shipAddr  LIKE rfq.ship-addr[1]
     FIELD shipAddr2 LIKE rfq.ship-addr[2]
     FIELD ship_city LIKE rfq.ship-city
     FIELD ship_state LIKE rfq.ship-state
     FIELD ship_zip LIKE rfq.ship-zip 
     FIELD sman LIKE rfq.sman 
     FIELD smanName LIKE sman.sname
     FIELD comm LIKE rfq.comm
     FIELD fob_code LIKE rfq.fob-code
     FIELD chg_method LIKE rfq.chg-method
     FIELD wh_month LIKE rfq.wh-month
     FIELD inst LIKE rfq.inst
     FIELD VRowid AS RECID.
        .
  

DEFINE DATASET dsViewRfq FOR ttViewRfq.
DEFINE QUERY q-ViewRfqQuery FOR ttViewRfq.

DEFINE DATA-SOURCE src-ViewRfq  FOR QUERY q-ViewRfqQuery.

BUFFER ttViewRfq :ATTACH-DATA-SOURCE(DATA-SOURCE src-ViewRfq  :HANDLE).




