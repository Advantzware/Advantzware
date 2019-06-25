
/*------------------------------------------------------------------------
    File        : list_rfqs.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Rfq Maintenance

    Author(s)   : Sewa Singh
    Created     : Tue Feb 12 2008
    Notes       :
  ----------------------------------------------------------------------*/
 
/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttListRfqs NO-UNDO 
    FIELD vRfqNo LIKE rfq.rfq-no FORMAT "->,>>>,>>9"
    FIELD prmPartno LIKE rfqitem.part-no FORMAT "x(15)"
    FIELD prmCust LIKE rfq.cust-no FORMAT "X(8)"
    FIELD ReqDt LIKE rfq.req-date FORMAT "99/99/9999"
    FIELD vStyle LIKE rfqitem.style  FORMAT "x(4)"
    FIELD Procat LIKE rfqitem.procat FORMAT "x(5)"
    FIELD Qty LIKE rfqitem.qty[1]  FORMAT "->,>>>,>>9"
    FIELD iCol LIKE rfqitem.i-col FORMAT ">9"
    FIELD iCoat LIKE rfqitem.i-coat FORMAT ">9"
    FIELD vLength LIKE rfqitem.len
    FIELD Board LIKE rfqitem.board FORMAT "x(10)"
    FIELD vWidth LIKE rfqitem.wid
    FIELD vCal LIKE rfqitem.cal FORMAT "9.99999"
    FIELD vDepth LIKE rfqitem.dep
    FIELD prmEst LIKE  rfqitem.est-no FORMAT "x(8)"
    FIELD vPartDscr  LIKE rfqitem.part-dscr1 FORMAT "x(60)"
    FIELD SeqNo  LIKE rfqitem.seq
    
    .
        
DEFINE DATASET dsListRfqs FOR ttListRfqs.
DEFINE QUERY q-ListRfqsQuery FOR ttListRfqs.
DEFINE DATA-SOURCE src-ListRfqs  FOR QUERY q-ListRfqsQuery.
BUFFER ttListRfqs :ATTACH-DATA-SOURCE(DATA-SOURCE src-ListRfqs  :HANDLE).
