


/*------------------------------------------------------------------------
    File        : ListRfqItem.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Rfq Maintenance

    Author(s)   : Sewa Singh
    Created     : Sat Feb 16, 2008
    Notes       :
  ----------------------------------------------------------------------*/
 
/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttLisrRfqitem NO-UNDO 
    FIELD RfqSeqNo AS INTEGER  FORMAT ">9"
    FIELD RfqQty   AS INTEGER FORMAT "99999999"
    FIELD RfqStock AS CHARACTER  FORMAT "x(15)"
    FIELD RfqName  AS CHARACTER  FORMAT "x(30)"
    FIELD RfqPartno LIKE rfqitem.part-no FORMAT "x(15)"
    FIELD Rfqstyle AS CHARACTER  FORMAT "x(6)"
    FIELD RfqProcat AS CHARACTER  FORMAT "x(5)"
    FIELD RfqCol   AS INTEGER FORMAT ">9"
    FIELD RfqCoat  AS INTEGER FORMAT ">9"
    FIELD RfqLength LIKE rfqitem.len
    FIELD RfqWidth LIKE rfqitem.wid
    FIELD RfqDepth LIKE rfqitem.dep
    FIELD RfqBoard LIKE rfqitem.board FORMAT "x(10)"
    FIELD RfqCal LIKE rfqitem.cal FORMAT "9.99999"
    FIELD RfqQuantity AS INTEGER FORMAT "->,>>>,>>9":U
    FIELD RfqRowid AS RECID
    FIELD aRowid AS RECID
    FIELD lv_qty_2 AS INTEGER FORMAT "99999999"
    FIELD lv_qty_3 AS INTEGER FORMAT "99999999"
    FIELD lv_qty_4 AS INTEGER FORMAT "99999999"
    FIELD lv_qty_5  AS INTEGER FORMAT "99999999"
    FIELD lv_qty_6 AS INTEGER FORMAT "99999999"
    FIELD lv_qty_7 AS INTEGER FORMAT "99999999"
    FIELD lv_qty_8 AS INTEGER FORMAT "99999999"
    FIELD lv_qty_9 AS INTEGER FORMAT "99999999"
    FIELD lv_qty_10 AS INTEGER FORMAT "99999999"
    FIELD lv_price_1 LIKE rfqitem.qty-price[1]
    FIELD lv_price_2 LIKE rfqitem.qty-price[1]
    FIELD lv_price_3 LIKE rfqitem.qty-price[1]
    FIELD lv_price_4 LIKE rfqitem.qty-price[1]
    FIELD lv_price_5 LIKE rfqitem.qty-price[1]
    FIELD lv_price_6 LIKE rfqitem.qty-price[1]
    FIELD lv_price_7 LIKE rfqitem.qty-price[1]
    FIELD lv_price_8 LIKE rfqitem.qty-price[1]
    FIELD lv_price_9 LIKE rfqitem.qty-price[1]
    FIELD lv_price_10 LIKE rfqitem.qty-price[1]
    FIELD lv_uom_1 LIKE rfqitem.qty-uom[1]       
    FIELD lv_uom_2 LIKE rfqitem.qty-uom[2]       
    FIELD lv_uom_3 LIKE rfqitem.qty-uom[3]       
    FIELD lv_uom_4 LIKE rfqitem.qty-uom[4]       
    FIELD lv_uom_5 LIKE rfqitem.qty-uom[5]       
    FIELD lv_uom_6 LIKE rfqitem.qty-uom[6]       
    FIELD lv_uom_7 LIKE rfqitem.qty-uom[7]       
    FIELD lv_uom_8 LIKE rfqitem.qty-uom[8]       
    FIELD lv_uom_9 LIKE rfqitem.qty-uom[9]       
    FIELD lv_uom_10 LIKE rfqitem.qty-uom[10]     
    FIELD lv_date_1 LIKE rfqitem.qty-date[1]     
    FIELD lv_date_2 LIKE rfqitem.qty-date[2]     
    FIELD lv_date_3 LIKE rfqitem.qty-date[3]       
    FIELD lv_date_4 LIKE rfqitem.qty-date[4]    
    FIELD lv_date_5 LIKE rfqitem.qty-date[5]    
    FIELD lv_date_6 LIKE rfqitem.qty-date[6]    
    FIELD lv_date_7 LIKE rfqitem.qty-date[7]    
    FIELD lv_date_8 LIKE rfqitem.qty-date[8]    
    FIELD lv_date_9 LIKE rfqitem.qty-date[9]    
    FIELD lv_date_10 LIKE rfqitem.qty-date[10]
    FIELD lv_delivery_1 LIKE rfqitem.delivery[1]      
    FIELD lv_delivery_2 LIKE rfqitem.delivery[2]      
    FIELD lv_delivery_3 LIKE rfqitem.delivery[3]      
    FIELD lv_delivery_4 LIKE rfqitem.delivery[4]      
    FIELD lv_delivery_5 LIKE rfqitem.delivery[5]      
    FIELD lv_delivery_6 LIKE rfqitem.delivery[6]      
    FIELD lv_delivery_7 LIKE rfqitem.delivery[7]      
    FIELD lv_delivery_8 LIKE rfqitem.delivery[8]      
    FIELD lv_delivery_9 LIKE rfqitem.delivery[9]      
    FIELD lv_delivery_10 LIKE rfqitem.delivery[10] 
     
     .

DEFINE DATASET dsLisrRfqitem FOR ttLisrRfqitem.
DEFINE QUERY q-LisrRfqitemQuery FOR ttLisrRfqitem.
DEFINE DATA-SOURCE src-LisrRfqitem  FOR QUERY q-LisrRfqitemQuery.
BUFFER ttLisrRfqitem :ATTACH-DATA-SOURCE(DATA-SOURCE src-LisrRfqitem  :HANDLE).


