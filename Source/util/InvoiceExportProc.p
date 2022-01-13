DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcBeginInvoice AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcEndInvoice AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcBeginInvoiceDate AS DATE NO-UNDO.
DEFINE INPUT PARAMETER ipcEndInvoiceDate AS DATE NO-UNDO.
DEFINE INPUT PARAMETER ipFilePath AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE ttInvoiceExport
    FIELD record_no  		    LIKE ar-invl.rec_key LABEL "Record No"
    FIELD cust_id  		        LIKE ar-invl.cust-no
    FIELD company_name  		LIKE ar-invl.company
    FIELD po_no  		        LIKE ar-invl.po-no
    FIELD invoice_no  		    LIKE ar-invl.inv-no
    FIELD line_no  		        LIKE ar-invl.LINE
    FIELD invoice_date  		AS CHARACTER LABEL "Invoice Date"
    FIELD InvoiceStatus  		AS CHARACTER  LABEL "Status"
    FIELD Salesman1  		    AS CHARACTER FORMAT "x(3)" 
    FIELD Salesman2  		    AS CHARACTER FORMAT "x(3)" 
    FIELD salesman1_name  		AS CHARACTER LABEL "Salesman1 Name" FORMAT "x(20)" 
    FIELD salesman2_name  		AS CHARACTER LABEL "Salesman2 Name" FORMAT "x(20)" 
    FIELD salesman1_percentage  AS CHARACTER LABEL "Salesman1 Percentage" 
    FIELD salesman2_percentage  AS CHARACTER LABEL "Salesman2 Percentage"  
    FIELD identifier  		    AS CHARACTER LABEL "Identifier" FORMAT "X"
    FIELD DESCRIPTION  		    LIKE ar-invl.i-dscr LABEL "Description"
    FIELD unit_price  		    LIKE ar-invl.unit-pr LABEL "Unit Price"
    FIELD uom  		            LIKE ar-invl.pr-qty-uom LABEL "UOM"
    FIELD extended_price  		LIKE ar-invl.amt LABEL "Extended Price"
    FIELD qty_ordered  		    LIKE ar-invl.qty LABEL "Ordered Quantity"
    FIELD qty_shipped  		    LIKE ar-invl.ship-qty
    FIELD qty_invoiced  		LIKE ar-invl.inv-qty
    FIELD prod_type  		    AS CHARACTER LABEL "Prod. Type" FORMAT "x(3)"
    FIELD id_type  		        AS CHARACTER LABEL "Id Type" FORMAT "x(3)"
    FIELD addr_line_1  		    AS CHARACTER LABEL "ShipTo Address1" FORMAT "x(30)"
    FIELD addr_line_2  		    AS CHARACTER LABEL "ShipTo Address2" FORMAT "x(30)"
    FIELD addr_line_3  		    AS CHARACTER LABEL "ShipTo Address3" FORMAT "x(30)"
    FIELD city  		        LIKE shipto.ship-city
    FIELD state  		        LIKE shipto.ship-state
    FIELD zip  		            LIKE shipto.ship-zip
    FIELD country  		        LIKE shipto.country
    FIELD amount  		        AS CHARACTER LABEL "Total Amount"
    FIELD freight  		        AS CHARACTER LABEL "Freight"
    FIELD freight_adj  		    AS CHARACTER LABEL "Freight Adj."
    .

DEFINE VARIABLE hdOutput     AS HANDLE    NO-UNDO.
DEFINE VARIABLE lError       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage     AS CHARACTER NO-UNDO.

RUN system\OutputProcs.p PERSISTENT SET hdOutput.

{sys/inc/var.i new shared }

DEFINE BUFFER bf-ar-inv  FOR ar-inv.
DEFINE BUFFER bf-ar-invl FOR ar-invl.
DEFINE BUFFER bf-shipto  FOR shipto.
DEFINE BUFFER bf-item    FOR ITEM.
DEFINE BUFFER bf-itemfg  FOR itemfg.

ASSIGN
    cocode = ipcCompany.
    
FOR EACH bf-ar-inv NO-LOCK WHERE 
            bf-ar-inv.company EQ cocode AND 
            bf-ar-inv.inv-no GE INT(ipcBeginInvoice) AND 
            bf-ar-inv.inv-no LE INT(ipcEndInvoice) AND 
            bf-ar-inv.inv-date GE ipcBeginInvoiceDate AND 
            bf-ar-inv.inv-date LE ipcEndInvoiceDate,
            EACH bf-ar-invl NO-LOCK WHERE 
            bf-ar-invl.company EQ bf-ar-inv.company AND 
            bf-ar-invl.inv-no EQ bf-ar-inv.inv-no AND
            bf-ar-invl.x-no EQ bf-ar-inv.x-no  
            BREAK BY bf-ar-inv.inv-no DESCENDING 
            BY bf-ar-invl.line:
            
            CREATE ttInvoiceExport.
            ASSIGN 
                ttInvoiceExport.record_no             = bf-ar-invl.rec_key
                ttInvoiceExport.cust_id               = bf-ar-invl.cust-no
                ttInvoiceExport.company_name          = bf-ar-invl.company
                ttInvoiceExport.po_no                 = bf-ar-invl.po-no
                ttInvoiceExport.invoice_no            = bf-ar-invl.inv-no
                ttInvoiceExport.line_no               = bf-ar-invl.LINE
                ttInvoiceExport.invoice_date          = IF bf-ar-invl.inv-date NE ? THEN STRING(bf-ar-invl.inv-date) ELSE ""
                ttInvoiceExport.InvoiceStatus         = bf-ar-inv.stat      
                ttInvoiceExport.Salesman1             = bf-ar-invl.sman[1]
                ttInvoiceExport.Salesman2             = bf-ar-invl.sman[2]
                ttInvoiceExport.salesman1_name        = bf-ar-invl.sname[1]
                ttInvoiceExport.salesman2_name        = bf-ar-invl.sname[2]
                ttInvoiceExport.salesman1_percentage  = STRING(bf-ar-invl.s-pct[1],">>9.99")   
                ttInvoiceExport.salesman2_percentage  = STRING(bf-ar-invl.s-pct[2],">>9.99")
                 
                ttInvoiceExport.identifier            = bf-ar-invl.i-no
                ttInvoiceExport.unit_price            = bf-ar-invl.unit-pr  
                ttInvoiceExport.uom                   = IF bf-ar-invl.pr-qty-uom NE "" THEN bf-ar-invl.pr-qty-uom ELSE "EA"
                ttInvoiceExport.extended_price        = bf-ar-invl.amt
                ttInvoiceExport.qty_ordered           = bf-ar-invl.qty       
                ttInvoiceExport.qty_shipped           = bf-ar-invl.ship-qty
                ttInvoiceExport.qty_invoiced          = bf-ar-invl.inv-qty
                ttInvoiceExport.id_type               = IF bf-ar-invl.misc THEN "gen" 
                                                        ELSE IF bf-ar-invl.billable THEN "frt" 
                                                        ELSE "itm" 
                .
            
            IF FIRST-OF(bf-ar-inv.inv-no) THEN
            DO:
                ASSIGN
                    ttInvoiceExport.freight           = IF bf-ar-inv.f-bill THEN STRING(bf-ar-inv.freight) ELSE ""
                    ttInvoiceExport.freight_adj       = IF NOT bf-ar-inv.f-bill THEN STRING(bf-ar-inv.freight) ELSE ""
                    ttInvoiceExport.amount            = STRING(bf-ar-inv.gross)
                    .
            END.
                
            
            FIND FIRST bf-item NO-LOCK
                WHERE bf-item.company EQ bf-ar-invl.company
                AND bf-item.i-no      EQ bf-ar-invl.i-no
                NO-ERROR.
            IF AVAIL bf-item THEN
            DO:
               ASSIGN
                   ttInvoiceExport.prod_type  =  IF bf-item.industry EQ "1" THEN "FCI" 
                                                 ELSE IF bf-item.industry EQ "2" 
                                                     AND bf-item.procat EQ "DIE" THEN "DIE"
                                                 ELSE "".
            END.
            
            FIND FIRST bf-itemfg
                WHERE bf-itemfg.company EQ bf-ar-invl.company
                AND bf-itemfg.i-no      EQ bf-ar-invl.i-no
                NO-LOCK NO-ERROR.
            
            ttInvoiceExport.DESCRIPTION  = IF bf-ar-invl.i-dscr NE "" THEN bf-ar-invl.i-dscr
                                           ELSE IF AVAIL bf-itemfg AND bf-itemfg.part-dscr1 NE "" THEN bf-itemfg.part-dscr1
                                           ELSE IF AVAIL bf-item AND bf-item.i-dscr NE "" THEN bf-item.i-dscr
                                           ELSE "".
            
            FIND FIRST bf-shipto NO-LOCK
                WHERE bf-shipto.company = bf-ar-invl.company
                AND bf-shipto.cust-no   = bf-ar-invl.cust-no
                AND bf-shipto.ship-id   = bf-ar-inv.ship-id
                NO-ERROR.
            IF AVAIL bf-shipto THEN
            DO:
                ASSIGN
                    ttInvoiceExport.addr_line_1 = bf-shipto.ship-addr[1]
                    ttInvoiceExport.addr_line_2 = bf-shipto.ship-addr[2]
                    ttInvoiceExport.addr_line_3 = ""
                    ttInvoiceExport.city        = bf-shipto.ship-city
                    ttInvoiceExport.state       = bf-shipto.ship-state
                    ttInvoiceExport.zip         = bf-shipto.ship-zip
                    ttInvoiceExport.country     = IF bf-shipto.country NE "" THEN bf-shipto.country ELSE "US"
                    .
            END. /* IF AVAIL shipto */
            
END. /*  FOR EACH bf-ar-inv */


RUN Output_TempTableToCSV IN hdOutput (TEMP-TABLE ttInvoiceExport:HANDLE, ipFilePath ,YES,YES, OUTPUT lError, OUTPUT cMessage).

THIS-PROCEDURE:REMOVE-SUPER-PROCEDURE (hdOutput).
DELETE OBJECT hdOutput. 
