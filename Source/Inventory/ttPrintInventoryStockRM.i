DEFINE TEMP-TABLE ttPrintInventoryStockRM NO-UNDO
    FIELDS tagID                 AS CHARACTER LABEL 'Tag#'                                                                  
    FIELDS acknowledgement       AS LOGICAL   LABEL 'Acknowledgement' FORMAT "Yes/No"                             
    FIELDS accountNo             AS CHARACTER LABEL 'Account No'                                                            
    FIELDS shipToAddress         AS CHARACTER LABEL 'Ship To Address' EXTENT 2
    FIELDS blankNo               AS INTEGER   LABEL 'Blank #' FORMAT ">9"                                 
    FIELDS billTo                AS CHARACTER LABEL 'Bill to'                                                               
    FIELDS buyer                 AS CHARACTER LABEL 'Buyer'                                                                 
    FIELDS shippingCarrier       AS CHARACTER LABEL 'Shipping Carrier'                                                      
    FIELDS city                  AS CHARACTER LABEL 'City'                                                                  
    FIELDS company               AS CHARACTER LABEL 'Company'                                                               
    FIELDS companyName           AS CHARACTER LABEL 'Company Name'                                                          
    FIELDS costs                 AS DECIMAL   LABEL 'Costs' FORMAT "->>,>>9.99<<<<"                     
    FIELDS consumptionQuantity   AS DECIMAL   LABEL 'Consumption Quantity' FORMAT "->>,>>>,>>9.9<<<<<"                 
    FIELDS uom                   AS CHARACTER LABEL 'Unit of Measure'                                                       
    FIELDS contact               AS CHARACTER LABEL 'Contact'                                                               
    FIELDS unitCost              AS DECIMAL   LABEL 'Unit Cost' FORMAT "->>>,>>9.99<<<<"                    
    FIELDS currencyCode          AS CHARACTER LABEL 'Currency Code' EXTENT 2
    FIELDS customerNumber        AS CHARACTER LABEL 'Customer Number'                                                       
    FIELDS deleted               AS LOGICAL   LABEL 'Deleted?' FORMAT "Y/N"                                
    FIELDS discount              AS DECIMAL   LABEL 'Discount' FORMAT "->>>,>>9.99"                        
    FIELDS description           AS CHARACTER LABEL 'Description' EXTENT 2
    FIELDS requiredDate          AS DATE      LABEL 'Required Date'                                                         
    FIELDS exRate                AS DECIMAL   LABEL 'ex-rate' FORMAT "->>,>>9.99<<<<<"                    
    FIELDS fobOriginDest         AS CHARACTER LABEL 'FOB Origin/Dest'                                                       
    FIELDS freightPayment        AS CHARACTER LABEL 'Freight Payment'                                                       
    FIELDS name                  AS CHARACTER LABEL 'Name'                                                                  
    FIELDS itemID                AS CHARACTER LABEL 'Item#'                                                                 
    FIELDS itemType              AS LOGICAL   LABEL 'Item Type' FORMAT "R/F" INITIAL "Y"   
    FIELDS internalJobNumber     AS INTEGER   LABEL 'Internal Job Number' FORMAT ">>>>>>9"                            
    FIELDS jobID                 AS CHARACTER LABEL 'Job Number'                                                            
    FIELDS jobID2                AS INTEGER   LABEL 'Run #' FORMAT ">9"                                 
    FIELDS lastShipDate          AS DATE      LABEL 'Last Ship Date'                                                        
    FIELDS line                  AS INTEGER   LABEL 'Line' FORMAT "99"                                 
    FIELDS warehouseID           AS CHARACTER LABEL 'Whse'                                                                  
    FIELDS locationID            AS CHARACTER LABEL 'Bin'                                                                   
    FIELDS opened                AS LOGICAL   LABEL 'opened' FORMAT "Open/Closed"                        
    FIELDS customerOrderNo       AS INTEGER   LABEL 'Customer Order Number' FORMAT ">>>>>9"                             
    FIELDS orderQuantity         AS DECIMAL   LABEL 'Order Quantity' FORMAT "->>>,>>>,>>9.9<<<<<"                
    FIELDS overrunPct            AS DECIMAL   LABEL 'Overrun %' FORMAT ">>9.99%"                            
    FIELDS partialQty            AS INTEGER   LABEL 'Partial Qty' FORMAT ">>>,>>9"                            
    FIELDS dateChanged           AS DATE      LABEL 'Date Changed'                                                          
    FIELDS poDate                AS DATE      LABEL 'PO Date'                                                               
    FIELDS poID                  AS INTEGER   LABEL 'PO Number' FORMAT ">>>>>9"                             
    FIELDS purchaseQuantityUom   AS CHARACTER LABEL 'Purchase Quantity Uom'                                                 
    FIELDS purchasedUOM          AS CHARACTER LABEL 'Purchased UOM'                                                         
    FIELDS printed               AS LOGICAL   LABEL 'Printed?' FORMAT "y/n"                                
    FIELDS purchaseCount         AS INTEGER   LABEL 'Purchase Count' FORMAT ">,>>9"                              
    FIELDS receiptQty            AS DECIMAL   LABEL 'Receipt Qty' FORMAT "->>>>>>9.9<<<<<"                    
    FIELDS received              AS LOGICAL   LABEL 'Received' FORMAT "y/n"                                
    FIELDS releaseQuantity       AS DECIMAL   LABEL 'Release Quantity' FORMAT "->>>,>>>,>>9.9<<<<<"                
    FIELDS sheetLen              AS DECIMAL   LABEL 'Sheet Len' FORMAT ">>9.9999"                           
    FIELDS sheetNo               AS INTEGER   LABEL 'Sheet #' FORMAT ">9"                                 
    FIELDS sheetWid              AS DECIMAL   LABEL 'Sheet Wid' FORMAT ">>9.9999"                           
    FIELDS setupCharge           AS DECIMAL   LABEL 'Setup Charge' FORMAT ">>,>>9.99"                          
    FIELDS shippingAddress       AS CHARACTER LABEL 'Shipping Address' EXTENT 2
    FIELDS shippingCity          AS CHARACTER LABEL 'Shipping City'                                                         
    FIELDS shippingInstructions  AS CHARACTER LABEL 'Shipping Instructions' EXTENT 4
    FIELDS shipTo                AS CHARACTER LABEL 'Ship To'                                                               
    FIELDS shippingName          AS CHARACTER LABEL 'Shipping Name'                                                         
    FIELDS shipToNumber          AS INTEGER   LABEL 'Ship To Number' FORMAT ">>9"                                
    FIELDS shippingState         AS CHARACTER LABEL 'Shipping State'                                                        
    FIELDS shippingZip           AS CHARACTER LABEL 'Shipping Zip' FORMAT "xxxxx-xxxx"                         
    FIELDS specialInstructions   AS CHARACTER LABEL 'Special Instructions' EXTENT 4
    FIELDS stat                  AS CHARACTER LABEL 'Status'                                                                
    FIELDS state                 AS CHARACTER LABEL 'State'                                                                 
    FIELDS totalCost             AS DECIMAL   LABEL 'Total Cost' FORMAT "->,>>>,>>9.99<<"                    
    FIELDS totalFreight          AS DECIMAL   LABEL 'Total Freight' FORMAT "->>,>>9.99"                         
    FIELDS totalInvoiced         AS DECIMAL   LABEL 'Total Invoiced' FORMAT "->>>,>>>,>>9.9<<<<<"                
    FIELDS totalReceived         AS DECIMAL   LABEL 'Total Received' FORMAT "->>>,>>>,>>9.9<<<<<"                
    FIELDS totalQuantityReleased AS DECIMAL   LABEL 'Total Quantity Released' FORMAT "->>>,>>>,>>9.9<<<<<"                
    FIELDS tagDate               AS DATE      LABEL 'Tag Date'                                                              
    FIELDS tax                   AS DECIMAL   LABEL 'Tax' FORMAT "->,>>9.99"                          
    FIELDS salesTaxGroup         AS CHARACTER LABEL 'Tax Group'                                                       
    FIELDS taxExemptNo           AS CHARACTER LABEL 'Tax Exempt No.'                                                        
    FIELDS paymentTerms          AS CHARACTER LABEL 'Payment Terms'                                                         
    FIELDS totalTags             AS INTEGER   LABEL 'Total Tags'                                                            
    FIELDS type                  AS CHARACTER LABEL 'Type' INITIAL "R"   
    FIELDS underrunPct           AS DECIMAL   LABEL 'Underrun %' FORMAT ">>9.99%"                            
    FIELDS updatedDate           AS DATE      LABEL 'Updated Date'                                                          
    FIELDS updatedTime           AS INTEGER   LABEL 'Updated Time' FORMAT "->,>>>,>>9"                         
    FIELDS vendorItemID          AS CHARACTER LABEL 'Vendor Item #'                                                         
    FIELDS vendorName            AS CHARACTER LABEL 'Vendor Name'                                                           
    FIELDS vendor                AS CHARACTER LABEL 'Vendor'                                                                
    FIELDS zipCode               AS CHARACTER LABEL 'Zip Code'                                                              
    FIELDS firstMachine          AS CHARACTER LABEL 'First Machine' INITIAL " "   
    FIELDS firstInternalMachine  AS CHARACTER LABEL 'First Internal Machine'                                                
    FIELDS firstPress            AS CHARACTER LABEL 'First Press'                                                           
    .
