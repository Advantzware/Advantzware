"201908281508503772296" "SendCustomer" "https://a.redecon-sa.com:2085/api/Customers/add" no "basic" "ewms_asi" "@s13wms" "requestData!1252!28488_30.blb" "JSON" "POST" "Siggins" no ? "" "" "api/ResponseHandlerSiggins.p" "001" 1 no "" "Adds new customer" "API" 0
"201908281510903772297" "SendVendor" "https://a.redecon-sa.com:2085/api/Suppliers/add" no "basic" "ewms_asi" "@s13wms" "requestData!1252!28488_31.blb" "JSON" "POST" "Siggins" no ? "" "" "api/ResponseHandlerSiggins.p" "001" 2 no "" "AP to add a vendor" "API" 0
"201908281512403772298" "SendFinishedGood" "https://a.redecon-sa.com:2085/api/Products/add" no "basic" "ewms_asi" "@s13wms" "requestData!1252!28488_32.blb" "JSON" "POST" "Siggins" no ? "" "" "api/ResponseHandlerSiggins.p" "001" 3 yes "C:\tmp\API\randy" "Adds new Finished Good" "API" 0
"201908281513503772299" "SendPurchaseOrder" "https://a.redecon-sa.com:2085/api/ReceivingTickets/add" no "basic" "ewms_asi" "@s13wms" "requestData!1252!28488_33.blb" "JSON" "POST" "Siggins" no ? "" "" "api/ResponseHandlerSiggins.p" "001" 4 no "" "API to add Purchase Order" "API" 0
"201908281514503772300" "SendRelease" "https://34.237.130.6:8443/api/cxmlorder" no "basic" "ewms_asi" "@s13wms" "requestData!1252!28488_34.blb" "JSON" "POST" "Siggins" no ? "" "" "api/ResponseHandlerSiggins.p" "001" 5 yes "C:\tmp\API\randy" "API to send release" "API" 0
"201909253849004020188" "CheckTransfer" "" no "none" "" "" "requestData!1252!28488_35.blb" "" "" "8" no ? "" "" "" "001" 6 no "" "CheckTransfer for Bank Code - 8. This will accept the file and file path and transfer the file using FTP protocol" "FTP" 0
"201910041192404020313" "SendPurchaseOrderStatus" "" no "none" "" "" "requestData!1252!28488_36.blb" "JSON" "POST" "Siggins" no ? "" "" "api/ResponseHandlerSiggins.p" "001" 7 no "" "API to send the purchase order status information" "API" 0
"201910041232604020314" "SendPurchaseOrderLineStatus" "" no "none" "" "" "requestData!1252!28488_37.blb" "JSON" "POST" "Siggins" no ? "" "" "api/ResponseHandlerSiggins.p" "001" 8 no "" "API to send the purchase order line status information" "API" 0
"202001272232305315978" "SendAdvancedShipNotice" "https://amazon-stage.coupahost.com/cxml/ship_notice_request" no "none" "" "" "requestData!1252!28488_38.blb" "XML" "POST" "Amazon" no ? "" "" "api/ResponseHandlerXMLAmazon.p" "001" 9 no "" "API to post the BOL to Amazon" "API" 0
"202004083938405003472" "SendInvoice" "" no "basic" "" "" "requestData!1252!28488_39.blb" "" "" "Amazon" no ? "" "" "api/ResponseHandlerSiggins.p" "001" 10 yes "custfiles\edifiles\invoices\Amazon\Send" "API to send invoice" "SAVE" 0
"202003040546704702605" "SendJob" "" no "basic" "user1" "user1" "requestData!1252!28488_40.blb" "XML" "POST" "Siggins" no ? "" "" "api/ResponseHandlerXMLAmazon.p" "001" 11 yes "C:\tmp\API\randy" "API to send Job details" "API" 0
"202004141953305007326" "SendPurchaseOrder" "" no "none" "" "" "requestData!1252!28488_41.blb" "CSV" "" "CSC" no ? "" "" "" "001" 12 yes "C:\Temp\" "Print purchase order in X12 format" "SAVE" 0
"202004204564705140301" "SendRelease" "" no "basic" "" "" "requestData!1252!28488_42.blb" "JSON" "POST" "GSWELL" no ? "" "" "" "001" 16 yes "C:\tmp" "API to send release" "SAVE" 0
"202004225622505165878" "SendFinishedGood2" "" no "basic" "" "" "requestData!1252!28488_43.blb" "JSON" "POST" "GSWELL" no ? "" "" "" "001" 17 yes "C:\tmp\API" "Adds new Finished Good" "SAVE" 0
