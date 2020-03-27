"201908281508503772296" "SendCustomer" "https://a.redecon-sa.com:2085/api/Customers/add" no "basic" "ewms_asi" "@s13wms" "requestData!1252!19848_1.blb" "JSON" "POST" "Siggins" yes ? "" "" "api/ResponseHandlerSiggins.p" "001" 1 no ""
"201908281510903772297" "SendVendor" "https://a.redecon-sa.com:2085/api/Suppliers/add" no "basic" "ewms_asi" "@s13wms" "requestData!1252!19848_2.blb" "JSON" "POST" "Siggins" yes ? "" "" "api/ResponseHandlerSiggins.p" "001" 2 no ""
"201908281512403772298" "SendFinishedGood" "https://a.redecon-sa.com:2085/api/Products/add" no "basic" "ewms_asi" "@s13wms" "requestData!1252!19848_3.blb" "JSON" "POST" "Siggins" yes ? "" "" "api/ResponseHandlerSiggins.p" "001" 3 no ""
"201908281513503772299" "SendPurchaseOrder" "https://a.redecon-sa.com:2085/api/ReceivingTickets/add" no "basic" "ewms_asi" "@s13wms" "requestData!1252!19848_4.blb" "JSON" "POST" "Siggins" yes ? "" "" "api/ResponseHandlerSiggins.p" "001" 4 no ""
"201908281514503772300" "SendRelease" "https://a.redecon-sa.com:2085/api/SalesTickets/add" no "basic" "ewms_asi" "@s13wms" "requestData!1252!19848_5.blb" "JSON" "POST" "Siggins" yes ? "" "" "api/ResponseHandlerSiggins.p" "001" 5 no ""
"201909253849004020188" "CheckTransfer" "" yes "none" "" "" "requestData!1252!19848_6.blb" "FTP" "POST" "8" yes ? "" "" "" "001" 7 no ""
"201910041192404020313" "SendPurchaseOrderStatus" "https://ppmwsapp:2082/api/ReceivingTickets/add" no "basic" "ewms_asi" "@s13wmS2019" "requestData!1252!19848_7.blb" "JSON" "POST" "Siggins" yes ? "" "" "api/ResponseHandlerSiggins.p" "001" 8 no ""
"201910041232604020314" "SendPurchaseOrderLineStatus" "https://ppmwsapp:2082/api/ReceivingTickets/add" no "basic" "ewms_asi" "@s13wmS2019" "requestData!1252!19848_8.blb" "JSON" "POST" "Siggins" yes ? "" "" "api/ResponseHandlerSiggins.p" "001" 9 no ""
"202001272232305315978" "SendAdvancedShipNotice" "https://amazon-stage.coupahost.com/cxml/ship_notice_request" no "basic" "amazon" "amazon" "requestData!1252!19848_9.blb" "XML" "POST" "Amazon" yes ? "" "" "api/ResponseHandlerXMLAmazon.p" "001" 10 no ""
"202003040546704702605" "SendJob" "" no "basic" "user1" "user1" "requestData!1252!19848_10.blb" "XML" "POST" "Siggins" yes ? "" "" "api/ResponseHandlerXMLAmazon.p" "001" 13 yes "C:\Tmp\Test"
.
PSC
filename=APIOutbound
records=0000000000010
ldbname=ASI
timestamp=2020/03/27-12:15:05
numformat=44,46
dateformat=mdy-1950
map=NO-MAP
cpstream=IBM850
.
0000002272
