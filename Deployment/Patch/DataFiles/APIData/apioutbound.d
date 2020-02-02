"201908281508503772296" "SendCustomer" "https://a.redecon-sa.com:2085/api/Customers/add" no "basic" "ewms_asi" "@s13wms" "requestData!1252!183152_23.blb" "JSON" "POST" "Siggins" yes ? "" "" "api/ResponseHandlerSiggins.p" "001" 1
"201908281510903772297" "SendVendor" "https://a.redecon-sa.com:2085/api/Suppliers/add" no "basic" "ewms_asi" "@s13wms" "requestData!1252!183152_24.blb" "JSON" "POST" "Siggins" yes ? "" "" "api/ResponseHandlerSiggins.p" "001" 2
"201908281512403772298" "SendFinishedGood" "https://a.redecon-sa.com:2085/api/Products/add" no "basic" "ewms_asi" "@s13wms" "requestData!1252!183152_25.blb" "JSON" "POST" "Siggins" yes ? "" "" "api/ResponseHandlerSiggins.p" "001" 3
"201908281513503772299" "SendPurchaseOrder" "https://a.redecon-sa.com:2085/api/ReceivingTickets/add" no "basic" "ewms_asi" "@s13wms" "requestData!1252!183152_26.blb" "JSON" "POST" "Siggins" yes ? "" "" "api/ResponseHandlerSiggins.p" "001" 4
"201908281514503772300" "SendRelease" "https://a.redecon-sa.com:2085/api/SalesTickets/add" no "basic" "ewms_asi" "@s13wms" "requestData!1252!183152_27.blb" "JSON" "POST" "Siggins" yes ? "" "" "api/ResponseHandlerSiggins.p" "001" 5
"201909253849004020188" "CheckTransfer" "" yes "none" "" "" "requestData!1252!183152_28.blb" "FTP" "POST" "8" yes ? "" "" "" "001" 7
"201910041192404020313" "SendPurchaseOrderStatus" "https://ppmwsapp:2082/api/ReceivingTickets/add" no "basic" "ewms_asi" "@s13wmS2019" "requestData!1252!183152_29.blb" "JSON" "POST" "Siggins" yes ? "" "" "api/ResponseHandlerSiggins.p" "001" 8
"201910041232604020314" "SendPurchaseOrderLineStatus" "https://ppmwsapp:2082/api/ReceivingTickets/add" no "basic" "ewms_asi" "@s13wmS2019" "requestData!1252!183152_30.blb" "JSON" "POST" "Siggins" yes ? "" "" "api/ResponseHandlerSiggins.p" "001" 9
.
PSC
filename=APIOutbound
records=0000000000008
ldbname=ASI
timestamp=2020/01/31-13:36:30
numformat=44,46
dateformat=mdy-1950
map=NO-MAP
cpstream=IBM850
.
0000001794
