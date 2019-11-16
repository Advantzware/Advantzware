"201908281508503772296" "SendCustomer" "https://10.7.4.36:2082/api/Customers/add" no "basic" "ewms_asi" "@13wmS2019" "requestData!1252!48764_41.blb" "JSON" "POST" "Siggins" yes ? "" "" "api/ResponseHandlerSiggins.p" "001" 1
"201908281510903772297" "SendVendor" "https://ppmwsapp:2082/api/Suppliers/add" no "basic" "ewms_asi" "@313wmS2019" "requestData!1252!48764_42.blb" "JSON" "POST" "Siggins" yes ? "" "" "api/ResponseHandlerSiggins.p" "001" 2
"201908281512403772298" "SendFinishedGood" "https://ppmwsapp:2082/api/Products/add" no "basic" "ewms_asi" "@s13wmS2019" "requestData!1252!48764_43.blb" "JSON" "POST" "Siggins" yes ? "" "" "api/ResponseHandlerSiggins.p" "001" 3
"201908281513503772299" "SendPurchaseOrder" "https://ppmwsapp:2082/api/ReceivingTickets/add" no "basic" "ewms_asi" "@s13wmS2019" "requestData!1252!48764_44.blb" "JSON" "POST" "Siggins" yes ? "" "" "api/ResponseHandlerSiggins.p" "001" 4
"201908281514503772300" "SendRelease" "https://ppmwsapp:2082/api/SalesTickets/add" no "basic" "ewms_asi" "@s13wmS2019" "requestData!1252!48764_45.blb" "JSON" "POST" "Siggins" yes ? "" "" "api/ResponseHandlerSiggins.p" "001" 5
"201909253849004020188" "CheckTransfer" "" no "none" "" "" "requestData!1252!48764_46.blb" "FTP" "POST" "8" yes ? "" "" "" "001" 7
"201910041192404020313" "SendPurchaseOrderStatus" "https://ppmwsapp:2082/api/ReceivingTickets/add" no "basic" "ewms_asi" "@s13wmS2019" "requestData!1252!48764_47.blb" "JSON" "POST" "Siggins" yes ? "" "" "api/ResponseHandlerSiggins.p" "001" 8
"201910041232604020314" "SendPurchaseOrderLineStatus" "https://ppmwsapp:2082/api/ReceivingTickets/add" no "basic" "ewms_asi" "@s13wmS2019" "requestData!1252!48764_48.blb" "JSON" "POST" "Siggins" yes ? "" "" "api/ResponseHandlerSiggins.p" "001" 9
.
PSC
filename=APIOutbound
records=0000000000008
ldbname=ASI
timestamp=2019/11/15-12:43:02
numformat=44,46
dateformat=mdy-1950
map=NO-MAP
cpstream=IBM850
.
0000001765
