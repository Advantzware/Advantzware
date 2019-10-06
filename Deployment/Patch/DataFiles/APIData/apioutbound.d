"201908281508503772296" "SendCustomer" "https://a.redecon-sa.com:2085/api/Customers/add" no "basic" "ewms_asi" "@s13wms" "requestData!1252!156204_5.blb" "JSON" "POST" "Siggins" yes ? "" "" "api/ResponseHandlerSiggins.p" "001" 1
"201908281510903772297" "SendVendor" "https://a.redecon-sa.com:2085/api/Suppliers/add" no "basic" "ewms_asi" "@s13wms" "requestData!1252!156204_6.blb" "JSON" "POST" "Siggins" yes ? "" "" "api/ResponseHandlerSiggins.p" "001" 2
"201908281512403772298" "SendFinishedGood" "https://a.redecon-sa.com:2085/api/Products/add" no "basic" "ewms_asi" "@s13wms" "requestData!1252!156204_7.blb" "JSON" "POST" "Siggins" yes ? "" "" "api/ResponseHandlerSiggins.p" "001" 3
"201908281513503772299" "SendPurchaseOrder" "https://a.redecon-sa.com:2085/api/ReceivingTickets/add" no "basic" "ewms_asi" "@s13wms" "requestData!1252!156204_8.blb" "JSON" "POST" "Siggins" yes ? "" "" "api/ResponseHandlerSiggins.p" "001" 4
"201908281514503772300" "SendRelease" "https://a.redecon-sa.com:2085/api/SalesTickets/add" no "basic" "ewms_asi" "@s13wms" "requestData!1252!156204_9.blb" "JSON" "POST" "Siggins" yes ? "" "" "api/ResponseHandlerSiggins.p" "001" 5
"201909267181904295288" "CheckTransfer" "" no "none" "" "" "requestData!1252!156204_10.blb" "FTP" "POST" "8" yes ? "" "" "" "001" 6
.
PSC
filename=APIOutbound
records=0000000000006
ldbname=ASI
timestamp=2019/09/27-13:15:14
numformat=44,46
dateformat=mdy-1950
map=NO-MAP
cpstream=IBM850
.
0000001296
