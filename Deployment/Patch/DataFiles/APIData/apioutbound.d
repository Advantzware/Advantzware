"201908281508503772296" "SendCustomer" "" no "none" "" "" "requestData!1252!25340_47.blb" "JSON" "POST" "_default" yes ? "" "" "api/ResponseHandlerSiggins.p" "" 1 no "" "Adds new customer" "API" 4 "" no no
"201908281510903772297" "SendVendor" "" no "none" "" "" "requestData!1252!25340_48.blb" "JSON" "POST" "_default" yes ? "" "" "api/ResponseHandlerSiggins.p" "" 2 no "" "AP to add a vendor" "API" 6 "" no no
"201908281512403772298" "SendFinishedGood" "" no "none" "" "" "requestData!1252!25340_49.blb" "JSON" "POST" "_default" yes ? "" "" "api/ResponseHandlerSiggins.p" "" 3 no "" "Adds new Finished Good" "API" 15 "" no no
"201908281513503772299" "SendPurchaseOrder" "" no "none" "" "" "requestData!1252!25340_50.blb" "JSON" "POST" "_default" yes ? "" "" "api/ResponseHandlerSiggins.p" "" 4 no "" "API to add Purchase Order" "API" 52 "" no no
"201908281514503772300" "SendRelease" "" no "none" "" "" "requestData!1252!25340_51.blb" "JSON" "POST" "_default" yes ? "" "" "api/ResponseHandlerSiggins.p" "" 5 no "" "API to send release" "API" 197 "" no no
"201909253849004020188" "CheckTransfer" "" no "none" "" "" "requestData!1252!25340_52.blb" "TXT" "" "_default" yes ? "" "" "" "" 6 yes "c:\tmp" "CheckTransfer for Bank Code - 8. This will accept the file and file path and transfer the file using FTP protocol" "FTP" 0 "" no no
"201910041192404020313" "SendPurchaseOrderStatus" "" no "none" "" "" "requestData!1252!25340_53.blb" "JSON" "POST" "_default" yes ? "" "" "api/ResponseHandlerSiggins.p" "" 7 no "" "API to send the purchase order status information" "API" 0 "" no no
"201910041232604020314" "SendPurchaseOrderLineStatus" "" no "none" "" "" "requestData!1252!25340_54.blb" "JSON" "POST" "_default" yes ? "" "" "api/ResponseHandlerSiggins.p" "" 8 no "" "API to send the purchase order line status information" "API" 0 "" no no
"202001272232305315978" "SendAdvancedShipNotice" "" no "none" "" "" "requestData!1252!25340_55.blb" "XML" "POST" "_default" yes ? "" "" "api/ResponseHandlerXMLAmazon.p" "" 9 no "" "API to post the BOL to Amazon" "API" 38 "" no no
"202004083938405003472" "SendInvoice" "" no "none" "" "" "requestData!1252!25340_56.blb" "" "" "_default" yes ? "" "" "api/ResponseHandlerSiggins.p" "" 10 yes "C:\Tmp\SendInvoice" "API to send invoice" "SAVE" 1 "" no no
"202003040546704702605" "SendJob" "" no "none" "" "" "requestData!1252!25340_57.blb" "XML" "POST" "_default" yes ? "" "" "api/ResponseHandlerXMLAmazon.p" "" 11 yes "C:\tmp\Esko" "API to send Job details" "API" 2 "" no no
"202004141953305007326" "SendPurchaseOrder" "" no "none" "" "" "requestData!1252!25340_58.blb" "CSV" "" "_default1" yes ? "" "" "" "" 12 no "" "Print purchase order in X12 format" "SAVE" 0 "" no no
"202004204564705140301" "SendRelease" "" no "none" "" "" "requestData!1252!25340_59.blb" "JSON" "POST" "_default1" yes ? "" "" "" "" 13 no "" "API to send release" "SAVE" 0 "" no no
"202004225622505165878" "SendFinishedGood2" "" no "none" "" "" "requestData!1252!25340_60.blb" "JSON" "POST" "_default" yes ? "" "" "" "" 14 no "" "Adds new Finished Good" "SAVE" 0 "" no no
"202005142461105162364" "SendAdvancedShipNotice" "" no "none" "" "" "requestData!1252!25340_61.blb" "JSON" "" "_default1" yes ? "" "" "" "" 15 no "" "EDI 214 document for Target" "SAVE" 0 "" no no
"202005225597205311596" "SendOrderAck" "" no "none" "" "" "requestData!1252!25340_62.blb" "TXT" "POST" "_default" yes ? "" "" "api/ResponseHandlerXMLAmazon.p" "" 16 no "" "" "API" 0 "" no no
"202006232171005369548" "CalculateTax" "" no "none" "" "" "requestData!1252!25340_63.blb" "JSON" "POST" "_default" yes 2020-06-23T06:01:50.891 "user1" "" "api/ResponseHandlerVertex.p" "" 17 no "" "API to calculate tax amount" "API" 118 "" no no
"202007164000905473693" "SendPurchaseOrder" "" no "none" "" "" "requestData!1252!25340_64.blb" "TXT" "" "_default2" yes 2020-07-16T11:06:49.686 "user1" "" "api/ResponseHandlerSiggins.p" "" 18 yes "C:\Tmp" "SendPurchaseOrder API for Westrock" "SAVE" 5 "" no no
"202009022849505624130" "SendInvoice" "" no "none" "" "" "requestData!1252!25340_65.blb" "XML" "" "_default1" yes 2020-09-02T07:54:55.740 "user1" "" "api/ResponseHandlerXMLAmazon.p" "" 19 yes "C:\Tmp\SendInvoice\cXML" "API to send invoice" "SAVE" 0 "" no no
"202009031422405580166" "SendPurchaseOrder" "www.filegenie.com" no "none" "wadefk" "4178-SJIS" "requestData!1252!25340_66.blb" "TXT" "" "_default3" yes 2020-09-03T03:57:04.936 "user1" "" "" "" 20 yes "C:\Tmp" "SendPurchaseOrder API for HRMS file format" "FTP" 0 "" no no
"202009113680605643278" "SendPurchaseOrder" "www.filegenie.com" no "none" "wadefk" "4178-SJIS" "requestData!1252!25340_67.blb" "TXT" "" "_default4" yes 2020-09-11T10:13:26.042 "user1" "" "" "" 21 yes "C:\Tmp" "SendPurchaseOrder API for AlliFlutes file format" "FTP" 0 "" no no
"202009113697605643289" "SendPurchaseOrder" "www.filegenie.com" no "none" "wadefk" "4178-SJIS" "requestData!1252!25340_68.blb" "TXT" "" "_default5" yes 2020-09-11T10:16:16.731 "user1" "" "" "" 22 yes "C:\Tmp" "SendPurchaseOrder API for Corr Choice file format" "FTP" 0 "" no no
"202009144026005652901" "SendPurchaseOrder" "www.filegenie.com" no "none" "wadefk" "4178-SJIS" "requestData!1252!25340_69.blb" "XML" "" "_default6" yes 2020-09-14T11:11:00.401 "asi" "" "" "" 23 yes "C:\Tmp\GP" "SendPurchaseOrder API for GP file format" "FTP" 0 "" no no
"202009153873605655288" "SendPurchaseOrder" "www.filegenie.com" no "none" "wadefk" "4178-SJIS" "requestData!1252!25340_70.blb" "TXT" "" "_default7" yes 2020-09-15T10:45:36.648 "asi" "" "" "" 24 yes "C:\Tmp\Alliance" "SendPurchaseOrder API for Alliance file format" "FTP" 0 "" no no
"202009153888905655301" "SendPurchaseOrder" "www.filegenie.com" no "none" "wadefk" "4178-SJIS" "requestData!1252!25340_71.blb" "TXT" "" "_default8" yes 2020-09-15T10:48:09.042 "asi" "" "" "" 25 yes "C:\Tmp\iPaper" "SendPurchaseOrder API for IPaper file format" "FTP" 0 "" no no
"202009172469905668404" "SendPurchaseOrder" "www.filegenie.com" no "none" "wadefk" "4178-SJIS" "requestData!1252!25340_72.blb" "TXT" "" "_default9" yes 2020-09-17T06:51:39.234 "asi" "" "" "" 26 yes "C:\Tmp\CorKraft" "SendPurchaseOrder API for Corr Kraft  file format" "FTP" 0 "" no no
"202009172482305668415" "SendPurchaseOrder" "www.filegenie.com" no "none" "wadefk" "4178-SJIS" "requestData!1252!25340_73.blb" "TXT" "" "_default10" yes 2020-09-17T06:53:43.376 "asi" "" "" "" 27 yes "C:\Tmp\Smurfit" "SendPurchaseOrder API for Smurfit file format" "FTP" 0 "" no no
"202009172489805668426" "SendPurchaseOrder" "www.filegenie.com" no "none" "wadefk" "4178-SJIS" "requestData!1252!25340_74.blb" "TXT" "" "_default11" yes 2020-09-17T06:54:58.347 "asi" "" "" "" 28 yes "C:\Tmp\Pratt" "SendPurchaseOrder API for Pratt file format" "FTP" 0 "" no no
"202009224178005675980" "SendPurchaseOrder" "www.filegenie.com" no "none" "wadefk" "4178-SJIS" "requestData!1252!25340_75.blb" "TXT" "" "_default12" yes 2020-09-22T11:36:20.488 "asi" "" "" "" 29 yes "C:\Tmp\Kiwi" "SendPurchaseOrder API for Kiwi & KiwiT format" "FTP" 0 "" no no
"202009234161505678781" "SendPurchaseOrder" "www.filegenie.com" no "none" "wadefk" "4178-SJIS" "requestData!1252!25340_76.blb" "TXT" "" "_default13" yes 2020-09-23T11:33:35.354 "user1" "" "" "" 30 yes "C:\Tmp\Liberty" "SendPurchaseOrder API for Liberty format" "FTP" 0 "" no no
"202010273657605809474" "SendPurchaseOrder" "www.filegenie.com" no "none" "wadefk" "4178-SJIS" "requestData!1252!25340_77.blb" "TXT" "" "_default14" yes 2020-10-27T10:09:36.005 "asi" "" "" "" 31 yes "C:\Tmp\CorrTrim" "SendPurchaseOrder API for CorrTrim format" "FTP" 0 "" no no
"202011062110905822545" "SendInvoice" "" no "none" "" "" "requestData!1252!25340_78.blb" "" "" "_default2" yes 2020-11-06T05:51:49.751 "asi" "" "api/ResponseHandlerSiggins.p" "" 32 yes "C:\tmp\SendInvoice" "API to send invoice for SunCast" "SAVE" 0 "" no yes
"202101264057206026226" "SendInvoice" "" no "none" "" "" "requestData!1252!25340_79.blb" "TXT" "" "_default3" yes 2021-01-26T11:16:12.117 "user1" "api/SendInvoiceEDI.p" "" "" 33 yes "C:\temp" "Generate invoice in EDI 810 format" "SAVE" 0 "" no yes
"202101252222606026151" "CreateLoadtag" "" no "basic" "" "" "requestData!1252!25340_80.blb" "CSV" "" "_default" yes 2021-01-25T06:10:26.285 "rajesh" "" "" "" 34 yes "C:\BA\Label" "Populate loadtax file using Label Matrix format" "SAVE" 0 "" no no
"202103161643807065812" "SendJob" "" no "none" "" "" "requestData!1252!25340_81.blb" "x-www-form-urlencoded" "POST" "_default1" yes 2021-03-16T04:33:58.463 "rajesh" "" "api/ResponseHandlerAMS.p" "" 35 no "" "API to send Job details to AMS" "API" 0 "" no no
"202103163290507065825" "CreateLoadtag" "" no "basic" "" "" "requestData!1252!25340_82.blb" "XML" "" "_default1" yes 2021-03-16T09:08:25.380 "rajesh" "" "" "" 36 yes "C:\BA\Label" "Populate loadtax file using XPrint format (lodxprntstd.i)" "SAVE" 0 "" no no
"202105052213807157990" "SendAdvancedShipNotice" "" no "none" "" "" "requestData!1252!25340_83.blb" "EDI" "" "_default2" yes 2021-05-05T06:08:58.975 "rajesh" "" "" "" 37 yes "C:\tmp\" "EDI 856 document for EDI Format" "SAVE" 0 "" no no
