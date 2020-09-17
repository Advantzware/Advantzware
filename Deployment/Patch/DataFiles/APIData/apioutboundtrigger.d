"001" "201909061657203786361" "SendCustomer" "AddCustomer" "Adds a customer" yes 2019-11-26T09:05:46.292 "bpvasi" "_default" 1 1
"001" "201911263255004495512" "SendCustomer" "PrintRelease" "SendCustomer when Release is Printed First Time" yes 2020-07-16T21:21:36.674 "jay" "_default" 2 1
"001" "201911263256804495513" "SendCustomer" "ReprintRelease" "SendCustomer when Release is Reprinted" yes 2020-07-16T21:21:41.163 "jay" "_default" 3 1
"001" "201909061660803786362" "SendVendor" "AddVendor" "Adds a Vendor" yes 2019-11-26T09:11:37.720 "bpvasi" "_default" 4 2
"001" "201911263308504495517" "SendVendor" "PrintPurchaseOrder" "Sends Vendor when Purchase Order is printed for first time" yes 2020-07-16T21:29:46.515 "jay" "_default" 5 2
"001" "201911263309404495518" "SendVendor" "ReprintPurchaseOrder" "Sends Vendor when Purchase Order is reprinted" yes 2020-07-16T21:29:49.867 "jay" "_default" 6 2
"001" "201909061662903786363" "SendFinishedGood" "AddFinishedGood" "Update a Finished Good" yes 2019-11-26T09:05:15.713 "bpvasi" "_default" 7 3
"001" "201911263264704495514" "SendFinishedGood" "PrintRelease" "Send Finished Good when release is first printed" yes 2020-07-16T21:22:04.886 "jay" "_default" 8 3
"001" "201911263265804495515" "SendFinishedGood" "ReprintRelease" "Send Finished Good when release is reprinted" yes 2020-07-16T21:22:08.331 "jay" "_default" 9 3
"001" "202003170950604718910" "SendFinishedGood" "CreateLoadtag" "On creation of loadtags on a Job or Order from O-U-7 screen" yes 2020-07-16T21:22:12.303 "jay" "_default" 10 3
"001" "201909061667903786364" "SendPurchaseOrder" "UpdatePurchaseOrder" "Updates a Purchase Order" yes 2020-07-16T21:27:15.200 "jay" "_default" 11 4
"001" "201910032303704020299" "SendPurchaseOrder" "HoldPurchaseOrder" "Purchase Order status changing to ""H"" (Hold)" yes 2020-07-16T21:27:18.849 "jay" "_default" 12 4
"001" "201910032303704020300" "SendPurchaseOrder" "ReleasePurchaseOrder" "Purchase Order status changing from ""H"" (Hold)" yes 2020-07-16T21:27:22.379 "jay" "_default" 13 4
"001" "201910032303704020301" "SendPurchaseOrder" "PrintPurchaseOrder" "Trigger when Purchase Order Printed flag is ""NO""" yes 2020-07-16T21:27:26.367 "jay" "_default" 14 4
"001" "201910032303704020302" "SendPurchaseOrder" "ReprintPurchaseOrder" "Trigger when Purchase Order Printed flag is ""YES""" yes 2020-07-16T21:27:29.666 "jay" "_default" 15 4
"001" "201910032303704020303" "SendPurchaseOrder" "DeletePurchaseOrder" "Trigger when Purchase order is deleted" yes 2020-07-16T21:27:33.425 "jay" "_default" 16 4
"001" "201910032303704020304" "SendPurchaseOrder" "ClosePurchaseOrder" "Trigger when Purchase Order is closed from closepo.p" yes 2020-07-16T21:27:39.733 "jay" "_default" 17 4
"001" "201910032303704020305" "SendPurchaseOrder" "ReopenPurchaseOrder" "Trigger when Purchase Order is re-opened" yes 2020-07-16T21:27:42.638 "jay" "_default" 18 4
"001" "201910045101604020337" "SendPurchaseOrder" "TriggerGetPurchaseOrder" "Trigger to fetch purchase order details" yes 2020-07-16T21:27:47.453 "jay" "_default" 19 4
"001" "201909061651603786360" "SendRelease" "PrintRelease" "Sends Release when Release is printed" no 2020-05-15T10:44:49.388 "user1" "_default" 20 5
"001" "201911263304604495516" "SendRelease" "ReprintRelease" "Sends Release when Release is reprinted" no 2019-11-26T09:10:46.075 "bpvasi" "_default" 21 5
"001" "202005153733805141655" "SendRelease" "UpdateRelease" "Sends Release when Release is updated" no 2020-05-15T10:45:00.394 "user1" "_default" 22 5
"001" "202005153738005141656" "SendRelease" "DeleteRelease" "Sends Release when Release is deleted" no 2020-05-15T10:45:17.159 "user1" "_default" 23 5
"001" "202005153740905141657" "SendRelease" "CreateRelease" "Sends Release when Release is created" no 2020-05-15T10:45:33.742 "user1" "_default" 24 5
"001" "202005153888605141666" "SendRelease" "ApproveRelease" "Sends Release when Release is approved" no 2020-05-15T10:48:06.934 "user1" "_default" 25 5
"001" "202005154397805141667" "SendRelease" "HoldRelease" "Sends Release when Release is hold" no 2020-05-15T12:12:58.160 "user1" "_default" 26 5
"001" "201909253853104020189" "CheckTransfer" "TransmitBankFile" "Transmit Bank File" yes 2020-07-16T21:20:18.700 "jay" "_default" 27 6
"001" "201910041242304020315" "SendPurchaseOrderStatus" "ClosePurchaseOrder" "Trigger on close of PO (manual or automatic), send when PO Status is being set to ""C""" yes 2019-11-26T09:09:56.356 "bpvasi" "_default" 28 7
"001" "201910041248104020316" "SendPurchaseOrderStatus" "HoldPurchaseOrder" "Trigger on status change to Hold of PO" yes 2019-11-26T09:09:59.068 "bpvasi" "_default" 29 7
"001" "201910041264504020317" "SendPurchaseOrderStatus" "ReleasePurchaseOrder" "Trigger on PO status change from Hold to Release ( Except for status ""C"")" yes 2019-11-26T09:10:02.511 "bpvasi" "_default" 30 7
"001" "201910041274804020318" "SendPurchaseOrderStatus" "ReopenPurchaseOrder" "Trigger on close of PO (manual or automatic). PO status being changed from ""C""" yes 2019-11-26T09:10:05.172 "bpvasi" "_default" 31 7
"001" "201910041280404020319" "SendPurchaseOrderLineStatus" "ClosePurchaseOrderLine" "Trigger on close of PO line (manual or automatic)" yes 2019-11-26T09:09:23.831 "bpvasi" "_default" 32 8
"001" "201910041288804020320" "SendPurchaseOrderLineStatus" "ReopenPurchaseOrderLine" "Trigger on open of PO line (manual or automatic)" yes 2019-11-26T09:09:26.667 "bpvasi" "_default" 33 8
"001" "202002061940804639939" "SendAdvancedShipNotice" "ReprintBillOfLading" "Triggers when BOL is re-printed" yes 2020-07-16T21:20:42.729 "jay" "_default" 34 9
"001" "202001281704705316001" "SendAdvancedShipNotice" "PrintBillOfLading" "Triggers when BOL is printed" yes 2020-07-16T21:20:46.494 "jay" "_default" 35 9
"001" "202005225572205311507" "SendInvoice" "PrintInvoice" "Send EDI invoice on print of invoice" yes 2020-07-16T21:24:36.009 "jay" "_default" 36 10
"001" "202005225573405311508" "SendInvoice" "RePrintInvoice" "Send EDI invoice on re-print of invoice" yes 2020-07-16T21:24:39.854 "jay" "_default" 37 10
"001" "202005225575005311509" "SendInvoice" "PostInvoice" "Send EDI invoice on post of invoice" yes 2020-07-16T21:24:45.098 "jay" "_default" 38 10
"001" "202003274269104743620" "SendJob" "PrintJob" "Triggers when Job printed flag is NO" yes 2020-07-16T21:25:18.010 "jay" "_default" 39 11
"001" "202003274271604743621" "SendJob" "ReprintJob" "Triggers when Job printed flag is YES" yes 2020-07-16T21:25:23.462 "jay" "_default" 40 11
"001" "202004142003005007333" "SendPurchaseOrder" "PrintPurchaseOrder" "Print purchase order" yes 2020-07-16T21:28:16.676 "jay" "_default1" 41 12
"001" "202004142004005007334" "SendPurchaseOrder" "RePrintPurchaseOrder" "RePrint purchase order" yes 2020-07-16T21:28:10.223 "jay" "_default1" 42 12
"001" "202004225650705165879" "SendFinishedGood2" "AddFinishedGood" "Update A Finished Good" yes 2020-04-22T15:45:24.984 "Randy" "_default" 43 14
"001" "202004225659505165880" "SendFinishedGood2" "PrintRelease" "Send Finished Good when release is first printed" yes 2020-07-16T21:22:30.724 "jay" "_default" 44 14
"001" "202004225662005165881" "SendFinishedGood2" "ReprintRelease" "Send Finished Good when release is reprinted" yes 2020-07-16T21:22:33.914 "jay" "_default" 45 14
"001" "202004225669505165882" "SendFinishedGood2" "CreateLoadtag" "On creation of loadtags on a Job or Order from O-U-7 screen" yes 2020-07-16T21:22:39.459 "jay" "_default" 46 14
"001" "202005142461105162367" "SendAdvancedShipNotice" "ReprintBillOfLading" "Triggers when BOL is re-printed" yes 2020-07-16T21:21:05.954 "jay" "_default1" 47 15
"001" "202005142461105162368" "SendAdvancedShipNotice" "PrintBillOfLading" "Triggers when BOL is printed" yes 2020-07-16T21:21:10.565 "jay" "_default1" 48 15
"001" "202005142461105162369" "SendAdvancedShipNotice" "AddCustomer" "Adds a customer" yes 2019-11-26T09:05:46.292 "user1" "_default1" 49 15
"001" "202005142461105162370" "SendAdvancedShipNotice" "PrintRelease" "SendCustomer when Release is Printed First Time" yes 2020-07-16T21:21:14.922 "jay" "_default1" 50 15
"001" "202005142461105162371" "SendAdvancedShipNotice" "ReprintRelease" "SendCustomer when Release is Reprinted" yes 2020-07-16T21:21:19.014 "jay" "_default1" 51 15
"001" "202005225605905311655" "SendOrderAck" "PrintOrderAck" "Print Order Acknowledgement" yes 2020-07-16T21:26:53.639 "jay" "_default" 52 16
"001" "202005225607305311683" "SendOrderAck" "RePrintOrderAck" "Reprint of Order Acknowledgement" yes 2020-07-16T21:26:37.761 "jay" "_default" 53 16
"001" "202006242742105369560" "CalculateTax" "GetTaxAmount" "Fetch Tax Amount" no 2020-06-29T03:30:34.738 "user1" "_default" 54 17
"001" "202007164000905473697" "SendPurchaseOrder" "UpdatePurchaseOrder" "Updates a Purchase Order" yes 2020-07-16T11:08:09.139 "user1" "_default2" 55 18
"001" "202007164000905473698" "SendPurchaseOrder" "HoldPurchaseOrder" "Purchase Order status changing to ""H"" (Hold)" yes 2020-07-16T11:08:14.067 "user1" "_default2" 56 18
"001" "202007164000905473699" "SendPurchaseOrder" "ReleasePurchaseOrder" "Purchase Order status changing from ""H"" (Hold)" yes 2020-07-16T11:08:20.506 "user1" "_default2" 57 18
"001" "202007164000905473700" "SendPurchaseOrder" "PrintPurchaseOrder" "Trigger when Purchase Order Printed flag is ""NO""" yes 2020-07-17T02:39:22.359 "user1" "_default2" 58 18
"001" "202007164000905473701" "SendPurchaseOrder" "ReprintPurchaseOrder" "Trigger when Purchase Order Printed flag is ""YES""" yes 2020-07-16T21:19:50.030 "jay" "_default2" 59 18
"001" "202007164000905473702" "SendPurchaseOrder" "DeletePurchaseOrder" "Trigger when Purchase order is deleted" yes 2020-07-16T11:08:34.823 "user1" "_default2" 60 18
"001" "202007164000905473703" "SendPurchaseOrder" "ClosePurchaseOrder" "Trigger when Purchase Order is closed from closepo.p" yes 2020-07-16T11:08:39.741 "user1" "_default2" 61 18
"001" "202007164000905473704" "SendPurchaseOrder" "ReopenPurchaseOrder" "Trigger when Purchase Order is re-opened" yes 2020-07-16T11:06:49.845 "user1" "_default2" 62 18
"001" "202007164000905473705" "SendPurchaseOrder" "TriggerGetPurchaseOrder" "Trigger to fetch purchase order details" yes 2020-07-16T11:08:47.263 "user1" "_default2" 63 18
"001" "202008102357105530174" "CalculateTax" "GetTaxAmountFinal" "Notifies Vertex this is of Invoice message type" no 2020-08-10T06:32:51.145 "user1" "_default" 64 17
"001" "202008241647405580064" "SendRelease" "PrintRelease" "Sends Release when Release is printed" no 2020-08-24T04:34:34.320 "user1" "_default1" 65 13
"001" "202008241647405580065" "SendRelease" "ReprintRelease" "Sends Release when Release is reprinted" no 2020-08-24T04:34:34.336 "user1" "_default1" 66 13
"001" "202008241647405580066" "SendRelease" "UpdateRelease" "Sends Release when Release is updated" no 2020-08-24T04:34:34.351 "user1" "_default1" 67 13
"001" "202008241647405580067" "SendRelease" "DeleteRelease" "Sends Release when Release is deleted" no 2020-08-24T04:34:34.351 "user1" "_default1" 68 13
"001" "202008241647405580068" "SendRelease" "CreateRelease" "Sends Release when Release is created" no 2020-08-24T04:34:34.367 "user1" "_default1" 69 13
"001" "202008241647405580069" "SendRelease" "ApproveRelease" "Sends Release when Release is approved" no 2020-08-24T04:34:34.367 "user1" "_default1" 70 13
"001" "202008241647405580070" "SendRelease" "HoldRelease" "Sends Release when Release is hold" no 2020-08-24T04:34:34.367 "user1" "_default1" 71 13
"001" "202009022849505624136" "SendInvoice" "PrintInvoice" "Generate cXML invoice on print of invoice" yes 2020-09-02T07:54:55.911 "user1" "_default1" 72 19
"001" "202009022849505624137" "SendInvoice" "RePrintInvoice" "Generate cXML invoice on re-print of invoice" yes 2020-09-02T07:54:55.934 "user1" "_default1" 73 19
"001" "202009022849505624138" "SendInvoice" "PostInvoice" "Generate cXML invoice on post of invoice" yes 2020-09-02T07:54:55.947 "user1" "_default1" 74 19
"001" "202009042429005624130" "SendCustomer" "CreateRelease" "Send Customer when Release is Created" yes 2020-09-04T06:44:50.263 "user1" "_default" 75 1
"001" "202009042433105624131" "SendFinishedGood" "CreateRelease" "Send Finished Good when Release is Created" yes 2020-09-04T06:45:31.534 "user1" "_default" 76 3
"001" "202009031422505580168" "SendPurchaseOrder" "UpdatePurchaseOrder" "Updates a Purchase Order" yes 2020-09-08T06:32:41.173 "user1" "_default3" 77 20
"001" "202009031422505580169" "SendPurchaseOrder" "HoldPurchaseOrder" "Purchase Order status changing to ""H"" (Hold)" yes 2020-09-03T03:57:05.029 "user1" "_default3" 78 20
"001" "202009031422505580170" "SendPurchaseOrder" "ReleasePurchaseOrder" "Purchase Order status changing from ""H"" (Hold)" yes 2020-09-03T03:57:05.045 "user1" "_default3" 79 20
"001" "202009031422505580172" "SendPurchaseOrder" "ReopenPurchaseOrder" "Trigger when Purchase Order is re-opened" yes 2020-09-03T03:57:05.061 "user1" "_default3" 81 20
"001" "202009031422505580173" "SendPurchaseOrder" "ClosePurchaseOrder" "Trigger when Purchase Order is closed from closepo.p" yes 2020-09-03T03:57:05.061 "user1" "_default3" 82 20
"001" "202009031422505580174" "SendPurchaseOrder" "DeletePurchaseOrder" "Trigger when Purchase order is deleted" yes 2020-09-03T03:57:05.076 "user1" "_default3" 83 20
"001" "202009031422505580175" "SendPurchaseOrder" "ReprintPurchaseOrder" "Trigger when Purchase Order Printed flag is ""YES""" yes 2020-09-03T03:57:05.076 "user1" "_default3" 84 20
"001" "202009031422505580176" "SendPurchaseOrder" "PrintPurchaseOrder" "Trigger when Purchase Order Printed flag is ""NO""" yes 2020-09-08T06:32:48.834 "user1" "_default3" 85 20
"001" "202009113680605643280" "SendPurchaseOrder" "UpdatePurchaseOrder" "Updates a Purchase Order" yes 2020-09-11T10:13:26.135 "user1" "_default4" 86 21
"001" "202009113680605643281" "SendPurchaseOrder" "HoldPurchaseOrder" "Purchase Order status changing to ""H"" (Hold)" yes 2020-09-11T10:13:26.154 "user1" "_default4" 87 21
"001" "202009113680605643282" "SendPurchaseOrder" "ReleasePurchaseOrder" "Purchase Order status changing from ""H"" (Hold)" yes 2020-09-11T10:13:26.166 "user1" "_default4" 88 21
"001" "202009113680605643283" "SendPurchaseOrder" "ReopenPurchaseOrder" "Trigger when Purchase Order is re-opened" yes 2020-09-11T10:13:26.178 "user1" "_default4" 89 21
"001" "202009113680605643284" "SendPurchaseOrder" "ClosePurchaseOrder" "Trigger when Purchase Order is closed from closepo.p" yes 2020-09-11T10:13:26.191 "user1" "_default4" 90 21
"001" "202009113680605643285" "SendPurchaseOrder" "DeletePurchaseOrder" "Trigger when Purchase order is deleted" yes 2020-09-11T10:13:26.203 "user1" "_default4" 91 21
"001" "202009113680605643286" "SendPurchaseOrder" "ReprintPurchaseOrder" "Trigger when Purchase Order Printed flag is ""YES""" yes 2020-09-11T10:13:26.215 "user1" "_default4" 92 21
"001" "202009113680605643287" "SendPurchaseOrder" "PrintPurchaseOrder" "Trigger when Purchase Order Printed flag is ""NO""" yes 2020-09-11T10:13:26.227 "user1" "_default4" 93 21
"001" "202009113697605643291" "SendPurchaseOrder" "UpdatePurchaseOrder" "Updates a Purchase Order" yes 2020-09-11T10:16:16.848 "user1" "_default5" 94 22
"001" "202009113697605643292" "SendPurchaseOrder" "HoldPurchaseOrder" "Purchase Order status changing to ""H"" (Hold)" yes 2020-09-11T10:16:16.861 "user1" "_default5" 95 22
"001" "202009113697605643293" "SendPurchaseOrder" "ReleasePurchaseOrder" "Purchase Order status changing from ""H"" (Hold)" yes 2020-09-11T10:16:16.876 "user1" "_default5" 96 22
"001" "202009113697605643294" "SendPurchaseOrder" "ReopenPurchaseOrder" "Trigger when Purchase Order is re-opened" yes 2020-09-11T10:16:16.889 "user1" "_default5" 97 22
"001" "202009113697605643295" "SendPurchaseOrder" "ClosePurchaseOrder" "Trigger when Purchase Order is closed from closepo.p" yes 2020-09-11T10:16:16.903 "user1" "_default5" 98 22
"001" "202009113697605643296" "SendPurchaseOrder" "DeletePurchaseOrder" "Trigger when Purchase order is deleted" yes 2020-09-11T10:16:16.916 "user1" "_default5" 99 22
"001" "202009113697605643297" "SendPurchaseOrder" "ReprintPurchaseOrder" "Trigger when Purchase Order Printed flag is ""YES""" yes 2020-09-11T10:16:16.930 "user1" "_default5" 100 22
"001" "202009113697605643298" "SendPurchaseOrder" "PrintPurchaseOrder" "Trigger when Purchase Order Printed flag is ""NO""" yes 2020-09-11T10:16:16.944 "user1" "_default5" 101 22
"001" "202009144026005652903" "SendPurchaseOrder" "PrintPurchaseOrder" "Trigger when Purchase Order Printed flag is ""NO""" yes 2020-09-14T11:11:00.515 "asi" "_default6" 102 23
"001" "202009144026005652904" "SendPurchaseOrder" "UpdatePurchaseOrder" "Updates a Purchase Order" yes 2020-09-14T11:11:00.531 "asi" "_default6" 103 23
"001" "202009144026005652905" "SendPurchaseOrder" "HoldPurchaseOrder" "Purchase Order status changing to ""H"" (Hold)" yes 2020-09-14T11:11:00.544 "asi" "_default6" 104 23
"001" "202009144026005652906" "SendPurchaseOrder" "ReleasePurchaseOrder" "Purchase Order status changing from ""H"" (Hold)" yes 2020-09-14T11:11:00.556 "asi" "_default6" 105 23
"001" "202009144026005652907" "SendPurchaseOrder" "ReopenPurchaseOrder" "Trigger when Purchase Order is re-opened" yes 2020-09-14T11:11:00.569 "asi" "_default6" 106 23
"001" "202009144026005652908" "SendPurchaseOrder" "ClosePurchaseOrder" "Trigger when Purchase Order is closed from closepo.p" yes 2020-09-14T11:11:00.581 "asi" "_default6" 107 23
"001" "202009144026005652909" "SendPurchaseOrder" "DeletePurchaseOrder" "Trigger when Purchase order is deleted" yes 2020-09-14T11:11:00.593 "asi" "_default6" 108 23
"001" "202009144026005652910" "SendPurchaseOrder" "ReprintPurchaseOrder" "Trigger when Purchase Order Printed flag is ""YES""" yes 2020-09-14T11:11:00.605 "asi" "_default6" 109 23
"001" "202009153873605655292" "SendPurchaseOrder" "PrintPurchaseOrder" "Trigger when Purchase Order Printed flag is ""NO""" yes 2020-09-15T10:45:36.920 "asi" "_default7" 110 24
"001" "202009153873605655293" "SendPurchaseOrder" "UpdatePurchaseOrder" "Updates a Purchase Order" yes 2020-09-15T10:45:36.939 "asi" "_default7" 111 24
"001" "202009153873605655294" "SendPurchaseOrder" "HoldPurchaseOrder" "Purchase Order status changing to ""H"" (Hold)" yes 2020-09-15T10:45:36.950 "asi" "_default7" 112 24
"001" "202009153873605655295" "SendPurchaseOrder" "ReleasePurchaseOrder" "Purchase Order status changing from ""H"" (Hold)" yes 2020-09-15T10:45:36.963 "asi" "_default7" 113 24
"001" "202009153873605655296" "SendPurchaseOrder" "ReopenPurchaseOrder" "Trigger when Purchase Order is re-opened" yes 2020-09-15T10:45:36.975 "asi" "_default7" 114 24
"001" "202009153873605655297" "SendPurchaseOrder" "ClosePurchaseOrder" "Trigger when Purchase Order is closed from closepo.p" yes 2020-09-15T10:45:36.988 "asi" "_default7" 115 24
"001" "202009153873705655298" "SendPurchaseOrder" "DeletePurchaseOrder" "Trigger when Purchase order is deleted" yes 2020-09-15T10:45:37.001 "asi" "_default7" 116 24
"001" "202009153873705655299" "SendPurchaseOrder" "ReprintPurchaseOrder" "Trigger when Purchase Order Printed flag is ""YES""" yes 2020-09-15T10:45:37.014 "asi" "_default7" 117 24
"001" "202009153888905655305" "SendPurchaseOrder" "PrintPurchaseOrder" "Trigger when Purchase Order Printed flag is ""NO""" yes 2020-09-15T10:48:09.134 "asi" "_default8" 118 25
"001" "202009153888905655306" "SendPurchaseOrder" "UpdatePurchaseOrder" "Updates a Purchase Order" yes 2020-09-15T10:48:09.146 "asi" "_default8" 119 25
"001" "202009153888905655307" "SendPurchaseOrder" "HoldPurchaseOrder" "Purchase Order status changing to ""H"" (Hold)" yes 2020-09-15T10:48:09.157 "asi" "_default8" 120 25
"001" "202009153888905655308" "SendPurchaseOrder" "ReleasePurchaseOrder" "Purchase Order status changing from ""H"" (Hold)" yes 2020-09-15T10:48:09.169 "asi" "_default8" 121 25
"001" "202009153888905655309" "SendPurchaseOrder" "ReopenPurchaseOrder" "Trigger when Purchase Order is re-opened" yes 2020-09-15T10:48:09.181 "asi" "_default8" 122 25
"001" "202009153888905655310" "SendPurchaseOrder" "ClosePurchaseOrder" "Trigger when Purchase Order is closed from closepo.p" yes 2020-09-15T10:48:09.193 "asi" "_default8" 123 25
"001" "202009153888905655311" "SendPurchaseOrder" "DeletePurchaseOrder" "Trigger when Purchase order is deleted" yes 2020-09-15T10:48:09.205 "asi" "_default8" 124 25
"001" "202009153888905655312" "SendPurchaseOrder" "ReprintPurchaseOrder" "Trigger when Purchase Order Printed flag is ""YES""" yes 2020-09-15T10:48:09.217 "asi" "_default8" 125 25
"001" "202005312751105329006" "SendAdvancedShipNotice" "ReprintBillOfLading" "Triggers when BOL is re-printed" no 2020-05-31T07:38:31.905 "jay" "Test" 5023 5007
"001" "202005312751105329007" "SendAdvancedShipNotice" "PrintBillOfLading" "Triggers when BOL is printed" no 2020-05-31T07:38:31.925 "jay" "Test" 5024 5007
"001" "202005312751105329008" "SendAdvancedShipNotice" "AddCustomer" "Adds a customer" yes 2020-05-31T07:38:31.931 "jay" "Test" 5025 5007
"001" "202005312751105329009" "SendAdvancedShipNotice" "PrintRelease" "SendCustomer when Release is Printed First Time" no 2020-05-31T07:38:31.938 "jay" "Test" 5026 5007
"001" "202005312751105329010" "SendAdvancedShipNotice" "ReprintRelease" "SendCustomer when Release is Reprinted" no 2020-05-31T07:38:31.945 "jay" "Test" 5027 5007
"001" "202006053267605337059" "SendJob" "PrintJob" "Triggers when Job printed flag is NO" no 2020-06-05T09:04:36.014 "jay" "eWMS" 5036 5010
"001" "202006053267605337060" "SendJob" "ReprintJob" "Triggers when Job printed flag is YES" no 2020-06-05T09:04:36.021 "jay" "eWMS" 5037 5010
"001" "202006258075805408466" "SendOrderAck" "PrintOrderAck" "" no 2020-06-25T22:25:58.705 "matt" "test" 5039 5013
"001" "202006258075805408467" "SendOrderAck" "RePrintOrderAck" "" no 2020-06-25T22:25:58.724 "matt" "test" 5040 5013
"001" "202006294318505415375" "SendAdvancedShipNotice" "ReprintBillOfLading" "Triggers when BOL is re-printed" no 2020-06-29T11:59:45.991 "wade" "DSG" 5041 5014
"001" "202006294318605415376" "SendAdvancedShipNotice" "PrintBillOfLading" "Triggers when BOL is printed" no 2020-06-29T11:59:46.005 "wade" "DSG" 5042 5014
"001" "202006294318605415377" "SendAdvancedShipNotice" "AddCustomer" "Adds a customer" yes 2020-06-29T11:59:46.012 "wade" "DSG" 5043 5014
"001" "202006294318605415378" "SendAdvancedShipNotice" "PrintRelease" "SendCustomer when Release is Printed First Time" no 2020-06-29T11:59:46.018 "wade" "DSG" 5044 5014
"001" "202006294318605415379" "SendAdvancedShipNotice" "ReprintRelease" "SendCustomer when Release is Reprinted" no 2020-06-29T11:59:46.024 "wade" "DSG" 5045 5014
"001" "202006303733805419428" "CalculateTax" "GetTaxAmount" "Fetch Tax Amount" no 2020-07-22T15:47:44.360 "jay" "Vertex" 5046 5015
"001" "202007315084905520535" "SendPurchaseOrder" "PrintPurchaseOrder" "Print purchase order" yes 2020-09-01T15:23:54.921 "paul" "CSC Penn" 5056 5017
"001" "202007315084905520536" "SendPurchaseOrder" "RePrintPurchaseOrder" "RePrint purchase order" yes 2020-09-01T15:23:58.860 "paul" "CSC Penn" 5057 5017
"001" "202008102359505530175" "CalculateTax" "GetTaxAmountFinal" "Notifies Vertex this is of Invoice message type" no 2020-08-10T06:33:15.508 "user1" "Vertex" 5058 5015
"001" "202008185634805591278" "SendRelease" "PrintRelease" "Sends Release when Release is printed" no 2020-08-18T15:39:08.709 "laurel" "Tester" 5097 5025
"001" "202008185634805591279" "SendRelease" "ReprintRelease" "Sends Release when Release is reprinted" no 2020-08-18T15:39:08.731 "laurel" "Tester" 5098 5025
"001" "202008185634805591280" "SendRelease" "UpdateRelease" "Sends Release when Release is updated" no 2020-08-18T15:39:08.737 "laurel" "Tester" 5099 5025
"001" "202008185634805591281" "SendRelease" "DeleteRelease" "Sends Release when Release is deleted" no 2020-08-18T15:39:08.744 "laurel" "Tester" 5100 5025
"001" "202008185634805591282" "SendRelease" "CreateRelease" "Sends Release when Release is created" no 2020-08-18T15:39:08.750 "laurel" "Tester" 5101 5025
"001" "202008185634805591283" "SendRelease" "ApproveRelease" "Sends Release when Release is approved" no 2020-08-18T15:39:08.756 "laurel" "Tester" 5102 5025
"001" "202008185634805591284" "SendRelease" "HoldRelease" "Sends Release when Release is hold" no 2020-08-18T15:39:08.762 "laurel" "Tester" 5103 5025
"001" "202009036079905625995" "SendInvoice" "PrintInvoice" "Generate cXML invoice on print of invoice" no 2020-09-03T16:53:28.363 "jay" "UPS" 5114 5028
"001" "202009036079905625996" "SendInvoice" "RePrintInvoice" "Generate cXML invoice on re-print of invoice" yes 2020-09-03T16:53:19.887 "jay" "UPS" 5115 5028
"001" "202009036079905625997" "SendInvoice" "PostInvoice" "Generate cXML invoice on post of invoice" yes 2020-09-03T16:53:19.895 "jay" "UPS" 5116 5028
"001" "202009147125105654808" "SendPurchaseOrder" "PrintPurchaseOrder" "Print purchase order" no 2020-09-14T19:54:48.193 "matt" "AlliFlutes" 5145 5033
"001" "202009147125105654809" "SendPurchaseOrder" "RePrintPurchaseOrder" "RePrint purchase order" no 2020-09-14T19:57:17.245 "matt" "AlliFlutes" 5146 5033
"001" "202009147813205654884" "SendPurchaseOrder" "PrintPurchaseOrder" "Print purchase order" no 2020-09-14T21:42:12.032 "mattasi" "CorrChoice" 5147 5034
"001" "202009147813205654885" "SendPurchaseOrder" "RePrintPurchaseOrder" "RePrint purchase order" no 2020-09-14T21:42:12.047 "mattasi" "CorrChoice" 5148 5034
"001" "202009148145205654917" "SendPurchaseOrder" "PrintPurchaseOrder" "Print purchase order" no 2020-09-14T22:37:32.939 "matt" "GP" 5149 5035
"001" "202009148145205654918" "SendPurchaseOrder" "RePrintPurchaseOrder" "RePrint purchase order" no 2020-09-14T22:37:32.945 "matt" "GP" 5150 5035
