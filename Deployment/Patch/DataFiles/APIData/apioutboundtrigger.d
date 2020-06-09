"001" "201909061657203786361" "SendCustomer" "AddCustomer" "Adds a customer" yes 2019-11-26T09:05:46.292 "bpvasi" "_default" 1 1
"001" "201911263255004495512" "SendCustomer" "PrintRelease" "SendCustomer when Release is Printed First Time" no 2019-11-26T09:02:30.899 "bpvasi" "_default" 2 1
"001" "201911263256804495513" "SendCustomer" "ReprintRelease" "SendCustomer when Release is Reprinted" no 2019-11-26T09:02:48.895 "bpvasi" "_default" 3 1
"001" "201909061660803786362" "SendVendor" "AddVendor" "Adds a Vendor" yes 2019-11-26T09:11:37.720 "bpvasi" "_default" 4 2
"001" "201911263308504495517" "SendVendor" "PrintPurchaseOrder" "Sends Vendor when Purchase Order is printed for first time" no 2019-11-26T09:11:25.551 "bpvasi" "_default" 5 2
"001" "201911263309404495518" "SendVendor" "ReprintPurchaseOrder" "Sends Vendor when Purchase Order is reprinted" no 2019-11-26T09:11:34.734 "bpvasi" "_default" 6 2
"001" "201909061662903786363" "SendFinishedGood" "AddFinishedGood" "Update a Finished Good" yes 2019-11-26T09:05:15.713 "bpvasi" "_default" 7 3
"001" "201911263264704495514" "SendFinishedGood" "PrintRelease" "Send Finished Good when release is first printed" no 2019-11-26T09:04:07.777 "bpvasi" "_default" 8 3
"001" "201911263265804495515" "SendFinishedGood" "ReprintRelease" "Send Finished Good when release is reprinted" no 2019-11-26T09:04:18.248 "bpvasi" "_default" 9 3
"001" "202003170950604718910" "SendFinishedGood" "CreateLoadtag" "On creation of loadtags on a Job or Order from O-U-7 screen" no 2020-03-17T02:38:26.846 "user1" "_default" 10 3
"001" "201909061667903786364" "SendPurchaseOrder" "UpdatePurchaseOrder" "Updates a Purchase Order" no 2020-05-25T21:45:38.937 "mattasi" "_default" 11 4
"001" "201910032303704020299" "SendPurchaseOrder" "HoldPurchaseOrder" "Purchase Order status changing to ""H"" (Hold)" no 2020-05-25T21:45:42.552 "mattasi" "_default" 12 4
"001" "201910032303704020300" "SendPurchaseOrder" "ReleasePurchaseOrder" "Purchase Order status changing from ""H"" (Hold)" no 2020-05-25T21:45:50.329 "mattasi" "_default" 13 4
"001" "201910032303704020301" "SendPurchaseOrder" "PrintPurchaseOrder" "Trigger when Purchase Order Printed flag is ""NO""" no 2019-10-03T06:30:55.974 "user1" "_default" 14 4
"001" "201910032303704020302" "SendPurchaseOrder" "ReprintPurchaseOrder" "Trigger when Purchase Order Printed flag is ""YES""" no 2019-10-03T06:31:17.207 "user1" "_default" 15 4
"001" "201910032303704020303" "SendPurchaseOrder" "DeletePurchaseOrder" "Trigger when Purchase order is deleted" no 2019-10-03T06:31:40.006 "user1" "_default" 16 4
"001" "201910032303704020304" "SendPurchaseOrder" "ClosePurchaseOrder" "Trigger when Purchase Order is closed from closepo.p" no 2020-05-25T21:46:00.981 "mattasi" "_default" 17 4
"001" "201910032303704020305" "SendPurchaseOrder" "ReopenPurchaseOrder" "Trigger when Purchase Order is re-opened" yes 2019-11-26T09:06:21.904 "bpvasi" "_default" 18 4
"001" "201910045101604020337" "SendPurchaseOrder" "TriggerGetPurchaseOrder" "Trigger to fetch purchase order details" no 2019-10-04T14:10:16.921 "user1" "_default" 19 4
"001" "201909061651603786360" "SendRelease" "PrintRelease" "Sends Release when Release is printed" no 2020-05-15T10:44:49.388 "user1" "_default" 20 5
"001" "201911263304604495516" "SendRelease" "ReprintRelease" "Sends Release when Release is reprinted" no 2019-11-26T09:10:46.075 "bpvasi" "_default" 21 5
"001" "202005153733805141655" "SendRelease" "UpdateRelease" "Sends Release when Release is updated" no 2020-05-15T10:45:00.394 "user1" "_default" 22 5
"001" "202005153738005141656" "SendRelease" "DeleteRelease" "Sends Release when Release is deleted" no 2020-05-15T10:45:17.159 "user1" "_default" 23 5
"001" "202005153740905141657" "SendRelease" "CreateRelease" "Sends Release when Release is created" no 2020-05-15T10:45:33.742 "user1" "_default" 24 5
"001" "202005153888605141666" "SendRelease" "ApproveRelease" "Sends Release when Release is approved" no 2020-05-15T10:48:06.934 "user1" "_default" 25 5
"001" "202005154397805141667" "SendRelease" "HoldRelease" "Sends Release when Release is hold" no 2020-05-15T12:12:58.160 "user1" "_default" 26 5
"001" "201909253853104020189" "CheckTransfer" "TransmitBankFile" "Transmit Bank File" no 2020-04-13T06:33:12.138 "user1" "_default" 27 6
"001" "201910041242304020315" "SendPurchaseOrderStatus" "ClosePurchaseOrder" "Trigger on close of PO (manual or automatic), send when PO Status is being set to ""C""" yes 2019-11-26T09:09:56.356 "bpvasi" "_default" 28 7
"001" "201910041248104020316" "SendPurchaseOrderStatus" "HoldPurchaseOrder" "Trigger on status change to Hold of PO" yes 2019-11-26T09:09:59.068 "bpvasi" "_default" 29 7
"001" "201910041264504020317" "SendPurchaseOrderStatus" "ReleasePurchaseOrder" "Trigger on PO status change from Hold to Release ( Except for status ""C"")" yes 2019-11-26T09:10:02.511 "bpvasi" "_default" 30 7
"001" "201910041274804020318" "SendPurchaseOrderStatus" "ReopenPurchaseOrder" "Trigger on close of PO (manual or automatic). PO status being changed from ""C""" yes 2019-11-26T09:10:05.172 "bpvasi" "_default" 31 7
"001" "201910041280404020319" "SendPurchaseOrderLineStatus" "ClosePurchaseOrderLine" "Trigger on close of PO line (manual or automatic)" yes 2019-11-26T09:09:23.831 "bpvasi" "_default" 32 8
"001" "201910041288804020320" "SendPurchaseOrderLineStatus" "ReopenPurchaseOrderLine" "Trigger on open of PO line (manual or automatic)" yes 2019-11-26T09:09:26.667 "bpvasi" "_default" 33 8
"001" "202002061940804639939" "SendAdvancedShipNotice" "ReprintBillOfLading" "Triggers when BOL is re-printed" no 2020-04-13T06:33:36.665 "user1" "_default" 34 9
"001" "202001281704705316001" "SendAdvancedShipNotice" "PrintBillOfLading" "Triggers when BOL is printed" no 2020-04-13T06:33:43.667 "user1" "_default" 35 9
"001" "202005225572205311507" "SendInvoice" "PrintInvoice" "Send EDI invoice on print of invoice" no 2020-05-22T15:28:42.411 "wade" "_default" 36 10
"001" "202005225573405311508" "SendInvoice" "RePrintInvoice" "Send EDI invoice on re-print of invoice" no 2020-05-22T15:28:54.367 "wade" "_default" 37 10
"001" "202005225575005311509" "SendInvoice" "PostInvoice" "Send EDI invoice on post of invoice" no 2020-05-22T15:29:10.929 "wade" "_default" 38 10
"001" "202003274269104743620" "SendJob" "PrintJob" "Triggers when Job printed flag is NO" no 2020-03-27T11:51:31.036 "user1" "_default" 39 11
"001" "202003274271604743621" "SendJob" "ReprintJob" "Triggers when Job printed flag is YES" no 2020-03-27T11:51:56.445 "user1" "_default" 40 11
"001" "202004142003005007333" "SendPurchaseOrder" "PrintPurchaseOrder" "Prints purchase order" no 2020-04-14T05:33:50.784 "user1" "_default1" 41 12
"001" "202004142004005007334" "SendPurchaseOrder" "RePrintPurchaseOrder" "RePrints purchase order" no 2020-04-14T05:34:00.696 "user1" "_default1" 42 12
"001" "202004225650705165879" "SendFinishedGood2" "AddFinishedGood" "Update A Finished Good" yes 2020-04-22T15:45:24.984 "Randy" "_default" 43 14
"001" "202004225659505165880" "SendFinishedGood2" "PrintRelease" "Send Finished Good when release is first printed" no 2020-04-22T15:43:15.114 "Randy" "_default" 44 14
"001" "202004225662005165881" "SendFinishedGood2" "ReprintRelease" "Send Finished Good when release is reprinted" no 2020-04-22T15:43:40.859 "Randy" "_default" 45 14
"001" "202004225669505165882" "SendFinishedGood2" "CreateLoadtag" "On creation o floadtags on a Job or Order from O-U-7 screen" no 2020-04-22T15:44:55.302 "Randy" "_default" 46 14
"001" "202005142461105162367" "SendAdvancedShipNotice" "ReprintBillOfLading" "Triggers when BOL is re-printed" no 2020-04-13T06:33:36.665 "user1" "_default1" 47 15
"001" "202005142461105162368" "SendAdvancedShipNotice" "PrintBillOfLading" "Triggers when BOL is printed" no 2020-04-13T06:33:43.667 "user1" "_default1" 48 15
"001" "202005142461105162369" "SendAdvancedShipNotice" "AddCustomer" "Adds a customer" yes 2019-11-26T09:05:46.292 "user1" "_default1" 49 15
"001" "202005142461105162370" "SendAdvancedShipNotice" "PrintRelease" "SendCustomer when Release is Printed First Time" no 2019-11-26T09:02:30.899 "user1" "_default1" 50 15
"001" "202005142461105162371" "SendAdvancedShipNotice" "ReprintRelease" "SendCustomer when Release is Reprinted" no 2019-11-26T09:02:48.895 "user1" "_default1" 51 15
"001" "202005225605905311655" "SendOrderAck" "PrintOrderAck" "" no 2020-05-22T15:34:19.739 "wade" "_default" 52 16
"001" "202005225607305311683" "SendOrderAck" "RePrintOrderAck" "" no 2020-05-22T15:34:33.533 "wade" "_default" 53 16
