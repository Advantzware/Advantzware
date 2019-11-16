"001" "201909253853104020189" "CheckTransfer" "TransmitBankFile" "Transmit Bank File" yes 2019-09-25T10:42:11.614 "user1" "8" 2 7
"001" "201909061651603786360" "SendRelease" "PrintRelease" "Prints a release" yes 2019-09-06T04:35:16.781 "user1" "Siggins" 3 5
"001" "201909061657203786361" "SendCustomer" "AddCustomer" "Adds a customer" yes 2019-09-06T04:36:12.924 "user1" "Siggins" 4 1
"001" "201909061660803786362" "SendVendor" "AddVendor" "Adds a Vendor" yes 2019-09-06T04:36:48.272 "user1" "Siggins" 5 2
"001" "201909061662903786363" "SendFinishedGood" "AddFinishedGood" "Update a Finished Good" yes 2019-09-06T04:37:09.363 "user1" "Siggins" 6 3
"001" "201909061667903786364" "SendPurchaseOrder" "UpdatePurchaseOrder" "Updates a Purchase Order" yes 2019-09-06T04:37:59.897 "user1" "Siggins" 7 4
"001" "201910032303704020298" "SendPurchaseOrder" "ModifyPurchaseOrder" "Purchase Order's Ship ID is changed and Status not equal to ""H"" (Hold)" yes 2019-10-03T06:27:16.852 "user1" "Siggins" 8 4
"001" "201910032303704020299" "SendPurchaseOrder" "HoldPurchaseOrder" "Purchase Order status changing to ""H"" (Hold)" yes 2019-10-03T06:27:11.276 "user1" "Siggins" 9 4
"001" "201910032303704020300" "SendPurchaseOrder" "ReleasePurchaseOrder" "Purchase Order status changing from ""H"" (Hold)" yes 2019-10-03T06:27:54.790 "user1" "Siggins" 10 4
"001" "201910032303704020301" "SendPurchaseOrder" "PrintPurchaseOrder" "Trigger when Purchase Order Printed flag is ""NO""" yes 2019-10-03T06:30:55.974 "user1" "Siggins" 11 4
"001" "201910032303704020302" "SendPurchaseOrder" "ReprintPurchaseOrder" "Trigger when Purchase Order Printed flag is ""YES""" yes 2019-10-03T06:31:17.207 "user1" "Siggins" 12 4
"001" "201910032303704020303" "SendPurchaseOrder" "DeletePurchaseOrder" "Trigger when Purchase order is deleted" yes 2019-10-03T06:31:40.006 "user1" "Siggins" 13 4
"001" "201910032303704020304" "SendPurchaseOrder" "ClosePurchaseOrder" "Trigger when Purchase Order is closed from closepo.p" yes 2019-10-03T06:32:31.996 "user1" "Siggins" 14 4
"001" "201910032303704020305" "SendPurchaseOrder" "ReopenPurchaseOrder" "Trigger when Purchase Order is re-opened" yes 2019-10-03T06:33:01.455 "user1" "Siggins" 15 4
"001" "201910041242304020315" "SendPurchaseOrderStatus" "ClosePurchaseOrder" "Trigger on close of PO (manual or automatic), send when PO Status is being set to ""C""" yes 2019-10-04T03:35:44.136 "user1" "Siggins" 16 8
"001" "201910041248104020316" "SendPurchaseOrderStatus" "HoldPurchaseOrder" "Trigger on status change to Hold of PO" yes 2019-10-04T03:28:01.813 "user1" "Siggins" 17 8
"001" "201910041264504020317" "SendPurchaseOrderStatus" "ReleasePurchaseOrder" "Trigger on PO status change from Hold to Release ( Except for status ""C"")" yes 2019-10-04T03:30:45.886 "user1" "Siggins" 18 8
"001" "201910041274804020318" "SendPurchaseOrderStatus" "ReopenPurchaseOrder" "Trigger on close of PO (manual or automatic). PO status being changed from ""C""" yes 2019-10-04T03:32:28.624 "user1" "Siggins" 19 8
"001" "201910041280404020319" "SendPurchaseOrderLineStatus" "ClosePurchaseOrderLine" "Trigger on close of PO line (manual or automatic)" yes 2019-10-04T03:33:24.075 "user1" "Siggins" 20 9
"001" "201910041288804020320" "SendPurchaseOrderLineStatus" "ReopenPurchaseOrderLine" "Trigger on open of PO line (manual or automatic)" yes 2019-10-04T03:34:48.260 "user1" "Siggins" 21 9
"001" "201910045101604020337" "SendPurchaseOrder" "TriggerGetPurchaseOrder" "Trigger to fetch purchase order details" yes 2019-10-04T14:10:16.921 "user1" "Siggins" 22 4
.
PSC
filename=APIOutboundTrigger
records=0000000000021
ldbname=ASI
timestamp=2019/11/15-12:43:02
numformat=44,46
dateformat=mdy-1950
map=NO-MAP
cpstream=IBM850
.
0000003573
