"10" "OE" 0 "OU1" "" "Confirm Order Release" "Confirm Order To Be Released" "" "Are you sure you want to release this order from hold?" "" "" yes 0 900 no no "MESSAGE-ACTION" "yes" "" "2020080355168.NoSuper"
"11" "NS" 0 "NS" "" "Changing Company/Location" "Changing Company/Location" "" "Changing Company/Location will CLOSE all currently OPEN Programs." "" "" yes 0 900 no no "QUESTION-YN" "Yes" "" "2020080355168.NoSuper"
"9" "OE" 0 "OU1" "" "Confirm Order Hold" "Confirm Order To Be On Hold" "" "Are you sure you want to place this order on hold?" "" "" yes 0 900 no no "QUESTION-YN" "Yes" "" "2020080355168.NoSuper"
"4" "EF" 0 "EF" "" "Copy Inks in Estimate" "Do you want to copy Inks?" "" "Do you want to  'Update Other Estimates'  for inks?" "" "-OptionsXXXXXXXXXX" yes 0 900 no yes "QUESTION-YN" "Yes" "" "2020080355168.NoSuper"
"5" "PO" 0 "PU1" "" "Warn if duplicate PO exists for the same Job" "Duplicate PO Number" "" "An existing PO exists for this job. " "" "" yes 0 900 yes no "MESSAGE-ACTION" "YES" "" "2020080355168.NoSuper"
"6" "DW" 0 "DW" "" "DW and DT Display warrning message when post" "Warning" "" "NK1 - RM=FG setting is set to force the FG quantity to be equal to the raw materials issued plus spoilage is set, and a record does not meet this criteria, so will not post.  Either change NK1 or adjust the quantity to match as expected." "" "" yes 0 900 no no "ERROR" "" "" "2020080355168.NoSuper"
"7" "EC" 0 "EF" "" "EC and EF Import Price button on print tab" "Import Prices for All Quantities" "" "Import Prices for All Quantities' - Yes - will update all quantities', No - will update only selected quantity" "" "" yes 0 900 no no "QUESTION-YN" "Yes" "" "2020080355168.NoSuper"
"8" "OE" 0 "OU1" "" "Credit Hold Message" "Credit Hold Message (Default)" "" "The customer does not pass the credit check, thus this order will be placed on Hold." "" "-OptionsXXXXXXXXXX" yes 0 900 no yes "INFO" "" "" "2020080355168.NoSuper"
"12" "PO" 0 "PU1" "" "Warn if customer on credit hold" "Customer is on credit hold" "" "Please note: This FG is defined for a customer that is on credit hold. " "" "" yes 0 900 yes no "MESSAGE-ACTION" "yes" "" "2020080355168.NoSuper"
"13" "AP" 0 "VU1" "" "Invoice with no line items" "Invoice with no line items" "" "You cannot have an invoice with no line items - either add line item or delete the invoice." "" "" ? 0 900 no no "INFO" "" "" "2020080355168.NoSuper"
"14" "IC2" 0 "IC2" "" "Physical Count tag" "Physical Count tag" "" "There is already a count entry for this tag in location. Are you sure you want to add another count entry for this tag?" "" "" ? 0 900 yes no "QUESTION-YN" "Yes" "" "2020080355168.NoSuper"
"15" "FG" 0 "IF1" "" "Unposted Inventory Transactions" "Unposted Inventory Transactions" "" "There are unposted FG transactions for this item that may affect quantities currently available.  Post all transactions first for an accurate current balance.  Do you want to proceed?" "" "" ? 0 900 no no "QUESTION-YN" "Yes" "" "2020080355168.NoSuper"
"16" "FG" 0 "IF1" "" "Unposted Inventory Transactions" "Unposted Inventory Transactions" "" "There are unposted FG transactions for this item that may affect this items costs. Post all transactions first if desired." "" "" ? 0 900 yes no "QUESTION-YN" "no" "" "2020080355168.NoSuper"
"17" "AH" 0 "ALL" "" "Audit History Not Enabled" "Audit History Not Enabled" "" "The audit history is not currently enabled for this table or field.  The table must be enabled to audited in order to track the history of changes before auditing can be accesed.  Check anyway?" "" "" ? 0 900 no no "QUESTION-YN" "No" "" "2020080355168.NoSuper"
"18" "GL" 0 "GF2" "" "G/L Account Deletion Message" "G/L Account Deletion" "" "This G/L Account has existing transaction records and cannot be deleted.  Changing this account to inactive will remove its ability to be used." "" "" ? 0 900 no no "INFO" "" "" "2020080355168.NoSuper"
"19" "FG" 0 "IU1" "" "Receipt Rules" "Receipt Rules" "" "The system settings do not allow receipts to a closed job or PO and as such, this receipt is not permitted without re-opening the job or PO before receipt." "" "" ? 0 900 no no "ERROR" "" "" "2020080355168.NoSuper"
"20" "FG" 0 "IF1" "" "Recalculate Inventory locations" "Recalculate Inventory Locations" "" "Unspecified locations exist for the item. Do you want to recalculate the quantity?" "" "" ? 0 900 no no "QUESTION-YN" "Yes" "" "2020080355168.NoSuper"
"21" "FG" 0 "IF1" "" "Unspecified Allocations" "Unspecified Locations" "" "The unspecified location is usually because there are orders without releases for this item,  so the ship from location cannot be determined or a location is not configured for the item, but has a release planned for that location." "" "" ? 0 900 no no "INFO" "" "" "2020080355168.NoSuper"
"22" "ALL" 0 "F3" "" "Help Unavailable" "Help Unavailable" "" "Help is unavailable at the moment.  Please check back again at a later time or report to Advantzware support." "" "" ? 0 900 no no "INFO" "" "" "2020080355168.NoSuper"
"23" "ALL" 0 "MM" "" "Upgrade Permission Issue" "You do not have permission to upgrade" "" "Upgrades are only available for system administrators with a security level for an Admin user.  Please contact your administrator to schedule an upgrade." "" "" ? 0 900 no no "INFO" "" "" "2020080355168.NoSuper"
"24" "ALL" 0 "MM" "" "Upgrade Not Available" "No new upgrade available" "" "Congratulations! Your system has already been upgraded to the latest version of the Advantzware system." "" "" ? 0 900 no no "INFO" "" "" "2020080355168.NoSuper"
"25" "OE" 0 "OU1" "" "Freight Calculation Disabled" "Freight Calculation is disabled" "" "Freight calculation is disabled and will not be calculated.  See system administrator if freight calculation should be enabled." "" "" ? 0 900 no no "MESSAGE" "" "" "2020080355168.NoSuper"
"26" "OE" 0 "OU1" "" "Calculate Freight" "Calculate Freight?" "" "Calculate Freight?" "" "" ? 0 900 no no "QUESTION-YN" "Yes" "" "2020080355168.NoSuper"
"27" "CE" 0 "EC" "" "Inactive Machine" "Inactive Machine Cannot be Used" "" "This machine is set as inactive in E-B-1 (Machine File) and cannot be added to the routing while inactive." "" "" ? 0 900 no no "INFO" "" "" "2020080355168.NoSuper"
"28" "CE" 0 "EF" "" "Recalculate Quantity on Goto" "Recalculate Request/Yield Quantities?" "" "The sheets have changed for the form and since there are multiple items on this form, do you want to recalculate the yield quantities?" "" "" ? 0 900 no no "QUESTION-YN" "yes" "" "2020080355168.NoSuper"
"29" "CE" 0 "EF" "" "Save Goto Details?" "Do you want to save before exit?" "" "Some changes have not been saved for all items - Do you want to save them?" "" "" ? 0 900 no no "QUESTION-YN" "yes" "" "2020080355168.NoSuper"
"30" "CE" 0 "EF" "" "Estimate Calc Message" "Estimate Calculation Message" "" "You are about to calculate the estimate and this message can be used to provide a warning or message to the user.  it is exected as part of the calculation in estimating, but is Suppressed by default." "" "" ? 0 900 no no "MESSAGE" "" "" "2020080355168.NoSuper"
"31" "SYS" 0 "NA" "" "Starting Upgrade Process" "About to Start Upgrade" "" "You are about to start the Advantzware Upgrade process.
Press Yes to close this session and begin the download.  You can continue the upgrade by following the instructions on that page.
Press No to cancel the upgrade and return to the main menu." "" "" ? 0 900 no no "QUESTION-YN" "yes" "" "2020080355168.NoSuper"
"32" "OE" 0 "OU1" "" "Price Hold Warning - Acknowledgement" "Order is on Price Hold" "" "This order is on Price Hold and until removed from price hold status, this acknowledgement cannot be printed.  " "" "" ? 0 900 no no "WARNING" "" "" "2020080355168.NoSuper"
"33" "OE" 0 "OU1" "" "Create PO Warning for Price Hold" "Order is on Price Hold" "" "Order is on price hold, thus purchasing items for this order is not recommended unless you are sure that the order will be approved and processed.  Do you want to proceed?" "" "" ? 0 900 no no "QUESTION-YN" "Yes" "" "2020080355168.NoSuper"
"34" "FG" 0 "IU1" "" "Close Job when Receiving FG Receipts" "Close Job?" "" "The receipts are within the count expected to close the job.  Do you want to close the job?" "" "" ? 0 900 no no "QUESTION-YN" "No" "" "2020080355168.NoSuper"
"35" "AR" 0 "AF1" "" "Warn if change to AR Class" "Changing AR Class" "" "This customer has an account balance and changing class will not change GL balance in existing AR Class.  Are you sure you want to change this?" "" "" ? 0 900 no no "MESSAGE-ACTION" "Ask" "" "2020080355168.NoSuper"
"36" "OE" 0 "OU1" "" "Release Scheduled Quantity" "Release scheduled quantity is zero" "" "This release has no scheduled quantity and cannot be released." "" "" ? 0 900 yes no "MESSAGE-ACTION" "no" "" "2020080355168.NoSuper"
"37" "SS" 0 "BB" "" "Unplanned Item on Release" "Unplanned item on this release" "" "This release does not contain the item being scanned.  Do you want to add this to the release?" "" "" ? 0 900 no no "MESSAGE-ACTION" "No" "" "2020080355168.NoSuper"
"38" "ALL" 0 "ALL" "" "Save Before Leaving Panel" "Save your changes before leaving screen" "" "You are in update mode and must save or cancel your changes before leaving screen." "" "" ? 0 900 no no "WARNING" "" "" "2020080355168.NoSuper"
"39" "FG" 0 "IU3" "" "Transfer Confirmation Message" "Transfer Confirmation" "" "Transfer complete." "" "" ? 0 900 no no "INFO" "" "" "2020080355168.NoSuper"
"40" "CE" 0 "EC" "" "Reset Box Design" "Do you wish to reset box design?" "" "The style or size may have changed, do you wish to reset the box design?" "" "" ? 0 900 no no "MESSAGE-ACTION" "Ask" "" "2020080355168.NoSuper"
"41" "OE" 0 "OB4" "" "Warn about invoice posting" "Printed and approved invoices only" "" "Only printed and approved invoices will be posted." "" "" ? 0 900 no no "INFO" "" "" "2020080355168.NoSuper"
"42" "OE" 0 "OB4" "" "Invoice Date Problem" "Invoice Date Range Exception" "" "Only invoices in the period specified can be posted.  Select a date range within the period specified or change the period." "" "" ? 0 900 no no "WARNING" "" "" "2020080355168.NoSuper"
"43" "OE" 0 "OB4" "" "Post Invoices Prompt" "Posting Invoices" "" "Do you want to post the invoices listed?" "" "" ? 0 900 no no "MESSAGE-ACTION" "Ask" "" "2020080355168.NoSuper"
"44" "OE" 0 "OU1" "" "Update Carrier Release" "Update Carrier on Release" "" "Do you want to update the carrier on all planned releases?" "" "" ? 0 900 no no "MESSAGE-ACTION" "Ask" "" "2020080355168.NoSuper"
"45" "OE" 0 "OU1" "" "Confirm Field Impact" "Notice of Performance Impact" "" "The following selections will significantly impact the speed of this report:
1) Including Zero balances
2) Print Summary by Bin Qty
or adding any field from a Release including: 
3) Release Quantity
4) Release PO Number
" "" "" ? 0 900 no no "WARNING" "" "" "2020080355168.NoSuper"
"46" "OE" 0 "" "" "Deleted Order" "Order has been deleted" "" "This order has been deleted." "" "" ? 0 900 no no "INFO" "" "" "202009034506405625862"
"47" "OE" 0 "" "" "Deleted Job" "Job has been deleted" "" "This Job has been deleted." "" "" ? 0 900 no no "INFO" "" "" "202009034650705625864"
"48" "OE" 0 "OU1" "" "Cannot Create Job" "Order is on hold" "" "The order status is on hold, thus your system settings O-F-2 will not create a job until the order is removed from hold status." "" "" ? 0 900 no no "WARNING" "" "" "202009194444805672425"
"49" "OE" 0 "OU1" "" "Cannot Create Job" "Customer is on hold" "" "The Customer is on hold, thus your system settings will not create a job until the customer is removed from hold status." "" "" ? 0 900 no no "WARNING" "" "" "202009194454205672426"
"50" "AR" 0 "AF4" "" "Duplicate Terms Code" "Duplicate Terms Code" "" "This is not a unique terms code so enter a unique value." "" "" ? 0 900 no no "WARNING" "" "" "202009201736205672446"
"51" "OE" 0 "OU1" "" "Prep Item Disposed" "Prep Item Disposed" "" "The prep item is not available as it has been disposed of according to the disposal date in the prep file." "" "" ? 0 900 no no "WARNING" "" "" "202009208180905672952"
"52" "PO" 0 "PU1" "" "No Vendor Adder Cost" "No Vendor cost for this adder" "" "There is no cost defined for this vendor for the adder.  Cost will be zero." "" "" ? 0 900 no no "WARNING" "" "" "202009283141105691273"
"53" "FG" 0 "OS1" "" "Tag on Hold" "Tag on Hold" "" "This tag has a status is on Hold. Are you sure you want to use this tag?" "" "" ? 0 900 yes no "MESSAGE-ACTION" "Yes" "StockId,TagStatus,TagStatusDescription,ItemName" "202010037986505744393"
"54" "ALL" 0 "" "" "Must save before leaving" "Cannot leave with unsaved changes" "" "Please save or cancel your changes before leaving the screen." "" "" ? 0 900 no no "WARNING" "" "" "202010065826205746736"
"55" "FG" 0 "OS1" "" "Tag on Hold" "Tag on Hold" "" "Tag $StockId$ has a status of $TagStatusDescription$ which is on Hold and thus cannot be changed until it is removed from being on hold. This is controlled by the NK1 = SSTagStatus value being set to 'No'" "" "" ? 0 900 no no "WARNING" "No" "StockId,TagStatus,TagStatusDescription,ItemName" "202010194761705770599"
"56" "CE" 0 "OU1" "" "Delete FG Item from Estimate" "Delete FG Item from Estimate" "" "Do you want to permanently delete $FGItemNumber$ from estimate $Estimate$?" "" "" ? 0 900 no no "MESSAGE-ACTION" "Ask" "FGItemNumber,Estimate" "202011045730305820189"
"57" "PO" 0 "PU1" "" "Purchase Limit Exceeded" "Purchase Order Limit Exceeded" "" "The purchase order total value exceeds your purchase limit of $PurchaseLimit$ and will be placed on hold until approved by a user with a higher purchase limit." "" "" ? 0 900 no no "WARNING" "" "PurchaseLimit" "202011271731205884493"
"58" "SS" 0 "BB" "" "Print BOL Confirmation" "Print BOL Confirmation" "" "Are you sure you want to print the BOL now?" "" "" ? 0 900 no no "MESSAGE-ACTION" "Ask" "" "202012065248605902988"
"59" "ALL" 0 "" "" "Convert User Defined Column Data" "Convert User Defined Columns" "" "Do you want to convert your previous column layout information to the new format?" "" "" ? 0 900 yes no "MESSAGE-ACTION" "yes" "" "202012283206305926639"
"60" "GL" 0 "GC9" "" "Open Sub Ledgers Warning" "Open Sub Ledgers Exist" "" "There are open sub ledgers that must be closed before the month can be closed.  Do you want to continue?" "" "" ? 0 900 no no "MESSAGE-ACTION" "Yes" "" "202101030076605935942"
"61" "AP" 0 "VU1" "" "AP Invoice Quantity Warning" "Invoice Quantity Match" "" "The quantity entered exceeds the received quantity.  Are you sure you want to continue?" "" "" ? 0 900 no no "MESSAGE-ACTION" "Ask" "" "202103177367207076125"
"62" "OP" 0 "OU1" "" "Order Overs Update" "Update Overs on order lines?" "" "Do you want to update all order lines overs based on this change?" "" "" ? 0 900 no no "MESSAGE-ACTION" "Ask" "" "202103250166407100688"
"63" "AP" 0 "VU1" "" "VU1 Receipts Selection" "Reselect Receipts" "" "Negative receipts available for PO. Do you want to re-select receipts?" "" "" ? 0 900 no no "QUESTION-YN" "No" "" "202103252351507100924"
"64" "AP" 0 "VU1" "" "VU1 Vendor Match" "Vendor does not match PO" "" "This vendor does not match the vendor on the PO - do you want to continue?" "" "" ? 0 900 no no "MESSAGE-ACTION" "Ask" "" "202104090068707142102"
"65" "SS" 0 "BS" "" "Confirm RM Item Issue" "Confirm RM Item Issue" "" "Tag ""$Tag$"" belongs to a different item # ""$RMItemID$"". 
Do you want to continue?" "" "" ? 0 900 no no "QUESTION-YN" "yes" "Tag,RMItemID" "202104124460907148308"
"66" "FG" 0 "IF1" "" "Confirm FG Transactions Exist" "Confirm FG Transactions Exist" "" "There are unposted transactions for this item.  Do you want to recalculate before these are processed?" "" "" ? 0 900 no no "MESSAGE-ACTION" "No" "" "202104144541707155056"
"67" "FG" 0 "BS" "" "Confirm Printing Loadtags" "Job is Closed" "" "This job is closed.  Are you sure you want to print loadtags for this closed job?" "" "" ? 0 900 no no "MESSAGE-ACTION" "Ask" "" "202105135818707285426"
"68" "FG" 0 "BS" "" "No Records Found" "No Records Found" "" "There are no detail records found for this $Item$ in location $Loc$." "" "" ? 0 900 no no "MESSAGE" "" "Item, Loc" "202106062900607379006"
"69" "FG" 0 "IF1" "" "Post Adjustment Entry" "Post inventory Adjustment" "" "This adjustment will create an adjustment in I-U-3.  Do you want to post this transaction now? " "" "" ? 0 900 no no "MESSAGE-ACTION" "Ask" "" "202106166124607438518"
"70" "FG" 0 "IU1" "" "Negative Receipt Verification" "Negative Receipt Verification" "" "You are attempting to enter a negative receipt that will reduce this tag to a negative value - Are you sure you want to enter this negative receipt?" "" "" ? 0 900 no no "QUESTION-YN" "Yes" "" "202106293664107522148"
"71" "FG" 0 "IU5" "" "Close Job Without Materials Issues" "Close Job Without Material Issues" "" "This job should be closed, but materials are not yet issued in full.  Do you want to close this job?" "" "" ? 0 1000 yes no "MESSAGE-ACTION" "no" "" "202109138562807996172"
"72" "TS" 0 "TS" "" "Warn User if material issue is not complete" "Board is Not issued" "" "The board for this form and blank is not issued and should be issued before proceeding with this press operation." "" "" ? 0 900 yes no "WARNING" "" "" "202109140117907996200"
"73" "OE" 0 "OU1" "" "Prompt User to Update Release Dates" "Update Release Dates" "" "Update all line items and release dates with this due date?" "" "" ? 0 900 no no "MESSAGE-ACTION" "Ask" "" "202201110166308453941"
