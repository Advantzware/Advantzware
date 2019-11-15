"adddcrec.r" "FG" "" "Record FG Receipts for Posted Data Collection" "Create receipts for FG items based on data collection using the last machne." 900
"arinvpay.r" "AR" "NQ4" "Fix AR invoice/due dates" "Update invoices as paid to clear old invoices." 900
"AutoCloseJobs.r" "JC" "" "Close Older Jobs Left Open" "Close old jobs with a range of job numbers and a aged date.  Use to clear old open jobs that should have been closed." 900
"BOLweight.r" "OP" "OS1" "Fix Bill of Lading Weight per Line" "Update Bill of Lading weight for each line item on the bill of lading for a user selected range of Bill of Lading numbers.  The weight per 100 cartons in the I-F-1 finished goods file is used to update the weight for each bill of lading line.   " 900
"BuildFluteMatrix.r" "CE" "" "Restore Flute Stacking Matrix that were Deleted" "Utility to Export and restore stack matrix table for pallets, which have deleted stacking patterns defined on their matric." 900
"Changper.r" "GL" "" "Change Period of Run Number" "Change of a posted GL period and if the period is changed, that the customer and vendor as well as GL period totals can be recalculated." 900
"chgapchk.r" "AP" "VC" "Change AP Check Number" "Change check number utility that will allow the change of check numbers in A/P, A/P Checks, Disbursements for a posted check to correct numbering." 900
"chgorduser.r" "OP" "OU1" "Update Replace User ID on all Orders" "Updated orders to replace the user id on the orders from the old user id to the new user id.  OPtion for open orders only or all orders." 900
"Consolidatexfer.r" "FG" "IF1" "Consolidate Transferred Tags" "For any FG item, all transfers will be consolidated into as few transfer transactions as possible for that item." 900
"CopyCust.r" "AR" "AF1" "Copy Customer From 1 Company to Another Company" "Copy a customer record from one company to another." 900
"CopyNK1.r" "SA" "NK1" "Copy N-K-1 Parameters to Another Company" "Allows user to specify a company to copy from and a copy to company and moves all the NK1 settings to the new company so they match." 900
"copyrm.r" "RM" "MF" "Copy Raw Material from Company A to Company B" "Allows a user to copy an existing RM item from one company to another.  Range of RM items to copy." 900
"CopyRMTagCost.r" "RM" "MF1" "Copy RM Tag Receipt Cost To Issues and Transfer" "Updates the cost of a tag issue and transfer from the receipt cost and updates the job actual costs." 900
"CopyScores.r" "CE" "EC" "Copy Scores on Box Design to Spec Folder W & L" "No Prompt! Immediately copies all scores to the estimates referenced and updates the specs tab." 1000
"copystyle.r" "CE" "EB" "Copy Style from Company A to Company B" "Copy a range of styles from one company to another." 900
"CorKraft.r" "RM" "MF" "Build XRef Table for vendor item numbers." "Allows for the user to define a vendor item number to an existing RM item." 1000
"crtfgloc.r" "FG" "IF1" "Set Up Finished Goods by Location" "System will create locations for all FG items on any order in the system to track the item balances by location." 900
"crtfgrec.r" "FG" "IF" "FG Receipts from BOL Shipments" "Re-create FG receipts based on BOL shipments.  Costs come from job or PO based on all BOL Lines." 1000
"crtinv.r" "OP" "OB1" "Add invoice from BOL lines" "Utility to create missing invoice lines from a BOL.  Allows user to select a BOL (Posted or not) and create an invoice line that is missing." 900
"CustReck.r" "AR" "AF1" "Create Record Key for Customer" "Creates a rec key for any customer that has a blank rec key." 1000
"d-custfg.r" "FG" "IF1" "Resequence FG Item By Customer" "Goes through all FG items and re-sequences them into new FG item numbers using the customer in the FG item number.  Should have devloper review code before running.  Only works for FIBRE FG format." 1000
"d-undord.r" "OP" "OU1" "Undo Deleted Orders" "Removed the deleted status flag for orders deleted to restore them.  Displays the orders that have been deleted on screen and user can change status on each one." 900
"def-rm-item.r" "RM" "MF1" "Update Raw Material Inventory Fields" "Allows a mass update of misc fields based on Estimate/Real/Both items by material type.  Set Cycle, Product code, lead time, warehouse, etc." 900
"DEFE-I-V.r" "RM" "MF1" "Set Board Vendor sheet Limits to 999" "No Prompt!.  Sets all e-item sheet limits to 0 and 999.999 for all RM items by type." 1000
"delapchk.r" "AP" "" "Purge AP Checks" "Purge AP checks and related payment details.  User enters bank number, then range of check numbers to purge." 900
"DeleteOF4Prices.r" "OP" "OF3" "Purge Obsolete Price Matrixes from OF3" "Allows for purging of Price Matrix entries for a customer with ranges for customer, type, item # and category." 900
"delnotes.r" "FG" "IF" "Delete Finished Goods Spec Notes  (Drop Down)" "Allows for mass delete of spec codes from a range of FG item numbers." 900
"delordln.r" "OP" "OU1" "Delete Duplicate Line Item with Qty 0" "Utility to delete duplicate order lines with zero quantity from an order." 900
"DPBOXIMG.R" "CE" "EB" "Export Box Images" "Export box images to a file.  user specifies location for export." 900
"DPINKS.r" "RM" "MF" "Export Inks" "Export of inks and vendor costs for inks." 900
"Dpstyle.r" "CE" "EB" "Export Styles to Style.Dat File" "Export of Styles to a text file." 900
"dumpdata.r" "" "" "Export Cust, Shipto,Soldto, FG,, Order, Invoices" "Allows exports of various tables.  User selects table and file location." 900
"DumpMach.r" "CE" "EB1" "Export Machine and Machine Matrix" "Asks for machine(s) and exports the machine and matrix such that they can be imported back.

LoadMach is the loader of the output." 900
"dumpmodl.r" "SA" "NS8" "Dump Module Records (To .d)" "Data dump of module records." 1000
"FGCAPS.r" "FG" "IF1" "Capitalize all Finished Goods Item Codes" "Converts all FG items to all Capital letters." 900
"fgtagsmm.r" "FC" "IF1" "Find any Tag num that does not contain FG Item" "Find Tag numbers that may not related to the item or are not consistent in the system.  Provides a list on screen of mismatches.

For example: FG Item = ABC123 and if the tag starts with anything other than ABC123 will be presented to the user" 900
"FindReplaceAccNum.r" "GL" "GF2" "Replace a section (level) of GL account numbers" "Search for all GL account segments and replace the segment identified with a replacement segment." 900
"FindReplaceFilePath.r" "FG" "IF1" "Replace file path or drive letter of attachments" "Searches all path locations and allows changing them for the following items: Box Designs, Stack Pattern, FG Item Images.

Used when drive letter or mapping changes." 1000
"Fix-sheet-po.r" "RM" "MF1" "Fix RM Cost of Rolls & Sheets via Outside Vendor" "Reads all Purchase orders and updates the RM cost, length, width and basis weight and related costs." 1000
"fixceitm.r" "CE" "EC" "Update Estimate FG Item Code" "Allows for updating FG item numbers for an estimate.  Normally for a set with multiple forms/blanks." 900
"fixCustOwned.r" "FG" "IF1" "Add physical count to zero out customer owned" "Summaizes the customer owned FG quantity and creates a count for that customer for their owned items based on a range of items and date." 900
"fixfghis.r" "FG" "IF1" "Clear PO# from FG Receipts if Job # present" "If there is a FG history record with both a job and PO, this will clear the PO number and only leave the job number on the history record.

Asks FG Item range." 900
"fixfguom.r" "FG" "IF1" "Set Finished Goods Cost UOM = M" "No Prompt!  Will check all FG items and if Custom, will set the FG cost uom = M." 1000
"fixglhistory.r" "GL" "GQ1" "Edit General Ledger history by Batch" "Editing of the amounts for a posted batch in GL Trans or GL History.

Enter batch id, then editing of any dollar amounts is possible." 900
"FixJ-No.r" "OP" "JU1" "Job-Hdr Already exists with Internal Job Number" "Creates job header if job is created without a header.

Asks job number and job no2." 1000
"fixmachp.r" "CE" "EC" "Build machine attachment partition records" "Just reads all machine attachements and creates a partition record for each." 1000
"fixorders.r" "OP" "OU1" "Delete Order Line Items with Blank Item" "Reads through all orders and deletes all order lines that do not contain an inventory item.  

Asks for date range and can do for open orders or all orders." 900
"Fixreckey.r" "OP" "" "Purge Blank Record Keys that Create Lockups" "Finds blank rec keys in multiple tables and needs to enhanced to allow for the creation of new keys." 1000
"fixsets.r" "ASI" "EC" "Fix Estimate Set Button Item Code and Part#" "Warning - No Prompt!  If the estimate is a set or a single with quantity per set >1 then set the estimate type to be a set estimate. " 1000
"fixsname.r" "ASI" "OU1" "Update Salesman Name on Order Entry" "Bad data clean up.

This should not be needed and this should be done when the salesperson is changed - whatever changes salesperson should change name but this does this." 900
"FixSNote.r" "ASI" "IF1" "Fix Rec Key for Finished Goods Items" "Fixes rec keys but not source of problems." 1000
"FixTransDate.r" "FG" "IU" "Fix Finished Goods Transaction Date" "Allows user to mass change dates if enteredincorrectly." 900
"fixtstme.r" "T" "TD" "Fix Total time of TS machine transactions" "No Prompt! Calculates the total time for all machine transactions." 900
"FixWip.r" "JC" "JC" "Fix Posted Work In Process" "Creates Job machine records and makes sure the number up/on are both not zero." 900
"fx0fgtrn.r" "FG" "IF1" "Fix FG Transfers with Wrong Quantity" "Calculates the total tranfer quantity for anything with a tag, then updates the load tag to represent the proper transferred quantity." 900
"fxacct2.r" "GL" "GC" "Update current year buckets" "Recalculate the period balances of all GL accounts for the current year using the GL history file.

Note: No prompt will be provided, so running this will execute immediately." 900
"fxacctg3.r" "GL" "GY" "Update current & Last year buckets" "Recalculate the period balances of all GL accounts for the current year & Last year using the GL history file.

Note: No prompt will be provided, so running this will execute immediately." 900
"fxactlyo.r" "GL" "" "Update last year opening balance" "Reset the Opening period balances of all GL accounts for last year using the GL account file setting the current year - current period balance.

Note: No prompt will be provided, so running this will execute immediately." 900
"FXBOLPOST.r" "ASI" "OS1" "Restore Bill of Lading to Non Posted Status" "** Needs Discussion **

Seems to just change satus - Is this viable?  Program was deleted so want to know if it makes sense to let people use this." 1000
"Fxestqty.r" "ASI" "E" "Restore Estimate Not Displaying" "** Needs Discussion **

Seems to restore estimate - Was deleted." 900
"FxIvOnly.r" "ASI" "OS1" "Invoice Only Missing Invoice Fix" "** Needs Discussion **" 1000
"FxMnote.r" "JC" "JU1" "Restore Job Machine Notes" "Restores the machine notes on the job from the estimate.  Asks begin and end job number." 900
"fxoeretl.r" "FG" "IF" "Returns should reduce the ship quantity" "No prompt!  This utility will automatiaclly recalulcate the shipped quantity by adding all shipments and reducing that by all returns." 900
"fxoeship.r" "OP" "OU1" "Update Order Browsers Ship Quantity" "Warning No Prompt!  Recalculates the ship quantity of all orders based on the quantity shipped.

** Needs discussion - Looks like sets ship qty twice rather than ship and invoiced? **" 1000
"FxOpened.r" "JC" "JU" "Fix Open Orders, Jobs and PO Status" "Runs (3) other utilities together - FxOpnord, FxopnJob, FxopnPO together." 900
"fxordcst.r" "OP" "OU1" "Fix Order Cost/Margin" "Fix an order cost and margin - User askes for order number, then displays the FG item and cost / margin for each and allows editing." 900
"fxorinvq.r" "OP" "OU1" "Update Order Line Item's Invoiced Qty" "From order to order number with begin and end FG item.  Reads invoices and updates the order inveoiced quantity." 900
"FxPMtxED.r" "OE" "OF3" "Fix Price Matrix Effective Date" "Sets an effective date based on invice date or todays date." 1000
"fxpuruom.r" "RM" "MF" "Purchase UOM Set to EA for all FG & RM Items" "Warning - No Prompt! Sets the purchase uom to be equal to the Purchase Order UOM for both RM and FG items." 1000
"FxRecKey.r" "AR" "AF1" "Fix Customer Record Keys to Fix Notes" "Fix rec key for customer notes and set a customer note date = customer setup or 01/01/2006." 1000
"fxshpqty.r" "OP" "OU1" "Updates Orders Ship Quantity" "Prompts for Begin/End order number.  Updates order ship quantity based on the order range, recalculate the allocation and back order quantity." 900
"FxTrncs2.r" "RM" "MF" "Updates RM History Cost from Purchase Order" "If purchased RM item, then reset the cost of RM item to that of the PO cost." 900
"glinvdif.r" "GL" "" "Report differences in invoice amt vs posted" "Report compares invoce lines to the GL entries created to see if out of balance." 900
"impest.r" "CE" "" "Import Estimate" "Import of estimates template." 900
"LDBOXIMG.R" "UTIL" "EB" "Load Box Image Into the sytem" "Utility to load the path location of the box images." 900
"ldinks.r" "RM" "MF" "Load Inks from Inks.Dat File" "Load of inks into the system from a .d file.  Used to import a standard set of inks." 900
"ldstyle.r" "CE" "EB" "Load Styles from Style.Dat File" "Load styles into the system." 900
"LoadMach.r" "CE" "EB1" "Copy Machine from Company A to Company B" "Allows for a copy of machines from one company to another." 900
"loadmodl.r" "SA" "" "Load Module Records" "Allows for the import of modules into the system." 1000
"LoadPrep.r" "CE" "EB" "Load Prep / Die file from Estimate Spec Info" "Utility to create Prep and Die file from all estimates." 900
"loadtag.r" "FG" "IF" "Update Job# on Loadtag FG transactions." "Update the job number on load tags for a range of tag numbers." 900
"MoveEst.r" "EF" "NR" "Move Estimate Files" "Move estimate files rather than deleting them." 900
"newir12.r" "FG" "IR@" "IR12 New Version" "Custom Version of IR12 reort.

Will be deleted after 16.15.00 release." 1000
"oepostgl.r" "OP" "OB4" "Post OE Invoices to G/L" "Post OE Invoices to G/L accounts." 900
"ohshiped.r" "FG" "IF1" "Show FG Tag # Shipped and Also On Hand Qty" "Based on item number rangt, set tag number that shippe and alsoupdate quntity on hand." 900
"palletissues.r" "SA" "NR" "Update the number of pallets" "" 900
"poreccst.r" "RM" "MF" "Fix Raw Material Invalid  Costs (Task 12270406)" "" 900
"Prodcode.r" "FG" "IF1" "Update the Items Production Code as New / Repeat" "" 900
"PurgeActMchBlankNo.r" "JC" "JQ1" "Purge Blank Number on Sheet Fed Transactions" "" 900
"purgeLO.r" "SA" "NR" "Purge Invalid List Objects" "" 900
"PurgeMU2.r" "RM" "MU2" "Delete UnPosted Raw Material Issues by Date Rang" "" 900
"purgeorders.r" "OP" "OU1" "Purge Orders without Customer and without F.G. items" "" 900
"PurgeOrphanSetParts.r" "FG" "IU1" "Remove Unposted Components Receipts not linked to Set" "" 900
"PurgeTags.r" "SS" "S" "Purge SS Load Tags" "" 900
"purgnote.r" "FG" "IF" "Purge Finished Goods Spec Notes Range" "" 900
"r-ordpst.r" "OP" "OU3" "Order Post/Purge Deleted" "" 900
"rctdpurg.r" "SA" "NR" "Purge FG Transactions by Type" "" 900
"reopenyr.r" "GL" "" "Reopen year after closing" "Re-opens last year after closing and resets balances.

It should be run if you want to reopen the year and once open will require closing each period again." 900
"replacecad.r" "CE" "EB8" "Replace CAD# and File# in Prep Code" "" 900
"replacestyle.r" "" "" "Replace Style & Recalc for Customer" "" 900
"rmbin.r" "MF" "" "Auto Create R/M Bins" "" 900
"rmtagsoh.r" "RM" "MF1" "Show Issued Tags with Neg or On Hand Balance" "" 900
"SetEbOrd.r" "CE" "E" "Update Estimate Order Number" "" 900
"SetEbScores.r" "CE" "EC" "Increase Panels from 20 to 30" "" 900
"setestink.r" "CE" "EF" "Update RM Ink Name & Descriptionto All Estimates" "" 900
"Setfgia.r" "FG" "IF" "Set Inactive Items to Inactive Status" "" 900
"SetImage.r" "FG" "IF1" "Set FG Item Image to NK1 GRAPHIC and FG Item #.jpg" "" 900
"SetInk.r" "RM" "MF" "Ink Setting for Yield, Press, Ink Type and Press Type" "" 900
"SetJobMat.r" "JC" "JQ" "Fix Real Job Materials" "" 900
"setjobstart.r" "JC" "JU" "Set Job Start and Close Dates" "" 900
"Setposcores.r" "CE" "" "Move Estimate Scores to Purchase Order" "" 900
"setpruom.r" "RM" "MF" "Set Purchase FG & RM Purchase UOM via Last Purchase Order" "" 900
"Show-Neg-Jobs.r" "JC" "JU" "Find negative job quantity on Job Routing" "" 900
"unPostRel.r" "OP" "OT1" "Unpost Releases where the release post fails" "" 900
"upd-box.r" "CE" "EB" "Update Box Design Disk Drive Letter" "" 900
"updappay.r" "AP" "" "Update check date of Posted Checks" "Update date of a posted AP check." 900
"updarcal.r" "AR" "" "Update line item of OP/AR invoice" "Update invoice line details." 900
"UpdateEstNo.r" "CE" "NR" "Update Estimate Number" "" 900
"updateFG.r" "FG" "IF1" "Upload a CSV of FG Item Master changes" "" 900
"UpdateFGCounts.r" "FG" "IF1" "UpdateFGCounts" "" 900
"Updateimagepath.r" "CE" "E" "Update Attachments Disk Drive or Folder" "" 900
"UpdateShiptoZones.r" "AR" "AF1" "Update Shipto Zones" "Update ship from warehouse, carrier and delivery zone for a range of customers." 900
"UpdBinPO.r" "FG" "IF1" "Update Vendor PO# on I-F-1 Bin/Jobs Tab" "" 900
"UpdBnPOR.r" "RM" "MF1" "Update Vendor PO# on M-F-1 Bin/Jobs Tab" "" 900
"updcust.r" "AR" "AF1" "Update Customer Fields" "Mass update of customer records to set credit limit and other fields with a rnge of customers." 900
"updestink.r" "CE" "E" "Update Estimate Inks via Raw Goods" "" 900
"updFgShp.r" "FG" "IF" "Ship Method Default by Case or Pallet" "" 900
"UpdFlute.r" "CE" "EC" "To Replace Flute Code throughout System" "" 900
"UpdHistUnitQty.r" "FG" "IF1" "Update History Unit Qty UpdHistUntQty.r" "" 900
"updovun.r" "OP" "OU1" "Underrun/Overrun Transfer to Line Item via Header" "" 900
"updvend#.r" "AP" "" "Update/Change Vendor Number" "Update or Change a vendor number." 900
"upmstest.r" "CE" "EF" "Folding Estimates as Master Tandem Estimates" "" 900
"w-arhead.r" "SA" "NR" "Restore Invoice Purged via N-F-13" "" 900
"w-hrms-x.r" "RM" "MF" "HRMS Board Cross Reference File Utiltiy" "" 900
"w-prdnot.r" "OP" "OU1" "Case Label Colors / Notes" "" 900
"w-updsmn.r" "AF" "AF" "Update Salesman on Estimate,Quotes and Order from Customer" "" 900
"warinvrestore.r" "AR" "NF#" "Restores Purged Invoices via N-F-13" "Retore invoices purges with N-F-13" 1000
"XrefTypetoGroup.r" "AF" "AF1" "COPY the A-F-1 Customer Type to the A-F-1 Cust Group Field." "" 900
"ZeroParts.r" "FG" "IF1" "Set Qty on Hand to Zero for Overrun Parts of Set" "" 900
"updship#.r" "MN" "MN" "Update/Fix Ship To Number" "" 900
"delAllFgRctd.r" "FG" "IU1" "Delete Unposted FG transactions" "Utility is used to delete FG transactions before they are posted.  Asks type and clears all transactions within the date range specified." 900
"module.r" "" "" "License Modules" "License File - Restricted to ASI user only" 1000
"ImpMaster.r" "IMP" "" "Import Master" "Import master that allows access to all the imports licensed." 900
"delDupOrds.r" "OE" "OW" "Delete Duplicate Web Orders" "Searches orders for duplicate orders with matching PO numbers based on a customer and order date.  Can search to find or delete duplicates." 900
"colorchg.r" "00" "" "Advantzware Color Changer" "Utility to change the default color palette for colors in the system." 900
"unPostBOL.r" "OP" "OS1" "Unpost BOLs where the post fails" "If a BOL is marked as posted but did not post to inventory or otherwise did not post correctly." 900
"run-xprint.r" "" "" "run xprint" "" 900
"invLnChck.r" "OP" "" "Invoice Line Check" "Searches based on date to determine if there are any invoices without line items.  Opton to export to a test file." 900
"autoCloseOrder.r" "OE" "" "Auto Close Order" "Close orders automatically based on a parameter settings used." 800
"FGHistoryPurge.r" "FG" "IF1" "FG History Purge/Consolidation" "This allows user to run a purge of FG History up to a date.  It will summarize all transactions up to that date and create an Inventory Count for each bin as of the date that represents all the previous transaction summary detail. 
" 900
"InvBolDateDiff.r" "UTIL" "OS1" "Check and fix Invoice Bol Date Differences" "This utility provides the user the ability to view by BOL #, differences in the dates between a BOL and an Invoice.

It also provides for the ability to change the date of a BOL." 900
"CloseOrderTester.r" "OE" "OU6" "Close Order Tester" "Tests orders to see why they are in current state" 900
"arch-est.r" "CE" "" "Archive Estimates" "Archive and remove old estimates from the system.  This utility can be used to remove old estimates from the system, with the option to output these estimates to and archive file to allow for retoration if desired at a later time." 900
"dupfgtagloc.r" "FG" "IF1" "Duplicate FG Tags Locator" "Creates a file of duplicate tags found in FG bins table." 900
"UpdARInv.r" "AR" "" "Utility to Fix AR Invoice Data" "Allows editing of the invoice totals including invoice date, amount due, tax and freight amounts.
" 900
"ftpTester.r" "EDI" "FF" "Ftp Tester" "FTP Test utility to test sending using FTP." 900
"ItemLocInvRecon.r" "FG" "" "Inventory Allocated Reconciliation Program" "This utility compares the quantity fields allocated by location to that of the FG item as a whole and exposes differences that can be corrected by running the Recalculation, but help find issues in code to correct." 900
"TranMon.r" "UTIL" "" "Transaction Monitor Utility" "" 1000
"updPriceMtxFlg.r" "OE" "" "Update online flag on price matrix(OF3)" "This utility allows for a user to update only the online flag on the price matrix and not allow them to change any other data." 900
"setTaxStat.r" "AR" "" "Set Tax Status Globally" "Allows for setting the customer ship to taxable flags, pdating order lines, invoice lines and such for proper sales tax processing." 900
"fghispur.r" "UTIL" "IF1" "FG History Purge by Job" "This utility will delete all FG detail records for a given range of item number, date, type, and job number.

Note that this function does NOT create summary transactions; use with EXTREME caution." 900
"wLockTransMonitor.r" "UTIL" "" "Transaction and Lock Monitor" "Monitor program to view record locks and track the details for each user to find record locking issues. Also used for monitoring open transactions within the system." 0
"recalcJobs.r" "SA" "NR" "Recalc Jobs" "" 900
"VendorCostConv.r" "FG" "" "Vendor Cost Table Conversion" "This program converts RM Vendor costs, FG Vendor costs as well as Estimating Farm costs to the new cost table format.

This utility should be used to verify and clean up data before upgrading.

" 900
"mkfginks.r" "FG" "" "Inks will copy to the I-F-1 colors" "WARNING: This utility will not prompt the user, and will run and delete the ink colors from the Estimate and update the IF1 ink details." 1000
"FGUpdateCost.r" "FG" "IF1" "Update FG Item Cost Util" "Utility to update a FG item cost details, with or without on hand quantity." 900
"DataLoader.r" "API" "" "Data Backup / Loader" "Make a backup of data and restore it back.  The primary purpose of this tool is to allow a fresh of test environments with data from a live environment and restore back configuration files." 900
"updArDue.r" "AR" "" "Correct amount due on Ar Invoices" "Correct amount due on AR invoices after reviewing spreadsheet." 900
