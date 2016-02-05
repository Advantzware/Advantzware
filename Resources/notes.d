"0116200601462827" 01/16/06 54464 "asi" no "Task#  11120611" "Utility to search data collection for quantity produced on the last machine in the job file, then create a finished goods receipt for the unit count in the finished goods bin file with the cases per pallet from the bin file.    This data may be available on a job previously created before the current job.  Add Date Range so they can select to from January 2, 2006 to January 11, 2006." "" 0 "" "" ""
"0713201208030201" 07/13/12 42174 "asi" no "Task# 07101202" "Task# 07101202
Order 204759 for FG Item IBM-4x4x12 has 5 tags with 5000 boxes each Received.
New Utilitity to consolidate Transfers in FG History File by Date, Time, Tag#, Whse and Bin.
Step to Duplicate.
1)  Create Release for INVOICE only for 25,000 boxes.
2)  Click INVOICE button to create invoice. 
      This creates a -25000 from Customer Blank and 25000 for customer IBM1000.
      Thus 25,000 boxes are owned by customer.
3)   Next create SHIP ONLY Release for FIVE Tags.
       Click BOL button to create Tags.
4)   Post BOL to create Invoice.
      For each BOL Line (Each tag) 
      This creates a 5000 from Customer Blank and -5000 for customer IBM1000.
      Since there are two tags, there are 10 new TRANSFER transactions.
      They all have the same Date and Time.  
      Five Transfers with 5000 with no Customer and Five -5000 Transfers for IBM1000
      
New Utilty.
If Date, Time, Job#, Tag#, Warehouse and Bin are the same, GRAND TOTAL lines.
Net Result is History Tab will show -25,000 for IBM1000 and 25,000 for Blank Customer." "" 0 "" "" ""
"0208200601498738" 02/08/06 37595 "asi" no "Util/CopyCust.r   Task# 08040516" "New Utility to copy customer master file from one company to the next company within the existing database.   ie. from company 001 to company 002.
Provide a dropdown list of customers that may be selected to copy." "" 0 "" "" ""
"0414201005535250" 04/30/10 47829 "asi" no "Copy RM Tag Cost for Board Cost to Tags Transactions" "This will copy the RM History cost from the Receipt to all Issues for that tag#.   
This will also copy this cost to all Transfers and Cycle Count transactions." "" 0 "" "" ""
"1103200501121390" 04/24/06 62592 "asi" no "Create FG Receipts from Posted BOLs Task# 03280303" "This utility will post a FG receipt for all Bill of Ladings that they have posted and are missing a finished good receipt record.   This utility could be run again and again without duplicating the records.   This utility will also transfer the cost from the FG bin record on the bill of lading to the invoice history file.  Therfore, if I reveiw job 879 which only has a bill of lading shipment, this will create a FG receipt matching that BOL.   Same job#, unit counts, partial unit. cost, etc." "" 0 "" "" ""
"0116200601462893" 01/16/06 54720 "asi" no "Task# 05030504  Worksheet to Renumber FG Items by Customer" "Create a worksheet browser that can be updated or deleted.  This browser will show all the FG#'s that you would like to change the customer# within the FG from the old to the new #.  You will be able to do all the items, or delete the ones from the browser that  you do not want to change at this time.  This will work similar to a find and replace utility.  You will be able to specify the characters within the number to change from old to new.  This must validate that the number it is being changed to does not already exist, if it does it will go to the next higher number in the sequence." "" 0 "" "" ""
"1209200501407195" 12/14/05 61572 "asi" no "Task# 12090507" "When orders are deleted, the status is changed.     Only when the order is purged will the order be purged from the system.    Add Utility to Undelete the deleted order." "" 0 "" "" ""
"0208200601499201" 02/08/06 41272 "asi" no "Util/DEFE-I-V.r   To set all RM cost matrix to 999" "This changes all raw material maximum sheet width and maximum sheet length to 999.   If the dimensions are zero, then the vendor names will not appear when calculating an estimate." "" 0 "" "" ""
"1206200601765398" 12/06/06 40955 "asi" no "Task# 01060306" "Util/DPboximg.r   This will download the box image from the database in which the program is run.  This will allow the user to download to any folder and disk drive a necessary.

Util/LDBoximg.r  This will load the box images from any disk drive and folder that is entered on the program." "" 0 "" "" ""
"0208200601499194" 02/08/06 41132 "asi" no "Dump Inks to File" "Utility to dump inks from Advantzware to a file.  Once dumpled, the inks may be loaded into a clients database via the Load utility." "" 0 "" "" ""
"0911200601692327" 09/11/06 37979 "asi" no "Task# 08240601  Dump Master Files to Excel" "Dump customer master, finished goods item master, order header and line item details and invoice history files into Excell format.   The program will display a list of files that may be dumped into excell.   The files must be selected from the list and moved to the right column to confirm the download.  Each file may be downloaded separately and saved as any file name that is typed by the user.  Clients should dump the data once to dump data and test interface.  Once an interface is written and 100% tested, then the dump can be run again to download current data." "" 0 "" "" ""
"0208200601498788" 02/08/06 37761 "asi" no "UTIL/FGCAPS.r  Task# 12210402" "Utility will capitalize all letters in the FG item code." "" 0 "" "" ""
"1208200601769784" 12/08/06 65264 "asi" no "Task# 12070604" "The tag number is SS is created with the FG item code plus additional sequential number.  This utility will show any tag number if the first 15 characters do not match." "" 0 "" "" ""
"0530201409273150" 06/13/14 59694 "asi" no "Utility updates Disk Drive Letter for Paperclip Attachments" "Util/FindReplaceFilePath.r - New Utility to Update the Disk Drive Letter

The customer stores attachments on the PAPER CLIP Icon throughout the system.
They have a new server and new disk drive letter.
They need the beginnning character replaced, which is the disk drive changed.

This utility will allow the user to find and replace the old drive letter (or any folder) with a new drive letter.  
There will be a ""Find File Path:"" and a ""Replace File Path with:""
To change the drives across all attachments, user would simply enter ""P:\"" in the Find field and ""Q:\"" in the Replace field.

Additionally, there should be a check box that says ""Include Box Designs"" that will include these special database tables in the find/replace search.
" "" 0 "" "" ""
"1103200501121383" 02/24/06 38488 "asi" no "Util/UpdEstInk.r   Task# 02230616" "New Utility to Change the Ink Code on Estimates by Estimate Range & Customer Range.
From Old Ink Code  to NEW INK CODE.
Provide status messages what estimate is being updated as utility is running." "" 0 "" "" ""
"0923201107364532" 09/23/11 62103 "asi" no "Task 09221107" "Job-Hdr Already exists with Internal Job Number
Orders cannot be added.
Somehow the job number was reset to 1.


When adding an order, the job number appears on the screen.
Type this job number on the utitily that appears." "" 0 "" "" ""
"0325201308570394" 03/25/13 51028 "asi" no "Util/Fixreckey.r" "Util/Fixreckey.r      
Write New Utility for Customers to Run when Orders Lock Up.
This will delete blank record key values were found in premier's oe-relh table " "" 0 "" "" ""
"0330201106364431" 03/30/11 51312 "asi" no "Fix Release Status Codes" "Utility to Correct Release Status.
If Invoiced and Posted, change DB field to C.
If release is Invoiced by not Posted then Release DB field is Z.
If release is in BOL file then Release status is P.

We cannot afford to have you programming every other week for you to investigate.
We need to display ONLY the DB field in the Release Folder.
I think we are displaying a calculated TEMP field.
Remove the TEMP field from O-U-1 Release Folder and ONLY display the DB Field..

Reports need to use the DB Field." "" 0 "" "" ""
"0228200601526073" 02/28/06 66675 "asi" no "Utility to Fix Set Button Item and Part Codes" "Utility must transfer the SET MAINTENANCE folder to the estimate SET BUTTON including the FG item code, Customer Part#, Item Name, Description and Product Category from the Set FG item code.   ASSEMBLED / UNASSEMBLED Flag from the Set FG item file and the W , L, D from the FG item TOTALS screen." "" 0 "" "" ""
"0302201005453812" 03/02/10 51559 "asi" no "Notes" "Updates last year opening balance on GF2 account file.  It takes this years oponing balance and subtracts last year periods to get figure." "" 0 "" "" ""
"0627201308822019" 01/31/14 41516 "asi" no "Unpost Bill of Lading that Cannot be Found on O-B-1 Invoice" "When Release Status shows Z the Order should reside in the Invoice File of O-B-1.      However, if the Bill of Lading that Cannot be Found on O-B-1 Invoice run this Utility.
This will change BOL status to Not Posted so the Bill of Lading Appears in the O-S-1.
However, the Shipments from the Original Posted Bill of Lading must be Deleted.             Login as ASI                                                                                                         The BOL will not Post because the Tag#'s were previously Shipped, thus On Hand= 0.
Therefore, the Tag# listed on the BOL must be deleted from I-F-1 History.                       Click I-F-1 Inventory File and find the Items and click HISTORY tag.                          Find the SHIPMENT transactions for each Tag# and click the DELETE Button.        Then the BOL can be Reposted.           " "" 0 "" "" ""
"0506201106458382" 05/06/11 64908 "asi" no "Task 05051102" "Customer has orders and jobs for estimates that cannot be found in estimate file.
However, when they run the report E-R, the estimate numbers show.                               " "" 0 "" "" ""
"0318201106354263" 03/18/11 56026 "asi" no "util\fxitemv.r   Vend Cost Matrix Rec Keys Improperly Linked" "util\fxitemv.r   Vend Cost Matrix Rec Keys Improperly Linked
Find 200 C.  Click Vendor Cost Matrix.
If I update STONE's up charges at the bottom of the screen, Inland and LACORR's data changes.   If I change LACORR, then Stone and Inland Changes.

This utility fixes a bug with the COPY button which creates records with same record key.
ADD Button separates records.
" "" 0 "" "" ""
"0117200601464460" 04/25/06 43040 "asi" no "Fixes Orders, Jobs and PO's Open / Closed Status" "This utility updates the open or closed status for all jobs, orders and purchase orders." "" 0 "" "" ""
"1228200501417222" 12/30/05 41530 "asi" no "Task# 12280503" "Utility that replaces blank dates and notes that have no record key.   Without a date, the notes are not shown unless the user clicks the group or department radio button.   In addition a note without a record key will appear on multiple customer codes.
This fixes notes by customer or by contacts under the phone icon.

Likewise fixes problem when Customer Contact Phone info will not stay in the correct customer file.  After adding the  info and it dumps it back to a different account.    This new Utility fixes notes throughout the system that are BLANK and assigns a record key." "" 0 "" "" ""
"0616200601618388" 10/31/06 34313 "asi" no "Util/fxshpqty.r   Task#  04240605" "This utility fixed a bug when closing line iItems of an order.  The quantity allocated was not properly updated.   This will fix the Quantity ""Allocated to Orders"" for all old closed orders.  This also fixes the shipped quantity." "" 0 "" "" ""
"1206200601765400" 12/06/06 41064 "asi" no "Task# 01060306" "Util/DPboximg.r   This will download the box image from the database in which the program is run.  This will allow the user to download to any folder and disk drive a necessary.

Util/LDBoximg.r  This will load the box images from any disk drive and folder that is entered on the program." "" 0 "" "" ""
"1103200501121401" 02/08/06 40917 "asi" no "Load and Dump Inks" "Utility to dump inks from Advantzware to a file.  Once dumpled, the inks may be loaded into a clients database via the Load utility." "" 0 "" "" ""
"0208200601498677" 02/08/06 36761 "asi" no "Util/mkfginks.r  Task# 08040506" "Utility to copy ink colors from the last estimate created for each ink type and download the inks into the finished goods COLOR folder.  This will copy each unique ink type." "" 0 "" "" ""
"1103200501121413" 12/16/13 53749 "asi" no "Task#  12270406" "Printing M-R-11 report for 11/25/04 shows costs multiplied by 1000.    This was a result of a purchase cost UOM positng issue in receipts at the Purchase UOM rather than the Stock/Consumption UOM.

Write utiilty to fix RM receipts transactions and RM Bin Cost by date range.    This will recalculate the purchase UOM cost into the current Consumption UOM in the raw materials file.   Please note, if the INVENTORY folder Consumption UOM is blank, then use EA as the default UOM calculation.

This will convert the purchase orders unit of measure into the raw materials stock / consumption unit of measure.   For example, if you purchased 5000 sheets with UOM of EA or M at cost of $40 / MSF, then received these as cost of $40 per EACH by accident, this will correct the raw material receipt as well as the raw material bin cost.  The problem with $40 each is the cost is incorrectly far to expensive.     This utility will convert the cost / MSF into the cost per EA sheet." "" 0 "" "" ""
"0216200601514685" 02/16/06 54124 "asi" no "ASI Task# 05190508  Define FG Prod Cod as New or Repeat" "This Utility will update all FG items Production Code as New or Repeat based on if the item has 1 order or more orders for each item.  If one order exists, the item will be labels as NEW, whereas if multipe orders exist, then the production code will be defined as REPEAT.

The logical Value will turn on the logic for newly created item.  When creating new finished goods items via Order Entry, Estimating, Job File or Finished Goods, the inventory class will default to NEW.   When an item is added on a second order, then the FG item's Class field will be renamed as REPEAT.  Please note, an item may be on different estimates / jobs, hence the system must verify if another job exists for this item.  " "" 0 "" "" ""
"0517200601596453" 09/01/11 33758 "asi" no "Task#  0503061   util/r-impact.r  Import chart of accounts" "Task#  0503061   util/r-impact.r  Import chart of accounts from excel  
Customer should backup data.
Customer must select company.   
       Click USER above user ID:     Select  Set Company/Location

Filed must be saved as a .csv format.
File will have GL account number, Account Description and Account Type.
This will import to the general ledger accounts.  

Please note, this does not change the Company File.
Utility will DELETE all existing account numbers for the selected company.
User must manually change the field called GL# Account # of Levels found in the Veiw Company Folder to match the import.   For example, if GL account numbers are 9999-000 format then change level to 2 levels with 4 and 3 for digits per level." "" 0 "" "" ""
"0823201005745980" 08/23/10 52172 "asi" no "Task# 07171007" "This will replace the old style with the new style for the estimates of the specific customer entered.    When the Recalc  toggle box is checked, this will recalculate the blank length and blank width, blank square feet and scoring panels identical to the logic on the AUTOCALC button the SPEC folder.  This will also update the finished goods item code.

Old Style ________ 
New Style  _________
Customer _________ 

Add new toggle box.   Auto Recalc ___" "" 0 "" "" ""
"1211200601771474" 12/11/06 47103 "asi" no "Show tags Issued by Negative On Hand" "Tags may be issued from wrong bin location.
Tags issued should not have negative on hand, hence should be cycled counted to 0.
This utility shows the items with zero on hand." "" 0 "" "" ""
"1111200501124820" 11/11/05 50832 "asi" no "Util/SetEstInk.r   Task# 10100504" "New Utility to move the ink description in the raw materials file to all estimates with that ink code.     Please note, any manually changed ink descriptions will be over written by this utility.
Provide  Option to select Estimate Ink or Real Ink and Ink code Range From  INK to INK.
Also toggle box option for INK or VARNISH." "" 0 "" "" ""
"0616201409291787" 06/16/14 35202 "asi" no "UpdateShiptoZones.r  (Task# 06101405)" "Utility will look similar to Util/UpdCust.r  which updates customer Fields.
This new Utility will update the Customer SHIPTO File.
The Excel file will list the ZIP Code and the New Delivery Zone.
Utility will shows the following:

Type Truck/Carrier  _______     Carrier Location  ______ 
The user will enter the Carrier and Location that will be updated in the Shipto File.
For example, they will type TRUCK and MAIN.  F1 will all lookup of Valid Carriers which will import both Carrier and Location.
 
Excel File  ________________________
User will type the Disk Drive, Folder and File Name of the Excel File.  F1 will allow Lookup

Default Zone  __________
In the Excel File to Import, If a Customer Ship to does not have a zip code, then the Default Zone will be written into the zone for that Shipto.

The Excel files shows the ZIP Code and the Carrier to Update.
ONLY cusotmer ship to with the Typed Carrier and Location Typed Above will be updated from the Excel File.   The utility will search each customer ship to code for a match on the excel zip code.  If a match is found, Move the Zone on the Excel File to the Customer Shipto Zone. " "" 0 "" "" ""
"0120201409123068" 01/20/14 55221 "asi" no "Utility to Update Vendor PO# on I-F-1 Bin/Jobs Tab" "Utility will transfer the FG Receipt Vendor PO# on the History File to the Bins Table.
For each Finished Goods Receipt in History, transfer the Vendor PO# to the Tag#/Bin file.  The utility should start updating from 01/01/1990  to Today.
This will insure that the most recent PO# is transferred to the file.

Please note, the program should not create multiple bins for each unique vendor PO#.
For example, if I buy Bubble 1.25""  and receive this the Main warehouse and Floor Bin location.  If I have 4 purchase orders during the year for this item, the last Vendor PO# will be the only Vendor PO# displayed in the Bin file." "" 0 "" "" ""
"0524200601604184" 05/24/06 38684 "asi" no "Utility to Update Line Item Overrun / Underrun %" "New 4.10 database fields called Overrun% and Underrun% on the Line item.  The ADD button will now transfer the percentage to the line item screen from the overrun / underrun from the order entry view folder, which is transferred from the customer file when the estimate# is blank.  This new overrun / underrun would then like this to print on both folding and corrugated factory tickets.

This utility will upate all old orders." "" 0 "" "" ""
"1215200501410633" 12/15/05 38820 "asi" no "Task# 9290411" "Order Information will be downloaded to print case labels.  The format is dictated by N-K-1 Casetag.  This utility allows the user to enter predefined packing notes for the case label such as specific colors.  When selecting a Case Label to print via O-U-10, this will display the Packing Code with a label ""COLOR"" on the Create Case Label Screen.     The F1 key will  show the valid packing created via this utility.  For example, this will show the COLOR (Packing Note) to show a specific list of colors such as RED, BLUE, GREEN, GOLD, PINK, which represents the ribbons on the printer that is printing the labels. The actual field that is updated is on the estimate file is on the Inks/Pack Folder called ""Packing Notes"" which will be downloaded to the text file.  Specific notes may exist for each packing case code for each finished good item on the job.  Updating in the PACK NOTE field on the estimate will update the FG item file and updating the FG item file will update the estimate number defined on the FG item file.   " "" 0 "" "" ""
"0113201409115208" 01/14/14 57224 "asi" no "Task 01031417   util/w-updsmn.r" "When the salesman code is changed on the customer file, 
prompt ""Update Salesman on Estimate and Quotes?    This is just one customer at a time.
Utility can do range of customers.

See Attached.
Toggle Box to Update Estimates
Toggle Box to Update Open Orders
Toggle Box to Update Unposted Invoices

" "" 0 "" "" ""
