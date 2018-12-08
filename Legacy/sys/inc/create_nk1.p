/* This procedure should contain an entry for all NK1 values to be created */
/* This is for the purpose of eventually replacing the include files with  */
/* sys/ref/nk1look.p. As each nk1 value is replaced, check this program    */
/* to make sure it is handled here                                         */
DEF INPUT PARAMETER ip-co-code AS CHAR NO-UNDO.
DEF INPUT PARAMETER ip-nk1-value AS CHAR NO-UNDO. 

DEFINE VARIABLE cocode      AS CHARACTER NO-UNDO.
DEFINE VARIABLE g_company   AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcompany    AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-sort-name AS LOGICAL   NO-UNDO.
DEFINE VARIABLE v-chkflg    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE v-alloc     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE v-po-qty    AS LOGICAL   NO-UNDO.
/* If no special code is needed, the NK1 can be added in a standard way */
DEFINE VARIABLE v-std-list  AS CHARACTER NO-UNDO.

ASSIGN
  cocode    = ip-co-code
  gcompany  = ip-co-code
  g_company = ip-co-code.

/*add new NK1 to v-std-list first, then add it to the "When" clause below */
v-std-list = "LoadTagSSCC,IR12,OEDateChange,FGRecptPassWord,InvStatus,BOLQtyPopup,AgeDays,OEFGADD,HighBalDays,"
           + "oeShipFrom,SSFGSCAN,Paperless,FGSetAssembly,AutoFGIssue,CustomerList,SSLoadtag,ChkFmtACH,"
           + "OESellPriceXfer,OEPO#Xfer,SSBolEmail,OEDateAuto,QuoteNotes,OEPriceMatrixCheck,GLJournalPost,"
           + "FGRecptUnit,OeDateWarn,PREPMASTER,POFarmOutScores,OEQtyPerUnitWarn,APMatTypeExceptions," 
           + "OEJobHold,lmLock,CESAMPLE,DefaultDir,JobHoldReason,ASIHelpService,CRMAuthToken,TSAMPMWarn,SSScanVendor," 
           + "OEBOLPrompt,SHTCALCWarn,BOLFMTTran,BOLMaster,SalesBudget,CEMarkupMatrixInterpolate,CEMarkupMatrixLookup,"
           + "KiwiT,BusinessFormModal,LoadTagXprintImage,CEGotoCalc,FGKEEPZEROBIN,RMKEEPZEROBIN,PrePressHotFolderIn,"
           + "PrePressHotFolderOut,METRIC,CEImportForm,CEImportFormFolder,BusinessFormLogo,CalcBtnImage,CalcBtnLink,DCClosedJobs,"
           + "ImportFolder,ImportLog,TagFormat,FgItemHideCalcFields,VendCostMatrix,RelSkipRecalc,RMAllowAdd,CECostSave,RMOverrunCostProtection,"
           + "SSBOLPassword,BOLImageFooter,InvAddDate,POFGDims,OEPriceHold,POConfigDir,EDILogs,AutoLogout,AutoLogoutLocal,RMTagValidation,"
           + "MenuLink1,MenuLink2,MenuLink3,MenuLink4,MenuLink5,MenuLink6,MenuLink7,MenuLink8,MenuLinkASI,MenuLinkZoHo,MenuLinkUpgrade,"
           + "BitMap,CEMenu,BOLPartial,OEAutoDateUpdate,SSPostFGTransfer,FGUnderOver,FGSetAdjustReason,AdjustReason,ShipNotesExpanded,CTIDir,"
           + "TRBREAKSQTY,MENUIMAGE,CERouteFromStyle"
           .

IF CAN-DO(v-std-list,ip-nk1-value) THEN
CASE ip-nk1-value:
    WHEN "LoadTagSSCC" THEN
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
                INPUT "Load Tag SSCC",
                INPUT "" /* Char Value */, INPUT 0 /* Int value */,
                INPUT NO /* Logical value */).
    WHEN "IR12" THEN
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT YES /* Prompt? */,
                  INPUT "Prompt for custom IR12?",
                  INPUT "" /* Char Value */, INPUT 0 /* Int value */,
                  INPUT NO /* Logical value */).
    WHEN "OEDateChange" THEN
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
                  INPUT "Force entry of date change reason?",
                  INPUT "" /* Char Value */, INPUT 0 /* Int value */,
                  INPUT NO /* Logical value */).
    WHEN "Paperless" THEN
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
                  INPUT "Activate Paperless Logic by Customer?",
                  INPUT "" /* Char Value */, INPUT 0 /* Int value */,
                  INPUT NO /* Logical value */).
    WHEN "FGSetAssembly" THEN
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
                  INPUT "FG Set Receipt Logic to Relieve Component On Hand Inventory",
                  INPUT "" /* Char Value */, INPUT 0 /* Int value */,
                  INPUT NO /* Logical value */).
    WHEN "FGRecptPassWord" THEN
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
                  INPUT "Password for override of job # in I-U-1",
                  INPUT "" /* Char Value */, INPUT 0 /* Int value */,
                  INPUT NO /* Logical value */).
    WHEN "InvStatus" THEN
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
                  INPUT "Invoice Status and Post Bills / Create Invoices One at Time",
                  INPUT "" /* Char Value */, INPUT 0 /* Int value */,
                  INPUT NO /* Logical value */).
    WHEN "BOLQtyPopup" THEN
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
                  INPUT "Show BOL Qty Warning Message",
                  INPUT "" /* Char Value */, INPUT 0 /* Int value */,
                  INPUT YES /* Logical value */).
    WHEN "AgeDays" THEN
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
                   INPUT "Calc Avg Days to Pay only with invoices paid within X (Leave 0 for all)",
                   INPUT "" /* Char Value */, INPUT 0 /* Int value */,
                   INPUT YES /* Logical value */).
    WHEN "OEFGADD" THEN
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
                   INPUT "Prompts to Create an Item?",
                   INPUT "" /* Char Value */, INPUT 0 /* Int value */,
                   INPUT YES /* Logical value */).
    WHEN "HighBalDays" THEN
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
                   INPUT "Calc High Balance for customer within X days (Leave 0 for no recalc)",
                   INPUT "" /* Char Value */, INPUT 0 /* Int value */,
                   INPUT YES /* Logical value */).
    WHEN "oeShipFrom" THEN
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
                   INPUT "Prompt for a ship from code on each order line?",
                   INPUT "" /* Char Value */, INPUT 0 /* Int value */,
                   INPUT NO /* Logical value */).
    WHEN "SSFGSCAN" THEN
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
                   INPUT "Prompt for the Warehouse/Bin?",
                   INPUT "" /* Char Value */, INPUT 0 /* Int value */,
                   INPUT YES /* Logical value */).
    WHEN "AutoFGIssue" THEN
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
                   INPUT "AUTO ISSUE FARM OUTS to JOBS for POs with ORDER#",
                   INPUT "" /* Char Value */, INPUT 0 /* Int value */,
                   INPUT NO /* Logical value */).
    WHEN "FGPostCmp" THEN
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
                   INPUT "Post FG Components along with Set Header",
                   INPUT "" /* Char Value */, INPUT 0 /* Int value */,
                   INPUT NO /* Logical value */).
    WHEN "CustomerList" THEN
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
                   INPUT "Define Customer List for Reporting",
                   INPUT "" /* Char Value */, INPUT 0 /* Int value */,
                   INPUT NO /* Logical value */).
    WHEN "SSLoadtag" THEN
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
                   INPUT "Prompt for print confirmation?",
                   INPUT "" /* Char Value */, INPUT 0 /* Int value */,
                   INPUT YES /* Logical value */).
    WHEN "ChkFmtACH" THEN
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
                   INPUT "ACH Remittance format for ACH Check Runs",
                   INPUT "ASI" /* Char Value */, INPUT 0 /* Int value */,
                   INPUT NO /* Logical value */).
    WHEN "OESellPriceXfer" THEN
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
                   INPUT "Transfer OE Price to Invoice?",
                   INPUT "" /* Char Value */, INPUT 0 /* Int value */,
                   INPUT NO /* Logical value */).
    WHEN "OEPO#Xfer" THEN
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
                   INPUT "Transfer PO# from Order to Invoice?",
                   INPUT "" /* Char Value */, INPUT 0 /* Int value */,
                   INPUT NO /* Logical value */).
    WHEN "SSBolEmail" THEN
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
                   INPUT "Generate SS Emails from release & BOL Changes?",
                   INPUT "" /* Char Value */, INPUT 0 /* Int value */,
                   INPUT YES /* Logical value */).
    WHEN "OEDateAuto" THEN
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
                   INPUT "Auto calculate due date and promise date?",
                   INPUT "" /* Char Value */, INPUT 0 /* Int value */,
                   INPUT NO /* Logical value */).
    WHEN "QuoteNotes" THEN
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
                   INPUT "Specify quote number for default quote notes",
                   INPUT "" /* Char Value */, INPUT 0 /* Int value */,
                   INPUT NO /* Logical value */).
    WHEN "OEPriceMatrixCheck" THEN
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
                  INPUT "Compare Order Qty Against Price Matrix?",
                  INPUT "" /* Char Value */, INPUT 0 /* Int value */,
                  INPUT NO /* Logical value */).
    WHEN "GLJournalPost" THEN
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
                  INPUT "Prompt if GL journals are out of balance before posting?",
                  INPUT "" /* Char Value */, INPUT 0 /* Int value */,
                  INPUT NO /* Logical value */).
    WHEN "FGRecptUnit" THEN
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
                  INPUT "Unit Count for FGRecpt Character Value = AutoPost",
                  INPUT "Pallet Counts" /* Char Value */, INPUT 0 /* Int value */,
                  INPUT NO /* Logical value */).
    WHEN "OEDateWarn" THEN
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
                  INPUT "Warn user dates are entered too far into the future?",
                  INPUT "" /* Char Value */, INPUT 90 /* Int value */,
                  INPUT NO /* Logical value */).
    WHEN "PrepMaster" THEN
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
                  INPUT "Set Defaults for Prep File Creation",
                  INPUT "" /* Char Value */, INPUT 0 /* Int value */,
                  INPUT NO /* Logical value */).
    WHEN "POFarmOutScores" THEN
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
                  INPUT "For all FG Items, transfer dimensions to the scores button?",
                  INPUT "" /* Char Value */, INPUT 0 /* Int value */,
                  INPUT NO /* Logical value */).
    WHEN "OEQtyPerUnitWarn" THEN
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
                  INPUT "Disallow updating of the Order Qty if Order Qty > Qty/Unit",
                  INPUT "" /* Char Value */, INPUT 0 /* Int value */,
                  INPUT NO /* Logical value */).
    WHEN "APMatTypeExceptions" THEN
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
                  INPUT "Materials Types that don't require receipts for AP invoices",
                  INPUT "MOXY789@" /* Char Value */, INPUT 0 /* Int value */,
                  INPUT NO /* Logical value */).
    WHEN "OEJobHold" THEN 
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
                          INPUT "Synch Job Hold status with Order Hold Status",
                          INPUT "" /* Char Value */, INPUT 0 /* Int value */,
                          INPUT NO /* Logical value */).
    WHEN "lmLock" THEN 
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
                          INPUT "Label Matrix Locking Option",
                          INPUT "" /* Char Value */, INPUT 0 /* Int value */,
                          INPUT NO /* Logical value */).
    WHEN "CESample" THEN 
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
                          INPUT "Sample/Spec/NOC Format  ",
                          INPUT "Partitions" /* Char Value */, INPUT 0 /* Int value */,
                          INPUT NO /* Logical value */).
    WHEN "DefaultDir" THEN 
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
                          INPUT "Set default folder for Attachments and Images",
                          INPUT "" /* Char Value */, INPUT 0 /* Int value */,
                          INPUT NO /* Logical value */).                                                    
    WHEN "JobHoldReason" THEN 
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
                          INPUT "Prompt for Reason when Job Placed on Hold",
                          INPUT "" /* Char Value */, INPUT 0 /* Int value */,
                          INPUT NO /* Logical value */).
    WHEN "ASIHelpService" THEN
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
                          INPUT "ASI Help Services",
                          INPUT "-WSDL 'http:\\34.203.15.64/asihelpServices/helpmaintenance.asmx?WSDL'" /* Char Value */, INPUT 0 /* Int value */,
                          INPUT NO /* Logical value */).
    WHEN "CRMAuthToken" THEN
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
                          INPUT "ZOHO CRM Authorization Token",
                          INPUT "" /* Char Value */, INPUT 0 /* Int value */,
                          INPUT NO /* Logical value */).
    WHEN "TSAMPMWarn" THEN
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
                          INPUT "AM/PM Toggle Button - Warning when changing AM/PM",
                          INPUT "" /* Char Value */, INPUT 0 /* Int value */,
                          INPUT NO /* Logical value */).
    WHEN "SSScanVendor" THEN
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
                           INPUT "Additional fields required to scan in Scan Vendor Tags",
                           INPUT "" /* Char Value */, INPUT 0 /* Int value */,
                           INPUT NO /* Logical value */).
    WHEN "OEBOLPrompt" THEN
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
                           INPUT "Prompt for BOL/INV on Order Entry Screen ",
                           INPUT "" /* Char Value */, INPUT 0 /* Int value */,
                           INPUT NO /* Logical value */).
    WHEN "SHTCALCWarn" THEN 
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
                           INPUT "Prompt for board differences on Sheet Calc button ",
                           INPUT "" /* Char Value */, INPUT 0 /* Int value */,
                           INPUT NO /* Logical value */).
    WHEN "BOLFMTTran" THEN 
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
                           INPUT "Transfer Bill of Lading Creation ",
                           INPUT "" /* Char Value */, INPUT 0 /* Int value */,
                           INPUT NO /* Logical value */).
    WHEN "BOLMaster" THEN 
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
                           INPUT "Master Bill of Lading Creation ",
                           INPUT "" /* Char Value */, INPUT 0 /* Int value */,
                           INPUT NO /* Logical value */).
    WHEN "SalesBudget" THEN 
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
                           INPUT "Budget Report",
                           INPUT "" /* Char Value */, INPUT 0 /* Int value */,
                           INPUT NO /* Logical value */).
    WHEN "CEMarkupMatrixInterpolate" THEN 
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
                           INPUT "Interpolate markup value on Markup Matrix",
                           INPUT "" /* Char Value */, INPUT 0 /* Int value */,
                           INPUT NO /* Logical value */).  
    WHEN "CEMarkupMatrixLookup" THEN 
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
                           INPUT "Value to use as lookup on Markup Matrix",
                           INPUT "Square Feet" /* Char Value */, INPUT 0 /* Int value */,
                           INPUT NO /* Logical value */).
    WHEN "KiwiT" THEN 
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
                           INPUT "N:\rcode\Kiwi",
                           INPUT "trilakes" /* Char Value */, INPUT 0 /* Int value */,
                           INPUT NO /* Logical value */). 
    WHEN "BusinessFormModal" THEN 
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
                           INPUT "Set Business Form Preview window to Modal (wait to close)?",
                           INPUT "" /* Char Value */, INPUT 0 /* Int value */,
                           INPUT YES /* Logical value */).
    WHEN "LoadTagXprintImage" THEN 
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
                           INPUT "Set Logo on xprint loadtag ",
                           INPUT "" /* Char Value */, INPUT 0 /* Int value */,
                           INPUT NO /* Logical value */).

    WHEN "FGKEEPZEROBIN" THEN 
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
                           INPUT "Keep zero FG bins?",
                           INPUT "" /* Char Value */, INPUT 0 /* Int value */,
                           INPUT NO /* Logical value */).
    WHEN "RMKEEPZEROBIN" THEN 
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
                           INPUT "Keep zero RM bins?",
                           INPUT "" /* Char Value */, INPUT 0 /* Int value */,
                           INPUT NO /* Logical value */).
    WHEN "CEGotoCalc" THEN 
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
        INPUT "Use enhanced GOTO Screen from Estimate?",
        INPUT "" /* Char Value */, INPUT 0 /* Int value */,
        INPUT NO /* Logical value */).
    WHEN "CEPanel" THEN 
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
        INPUT "Check Panel sizes against limits on Machine File?",
        INPUT "" /* Char Value */, INPUT 0 /* Int value */,
        INPUT NO /* Logical value */).
    WHEN "PrePressHotFolderIn" THEN 
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
        INPUT "PrePress Hot Folder In",
        INPUT "" /* Char Value */, INPUT 0 /* Int value */,
        INPUT YES /* Logical value */).
    WHEN "PrePressHotFolderOut" THEN 
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
        INPUT "PrePress Hot Folder Out",
        INPUT "" /* Char Value */, INPUT 0 /* Int value */,
        INPUT YES /* Logical value */).
    WHEN "METRIC" THEN 
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
        INPUT "Default Metric flag to be set",
        INPUT "" /* Char Value */, INPUT 0 /* Int value */,
        INPUT NO /* Logical value */).
    WHEN "CEImportForm" THEN 
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
        INPUT "Enable Import Estimate Form from Excel",
        INPUT "" /* Char Value */, INPUT 0 /* Int value */,
        INPUT NO /* Logical value */).  
    WHEN "CEImportFormFolder" THEN 
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
        INPUT "Default Folder for Import Estimate Form",
        INPUT "C:\temp\" /* Char Value */, INPUT 0 /* Int value */,
        INPUT NO /* Logical value */).
    WHEN "BusinessFormLogo" THEN 
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
        INPUT "Define the path to the logo to be used on the standard Business forms",
        INPUT "" /* Char Value */, INPUT 0 /* Int value */,
        INPUT NO /* Logical value */).
    WHEN "CalcBtnImage" THEN 
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
        INPUT "Calculator Factor Button Image",
        INPUT "Graphics\32x32\calculator.ico" /* Char Value */, INPUT 0 /* Int value */,
        INPUT NO /* Logical value */).
    WHEN "CalcBtnLink" THEN   
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
        INPUT "Calculator Factor Button Link",
        INPUT "http://www.metric-conversions.org/length/millimeters-to-inches.htm" /* Char Value */, INPUT 0 /* Int value */,
        INPUT NO /* Logical value */).
    WHEN "DCClosedJobs" THEN   
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
        INPUT "Prevent Data Collection on Closed Jobs",
        INPUT "" /* Char Value */, INPUT 0 /* Int value */,
        INPUT NO /* Logical value */).
    WHEN "ImportFolder" THEN   
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
        INPUT "Default Folder for Importer File Lookup",
        INPUT "" /* Char Value */, INPUT 0 /* Int value */,
        INPUT NO /* Logical value */).
    WHEN "ImportLog" THEN   
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
        INPUT "Default Folder for Importer Logfile",
        INPUT "" /* Char Value */, INPUT 0 /* Int value */,
        INPUT NO /* Logical value */).
    WHEN "RelSkipRecalc" THEN   
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
        INPUT "Skip inventory recalc for create of actual release",
        INPUT "" /* Char Value */, INPUT 0 /* Int value */,
        INPUT NO /* Logical value */).        
    WHEN "FgItemHideCalcFields" THEN   
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
        INPUT "FG Item Hide IF1 Calculated Fields",
        INPUT "" /* Char Value */, INPUT 0 /* Int value */,
        INPUT NO /* Logical value */).        
    WHEN "TagFormat" THEN   
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
        INPUT "Tag Format string convert of integer value to excel output",
        INPUT "" /* Char Value */, INPUT 0 /* Int value */,
        INPUT NO /* Logical value */).  
    WHEN "VendCostMatrix" THEN   
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
        INPUT "Vendor Cost Matrix - Form or To",
        INPUT "" /* Char Value */, INPUT 0 /* Int value */,
        INPUT NO /* Logical value */). 
    WHEN "RMAllowAdd" THEN   
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
        INPUT "allow auto creation of new Raw material item",
        INPUT "" /* Char Value */, INPUT 0 /* Int value */,
        INPUT YES /* Logical value */).
    WHEN "RMOverrunCostProtection" THEN   
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
        INPUT "Limit Cost of Receipt to Overrun Quantity",
        INPUT "" /* Char Value */, INPUT 0 /* Int value */,
        INPUT NO /* Logical value */).
    WHEN "CECostSave" THEN   
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
        INPUT "Save Options for Cost Calc",
        INPUT "" /* Char Value */, INPUT 0 /* Int value */,
        INPUT NO /* Logical value */).
    WHEN "SSBOLPassword" THEN   
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
        INPUT "BOL password if tag scanned is not on the BOL and the driver scans a tag from a different job#",
        INPUT "" /* Char Value */, INPUT 0 /* Int value */,
        INPUT NO /* Logical value */). 
    WHEN "BOLImageFooter" THEN 
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
        INPUT "Bol Footer Image",
        INPUT "" /* Char Value */, INPUT 0 /* Int value */,
        INPUT NO /* Logical value */).
    WHEN "InvAddDate" THEN   
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
        INPUT "Enter Invoice Date on AU1",
        INPUT "" /* Char Value */, INPUT 0 /* Int value */,
        INPUT NO /* Logical value */).
    WHEN "POFGDims" THEN   
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
        INPUT "Use Box L W D for FG POs (specify Style Types optional)",
        INPUT "" /* Char Value */, INPUT 0 /* Int value */,
        INPUT NO /* Logical value */).
    WHEN "OEPriceHold" THEN   
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
        INPUT "Select Price Hold criteria",
        INPUT "" /* Char Value */, INPUT 0 /* Int value */,
        INPUT NO /* Logical value */).
    WHEN "POConfigDir" THEN   
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
        INPUT "PO Config Directory",
        INPUT ".\custfiles\EDIFiles\POs" /* Char Value */, INPUT 0 /* Int value */,
        INPUT NO /* Logical value */).        
    WHEN "EDILogs" THEN   
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
        INPUT "C:\tmp",
        INPUT "C:\tmp" /* Char Value */, INPUT 0 /* Int value */,
        INPUT NO /* Logical value */).
     WHEN "UserControl" THEN   
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
        INPUT "Folder to write disconnect instructions",
        INPUT "custfiles\userControl" /* Char Value */, INPUT 0 /* Int value */,
        INPUT NO /* Logical value */).       
    WHEN "RMTagValidation" THEN   
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
        INPUT "Validates the tag number upon issue Material Posting",
        INPUT "" /* Char Value */, INPUT 0 /* Int value */,
        INPUT NO /* Logical value */).
    WHEN "MenuLink1" OR WHEN "MenuLink2" OR WHEN "MenuLink3" OR WHEN "MenuLink4" OR
    WHEN "MenuLink5" OR WHEN "MenuLink6" OR WHEN "MenuLink7" OR WHEN "MenuLink8" THEN
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
        INPUT "",
        INPUT "" /* Char Value */, INPUT 0 /* Int value */,
        INPUT NO /* Logical value */).
    WHEN "MenuLinkASI" THEN
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
        INPUT "http://www.advantzware.com",
        INPUT "Graphics\asiicon.ico" /* Char Value */, INPUT 0 /* Int value */,
        INPUT YES /* Logical value */).
    WHEN "MenuLinkZoHo" THEN
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
        INPUT "https://support.zoho.com/portal/advantzware/kb",
        INPUT "Graphics\32x32\question.ico" /* Char Value */, INPUT 0 /* Int value */,
        INPUT YES /* Logical value */).
    WHEN "MenuLinkUpgrade" THEN
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
        INPUT "https://desk.zoho.com/support/advantzware/ShowHomePage.do#Solutions",
        INPUT "Graphics\32x32\question_and_answer.ico" /* Char Value */, INPUT 0 /* Int value */,
        INPUT YES /* Logical value */).
    WHEN "BitMap" THEN
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
        INPUT "Graphics\bigboxes",
        INPUT "" /* Char Value */, INPUT 0 /* Int value */,
        INPUT NO /* Logical value */).
    WHEN "CEMenu" THEN
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
        INPUT "Graphics\bigboxes",
        INPUT "" /* Char Value */, INPUT 0 /* Int value */,
        INPUT NO /* Logical value */).  
    WHEN "BOLPartial" THEN   
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
        INPUT "P/C on Bill of Lading by Order or by Release",
        INPUT "Order Quantity" /* Char Value */, INPUT 0 /* Int value */,
        INPUT NO /* Logical value */).
    WHEN "OEAutoDateUpdate" THEN   
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
        INPUT "Updates orders and releases with any change to transit days or dock appointment days",
        INPUT "Transit&Dock" /* Char Value */, INPUT 0 /* Int value */,
        INPUT NO /* Logical value */). 
   WHEN "FGUnderOver" THEN   
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
        INPUT "Finished Goods Check Overrun and Underrun Method",
        INPUT "UnderRuns and OverRun" /* Char Value */, INPUT 0 /* Int value */,
        INPUT NO /* Logical value */).
   WHEN "FGSetAdjustReason" THEN   
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
        INPUT " Specify the Adjustment Reason code for Set Component reductions",
        INPUT "" /* Char Value */, INPUT 0 /* Int value */,
        INPUT NO /* Logical value */).
  WHEN "AdjustReason" THEN   
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
        INPUT "Require Reason Code when entering inventory Adjustments Applies to RM and FG",
        INPUT "" /* Char Value */, INPUT 0 /* Int value */,
        INPUT NO /* Logical value */).
  WHEN "ShipNotesExpanded" THEN   
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
        INPUT "Ship Notes Expanded Control",
        INPUT "" /* Char Value */, INPUT 0 /* Int value */,
        INPUT NO /* Logical value */).
    WHEN "CTIDir" THEN   
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
        INPUT "Directory for corrtrim export files",
        INPUT ".\CustFiles\DataXfer\CorrTrim" /* Char Value */, INPUT 0 /* Int value */,
        INPUT NO /* Logical value */).       
   WHEN "SSPostFGTransfer" THEN   
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
        INPUT "Sharp Shooter FG Warehouse Trans Transfer Post",
        INPUT "" /* Char Value */, INPUT 0 /* Int value */,
        INPUT NO /* Logical value */).
   WHEN "TSBREAKSQTY" THEN
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
        INPUT "Automatically allocate Run/Waste Qty across Break Transactions",
        INPUT "" /* Char Value */, INPUT 0 /* Int value */,
        INPUT NO /* Logical value */).
   WHEN "MENUIMAGE" THEN
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
        INPUT "Use Menu Option Images",
        INPUT "" /* Char Value */, INPUT 0 /* Int value */,
        INPUT YES /* Logical value */).
   WHEN "CERouteFromStyle" THEN
    RUN sys/inc/addnk1.p (INPUT cocode, INPUT ip-nk1-value, INPUT NO /* Prompt? */,
        INPUT "Set Layout Machine to first machine in Style",
        INPUT "" /* Char Value */, INPUT 0 /* Int value */,
        INPUT NO /* Logical value */).
END CASE.
ELSE
CASE ip-nk1-value:
    WHEN "BOLWeight" THEN DO: {sys\inc\BOLWeight.i} END.
    WHEN "Rmemails" THEN DO: {sys\inc\Rmemails.i} END.
    WHEN "addprep" THEN DO: {sys\inc\addprep.i} END.
    WHEN "addrelse" THEN DO: {sys\inc\addrelse.i} END.
    WHEN "addxfer" THEN DO: {sys\inc\addxfer.i} END.
    WHEN "adjustgl" THEN DO: {sys\inc\adjustgl.i} END.
    WHEN "alliance" THEN DO: /* {sys\inc\alliance.i} poexport */ END.
    WHEN "ap gl#" THEN DO: {sys\inc\ap-gl#.i} END.
    WHEN "apautocheck" THEN DO: {sys\inc\apautocheck.i} END.
    WHEN "apcrmemo" THEN DO: {sys\inc\apcrmemo.i} END.
    WHEN "apdesc" THEN DO: {sys\inc\apdesc.i} END.
    WHEN "apinvmsg" THEN DO: {sys\inc\apinvmsg.i} END.
    WHEN "aplockbx" THEN DO: {sys\inc\aplockbx.i} END.
    WHEN "appaper" THEN DO: {sys\inc\appaper.i} END.
    WHEN "apsecure" THEN DO: {sys\inc\apsecure.i} END.
    WHEN "aptax" THEN DO: {sys\inc\aptax.i} END.
    WHEN "artioscad" THEN DO: {sys\inc\artioscad.i} END.
    WHEN "autopdc" THEN DO: {sys\inc\autopdc.i} END.
    WHEN "autopost" THEN DO: {sys\inc\autopost.i} END.
    WHEN "bardir" THEN DO: {sys\inc\bardir.i} END.
    WHEN "boldate" THEN DO: {sys\inc\boldate.i} END.
    WHEN "boltransfer" THEN DO: {sys\inc\boltransfer.i} END.
    WHEN "bolwhse" THEN DO: {sys\inc\bolwhse.i} END.
    WHEN "boreldate" THEN DO: {sys\inc\boreldate.i} END.
    WHEN "browser" THEN DO: {sys\inc\browser.i} END.
    WHEN "cadcam" THEN DO: {sys\inc\cadcam.i} END.
    WHEN "casetag" THEN DO: {sys\inc\casetag.i} END.
    WHEN "ceboard" THEN DO: {sys\inc\ceboard.i} END.
    WHEN "cecomm" THEN DO: {sys\inc\cecomm.i} END.
    WHEN "cecpurwas" THEN DO: {sys\inc\cecpurwas.i} END.
    WHEN "cecunit" THEN DO: {sys\inc\cecunit.i} END.
    WHEN "cedicad" THEN DO: {sys\inc\cedicad.i} END.
    WHEN "cefgitem" THEN DO: {sys\inc\cefgitem.i} END.
    WHEN "cegoto" THEN DO: {sys\inc\cegoto.i} END.
    WHEN "celayout" THEN DO: {sys\inc\celayout.i} END.
    WHEN "cematl" THEN DO: {sys\inc\cematl.i} END.
    WHEN "cemisc" THEN DO: {sys\inc\cemisc.i} END.
    WHEN "cepartition" THEN DO: {sys\inc\cepartition.i} END.
    WHEN "cepdies" THEN DO: {sys\inc\cepdies.i} END.
    WHEN "ceprep" THEN DO: {sys\inc\ceprep.i} END.
    WHEN "ceprepprice" THEN DO: {sys\inc\ceprepprice.i} END.
    WHEN "ceprice" THEN DO: {sys\inc\ceprice.i} END.
    WHEN "ceprint" THEN DO: {sys\inc\ceprint.i} END.
    WHEN "cercrout" THEN DO: {sys\inc\cercrout.i} END.
    WHEN "ceround" THEN DO: {sys\inc\ceround.i} END.
    WHEN "ceroute#out" THEN DO: {sys\inc\ceroute#out.i} END.
    WHEN "ceroute" THEN DO: {sys\inc\ceroute.i} END.
    WHEN "ceroute1a" THEN DO: {sys\inc\ceroute1a.i} END.
    WHEN "cerun" THEN DO: {sys\inc\cerun.i} END.
    WHEN "cestyle" THEN DO: {sys\inc\cestyle.i} END.
    WHEN "cewhatif" THEN DO: {sys\inc\cewhatif.i} END.
    WHEN "cewhschg" THEN DO: {sys\inc\cewhschg.i} END.
    WHEN "closejob" THEN DO: {sys\inc\closejob.i} END.
    WHEN "corkraft" THEN DO: /* {sys\inc\corkraft.i} poex */ END.
    WHEN "corrchoice" THEN DO: /* {sys\inc\corrchoice.i} poex */ END.
    WHEN "corrtrim" THEN DO: /* {sys\inc\corrtrim.i} poex */ END.
    WHEN "corsuply" THEN DO: {sys\inc\corsuply.i} END.
    WHEN "custpass" THEN DO: {sys\inc\custpass.i} END.
    WHEN "daytopay.p" THEN DO: END.
    WHEN "dcpostgl" THEN DO: {sys\inc\dcpostgl.i} END.
    WHEN "ecbrowse" THEN DO: {sys\inc\ecbrowse.i} END.
    WHEN "estopmch" THEN DO: {sys\inc\estopmch.i} END.
    WHEN "f16to32" THEN DO: END.
    WHEN "f3help" THEN DO: END.
    WHEN "fgbrowse" THEN DO: {sys\inc\fgbrowse.i} END.
    WHEN "fgcascnt" THEN DO: END.
    WHEN "fgclass" THEN DO: {sys\inc\fgclass.i} END.
    WHEN "fgcolors" THEN DO: {sys\inc\fgcolors.i} END.
    WHEN "fgemails" THEN DO: {sys\inc\fgemails.i} END.
    WHEN "fginvrec" THEN DO: {sys\inc\fginvrec.i} END.
    WHEN "fgitemsf" THEN DO: {sys\inc\fgitemsf.i} END.
    WHEN "fgmaster" THEN DO: {sys\inc\fgmaster.i} END.
    WHEN "fgpart#" THEN DO: {sys\inc\fgpart#.i} END.
    WHEN "fgpofrt" THEN DO: {sys\inc\fgpofrt.i} END.
    WHEN "fgpost" THEN DO: {sys\inc\fgpost.i} END.
    WHEN "fgpostgl" THEN DO: {sys\inc\fgpostgl.i} END.
    WHEN "fgrecpt" THEN DO: {sys\inc\fgrecpt.i} END.
    WHEN "fgreorder" THEN DO: {sys\inc\fgreorder.i} END.
    WHEN "fgsecure" THEN DO: {sys\inc\fgsecur.i} END.
    WHEN "fgsetrec" THEN DO: {sys\inc\fgsetrec.i} END.
    WHEN "fgwhsbin" THEN DO: {sys\inc\fgwhsbin.i} END.
    WHEN "foamcost" THEN DO: {sys\inc\foamcost.i} END.
    WHEN "gp" THEN DO: /* {sys\inc\gp.i} poex */ END.
    WHEN "graphic" THEN DO: {sys\inc\graphic.i} END.
    WHEN "hrms" THEN DO: /* {sys\inc\hrms.i} poex */ END.
    WHEN "inexport" THEN DO: {sys\inc\inexport.i} END.
    WHEN "invcopys" THEN DO: {sys\inc\invcopys.i} END.
    WHEN "invdate" THEN DO: {sys\inc\invdate.i} END.
    WHEN "invlotline" THEN DO: {sys\inc\invlotline.i} END.
    WHEN "invpass" THEN DO: {sys\inc\invpass.i} END.
    WHEN "jdedwdir" THEN DO: {sys\inc\jdedwdir.i} END.
    WHEN "job#" THEN DO: {sys\inc\job#.i} END.
    WHEN "jobcard" THEN DO: {sys\inc\jobcard.i} END.
    WHEN "jobdatesmax" THEN DO: {sys\inc\jobdatesmax.i} END.
    WHEN "jobpass" THEN DO: {sys\inc\jobpass.i} END.
    WHEN "jobreopn" THEN DO: {sys\inc\jobreopn.i} END.
    WHEN "kiwi" THEN DO: /* {sys\inc\kiwi.i} poex */ END.
    WHEN "kiwi dir" THEN DO: /* {sys\inc\kiwidir.i} duplicate v-out */ END.
    WHEN "lastship" THEN DO: /* {sys\inc\lastship.i} */ END.
    WHEN "maxbreak" THEN DO: /* {sys\inc\maxbreak.i} */ END.
    WHEN "msfcalc" THEN DO: /* {sys\inc\msfcalc.i} */ END.
    WHEN "ir12" THEN DO: {sys\inc\ir12.i} END.
    WHEN "notepad" THEN DO: {sys\inc\notepad.i} END.
    WHEN "notes" THEN DO: {sys\inc\notes.i} END.
    WHEN "oeautofg" THEN DO: {sys\inc\oeautofg.i} END.
    WHEN "oeclose" THEN DO: {sys\inc\oeclose.i} END.
    WHEN "oecomb" THEN DO: {sys\inc\oecomb.i} END.
    WHEN "oecomm" THEN DO: {sys\inc\oecomm.i} END.
    WHEN "oecredit" THEN DO: {sys\inc\oecredit.i} END.
    WHEN "oedate" THEN DO: {sys\inc\oedate.i} END.
    WHEN "oedelete" THEN DO: {sys\inc\oedelete.i} END.
    WHEN "oeestcom" THEN DO: {sys\inc\oeestcom.i} END.
    WHEN "oeexport" THEN DO: {sys\inc\oeexport.i} END.
    WHEN "oeimport" THEN DO: {sys\inc\oeimport.i} END.
    WHEN "oeinq" THEN DO: {sys\inc\oeinq.i} END.
    WHEN "oeprep" THEN DO: /* {sys\inc\oeprep.i} oepre-ch */ END.
    WHEN "oepreppo" THEN DO: {sys\inc\oepreppo.i} END.
    WHEN "oepreptaxcode" THEN DO: {sys\inc\oepreptaxcode.i} END.
    WHEN "oepricecheck" THEN DO: {sys\inc\oepricecheck.i} END.
    WHEN "oereleas" THEN DO: {sys\inc\oereleas.i} END.
    WHEN "oereleasepopup" THEN DO: {sys\inc\oereleasepop.i} END.
    WHEN "oereordr" THEN DO: {sys\inc\oereordr.i} END.
    WHEN "oeround" THEN DO: {sys\inc\oeround.i} END.
    WHEN "oescreen" THEN DO: {sys\inc\oescreen.i} END.
    WHEN "oeship" THEN DO: {sys\inc\oeship.i} END.
    WHEN "oetandem" THEN DO: {sys\inc\oetandem.i} END.
    WHEN "oeuserid" THEN DO: {sys\inc\oeuserid.i} END.
    WHEN "ou6brows" THEN DO: {sys\inc\ou6brows.i} END.
    WHEN "password" THEN DO: /* {sys\inc\password.i} */ END.
    WHEN "poattach" THEN DO: {sys\inc\poattach.i} END.
    WHEN "pocostq" THEN DO: {sys\inc\pocostq.i} END.
    WHEN "poexport" THEN DO: END.
    WHEN "poimage" THEN DO: /*{sys\inc\poimage.i}  */ END.
    WHEN "poqty" THEN DO: {sys\inc\poqty.i} END.
    WHEN "postdate" THEN DO: {sys\inc\postdate.i} END.
    WHEN "pouom" THEN DO: {sys\inc\pouom.i} END.
    WHEN "pratt" THEN DO: /* {sys\inc\pratt.i} poex */ END.
    WHEN "prepdiebin" THEN DO: {sys\inc\prepdiebin.i} END.
    WHEN "prepdiegl" THEN DO: {sys\inc\prepdiegl.i} END.
    WHEN "prepplbin" THEN DO: {sys\inc\prepplbin.i} END.
    WHEN "prepplgl" THEN DO: {sys\inc\prepplgl.i} END.
    WHEN "prodcode" THEN DO: {sys\inc\prodcode.i} END.
    WHEN "promptmd" THEN DO: {sys\inc\promptmd.i} END.
    WHEN "pushpin" THEN DO: {sys\inc\pushpin.i} END.
    WHEN "quoimage" THEN DO: {sys\inc\quoimage.i} END.
    WHEN "quoitem" THEN DO: {sys\inc\quoitem.i} END.
    WHEN "relcrhold" THEN DO: {sys\inc\relcrhold.i} END.
    WHEN "relmerge" THEN DO: {sys\inc\relmerge.i} END.
    WHEN "reltype" THEN DO: {sys\inc\reltype.i} END.
    WHEN "rfidtag" THEN DO: {sys\inc\rfidtag.i} END.
    WHEN "rfqiso#" THEN DO: {sys\inc\rfqiso#.i} END.
    WHEN "rfqprint" THEN DO: {sys\inc\rfqprint.i} END.
    WHEN "rmbardir" THEN DO: {sys\inc\rmbardir.i} END.
    WHEN "rmissue" THEN DO: {sys\inc\rmissue.i} END.
    WHEN "rmpostgl" THEN DO: {sys\inc\rmpostgl.i} END.
    WHEN "rmrecpt" THEN DO: {sys\inc\rmrecpt.i} END.
    WHEN "rmunderover" THEN DO: {sys\inc\rmunderover.i} END.
    WHEN "runship" THEN DO: {sys\inc\runship.i} END.
    WHEN "schdcard" THEN DO: {sys\inc\schdcard.i} END.
    WHEN "schedule" THEN DO: {sys\inc\schedule.i} END.
    WHEN "sellpric" THEN DO: {sys\inc\sellpric.i} END.
    WHEN "setfold" THEN DO: {sys\inc\setfold.i} END.
    WHEN "setprint" THEN DO: END.
    WHEN "smurfit" THEN DO: /* {sys\inc\smurfit.i} poex */ END.
    WHEN "ssbol" THEN DO: {sys\inc\ssbol.i} END.
    WHEN "ssbolprint" THEN DO: {sys\inc\ssbolprint.i} END.
    WHEN "ssbolscan" THEN DO: {sys\inc\ssbolscan.i} END.
    WHEN "ssfgretc" THEN DO: {sys\inc\ssfgretc.i} END.
    WHEN "ssfgscan" THEN DO: {sys\inc\ssfgscan.i} END.
    WHEN "ssmovefg" THEN DO: {sys\inc\ssmovefg.i} END.
    WHEN "sspostfg" THEN DO: {sys\inc\sspostfg.i} END.
    WHEN "sstransf" THEN DO: {sys\inc\sstransf.i} END.
    WHEN "st.clair" THEN DO: /* {sys\inc\stclair.i} poex */ END.
    WHEN "tag#" THEN DO: {sys\inc\tag#.i} END.
    WHEN "tasklist" THEN DO: {sys\inc\tasklist.i} END.
    WHEN "taxcode" THEN DO: {sys\inc\taxcode.i} END.
    WHEN "tsclock" THEN DO: {sys\inc\tsclock.i} END.
    WHEN "tscomplete" THEN DO: {sys\inc\tscomplete.i} END.
    WHEN "tscomplt" THEN DO: {sys\inc\tscomplt.i} END.
    WHEN "tsdocksec" THEN DO: {sys\inc\tsdocksec.i} END.
    WHEN "tsendwash" THEN DO: {sys\inc\tsendwash.i} END.
    WHEN "tsfinish" THEN DO: {sys\inc\tsfinish.i} END.
    WHEN "tskey" THEN DO: {sys\inc\tskey.i} END.
    WHEN "tslogin" THEN DO: {sys\inc\tslogin.i} END.
    WHEN "tspost" THEN DO: {sys\inc\tspost.i} END.
    WHEN "tspostfg" THEN DO: {sys\inc\tspostfg.i} END.
    WHEN "tsqty" THEN DO: {sys\inc\tsqty.i} END.
    WHEN "tssecure" THEN DO: {sys\inc\tssecure.i} END.
    WHEN "tssetrec" THEN DO: {sys\inc\tssetrec.i} END.
    WHEN "tstime" THEN DO: {sys\inc\tstime.i} END.
    WHEN "tstimeb" THEN DO: {sys\inc\tstimeb.i} END.
    WHEN "unappju2" THEN DO: {sys\inc\unappju2.i} END.
    WHEN "upsfile" THEN DO: {sys\inc\upsfile.i} END.
    WHEN "webroot" THEN DO: {sys\inc\webroot.i} END.
    WHEN "xmlorder" THEN DO: {sys\inc\xmlorder.i} END.
    WHEN "OverwriteJobPlan" THEN DO: {sys\inc\overwriteJobPlan.i} END.
    WHEN "CapacityPage" THEN DO: {sys\inc\capacityPage.i} END.
END CASE.
