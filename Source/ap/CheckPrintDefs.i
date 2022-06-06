
/*------------------------------------------------------------------------
    File        : CheckProcsDefs.i
    Purpose     : 

    Syntax      :

    Description : Holds temp-table and constant definitions for Processing and Printing of Checks

    Author(s)   : BV
BV
    Created     : Tue Feb 18 22:28:43 EST 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE cPrintOrderSCS     AS CHARACTER NO-UNDO INITIAL "Stub,Check,Stub".
DEFINE VARIABLE cPrintOrderCSS     AS CHARACTER NO-UNDO INITIAL "Check,Stub,Stub". 
DEFINE VARIABLE cPrintOrderSSC     AS CHARACTER NO-UNDO INITIAL "Check,Stub,Stub". 
DEFINE VARIABLE cPrintOrderDefault AS CHARACTER NO-UNDO INITIAL "Stub,Check,Stub".

/* Moving partial field defs here to avoid 16K character limit */
DEFINE TEMP-TABLE ttcheckconfig1
 /*Overall output settings*/
 FIELD company AS CHAR
 FIELD outputFile AS CHAR
 FIELD defaultFont AS CHAR INIT "Arial"
 FIELD defaultFontSize AS INT INIT 10
 
 /*Structure Configuration*/
 FIELD panel1Type AS CHAR INIT "Stub"
 FIELD panel2Type AS CHAR INIT "Check"
 FIELD panel3Type AS CHAR INIT "Stub"
 
 /*Overall panel Coordinates*/
 FIELD panel1Row AS DEC INIT 1
 FIELD panel2Row AS DEC INIT 25
 FIELD panel3Row AS DEC INIT 46
 
 /*Properties for check defaults*/
 FIELD checkFont AS CHAR 
 FIELD checkFontSize AS INT 
 
 /*Coordinates for Check Objects*/
 FIELD checkCheckNumberRow AS DEC INIT 5 
 FIELD checkCheckNumberCol AS DEC INIT 80
 FIELD checkAmtInWordsRow AS DEC INIT 9
 FIELD checkAmtInWordsCol AS DEC INIT 8
 FIELD checkAmtRow AS DEC INIT 8
 FIELD checkAmtCol AS DEC INIT 80
 FIELD checkPayDateRow AS DEC INIT 5
 FIELD checkPayDateCol AS DEC INIT 45
 FIELD checkRemitToNameRow AS DEC INIT 8 
 FIELD checkRemitToNameCol AS DEC INIT 8
 FIELD checkRemitToAddNameRow AS DEC INIT 8 
 FIELD checkRemitToAddNameCol AS DEC INIT 8
 FIELD checkRemitToAddressRow AS DEC INIT 10 
 FIELD checkRemitToAddressCol AS DEC INIT 8
 FIELD checkCompanyNameRow AS DEC INIT 2
 FIELD checkCompanyNameCol AS DEC INIT 8
 FIELD checkCompanyAddressRow AS DEC INIT 3
 FIELD checkCompanyAddressCol AS DEC INIT 8
 FIELD checkMemoRow AS DEC INIT 12
 FIELD checkMemoCol AS DEC INIT 50
 
 /*Coordinates for Stub Objects*/
 FIELD stubCheckNumberRow AS DEC INIT 3 
 FIELD stubCheckNumberCol AS DEC INIT 80
 FIELD stubPayDateRow AS DEC INIT 3
 FIELD stubPayDateCol AS DEC INIT 1
 FIELD stubRemitToNameRow AS DEC INIT 3 
 FIELD stubRemitToNameCol AS DEC INIT 20
 FIELD stubCompanyNameRow AS DEC INIT 2
 FIELD stubCompanyNameCol AS DEC INIT 24
 FIELD stubRegHeadRow AS INT INIT 5
 FIELD stubRegSuppNoticeRow AS INT INIT 5
 FIELD stubRegSuppNoticeCol AS INT INIT 5
 FIELD stubRegDetailRow AS INT INIT 6
 FIELD stubRegInvNoCol AS INT INIT 3
 FIELD stubRegPOCol AS INT INIT 18
 FIELD stubRegInvDateCol AS INT INIT 45
 FIELD stubRegAmtGrossCol AS INT INIT 55
 FIELD stubRegAmtDiscCol AS INT INIT 65
 FIELD stubRegAmtNetCol AS INT INIT 75
 FIELD stubRegTotLabelCol AS INT INIT 30
 
 /*Properties for check objects*/ 
 FIELD checkCheckNumberShow AS LOG INIT YES 
 FIELD checkCheckNumberFont AS CHAR 
 FIELD checkCheckNumberFontSize AS INT 
 FIELD checkCheckNumberNumSize AS INT INIT 9
 FIELD checkCheckNumberBold AS LOG INIT YES
 FIELD checkCheckNumberUnderline AS LOG
 FIELD checkCheckNumberItalic AS LOG 
 FIELD checkCheckNumberRightJustify AS LOG INIT YES
 
 FIELD checkAmtInWordsShow AS LOG INIT YES
 FIELD checkAmtInWordsFont AS CHAR 
 FIELD checkAmtInWordsFontSize AS INT 
 FIELD checkAmtInWordsSize AS INT INIT 70
 FIELD checkAmtInWordsBold AS LOG 
 FIELD checkAmtInWordsUnderline AS LOG
 FIELD checkAmtInWordsItalic AS LOG 
 FIELD checkAmtInWordsRightJustify AS LOG
 FIELD checkAmtInWordsAndAsFraction AS LOG INIT YES
 FIELD checkAmtInWordsZeroAsNo AS LOG INIT YES
 FIELD checkAmtInWordsFillChar AS CHAR INIT "*"
 FIELD checkAmtInWordsFillCenter AS LOG INIT YES
 FIELD checkAmtInWordsAllCaps AS LOG
 
 FIELD checkAmtShow AS LOG INIT YES 
 FIELD checkAmtFont AS CHAR 
 FIELD checkAmtFontSize AS INT 
 FIELD checkAmtNumSize AS INT INIT 9
 FIELD checkAmtDecimals AS INT INIT 2
 FIELD checkAmtBold AS LOG 
 FIELD checkAmtUnderline AS LOG
 FIELD checkAmtItalic AS LOG 
 FIELD checkAmtRightJustify AS LOG INIT YES
 FIELD checkAmtComma AS LOG INIT YES
 FIELD checkAmtCurrSymb AS CHAR 
 .

DEFINE {1} TEMP-TABLE ttCheckConfig LIKE ttcheckconfig1 
 FIELD checkPayDateShow AS LOG INIT YES
 FIELD checkPayDateFont AS CHAR 
 FIELD checkPayDateFontSize AS CHAR 
 FIELD checkPayDateSize AS INT INIT 30
 FIELD checkPayDateBold AS LOG 
 FIELD checkPayDateUnderline AS LOG INIT YES
 FIELD checkPayDateItalic AS LOG 
 FIELD checkPayDateRightJustify AS LOG
 FIELD checkPayDateAllCaps AS LOG
 FIELD checkPayDateFormat AS CHAR INIT "MM/DD/YYYY"

 FIELD checkMemoShow AS LOG INIT YES
 FIELD checkMemoFont AS CHAR 
 FIELD checkMemoFontSize AS INT 
 FIELD checkMemoSize AS INT INIT 70
 FIELD checkMemoBold AS LOG 
 FIELD checkMemoUnderline AS LOG 
 FIELD checkMemoItalic AS LOG 
 FIELD checkMemoRightJustify AS LOG 
 FIELD checkMemoAllCaps AS LOG 
 
 FIELD checkRemitToNameShow AS LOG INIT YES
 FIELD checkRemitToNameFont AS CHAR 
 FIELD checkRemitToNameFontSize AS INT 
 FIELD checkRemitToNameSize AS INT INIT 70
 FIELD checkRemitToNameBold AS LOG 
 FIELD checkRemitToNameUnderline AS LOG
 FIELD checkRemitToNameItalic AS LOG 
 FIELD checkRemitToNameRightJustify AS LOG
 FIELD checkRemitToNameAllCaps AS LOG INIT YES

 FIELD checkRemitToAddNameShow AS LOG INIT YES
 FIELD checkRemitToAddNameFont AS CHAR 
 FIELD checkRemitToAddNameFontSize AS INT 
 FIELD checkRemitToAddNameSize AS INT INIT 70
 FIELD checkRemitToAddNameBold AS LOG 
 FIELD checkRemitToAddNameUnderline AS LOG
 FIELD checkRemitToAddNameItalic AS LOG
 FIELD checkRemitToAddNameRightJustify AS LOG
 FIELD checkRemitToAddNameAllCaps AS LOG INIT YES
 
 FIELD checkRemitToAddressShow AS LOG INIT YES
 FIELD checkRemitToAddressFont AS CHAR 
 FIELD checkRemitToAddressFontSize AS INT INIT 8
 FIELD checkRemitToAddressSize AS INT INIT 70
 FIELD checkRemitToAddressBold AS LOG 
 FIELD checkRemitToAddressUnderline AS LOG
 FIELD checkRemitToAddressItalic AS LOG 
 FIELD checkRemitToAddressRightJustify AS LOG
 FIELD checkRemitToAddressCompact AS LOG INIT YES
 FIELD checkRemitToAddressRowSpace AS DEC INIT 1
 FIELD checkRemitToAddressAllCaps AS LOG INIT YES
 
 FIELD checkCompanyNameShow AS LOG INIT YES
 FIELD checkCompanyNameFont AS CHAR 
 FIELD checkCompanyNameFontSize AS INT 
 FIELD checkCompanyNameSize AS INT INIT 70
 FIELD checkCompanyNameBold AS LOG 
 FIELD checkCompanyNameUnderline AS LOG
 FIELD checkCompanyNameItalic AS LOG 
 FIELD checkCompanyNameRightJustify AS LOG
 FIELD checkCompanyNameAllCaps AS LOG INIT YES
 
 FIELD checkCompanyAddressShow AS LOG INIT YES
 FIELD checkCompanyAddressFont AS CHAR 
 FIELD checkCompanyAddressFontSize AS INT INIT 8
 FIELD checkCompanyAddressSize AS INT INIT 70
 FIELD checkCompanyAddressBold AS LOG 
 FIELD checkCompanyAddressUnderline AS LOG
 FIELD checkCompanyAddressItalic AS LOG 
 FIELD checkCompanyAddressRightJustify AS LOG
 FIELD checkCompanyAddressCompact AS LOG INIT YES
 FIELD checkCompanyAddressRowSpace AS DEC INIT 1
 FIELD checkCompanyAddressAllCaps AS LOG
 
 /*Properties for stub defaults*/
 FIELD stubFont AS CHAR 
 FIELD stubFontSize AS INT
 FIELD stubInvoiceColumns AS INT INIT 1
 FIELD stubInvoiceLines AS INT INIT 10
 FIELD stubRowSpace AS INT INIT 1
 FIELD stubUseSupplementalReg AS LOG INIT YES
 
 /*Properties for Stub Objects*/
 FIELD stubCheckNumberShow AS LOG INIT YES 
 FIELD stubCheckNumberFont AS CHAR 
 FIELD stubCheckNumberFontSize AS INT 
 FIELD stubCheckNumberNumSize AS INT INIT 9
 FIELD stubCheckNumberBold AS LOG INIT YES
 FIELD stubCheckNumberUnderline AS LOG
 FIELD stubCheckNumberItalic AS LOG 
 FIELD stubCheckNumberRightJustify AS LOG INIT YES
 
 FIELD stubCompanyNameShow AS LOG INIT YES
 FIELD stubCompanyNameFont AS CHAR 
 FIELD stubCompanyNameFontSize AS INT 
 FIELD stubCompanyNameSize AS INT INIT 70
 FIELD stubCompanyNameBold AS LOG INIT YES
 FIELD stubCompanyNameUnderline AS LOG
 FIELD stubCompanyNameItalic AS LOG 
 FIELD stubCompanyNameRightJustify AS LOG
 FIELD stubCompanyNameAllCaps AS LOG INIT YES
 
 FIELD stubRemitToNameShow AS LOG INIT YES
 FIELD stubRemitToNameFont AS CHAR 
 FIELD stubRemitToNameFontSize AS INT 
 FIELD stubRemitToNameSize AS INT INIT 70
 FIELD stubRemitToNameBold AS LOG 
 FIELD stubRemitToNameUnderline AS LOG
 FIELD stubRemitToNameItalic AS LOG INIT YES
 FIELD stubRemitToNameRightJustify AS LOG
 FIELD stubRemitToNameAllCaps AS LOG INIT YES
 
 FIELD stubPayDateShow AS LOG INIT YES
 FIELD stubPayDateFont AS CHAR 
 FIELD stubPayDateFontSize AS CHAR 
 FIELD stubPayDateSize AS INT INIT 30
 FIELD stubPayDateBold AS LOG 
 FIELD stubPayDateUnderline AS LOG
 FIELD stubPayDateItalic AS LOG 
 FIELD stubPayDateRightJustify AS LOG
 FIELD stubPayDateAllCaps AS LOG
 FIELD stubPayDateFormat AS CHAR INIT "M/D/YYYY"
 
 FIELD stubRegInvNoShow AS LOG INIT YES
 FIELD stubRegInvNoHead AS CHAR INIT "Invoice Number"
 FIELD stubRegInvNoHeadFont AS CHAR 
 FIELD stubRegInvNoHeadFontSize AS INT
 FIELD stubRegInvNoHeadSize AS INT INIT 15
 FIELD stubRegInvNoHeadBold AS LOG INIT YES 
 FIELD stubRegInvNoHeadUnderline AS LOG
 FIELD stubRegInvNoHeadItalic AS LOG INIT YES
 FIELD stubRegInvNoHeadRightJustify AS LOG
 FIELD stubRegInvNoHeadAllCaps AS LOG
 FIELD stubRegInvNoFont AS CHAR 
 FIELD stubRegInvNoFontSize AS INT
 FIELD stubRegInvNoSize AS INT INIT 15
 FIELD stubRegInvNoBold AS LOG 
 FIELD stubRegInvNoUnderline AS LOG
 FIELD stubRegInvNoItalic AS LOG 
 FIELD stubRegInvNoRightJustify AS LOG
 FIELD stubRegInvNoAllCaps AS LOG 
 
 FIELD stubRegPOShow AS LOG INIT YES
 FIELD stubRegPOHead AS CHAR INIT "Purchase Order #"
 FIELD stubRegPOHeadFont AS CHAR 
 FIELD stubRegPOHeadFontSize AS INT
 FIELD stubRegPOHeadSize AS INT INIT 20
 FIELD stubRegPOHeadBold AS LOG INIT YES
 FIELD stubRegPOHeadUnderline AS LOG
 FIELD stubRegPOHeadItalic AS LOG INIT YES
 FIELD stubRegPOHeadRightJustify AS LOG
 FIELD stubRegPOHeadAllCaps AS LOG
 FIELD stubRegPOFont AS CHAR 
 FIELD stubRegPOFontSize AS INT
 FIELD stubRegPONumSize AS INT INIT 6
 FIELD stubRegPOBold AS LOG 
 FIELD stubRegPOUnderline AS LOG
 FIELD stubRegPOItalic AS LOG 
 FIELD stubRegPORightJustify AS LOG
 
 FIELD stubRegInvDateShow AS LOG INIT YES
 FIELD stubRegInvDateHead AS CHAR INIT "Invoice Date"
 FIELD stubRegInvDateHeadFont AS CHAR 
 FIELD stubRegInvDateHeadFontSize AS INT
 FIELD stubRegInvDateHeadSize AS INT INIT 15
 FIELD stubRegInvDateHeadBold AS LOG INIT YES
 FIELD stubRegInvDateHeadUnderline AS LOG 
 FIELD stubRegInvDateHeadItalic AS LOG INIT YES
 FIELD stubRegInvDateHeadRightJustify AS LOG INIT YES
 FIELD stubRegInvDateHeadAllCaps AS LOG
 FIELD stubRegInvDateFont AS CHAR 
 FIELD stubRegInvDateFontSize AS INT
 FIELD stubRegInvDateSize AS INT INIT 15
 FIELD stubRegInvDateBold AS LOG 
 FIELD stubRegInvDateUnderline AS LOG
 FIELD stubRegInvDateItalic AS LOG 
 FIELD stubRegInvDateRightJustify AS LOG INIT YES
 FIELD stubRegInvDateAllCaps AS LOG
 FIELD stubRegInvDateFormat AS CHAR INIT "MM/DD/YYYY"
 
 FIELD stubRegAmtGrossShow AS LOG INIT YES
 FIELD stubRegAmtGrossHead AS CHAR INIT "Gross"
 FIELD stubRegAmtGrossHeadFont AS CHAR 
 FIELD stubRegAmtGrossHeadFontSize AS INT
 FIELD stubRegAmtGrossHeadSize AS INT INIT 15
 FIELD stubRegAmtGrossHeadBold AS LOG INIT YES
 FIELD stubRegAmtGrossHeadUnderline AS LOG
 FIELD stubRegAmtGrossHeadItalic AS LOG INIT YES
 FIELD stubRegAmtGrossHeadRightJustify AS LOG INIT YES
 FIELD stubRegAmtGrossHeadAllCaps AS LOG
 FIELD stubRegAmtGrossFont AS CHAR 
 FIELD stubRegAmtGrossFontSize AS INT
 FIELD stubRegAmtGrossNumSize AS INT INIT 9
 FIELD stubRegAmtGrossDecimals AS INT INIT 2
 FIELD stubRegAmtGrossBold AS LOG 
 FIELD stubRegAmtGrossUnderline AS LOG
 FIELD stubRegAmtGrossItalic AS LOG 
 FIELD stubRegAmtGrossRightJustify AS LOG INIT YES
 FIELD stubRegAmtGrossComma AS LOG INIT YES
 FIELD stubRegAmtGrossCurrSymb AS CHAR 
 FIELD stubRegAmtGrossCurrSymbFirstOnly AS LOG INIT YES
 FIELD stubRegAmtGrossTotShow AS LOG INIT YES
 FIELD stubRegAmtGrossTotFont AS CHAR
 FIELD stubRegAmtGrossTotFontSize AS INT
 FIELD stubRegAmtGrossTotNumSize AS INT INIT 9
 FIELD stubRegAmtGrossTotDecimals AS INT INIT 2
 FIELD stubRegAmtGrossTotBold AS LOG INIT YES
 FIELD stubRegAmtGrossTotUnderline AS LOG
 FIELD stubRegAmtGrossTotItalic AS LOG 
 FIELD stubRegAmtGrossTotRightJustify AS LOG INIT YES
 FIELD stubRegAmtGrossTotComma AS LOG INIT YES
 FIELD stubRegAmtGrossTotCurrSymb AS CHAR 
 
 FIELD stubRegAmtNetShow AS LOG INIT YES
 FIELD stubRegAmtNetHead AS CHAR INIT "Net"
 FIELD stubRegAmtNetHeadFont AS CHAR 
 FIELD stubRegAmtNetHeadFontSize AS INT
 FIELD stubRegAmtNetHeadSize AS INT INIT 15
 FIELD stubRegAmtNetHeadBold AS LOG INIT YES
 FIELD stubRegAmtNetHeadUnderline AS LOG
 FIELD stubRegAmtNetHeadItalic AS LOG INIT YES
 FIELD stubRegAmtNetHeadRightJustify AS LOG INIT YES
 FIELD stubRegAmtNetHeadAllCaps AS LOG
 FIELD stubRegAmtNetFont AS CHAR 
 FIELD stubRegAmtNetFontSize AS INT
 FIELD stubRegAmtNetNumSize AS INT INIT 9
 FIELD stubRegAmtNetDecimals AS INT INIT 2
 FIELD stubRegAmtNetBold AS LOG 
 FIELD stubRegAmtNetUnderline AS LOG
 FIELD stubRegAmtNetItalic AS LOG 
 FIELD stubRegAmtNetRightJustify AS LOG INIT YES
 FIELD stubRegAmtNetComma AS LOG INIT YES
 FIELD stubRegAmtNetCurrSymb AS CHAR 
 FIELD stubRegAmtNetCurrSymbFirstOnly AS LOG INIT YES
 FIELD stubRegAmtNetTotShow AS LOG INIT YES
 FIELD stubRegAmtNetTotFont AS CHAR
 FIELD stubRegAmtNetTotFontSize AS INT
 FIELD stubRegAmtNetTotNumSize AS INT INIT 9
 FIELD stubRegAmtNetTotDecimals AS INT INIT 2
 FIELD stubRegAmtNetTotBold AS LOG INIT YES
 FIELD stubRegAmtNetTotUnderline AS LOG
 FIELD stubRegAmtNetTotItalic AS LOG 
 FIELD stubRegAmtNetTotRightJustify AS LOG INIT YES
 FIELD stubRegAmtNetTotComma AS LOG INIT YES
 FIELD stubRegAmtNetTotCurrSymb AS CHAR
 
 FIELD stubRegAmtDiscShow AS LOG INIT YES
 FIELD stubRegAmtDiscHead AS CHAR INIT "Discount"
 FIELD stubRegAmtDiscHeadFont AS CHAR 
 FIELD stubRegAmtDiscHeadFontSize AS INT
 FIELD stubRegAmtDiscHeadSize AS INT INIT 15
 FIELD stubRegAmtDiscHeadBold AS LOG INIT YES
 FIELD stubRegAmtDiscHeadUnderline AS LOG
 FIELD stubRegAmtDiscHeadItalic AS LOG INIT YES
 FIELD stubRegAmtDiscHeadRightJustify AS LOG INIT YES
 FIELD stubRegAmtDiscHeadAllCaps AS LOG
 FIELD stubRegAmtDiscFont AS CHAR 
 FIELD stubRegAmtDiscFontSize AS INT
 FIELD stubRegAmtDiscNumSize AS INT INIT 9
 FIELD stubRegAmtDiscDecimals AS INT INIT 2
 FIELD stubRegAmtDiscBold AS LOG 
 FIELD stubRegAmtDiscUnderline AS LOG
 FIELD stubRegAmtDiscItalic AS LOG 
 FIELD stubRegAmtDiscRightJustify AS LOG INIT YES
 FIELD stubRegAmtDiscAllCaps AS LOG
 FIELD stubRegAmtDiscComma AS LOG INIT YES
 FIELD stubRegAmtDiscCurrSymb AS CHAR 
 FIELD stubRegAmtDiscCurrSymbFirstOnly AS LOG INIT YES
 FIELD stubRegAmtDiscTotShow AS LOG INIT YES
 FIELD stubRegAmtDiscTotFont AS CHAR
 FIELD stubRegAmtDiscTotFontSize AS INT
 FIELD stubRegAmtDiscTotNumSize AS INT INIT 9
 FIELD stubRegAmtDiscTotDecimals AS INT INIT 2
 FIELD stubRegAmtDiscTotBold AS LOG INIT YES
 FIELD stubRegAmtDiscTotUnderline AS LOG
 FIELD stubRegAmtDiscTotItalic AS LOG 
 FIELD stubRegAmtDiscTotRightJustify AS LOG INIT YES
 FIELD stubRegAmtDiscTotComma AS LOG INIT YES
 FIELD stubRegAmtDiscTotCurrSymb AS CHAR
 
 FIELD stubRegSuppNoticeShow AS LOG INIT YES
 FIELD stubRegSuppNotice AS CHAR INIT "Invoices for this vendor too numerous to fit. See Supplemental register for details."
 FIELD stubRegSuppNoticeFont AS CHAR 
 FIELD stubRegSuppNoticeFontSize AS INT
 FIELD stubRegSuppNoticeSize AS INT INIT 120
 FIELD stubRegSuppNoticeBold AS LOG INIT YES 
 FIELD stubRegSuppNoticeUnderline AS LOG
 FIELD stubRegSuppNoticeItalic AS LOG INIT YES
 FIELD stubRegSuppNoticeRightJustify AS LOG
 FIELD stubRegSuppNoticeAllCaps AS LOG
 
 FIELD stubRegTotLabelShow AS LOG INIT YES
 FIELD stubRegTotLabel AS CHAR INIT "Totals:"
 FIELD stubRegTotLabelFont AS CHAR 
 FIELD stubRegTotLabelFontSize AS INT
 FIELD stubRegTotLabelSize AS INT INIT 120
 FIELD stubRegTotLabelBold AS LOG INIT YES 
 FIELD stubRegTotLabelUnderline AS LOG
 FIELD stubRegTotLabelItalic AS LOG 
 FIELD stubRegTotLabelRightJustify AS LOG
 FIELD stubRegTotLabelAllCaps AS LOG
    .
       
DEFINE {1} TEMP-TABLE ttCheck
    FIELD ap-chkRowid     AS ROWID
    FIELD checkNo         AS INTEGER
    FIELD payDate         AS DATE
    FIELD vendorID        AS CHARACTER 
    FIELD remitToName     AS CHARACTER
    FIELD remitToLine1    AS CHARACTER
    FIELD remitToLine2    AS CHARACTER
    FIELD remitToLine3    AS CHARACTER 
    FIELD amount          AS DECIMAL
    FIELD amountGross     AS DECIMAL
    FIELD amountDisc      AS DECIMAL
    FIELD companyName     AS CHARACTER 
    FIELD companyLine1    AS CHARACTER 
    FIELD companyLine2    AS CHARACTER 
    FIELD companyLine3    AS CHARACTER
    FIELD memo            AS CHARACTER
    FIELD invoiceCount    AS INTEGER
    FIELD useSupplemental AS LOGICAL 
    FIELD isVoid          AS LOGICAL
    .

DEFINE {1} TEMP-TABLE ttCheckInvoice
    FIELD ap-selRowid        AS ROWID
    FIELD ap-invRowid        AS ROWID
    FIELD ap-chkRowid        AS ROWID
    FIELD checkNo            AS INTEGER 
    FIELD vendorID           AS CHARACTER
    FIELD sequence           AS INTEGER
    FIELD invoiceDate        AS DATE
    FIELD invoiceNumber      AS CHARACTER
    FIELD invoiceAmtGross    AS DECIMAL
    FIELD invoiceAmtDiscount AS DECIMAL 
    FIELD invoiceAmtNet      AS DECIMAL
    FIELD invoicePO          AS INTEGER 
    .

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
