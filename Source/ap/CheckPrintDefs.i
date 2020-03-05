
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

DEFINE {1} TEMP-TABLE ttCheckConfig
    /*Overall output settings*/
    FIELD company                          AS CHARACTER
    FIELD outputFile                       AS CHARACTER
    FIELD defaultFont                      AS CHARACTER INITIAL "Arial"
    FIELD defaultFontSize                  AS INTEGER   INITIAL 10
    
    /*Structure Configuration*/
    FIELD panel1Type                       AS CHARACTER INITIAL "Stub"
    FIELD panel2Type                       AS CHARACTER INITIAL "Check"
    FIELD panel3Type                       AS CHARACTER INITIAL "Stub"
    
    /*Overall panel Coordinates*/
    FIELD panel1Row                        AS DECIMAL   INITIAL 1
    FIELD panel2Row                        AS DECIMAL   INITIAL 25
    FIELD panel3Row                        AS DECIMAL   INITIAL 46
    
    /*Properties for check defaults*/
    FIELD checkFont                        AS CHARACTER 
    FIELD checkFontSize                    AS INTEGER 
    
    /*Coordinates for Check Objects*/
    FIELD checkCheckNumberRow              AS DECIMAL   INITIAL 5 
    FIELD checkCheckNumberCol              AS DECIMAL   INITIAL 80
    FIELD checkAmtInWordsRow               AS DECIMAL   INITIAL 9
    FIELD checkAmtInWordsCol               AS DECIMAL   INITIAL 8
    FIELD checkAmtRow                      AS DECIMAL   INITIAL 8
    FIELD checkAmtCol                      AS DECIMAL   INITIAL 80
    FIELD checkPayDateRow                  AS DECIMAL   INITIAL 5
    FIELD checkPayDateCol                  AS DECIMAL   INITIAL 45
    FIELD checkRemitToNameRow              AS DECIMAL   INITIAL 8 
    FIELD checkRemitToNameCol              AS DECIMAL   INITIAL 8
    FIELD checkRemitToAddressRow           AS DECIMAL   INITIAL 10 
    FIELD checkRemitToAddressCol           AS DECIMAL   INITIAL 8
    FIELD checkCompanyNameRow              AS DECIMAL   INITIAL 2
    FIELD checkCompanyNameCol              AS DECIMAL   INITIAL 8
    FIELD checkCompanyAddressRow           AS DECIMAL   INITIAL 3
    FIELD checkCompanyAddressCol           AS DECIMAL   INITIAL 8
    FIELD checkMemoRow                     AS DECIMAL   INITIAL 12
    FIELD checkMemoCol                     AS DECIMAL   INITIAL 50
    
    /*Coordinates for Stub Objects*/
    FIELD stubCheckNumberRow               AS DECIMAL   INITIAL 3 
    FIELD stubCheckNumberCol               AS DECIMAL   INITIAL 80
    FIELD stubPayDateRow                   AS DECIMAL   INITIAL 3
    FIELD stubPayDateCol                   AS DECIMAL   INITIAL 1
    FIELD stubRemitToNameRow               AS DECIMAL   INITIAL 3 
    FIELD stubRemitToNameCol               AS DECIMAL   INITIAL 20
    FIELD stubCompanyNameRow               AS DECIMAL   INITIAL 2
    FIELD stubCompanyNameCol               AS DECIMAL   INITIAL 24
    FIELD stubRegHeadRow                   AS INTEGER   INITIAL 5
    FIELD stubRegSuppNoticeRow             AS INTEGER   INITIAL 5
    FIELD stubRegSuppNoticeCol             AS INTEGER   INITIAL 5
    FIELD stubRegDetailRow                 AS INTEGER   INITIAL 6
    FIELD stubRegInvNoCol                  AS INTEGER   INITIAL 3
    FIELD stubRegPOCol                     AS INTEGER   INITIAL 18
    FIELD stubRegInvDateCol                AS INTEGER   INITIAL 45
    FIELD stubRegAmtGrossCol               AS INTEGER   INITIAL 55
    FIELD stubRegAmtDiscCol                AS INTEGER   INITIAL 65
    FIELD stubRegAmtNetCol                 AS INTEGER   INITIAL 75
    FIELD stubRegTotLabelCol               AS INTEGER   INITIAL 30
    
    /*Properties for check objects*/   
    FIELD checkCheckNumberShow             AS LOGICAL   INITIAL YES 
    FIELD checkCheckNumberFont             AS CHARACTER 
    FIELD checkCheckNumberFontSize         AS INTEGER 
    FIELD checkCheckNumberNumSize          AS INTEGER   INITIAL 9
    FIELD checkCheckNumberBold             AS LOGICAL   INITIAL YES
    FIELD checkCheckNumberUnderline        AS LOGICAL
    FIELD checkCheckNumberItalic           AS LOGICAL 
    FIELD checkCheckNumberRightJustify     AS LOGICAL   INITIAL YES
    
    FIELD checkAmtInWordsShow              AS LOGICAL   INITIAL YES
    FIELD checkAmtInWordsFont              AS CHARACTER 
    FIELD checkAmtInWordsFontSize          AS INTEGER 
    FIELD checkAmtInWordsSize              AS INTEGER   INITIAL 70
    FIELD checkAmtInWordsBold              AS LOGICAL 
    FIELD checkAmtInWordsUnderline         AS LOGICAL
    FIELD checkAmtInWordsItalic            AS LOGICAL 
    FIELD checkAmtInWordsRightJustify      AS LOGICAL
    FIELD checkAmtInWordsAndAsFraction     AS LOGICAL   INITIAL YES
    FIELD checkAmtInWordsZeroAsNo          AS LOGICAL   INITIAL YES
    FIELD checkAmtInWordsFillChar          AS CHARACTER INITIAL "*"
    FIELD checkAmtInWordsFillCenter        AS LOGICAL   INITIAL YES
    FIELD checkAmtInWordsAllCaps           AS LOGICAL
    
    FIELD checkAmtShow                     AS LOGICAL   INITIAL YES 
    FIELD checkAmtFont                     AS CHARACTER 
    FIELD checkAmtFontSize                 AS INTEGER 
    FIELD checkAmtNumSize                  AS INTEGER   INITIAL 9
    FIELD checkAmtDecimals                 AS INTEGER   INITIAL 2
    FIELD checkAmtBold                     AS LOGICAL 
    FIELD checkAmtUnderline                AS LOGICAL
    FIELD checkAmtItalic                   AS LOGICAL 
    FIELD checkAmtRightJustify             AS LOGICAL   INITIAL YES
    FIELD checkAmtComma                    AS LOGICAL   INITIAL YES
    FIELD checkAmtCurrSymb                 AS CHARACTER 
    
    FIELD checkPayDateShow                 AS LOGICAL   INITIAL YES
    FIELD checkPayDateFont                 AS CHARACTER 
    FIELD checkPayDateFontSize             AS CHARACTER 
    FIELD checkPayDateSize                 AS INTEGER   INITIAL 30
    FIELD checkPayDateBold                 AS LOGICAL 
    FIELD checkPayDateUnderline            AS LOGICAL   INITIAL YES
    FIELD checkPayDateItalic               AS LOGICAL 
    FIELD checkPayDateRightJustify         AS LOGICAL
    FIELD checkPayDateAllCaps              AS LOGICAL
    FIELD checkPayDateFormat               AS CHARACTER INITIAL "MM/DD/YYYY"

    FIELD checkMemoShow                    AS LOGICAL   INITIAL YES
    FIELD checkMemoFont                    AS CHARACTER 
    FIELD checkMemoFontSize                AS INTEGER   
    FIELD checkMemoSize                    AS INTEGER   INITIAL 70
    FIELD checkMemoBold                    AS LOGICAL   
    FIELD checkMemoUnderline               AS LOGICAL   
    FIELD checkMemoItalic                  AS LOGICAL 
    FIELD checkMemoRightJustify            AS LOGICAL   
    FIELD checkMemoAllCaps                 AS LOGICAL   
    
    FIELD checkRemitToNameShow             AS LOGICAL   INITIAL YES
    FIELD checkRemitToNameFont             AS CHARACTER 
    FIELD checkRemitToNameFontSize         AS INTEGER 
    FIELD checkRemitToNameSize             AS INTEGER   INITIAL 70
    FIELD checkRemitToNameBold             AS LOGICAL 
    FIELD checkRemitToNameUnderline        AS LOGICAL
    FIELD checkRemitToNameItalic           AS LOGICAL 
    FIELD checkRemitToNameRightJustify     AS LOGICAL
    FIELD checkRemitToNameAllCaps          AS LOGICAL   INITIAL YES
    
    FIELD checkRemitToAddressShow          AS LOGICAL   INITIAL YES
    FIELD checkRemitToAddressFont          AS CHARACTER 
    FIELD checkRemitToAddressFontSize      AS INTEGER   INITIAL 8
    FIELD checkRemitToAddressSize          AS INTEGER   INITIAL 70
    FIELD checkRemitToAddressBold          AS LOGICAL 
    FIELD checkRemitToAddressUnderline     AS LOGICAL
    FIELD checkRemitToAddressItalic        AS LOGICAL 
    FIELD checkRemitToAddressRightJustify  AS LOGICAL
    FIELD checkRemitToAddressCompact       AS LOGICAL   INITIAL YES
    FIELD checkRemitToAddressRowSpace      AS DECIMAL   INITIAL 1
    FIELD checkRemitToAddressAllCaps       AS LOGICAL   INITIAL YES
    
    FIELD checkCompanyNameShow             AS LOGICAL   INITIAL YES
    FIELD checkCompanyNameFont             AS CHARACTER 
    FIELD checkCompanyNameFontSize         AS INTEGER 
    FIELD checkCompanyNameSize             AS INTEGER   INITIAL 70
    FIELD checkCompanyNameBold             AS LOGICAL 
    FIELD checkCompanyNameUnderline        AS LOGICAL
    FIELD checkCompanyNameItalic           AS LOGICAL 
    FIELD checkCompanyNameRightJustify     AS LOGICAL
    FIELD checkCompanyNameAllCaps          AS LOGICAL   INITIAL YES
    
    FIELD checkCompanyAddressShow          AS LOGICAL   INITIAL YES
    FIELD checkCompanyAddressFont          AS CHARACTER 
    FIELD checkCompanyAddressFontSize      AS INTEGER   INITIAL 8
    FIELD checkCompanyAddressSize          AS INTEGER   INITIAL 70
    FIELD checkCompanyAddressBold          AS LOGICAL 
    FIELD checkCompanyAddressUnderline     AS LOGICAL
    FIELD checkCompanyAddressItalic        AS LOGICAL 
    FIELD checkCompanyAddressRightJustify  AS LOGICAL
    FIELD checkCompanyAddressCompact       AS LOGICAL   INITIAL YES
    FIELD checkCompanyAddressRowSpace      AS DECIMAL   INITIAL 1
    FIELD checkCompanyAddressAllCaps       AS LOGICAL
    
    /*Properties for stub defaults*/
    FIELD stubFont                         AS CHARACTER 
    FIELD stubFontSize                     AS INTEGER
    FIELD stubInvoiceColumns               AS INTEGER   INITIAL 1
    FIELD stubInvoiceLines                 AS INTEGER   INITIAL 10
    FIELD stubRowSpace                     AS INTEGER   INITIAL 1
    FIELD stubUseSupplementalReg           AS LOGICAL   INITIAL YES
    
    /*Properties for Stub Objects*/
    FIELD stubCheckNumberShow              AS LOGICAL   INITIAL YES 
    FIELD stubCheckNumberFont              AS CHARACTER 
    FIELD stubCheckNumberFontSize          AS INTEGER 
    FIELD stubCheckNumberNumSize           AS INTEGER   INITIAL 9
    FIELD stubCheckNumberBold              AS LOGICAL   INITIAL YES
    FIELD stubCheckNumberUnderline         AS LOGICAL
    FIELD stubCheckNumberItalic            AS LOGICAL 
    FIELD stubCheckNumberRightJustify      AS LOGICAL   INITIAL YES
    
    FIELD stubCompanyNameShow              AS LOGICAL   INITIAL YES
    FIELD stubCompanyNameFont              AS CHARACTER 
    FIELD stubCompanyNameFontSize          AS INTEGER 
    FIELD stubCompanyNameSize              AS INTEGER   INITIAL 70
    FIELD stubCompanyNameBold              AS LOGICAL   INITIAL YES
    FIELD stubCompanyNameUnderline         AS LOGICAL
    FIELD stubCompanyNameItalic            AS LOGICAL 
    FIELD stubCompanyNameRightJustify      AS LOGICAL
    FIELD stubCompanyNameAllCaps           AS LOGICAL   INITIAL YES
    
    FIELD stubRemitToNameShow              AS LOGICAL   INITIAL YES
    FIELD stubRemitToNameFont              AS CHARACTER 
    FIELD stubRemitToNameFontSize          AS INTEGER 
    FIELD stubRemitToNameSize              AS INTEGER   INITIAL 70
    FIELD stubRemitToNameBold              AS LOGICAL 
    FIELD stubRemitToNameUnderline         AS LOGICAL
    FIELD stubRemitToNameItalic            AS LOGICAL   INITIAL YES
    FIELD stubRemitToNameRightJustify      AS LOGICAL
    FIELD stubRemitToNameAllCaps           AS LOGICAL   INITIAL YES
    
    FIELD stubPayDateShow                  AS LOGICAL   INITIAL YES
    FIELD stubPayDateFont                  AS CHARACTER 
    FIELD stubPayDateFontSize              AS CHARACTER 
    FIELD stubPayDateSize                  AS INTEGER   INITIAL 30
    FIELD stubPayDateBold                  AS LOGICAL 
    FIELD stubPayDateUnderline             AS LOGICAL
    FIELD stubPayDateItalic                AS LOGICAL 
    FIELD stubPayDateRightJustify          AS LOGICAL
    FIELD stubPayDateAllCaps               AS LOGICAL
    FIELD stubPayDateFormat                AS CHARACTER INITIAL "M/D/YYYY"
    
    FIELD stubRegInvNoShow                 AS LOGICAL   INITIAL YES
    FIELD stubRegInvNoHead                 AS CHARACTER INITIAL "Invoice Number"
    FIELD stubRegInvNoHeadFont             AS CHARACTER 
    FIELD stubRegInvNoHeadFontSize         AS INTEGER
    FIELD stubRegInvNoHeadSize             AS INTEGER   INITIAL 15
    FIELD stubRegInvNoHeadBold             AS LOGICAL   INITIAL YES 
    FIELD stubRegInvNoHeadUnderline        AS LOGICAL
    FIELD stubRegInvNoHeadItalic           AS LOGICAL   INITIAL YES
    FIELD stubRegInvNoHeadRightJustify     AS LOGICAL
    FIELD stubRegInvNoHeadAllCaps          AS LOGICAL
    FIELD stubRegInvNoFont                 AS CHARACTER 
    FIELD stubRegInvNoFontSize             AS INTEGER
    FIELD stubRegInvNoSize                 AS INTEGER   INITIAL 15
    FIELD stubRegInvNoBold                 AS LOGICAL   
    FIELD stubRegInvNoUnderline            AS LOGICAL
    FIELD stubRegInvNoItalic               AS LOGICAL 
    FIELD stubRegInvNoRightJustify         AS LOGICAL
    FIELD stubRegInvNoAllCaps              AS LOGICAL 
    
    FIELD stubRegPOShow                    AS LOGICAL   INITIAL YES
    FIELD stubRegPOHead                    AS CHARACTER INITIAL "Purchase Order #"
    FIELD stubRegPOHeadFont                AS CHARACTER 
    FIELD stubRegPOHeadFontSize            AS INTEGER
    FIELD stubRegPOHeadSize                AS INTEGER   INITIAL 20
    FIELD stubRegPOHeadBold                AS LOGICAL   INITIAL YES
    FIELD stubRegPOHeadUnderline           AS LOGICAL
    FIELD stubRegPOHeadItalic              AS LOGICAL   INITIAL YES
    FIELD stubRegPOHeadRightJustify        AS LOGICAL
    FIELD stubRegPOHeadAllCaps             AS LOGICAL
    FIELD stubRegPOFont                    AS CHARACTER 
    FIELD stubRegPOFontSize                AS INTEGER
    FIELD stubRegPONumSize                 AS INTEGER   INITIAL 6
    FIELD stubRegPOBold                    AS LOGICAL 
    FIELD stubRegPOUnderline               AS LOGICAL
    FIELD stubRegPOItalic                  AS LOGICAL 
    FIELD stubRegPORightJustify            AS LOGICAL
    
    FIELD stubRegInvDateShow               AS LOGICAL   INITIAL YES
    FIELD stubRegInvDateHead               AS CHARACTER INITIAL "Invoice Date"
    FIELD stubRegInvDateHeadFont           AS CHARACTER 
    FIELD stubRegInvDateHeadFontSize       AS INTEGER
    FIELD stubRegInvDateHeadSize           AS INTEGER   INITIAL 15
    FIELD stubRegInvDateHeadBold           AS LOGICAL   INITIAL YES
    FIELD stubRegInvDateHeadUnderline      AS LOGICAL   
    FIELD stubRegInvDateHeadItalic         AS LOGICAL   INITIAL YES
    FIELD stubRegInvDateHeadRightJustify   AS LOGICAL   INITIAL YES
    FIELD stubRegInvDateHeadAllCaps        AS LOGICAL
    FIELD stubRegInvDateFont               AS CHARACTER 
    FIELD stubRegInvDateFontSize           AS INTEGER
    FIELD stubRegInvDateSize               AS INTEGER   INITIAL 15
    FIELD stubRegInvDateBold               AS LOGICAL 
    FIELD stubRegInvDateUnderline          AS LOGICAL
    FIELD stubRegInvDateItalic             AS LOGICAL 
    FIELD stubRegInvDateRightJustify       AS LOGICAL   INITIAL YES
    FIELD stubRegInvDateAllCaps            AS LOGICAL
    FIELD stubRegInvDateFormat             AS CHARACTER INITIAL "MM/DD/YYYY"
    
    FIELD stubRegAmtGrossShow              AS LOGICAL   INITIAL YES
    FIELD stubRegAmtGrossHead              AS CHARACTER INITIAL "Gross"
    FIELD stubRegAmtGrossHeadFont          AS CHARACTER 
    FIELD stubRegAmtGrossHeadFontSize      AS INTEGER
    FIELD stubRegAmtGrossHeadSize          AS INTEGER   INITIAL 15
    FIELD stubRegAmtGrossHeadBold          AS LOGICAL   INITIAL YES
    FIELD stubRegAmtGrossHeadUnderline     AS LOGICAL
    FIELD stubRegAmtGrossHeadItalic        AS LOGICAL   INITIAL YES
    FIELD stubRegAmtGrossHeadRightJustify  AS LOGICAL   INITIAL YES
    FIELD stubRegAmtGrossHeadAllCaps       AS LOGICAL
    FIELD stubRegAmtGrossFont              AS CHARACTER 
    FIELD stubRegAmtGrossFontSize          AS INTEGER
    FIELD stubRegAmtGrossNumSize           AS INTEGER   INITIAL 9
    FIELD stubRegAmtGrossDecimals          AS INTEGER   INITIAL 2
    FIELD stubRegAmtGrossBold              AS LOGICAL 
    FIELD stubRegAmtGrossUnderline         AS LOGICAL
    FIELD stubRegAmtGrossItalic            AS LOGICAL 
    FIELD stubRegAmtGrossRightJustify      AS LOGICAL   INITIAL YES
    FIELD stubRegAmtGrossComma             AS LOGICAL   INITIAL YES
    FIELD stubRegAmtGrossCurrSymb          AS CHARACTER 
    FIELD stubRegAmtGrossCurrSymbFirstOnly AS LOGICAL   INITIAL YES
    FIELD stubRegAmtGrossTotShow           AS LOGICAL   INITIAL YES
    FIELD stubRegAmtGrossTotFont           AS CHARACTER
    FIELD stubRegAmtGrossTotFontSize       AS INTEGER
    FIELD stubRegAmtGrossTotNumSize        AS INTEGER   INITIAL 9
    FIELD stubRegAmtGrossTotDecimals       AS INTEGER   INITIAL 2
    FIELD stubRegAmtGrossTotBold           AS LOGICAL   INITIAL YES
    FIELD stubRegAmtGrossTotUnderline      AS LOGICAL
    FIELD stubRegAmtGrossTotItalic         AS LOGICAL 
    FIELD stubRegAmtGrossTotRightJustify   AS LOGICAL   INITIAL YES
    FIELD stubRegAmtGrossTotComma          AS LOGICAL   INITIAL YES
    FIELD stubRegAmtGrossTotCurrSymb       AS CHARACTER 
    
    FIELD stubRegAmtNetShow                AS LOGICAL   INITIAL YES
    FIELD stubRegAmtNetHead                AS CHARACTER INITIAL "Net"
    FIELD stubRegAmtNetHeadFont            AS CHARACTER 
    FIELD stubRegAmtNetHeadFontSize        AS INTEGER
    FIELD stubRegAmtNetHeadSize            AS INTEGER   INITIAL 15
    FIELD stubRegAmtNetHeadBold            AS LOGICAL   INITIAL YES
    FIELD stubRegAmtNetHeadUnderline       AS LOGICAL
    FIELD stubRegAmtNetHeadItalic          AS LOGICAL   INITIAL YES
    FIELD stubRegAmtNetHeadRightJustify    AS LOGICAL   INITIAL YES
    FIELD stubRegAmtNetHeadAllCaps         AS LOGICAL
    FIELD stubRegAmtNetFont                AS CHARACTER 
    FIELD stubRegAmtNetFontSize            AS INTEGER
    FIELD stubRegAmtNetNumSize             AS INTEGER   INITIAL 9
    FIELD stubRegAmtNetDecimals            AS INTEGER   INITIAL 2
    FIELD stubRegAmtNetBold                AS LOGICAL 
    FIELD stubRegAmtNetUnderline           AS LOGICAL
    FIELD stubRegAmtNetItalic              AS LOGICAL 
    FIELD stubRegAmtNetRightJustify        AS LOGICAL   INITIAL YES
    FIELD stubRegAmtNetComma               AS LOGICAL   INITIAL YES
    FIELD stubRegAmtNetCurrSymb            AS CHARACTER 
    FIELD stubRegAmtNetCurrSymbFirstOnly   AS LOGICAL   INITIAL YES
    FIELD stubRegAmtNetTotShow             AS LOGICAL   INITIAL YES
    FIELD stubRegAmtNetTotFont             AS CHARACTER
    FIELD stubRegAmtNetTotFontSize         AS INTEGER
    FIELD stubRegAmtNetTotNumSize          AS INTEGER   INITIAL 9
    FIELD stubRegAmtNetTotDecimals         AS INTEGER   INITIAL 2
    FIELD stubRegAmtNetTotBold             AS LOGICAL   INITIAL YES
    FIELD stubRegAmtNetTotUnderline        AS LOGICAL
    FIELD stubRegAmtNetTotItalic           AS LOGICAL 
    FIELD stubRegAmtNetTotRightJustify     AS LOGICAL   INITIAL YES
    FIELD stubRegAmtNetTotComma            AS LOGICAL   INITIAL YES
    FIELD stubRegAmtNetTotCurrSymb         AS CHARACTER
    
    FIELD stubRegAmtDiscShow               AS LOGICAL   INITIAL YES
    FIELD stubRegAmtDiscHead               AS CHARACTER INITIAL "Discount"
    FIELD stubRegAmtDiscHeadFont           AS CHARACTER 
    FIELD stubRegAmtDiscHeadFontSize       AS INTEGER
    FIELD stubRegAmtDiscHeadSize           AS INTEGER   INITIAL 15
    FIELD stubRegAmtDiscHeadBold           AS LOGICAL   INITIAL YES
    FIELD stubRegAmtDiscHeadUnderline      AS LOGICAL
    FIELD stubRegAmtDiscHeadItalic         AS LOGICAL   INITIAL YES
    FIELD stubRegAmtDiscHeadRightJustify   AS LOGICAL   INITIAL YES
    FIELD stubRegAmtDiscHeadAllCaps        AS LOGICAL
    FIELD stubRegAmtDiscFont               AS CHARACTER 
    FIELD stubRegAmtDiscFontSize           AS INTEGER
    FIELD stubRegAmtDiscNumSize            AS INTEGER   INITIAL 9
    FIELD stubRegAmtDiscDecimals           AS INTEGER   INITIAL 2
    FIELD stubRegAmtDiscBold               AS LOGICAL 
    FIELD stubRegAmtDiscUnderline          AS LOGICAL
    FIELD stubRegAmtDiscItalic             AS LOGICAL 
    FIELD stubRegAmtDiscRightJustify       AS LOGICAL   INITIAL YES
    FIELD stubRegAmtDiscAllCaps            AS LOGICAL
    FIELD stubRegAmtDiscComma              AS LOGICAL   INITIAL YES
    FIELD stubRegAmtDiscCurrSymb           AS CHARACTER 
    FIELD stubRegAmtDiscCurrSymbFirstOnly  AS LOGICAL   INITIAL YES
    FIELD stubRegAmtDiscTotShow            AS LOGICAL   INITIAL YES
    FIELD stubRegAmtDiscTotFont            AS CHARACTER
    FIELD stubRegAmtDiscTotFontSize        AS INTEGER
    FIELD stubRegAmtDiscTotNumSize         AS INTEGER   INITIAL 9
    FIELD stubRegAmtDiscTotDecimals        AS INTEGER   INITIAL 2
    FIELD stubRegAmtDiscTotBold            AS LOGICAL   INITIAL YES
    FIELD stubRegAmtDiscTotUnderline       AS LOGICAL
    FIELD stubRegAmtDiscTotItalic          AS LOGICAL 
    FIELD stubRegAmtDiscTotRightJustify    AS LOGICAL   INITIAL YES
    FIELD stubRegAmtDiscTotComma           AS LOGICAL   INITIAL YES
    FIELD stubRegAmtDiscTotCurrSymb        AS CHARACTER
    
    FIELD stubRegSuppNoticeShow            AS LOGICAL   INITIAL YES
    FIELD stubRegSuppNotice                AS CHARACTER INITIAL "Invoices for this vendor too numerous to fit.  See Supplemental register for details."
    FIELD stubRegSuppNoticeFont            AS CHARACTER 
    FIELD stubRegSuppNoticeFontSize        AS INTEGER
    FIELD stubRegSuppNoticeSize            AS INTEGER   INITIAL 120
    FIELD stubRegSuppNoticeBold            AS LOGICAL   INITIAL YES 
    FIELD stubRegSuppNoticeUnderline       AS LOGICAL
    FIELD stubRegSuppNoticeItalic          AS LOGICAL   INITIAL YES
    FIELD stubRegSuppNoticeRightJustify    AS LOGICAL
    FIELD stubRegSuppNoticeAllCaps         AS LOGICAL
    
    FIELD stubRegTotLabelShow              AS LOGICAL   INITIAL YES
    FIELD stubRegTotLabel                  AS CHARACTER INITIAL "Totals:"
    FIELD stubRegTotLabelFont              AS CHARACTER 
    FIELD stubRegTotLabelFontSize          AS INTEGER
    FIELD stubRegTotLabelSize              AS INTEGER   INITIAL 120
    FIELD stubRegTotLabelBold              AS LOGICAL   INITIAL YES 
    FIELD stubRegTotLabelUnderline         AS LOGICAL
    FIELD stubRegTotLabelItalic            AS LOGICAL   
    FIELD stubRegTotLabelRightJustify      AS LOGICAL
    FIELD stubRegTotLabelAllCaps           AS LOGICAL
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
