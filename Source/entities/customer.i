
 /*------------------------------------------------------------------------
    File        : Customer
    Purpose     :
    Syntax      : 
    Description :
    Author(s)   : DEVA$!
    Created     : Sun Aug 29 08:27:10 EDT 2021
    Notes       : 
  ----------------------------------------------------------------------*/
  
  /* ***************************  Definitions  ************************** */
  
  /* ********************  Preprocessor Definitions  ******************** */
  
  /* ***************************  Main Block  *************************** */
  
  /** Dynamically generated schema file **/
   
DEFINE TEMP-TABLE ttCustomer {&referenceOnly} BEFORE-TABLE bttCustomer 
FIELD company AS CHARACTER LABEL "Company"
FIELD cust-no AS CHARACTER LABEL "Cust. #"
FIELD name AS CHARACTER LABEL "Customer Name"
FIELD addr AS CHARACTER EXTENT 2 LABEL "Customer Address"
FIELD city AS CHARACTER LABEL "City"
FIELD state AS CHARACTER LABEL "State"
FIELD zip AS CHARACTER LABEL "Zip Code"
FIELD active AS CHARACTER INITIAL "A" LABEL "Customer Status"
FIELD type AS CHARACTER LABEL "Customer Type"
FIELD sort AS CHARACTER LABEL "Taxable?"
FIELD area-code AS CHARACTER LABEL "Area Code"
FIELD phone AS CHARACTER LABEL "Phone Number"
FIELD fax AS CHARACTER LABEL "Fax #"
FIELD contact AS CHARACTER LABEL "Contact Name"
FIELD terms AS CHARACTER LABEL "Terms Code"
FIELD tax-gr AS CHARACTER LABEL "Sales Tax Group"
FIELD sman AS CHARACTER LABEL "Salesman"
FIELD stat-type AS LOGICAL INITIAL "O" LABEL "Statement Type" FORMAT "O/F"
FIELD stat-grp AS CHARACTER INITIAL "M" LABEL "Statement Group"
FIELD fin-chg AS LOGICAL INITIAL "n" LABEL "Finance Charges"
FIELD memo AS CHARACTER EXTENT 4 LABEL "Memo On Customer"
FIELD cr-use AS CHARACTER LABEL "Use for Credit"
FIELD cr-lim AS DECIMAL INITIAL "0" LABEL "Credit Limit"
FIELD cr-hold AS LOGICAL INITIAL "n" LABEL "Customer On Hold"
FIELD loc AS CHARACTER LABEL "Warehouse"
FIELD terr AS CHARACTER LABEL "Sales Territory"
FIELD cr-rating AS CHARACTER LABEL "Credit Rating"
FIELD disc AS DECIMAL INITIAL "0" LABEL "Customer Discount"
FIELD tax-id AS CHARACTER LABEL "Tax Exempt No."
FIELD ord-lim AS DECIMAL INITIAL "0" LABEL "Order Limit"
FIELD cost AS DECIMAL EXTENT 6 INITIAL "0" LABEL "Costs"
FIELD comm AS DECIMAL EXTENT 6 INITIAL "0" LABEL "Commissions"
FIELD hibal AS DECIMAL INITIAL "0" LABEL "Hi Balance"
FIELD hibal-date AS DATE LABEL "Hi Balance Date"
FIELD lpay AS DECIMAL INITIAL "0" LABEL "Last Payment"
FIELD lpay-date AS DATE LABEL "Last Pay Date"
FIELD num-inv AS INTEGER INITIAL "0" LABEL "Number of Invoices"
FIELD avg-pay AS INTEGER INITIAL "0" LABEL "Avg to Pay"
FIELD acc-bal AS DECIMAL INITIAL "0" LABEL "Account Balance"
FIELD ord-bal AS DECIMAL INITIAL "0" LABEL "Open Orders Balance"
FIELD carrier AS CHARACTER LABEL "Carrier"
FIELD frt-pay AS CHARACTER LABEL "FR PAY CD"
FIELD fob-code AS CHARACTER LABEL "FOB Code"
FIELD under-pct AS DECIMAL INITIAL "0" LABEL "Underrun %"
FIELD over-pct AS DECIMAL INITIAL "0" LABEL "Overrun %"
FIELD ship-days AS INTEGER INITIAL "0" LABEL "Must Ship Days"
FIELD del-zone AS CHARACTER LABEL "Delivery Zone"
FIELD ship-part AS LOGICAL INITIAL "no" LABEL "Part Ship"
FIELD country AS CHARACTER LABEL "Country"
FIELD postal AS CHARACTER LABEL "Postal Code"
FIELD notes AS CHARACTER EXTENT 18 LABEL "Notes"
FIELD sales AS DECIMAL EXTENT 13 INITIAL "0" LABEL "Sales by period"
FIELD n-sales AS DECIMAL EXTENT 13 INITIAL "0" LABEL "#Sales by Period"
FIELD on-account AS DECIMAL INITIAL "0" LABEL "On Account"
FIELD ptd-msf AS DECIMAL EXTENT 13 INITIAL "0" LABEL "PTD MSF"
FIELD ytd-msf AS DECIMAL INITIAL "0" LABEL "YTD MSF"
FIELD lyptd-msf AS DECIMAL EXTENT 13 INITIAL "0" LABEL "LY PTD MSF"
FIELD lyytd-msf AS DECIMAL INITIAL "0" LABEL "LY YTD MSF"
FIELD cr-hold-invdays AS INTEGER INITIAL "0" LABEL "Crdt Hld Days"
FIELD cust-level AS INTEGER INITIAL "0" LABEL "Cust Level"
FIELD auto-reprice AS LOGICAL INITIAL "no" LABEL "Auto Reprice"
FIELD inv-meth AS LOGICAL INITIAL "no" LABEL "Invoice Method"
FIELD an-edi-cust AS LOGICAL INITIAL "no" LABEL "EDI Cust"
FIELD pallet AS CHARACTER LABEL "Pallet"
FIELD case-bundle AS CHARACTER LABEL "Case/Bundle"
FIELD sell-by AS CHARACTER LABEL "Sell by Net or Gross %"
FIELD prof-mrkup AS DECIMAL INITIAL "0" LABEL "Profit % Mark Up"
FIELD p-sep AS LOGICAL INITIAL "no" LABEL "Seperate Invoices"
FIELD email AS CHARACTER LABEL "Email Address"
FIELD inv-meth2 AS CHARACTER LABEL "Invoice Method"
FIELD group-bol AS LOGICAL INITIAL "no" LABEL "Group BOL"
FIELD ord-meth AS CHARACTER LABEL "Order Entry Method"
FIELD imported AS LOGICAL INITIAL "no" LABEL "Created Externally?"
FIELD markup AS DECIMAL INITIAL "0" LABEL "Mark-Up"
FIELD char-field AS CHARACTER EXTENT 10
FIELD char-label AS CHARACTER EXTENT 10
FIELD int-field AS INTEGER EXTENT 10 INITIAL "0"
FIELD int-label AS CHARACTER EXTENT 10
FIELD log-field AS LOGICAL EXTENT 10 INITIAL "no"
FIELD log-label AS CHARACTER EXTENT 10
FIELD dec-field AS DECIMAL EXTENT 10 INITIAL "0"
FIELD dec-label AS CHARACTER EXTENT 10
FIELD date-field AS DATE EXTENT 10 INITIAL "TODAY"
FIELD date-label AS CHARACTER EXTENT 10
FIELD ytd-sales AS DECIMAL INITIAL "0" LABEL "YTD Sales"
FIELD lyr-sales AS DECIMAL INITIAL "0" LABEL "LYR Sales"
FIELD markups AS DECIMAL EXTENT 10 INITIAL "0" LABEL "Mark-Up"
FIELD markups-desc AS CHARACTER LABEL "Mark-Up Description"
FIELD lyr-bud AS DECIMAL EXTENT 13 INITIAL "0" LABEL "Last Year Budget"
FIELD cyr-bud AS DECIMAL EXTENT 13 INITIAL "0" LABEL "Current Year Budget"
FIELD nyr-bud AS DECIMAL EXTENT 13 INITIAL "0" LABEL "next Year Budget"
FIELD manf-day AS INTEGER INITIAL "0" LABEL "Manf Days"
FIELD convert-currency AS LOGICAL INITIAL "no" LABEL "Convert Currency?"
FIELD curr-code AS CHARACTER LABEL "Currency Code"
FIELD fax-country AS CHARACTER LABEL "Fax Country Prefix"
FIELD fax-prefix AS CHARACTER LABEL "Fax Prefix"
FIELD phone-country AS CHARACTER LABEL "Phone Country Prefix"
FIELD phone-prefix AS CHARACTER LABEL "Phone Prefix"
FIELD def-loc-bin AS CHARACTER LABEL "Bin"
FIELD def-loc AS CHARACTER LABEL "Warehouse"
FIELD rec_key AS CHARACTER LABEL "Rec Key"
FIELD ship-meth AS LOGICAL INITIAL "Pallet" LABEL "Shipping Method" FORMAT "Case/Pallet"
FIELD ch-type AS CHARACTER LABEL "Credit Hold Type"
FIELD cc-num AS CHARACTER LABEL "CC Number"
FIELD cc-type AS CHARACTER LABEL "CC Type"
FIELD cc-expiration AS DATE INITIAL "TODAY" LABEL "CC Expiration"
FIELD cc-name AS CHARACTER LABEL "CC Name"
FIELD cc-bank AS CHARACTER LABEL "CC Bank Name"
FIELD cc-company AS CHARACTER LABEL "CC Company Name"
FIELD cc-addr AS CHARACTER EXTENT 2 LABEL "CC Address"
FIELD cc-city AS CHARACTER LABEL "CC City"
FIELD cc-state AS CHARACTER LABEL "CC State"
FIELD cc-zip AS CHARACTER LABEL "CC Zip Code"
FIELD cc-country AS CHARACTER LABEL "CC Country"
FIELD cc-cvv-code AS CHARACTER LABEL "CC CVV Code"
FIELD cc-phone AS CHARACTER LABEL "CC Phone#"
FIELD upch-code AS CHARACTER LABEL "Upcharge"
FIELD upch-dscr AS CHARACTER LABEL "Upcharge Description"
FIELD upch-pct AS DECIMAL INITIAL "0" LABEL "Upcharge%"
FIELD upch-amt AS DECIMAL INITIAL "0" LABEL "Upcharge$"
FIELD factored AS LOGICAL INITIAL "no" LABEL "Factored?"
FIELD inv-freq-code AS CHARACTER LABEL "Invoicing Frequency Code"
FIELD cr-hold-invdue AS DECIMAL INITIAL "0" LABEL "Grace $"
FIELD brd-cost AS INTEGER EXTENT 20 INITIAL "0" LABEL "Board Cost"
FIELD brd-pct AS DECIMAL EXTENT 20 INITIAL "0" LABEL "Board Markup %"
FIELD s-basis AS CHARACTER EXTENT 10 LABEL "Basis"
FIELD s-comm AS DECIMAL EXTENT 3 INITIAL "0" LABEL "Commission Pct"
FIELD s-man AS CHARACTER EXTENT 3 LABEL "Salemsman"
FIELD s-pct AS DECIMAL EXTENT 3 INITIAL "0" LABEL "Pct of Sale"
FIELD scomm AS DECIMAL INITIAL "0" LABEL "Flat Comm%"
FIELD po-mandatory AS LOGICAL INITIAL "no" LABEL "PO# Mandatory?"
FIELD send-ack AS LOGICAL INITIAL "no" LABEL "Send Acknowledgment?"
FIELD spare-char-1 AS CHARACTER
FIELD spare-char-2 AS CHARACTER
FIELD spare-char-3 AS CHARACTER
FIELD spare-char-4 AS CHARACTER
FIELD spare-char-5 AS CHARACTER
FIELD spare-dec-1 AS DECIMAL INITIAL "0"
FIELD spare-dec-2 AS DECIMAL INITIAL "0"
FIELD spare-dec-3 AS DECIMAL INITIAL "0"
FIELD spare-dec-4 AS DECIMAL INITIAL "0"
FIELD spare-dec-5 AS DECIMAL INITIAL "0"
FIELD spare-int-1 AS INTEGER INITIAL "0"
FIELD spare-int-2 AS INTEGER INITIAL "0"
FIELD spare-int-3 AS INTEGER INITIAL "0"
FIELD spare-int-4 AS INTEGER INITIAL "0"
FIELD spare-int-5 AS INTEGER INITIAL "0"
FIELD flat-comm AS LOGICAL INITIAL "no" LABEL "Flat Comm"
FIELD show-set AS LOGICAL INITIAL "no" LABEL "Show Set"
FIELD Bank-RTN AS INTEGER INITIAL "0" LABEL "Bank RTN"
FIELD Bank-Acct AS CHARACTER LABEL "Bank Acct"
FIELD flatCommPct AS DECIMAL INITIAL "0" LABEL "Flat Comm Pct"
FIELD csrUser_id AS CHARACTER LABEL "CSR User ID"
FIELD SwiftBIC AS CHARACTER LABEL "Swift BIC"
FIELD minOrder AS DECIMAL INITIAL "0" LABEL "Min Order"
FIELD minOrderPenalty AS DECIMAL INITIAL "0" LABEL "Min Ord Penalty"
FIELD minOrdPenaltyPct AS LOGICAL INITIAL "no" LABEL "Penalty Pct"
FIELD nationalAcct AS LOGICAL INITIAL "no" LABEL "National"
FIELD ASNClientID AS CHARACTER LABEL "ASN Client ID"
FIELD accountType AS CHARACTER LABEL "Acct Type"
FIELD splitType AS INTEGER INITIAL "0" LABEL "Split Type"
FIELD parentCust AS CHARACTER LABEL "Master Cust"
FIELD marketSegment AS CHARACTER LABEL "Market Segment"
FIELD naicsCode AS CHARACTER INITIAL "999999" LABEL "NAICS Code"
FIELD classID AS INTEGER INITIAL "0" LABEL "AR Class ID"
FIELD balanceCurrent AS DECIMAL INITIAL "0" LABEL "Current Balance"
FIELD balanceWithinGrace AS DECIMAL INITIAL "0" LABEL "Within Grace  Balance"
FIELD balancePastDue AS DECIMAL INITIAL "0" LABEL "Past Due Balance"
FIELD accountant AS CHARACTER LABEL "Billing owner"
FIELD matrixRounding AS CHARACTER LABEL "Price Matrix Rounding"
FIELD matrixPrecision AS INTEGER INITIAL "0" LABEL "Price Matrix Precision"
FIELD industryID AS CHARACTER LABEL "Industry"
FIELD pricingMethod AS CHARACTER LABEL "Pricing Method"
FIELD tagStatus AS CHARACTER LABEL "Tag Status"
FIELD internal AS LOGICAL INITIAL "no" LABEL "Internal Customer indication"
INDEX active  company  ASCENDING  active  ASCENDING  cust-no  ASCENDING 
INDEX city  company  ASCENDING  city  ASCENDING  name  ASCENDING 
INDEX cust IS  PRIMARY  UNIQUE  company  ASCENDING  cust-no  ASCENDING 
INDEX name  company  ASCENDING  name  ASCENDING  active  ASCENDING 
INDEX phone  company  ASCENDING  area-code  ASCENDING  phone  ASCENDING 
INDEX rec_key  rec_key  ASCENDING 
INDEX sman  company  ASCENDING  sman  ASCENDING  name  ASCENDING  active  ASCENDING 
INDEX state  company  ASCENDING  state  ASCENDING  name  ASCENDING 
INDEX terr  company  ASCENDING  terr  ASCENDING  name  ASCENDING 
INDEX type  company  ASCENDING  type  ASCENDING  name  ASCENDING  active  ASCENDING 
INDEX wi-name  name  ASCENDING 
INDEX zip  company  ASCENDING  zip  ASCENDING  postal  ASCENDING  name  ASCENDING . 


DEFINE DATASET dsCustomer FOR ttCustomer.