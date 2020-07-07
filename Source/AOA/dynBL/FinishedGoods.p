/*------------------------------------------------------------------------
  File:         AOA/dynBL/FinishedGoods.p
  Description:  Business Logic
  Author:       Sewa Singh
  Date Created: 7.06.2020
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

&Scoped-define ttTempTable ttFinishGoods
DEFINE TEMP-TABLE ttFinishGoods NO-UNDO
    FIELD i-no            AS CHARACTER FORMAT "x(15)" LABEL "Item#"
    FIELD i-name          AS CHARACTER FORMAT "x(30)" LABEL "Item Name"
    FIELD part-no         AS CHARACTER FORMAT "x(15)" LABEL "Customer Part #"
    FIELD cust-no         AS CHARACTER FORMAT "x(8)" LABEL "Customer"
    FIELD cust-name       AS CHARACTER FORMAT "x(30)" LABEL "Customer Name #"
    FIELD est-no          AS CHARACTER FORMAT "x(8)" LABEL "Estimate"
    FIELD style           AS CHARACTER FORMAT "x(30)" LABEL "Style"
    FIELD procat          AS CHARACTER FORMAT "x(8)" LABEL "Category"
    FIELD procat-desc     AS CHARACTER FORMAT "x(30)" LABEL "Category Description"
    FIELD part-dscr1      AS CHARACTER FORMAT "x(30)" LABEL "Description"         
    FIELD part-dscr2      AS CHARACTER FORMAT "x(30)" LABEL "Description 2"
    FIELD part-dscr3      AS CHARACTER FORMAT "x(30)" LABEL "Description 3"
    FIELD i-code          AS CHARACTER FORMAT "x(15)" LABEL "Stock/Custom"
    FIELD die-no          AS CHARACTER FORMAT "x(15)" LABEL "Die #"
    FIELD plate-no        AS CHARACTER FORMAT "x(15)" LABEL "Plate #"
    FIELD upc-no          AS CHARACTER FORMAT "x(15)" LABEL "UPC#"
    FIELD cad-no          AS CHARACTER FORMAT "x(15)" LABEL "CAD #"
    FIELD spc-no          AS CHARACTER FORMAT "x(15)" LABEL "Quality/SPC #"
    FIELD stocked         AS LOGICAL   FORMAT "yes/No" LABEL "Stocked"    
    FIELD isaset          AS LOGICAL   FORMAT "Yes/No" LABEL "Set Header"
    FIELD spare-char-1    AS CHARACTER FORMAT "x(10)" LABEL "Group"    
    FIELD exempt-disc     AS LOGICAL   FORMAT "yes/no" LABEL "Exempt from Disc"    
    FIELD pur-man         AS LOGICAL   FORMAT "P/M" LABEL "P/M"
    FIELD sell-price      AS DECIMAL   FORMAT ">>,>>9.99<<<" LABEL "Sell Price"
    FIELD sell-uom        AS CHARACTER FORMAT "x(3)" LABEL "Sell Price UOM"
    FIELD type-code       AS CHARACTER FORMAT "x(5)" LABEL "Type Code"    
    FIELD curr-code1      AS CHARACTER FORMAT "x(10)" LABEL "Currency"    
    FIELD def-loc         AS CHARACTER FORMAT "x(5)" LABEL "Warehouse"
    FIELD def-loc-bin     AS CHARACTER FORMAT "x(8)" LABEL "Bin"
    FIELD class           AS CHARACTER FORMAT "x(10)" LABEL "Inventory Class"
    FIELD cc-code         AS CHARACTER FORMAT "x(2)" LABEL "Cycle Count Code"
    FIELD prod-code       AS CHARACTER FORMAT "x(5)" LABEL "Production Code"    
    FIELD case-count      AS INTEGER   FORMAT "->>,>>>,>>9" LABEL "Count"
    FIELD weight-100      AS DECIMAL   FORMAT "->>,>>>,>>9.99" LABEL "Weight"
    FIELD spare-int-1     AS INTEGER   FORMAT "->>,>>>,>>9" LABEL "Freeze Weight"
    FIELD prod-notes      AS CHARACTER FORMAT "x(30)" LABEL "Pk Note"
    FIELD frt-class       AS CHARACTER FORMAT "x(8)" LABEL "Freight Class"    
    FIELD frt-class-dscr  AS CHARACTER FORMAT "x(30)" LABEL "Freight Class Desc"
    FIELD trno            AS CHARACTER FORMAT "x(10)" LABEL "TrNo" 
    FIELD spare-char-4    AS CHARACTER FORMAT "x(10)" LABEL "Zone"     
    FIELD stackHeight     AS INTEGER   FORMAT ">>>9" LABEL "StackHeight"
    FIELD unitLength      AS DECIMAL   FORMAT ">>>,>>9.99" LABEL "PalletLen"
    FIELD unitWidth       AS DECIMAL   FORMAT ">>>,>>9.99" LABEL "PalletWid"
    FIELD unitHeight      AS DECIMAL   FORMAT ">>>,>>9.99" LABEL "PalletDep"
    FIELD palletVolume    AS DECIMAL   FORMAT "->>>,>>9.99<<" LABEL "StdPalletVol"       
    FIELD std-mat-cost    AS DECIMAL   FORMAT "->>,>>>,>>9.99" LABEL "Std Material Cost"
    FIELD std-lab-cost    AS DECIMAL   FORMAT "->>,>>>,>>9.99" LABEL "Std Labor Cost"
    FIELD std-var-cost    AS DECIMAL   FORMAT "->>,>>>,>>9.99" LABEL "Std Var OH Cost"
    FIELD std-fix-cost    AS DECIMAL   FORMAT "->>,>>>,>>9.99" LABEL "Std Fix OH Cost"    
    FIELD total-std-cost  AS DECIMAL   FORMAT "->>,>>>,>>9.99" LABEL "Total Std Cost"
    FIELD avg-cost        AS DECIMAL   FORMAT "->>,>>>,>>9.99" LABEL "Average Cost"     
    FIELD last-cost       AS DECIMAL   FORMAT "->>,>>>,>>9.99" LABEL "Last Cost"
    FIELD prod-uom        AS CHARACTER FORMAT "x(3)" LABEL "Cost UOM"
    FIELD spare-dec-1     AS DECIMAL   FORMAT "->>,>>>,>>9.99" LABEL "Full Cost"    
    FIELD spare-char-2    AS CHARACTER FORMAT "x(10)" LABEL "Varied"  
    FIELD taxable         AS LOGICAL   FORMAT "Yes/No" LABEL "Taxable" 
    FIELD stat            AS CHARACTER FORMAT "x(10)" LABEL "Status"
    FIELD ship-meth       AS LOGICAL   FORMAT "Case/Pallet" LABEL "Ship Method"    
    FIELD vend-no         AS CHARACTER FORMAT "x(8)" LABEL "Vendor 1"
    FIELD vend-item       AS CHARACTER FORMAT "x(15)" LABEL "Vendor 1 Item #"
    FIELD vend2-no        AS CHARACTER FORMAT "x(8)" LABEL "Vendor 2"
    FIELD vend2-item      AS CHARACTER FORMAT "x(15)" LABEL "Vendor 2 Item #"          
    FIELD dfuncAlloc      AS CHARACTER FORMAT "x(25)" LABEL "Set Allocation"  
    FIELD ord-policy      AS LOGICAL   FORMAT "R/L" LABEL "Reorder Policy" 
    FIELD ord-level       AS DECIMAL   FORMAT ">>>,>>>,>>9.999" LABEL "Reorder Level"       
    FIELD ord-min         AS DECIMAL   FORMAT "->>,>>>,>>9" LABEL "Minimum Order"
    FIELD ord-max         AS DECIMAL   FORMAT "->>,>>>,>>9" LABEL "Maximum Order"
    FIELD pur-uom         AS CHARACTER FORMAT "x(3)" LABEL "Purchased Qty UOM"
    FIELD lead-days       AS INTEGER   FORMAT "->>,>>>,>>9" LABEL "Lead Time (Days)"
    FIELD beg-date        AS DATE      FORMAT "99/99/9999" LABEL "Beginning Date"    
    FIELD beg-bal         AS DECIMAL   FORMAT "->>,>>>,>>>,>>9.99" LABEL "Beginning Balance"         
    FIELD q-onh           AS DECIMAL   FORMAT "->>,>>>,>>>,>>9.99" LABEL "Qty On-hand"
    FIELD q-ono           AS DECIMAL   FORMAT "->>,>>>,>>>,>>9.99" LABEL "Qty On Ord"
    FIELD q-alloc         AS DECIMAL   FORMAT "->>,>>>,>>>,>>9.99" LABEL "Qty Allocated"
    FIELD q-back          AS DECIMAL   FORMAT "->>,>>>,>>>,>>9.99" LABEL "Qty Backordered"
    FIELD q-avail         AS DECIMAL   FORMAT "->>,>>>,>>>,>>9.99" LABEL "Qty Available"    
    FIELD q-ptd           AS DECIMAL   FORMAT "->>,>>>,>>9" LABEL "Qty Ordered PTD"
    FIELD q-ord-ytd       AS DECIMAL   FORMAT "->>,>>>,>>9.99" LABEL "Qty Ordered YTD"
    FIELD u-ord-ytd       AS DECIMAL   FORMAT "->>>,>>>,>>9.99" LABEL "Qty Ordered Last Yr"       
    FIELD q-prod-ptd      AS DECIMAL   FORMAT "->>>,>>>,>>9.99" LABEL "Qty Produced PTD"
    FIELD q-prod-ytd      AS DECIMAL   FORMAT "->>>,>>>,>>9.99" LABEL "Qty Produced YTD"
    FIELD u-prod-ytd      AS DECIMAL   FORMAT "->>>,>>>,>>9.99" LABEL "Qty Produced Last Yr"    
    FIELD q-ship-ptd      AS DECIMAL   FORMAT "->>>,>>>,>>9.99" LABEL "Qty Shipped PTD"
    FIELD q-ship-ytd      AS DECIMAL   FORMAT "->>>,>>>,>>9.99" LABEL "Qty Shipped YTD"
    FIELD u-ship-ytd      AS DECIMAL   FORMAT "->>>,>>>,>>9.99" LABEL "Qty Shipped Last Yr"      
    FIELD q-inv-ptd       AS DECIMAL   FORMAT "->>>,>>>,>>9.99" LABEL "Qty Invoiced PTD"
    FIELD q-inv-ytd       AS DECIMAL   FORMAT "->>>,>>>,>>9.99" LABEL "Qty Invoiced YTD"
    FIELD u-inv-ytd       AS DECIMAL   FORMAT "->>>,>>>,>>9.99" LABEL "Qty Invoiced Last Yr"
    FIELD dfuncTotMSFPTD  AS DECIMAL   FORMAT "->>>,>>>,>>9.99" LABEL "Total MSF PTD"
    FIELD ytd-msf         AS DECIMAL   FORMAT "->>,>>>,>>>,>>9.99" LABEL "Total MSF YTD"         
    FIELD lyytd-msf       AS DECIMAL   FORMAT "->>,>>>,>>>,>>9.99" LABEL "Total MSF Last Yr"    
    FIELD l-score50       AS DECIMAL   FORMAT "->>,>>>,>>>,>>9.99" LABEL "Box Length"
    FIELD w-score50       AS DECIMAL   FORMAT "->>,>>>,>>>,>>9.99" LABEL "Box Width"
    FIELD d-score50       AS DECIMAL   FORMAT "->>,>>>,>>>,>>9.99" LABEL "Box Depth"
    FIELD t-len           AS DECIMAL   FORMAT ">>>,>>9.99" LABEL "Blank Length"
    FIELD t-wid           AS DECIMAL   FORMAT ">>>,>>9.99" LABEL "Blank Width"    
    FIELD t-sqin          AS DECIMAL   FORMAT "->>,>>>,>>9.99" LABEL "Total Sq In"
    FIELD t-sqft          AS DECIMAL   FORMAT "->>,>>>,>>9.99" LABEL "Total Sq Ft"
    FIELD col1            AS CHARACTER FORMAT "x(20)" LABEL "Est Ink1"
    FIELD col1desc        AS CHARACTER FORMAT "x(30)" LABEL "Est Ink1 Desc"
    FIELD col2            AS CHARACTER FORMAT "x(20)" LABEL "Est Ink2"
    FIELD col2desc        AS CHARACTER FORMAT "x(30)" LABEL "Est Ink2 Desc"
    FIELD col3            AS CHARACTER FORMAT "x(20)" LABEL "Est Ink3"
    FIELD col3desc        AS CHARACTER FORMAT "x(30)" LABEL "Est Ink3 Desc"
    FIELD col4            AS CHARACTER FORMAT "x(20)" LABEL "Est Ink4"
    FIELD col4desc        AS CHARACTER FORMAT "x(30)" LABEL "Est Ink4 Desc"
    FIELD col5            AS CHARACTER FORMAT "x(20)" LABEL "Est Ink5"
    FIELD col5desc        AS CHARACTER FORMAT "x(30)" LABEL "Est Ink5 Desc"    
    FIELD col6            AS CHARACTER FORMAT "x(20)" LABEL "Est Ink6"
    FIELD col6desc        AS CHARACTER FORMAT "x(30)" LABEL "Est Ink6 Desc"
    FIELD col7            AS CHARACTER FORMAT "x(20)" LABEL "Est Ink7"
    FIELD col7desc        AS CHARACTER FORMAT "x(30)" LABEL "Est Ink7 Desc"
    FIELD col8            AS CHARACTER FORMAT "x(20)" LABEL "Est Ink8"
    FIELD col8desc        AS CHARACTER FORMAT "x(30)" LABEL "Est Ink8 Desc"
    FIELD col9            AS CHARACTER FORMAT "x(20)" LABEL "Est Ink9"
    FIELD col9desc        AS CHARACTER FORMAT "x(30)" LABEL "Est Ink9 Desc"
    FIELD col10           AS CHARACTER FORMAT "x(20)" LABEL "Est Ink10"
    FIELD col10desc       AS CHARACTER FORMAT "x(30)" LABEL "Est Ink10 Desc"
    FIELD cat1            AS CHARACTER FORMAT "x(20)" LABEL "Est Coating1"         
    FIELD cat2            AS CHARACTER FORMAT "x(20)" LABEL "Est Coating2"
    FIELD cat3            AS CHARACTER FORMAT "x(20)" LABEL "Est Coating3"    
    FIELD brd-cd          AS CHARACTER FORMAT "x(10)" LABEL "Board Code"
    FIELD brd-nam         AS CHARACTER FORMAT "x(20)" LABEL "Board Name"
    FIELD calp            AS DECIMAL   FORMAT "->>,>>9.99" LABEL "Caliper"
    FIELD cas-cd          AS CHARACTER FORMAT "x(15)" LABEL "Case Code"
    FIELD cas-nam         AS CHARACTER FORMAT "x(30)" LABEL "Case Name"
    FIELD cas-qt          AS INTEGER   FORMAT "->>,>>>,>>9" LABEL "Case Qty"
    FIELD skid-cd         AS CHARACTER FORMAT "x(10)" LABEL "Skid Code"    
    FIELD skid-nam        AS CHARACTER FORMAT "x(30)" LABEL "Skid Name"
    FIELD skid-qt         AS INTEGER   FORMAT "->>,>>>,>>9" LABEL "Skid Qty"
    FIELD spare-int-2     AS INTEGER   FORMAT "->>,>>>,>>9" LABEL "Release Sequence"
    FIELD case-pall       AS INTEGER   FORMAT ">>>,>>>" LABEL "Units/Pall"
    FIELD factored        AS LOGICAL   FORMAT "Yes/No" LABEL "Factor Invoice"
    FIELD spare-char-3    AS CHARACTER FORMAT "x(5)" LABEL "Sales Rep"    
    FIELD spc-grp1        AS CHARACTER FORMAT "x(10)" LABEL "Spec Note 1 Group"
    FIELD spc-title1      AS CHARACTER FORMAT "x(30)" LABEL "Spec Note 1 Title"
    FIELD spc-note1       AS CHARACTER FORMAT "x(100)" LABEL "Spec Note 1 Note [Large]"    
    FIELD spc-grp2        AS CHARACTER FORMAT "x(10)" LABEL "Spec Note 2 Group"
    FIELD spc-title2      AS CHARACTER FORMAT "x(30)" LABEL "Spec Note 2 Title"
    FIELD spc-note2       AS CHARACTER FORMAT "x(100)" LABEL "Spec Note 2 Note [Large]"    
    FIELD spc-grp3        AS CHARACTER FORMAT "x(10)" LABEL "Spec Note 3 Group"
    FIELD spc-title3      AS CHARACTER FORMAT "x(30)" LABEL "Spec Note 3 Title"
    FIELD spc-note3       AS CHARACTER FORMAT "x(100)" LABEL "Spec Note 3 Note [Large]"    
    FIELD spc-grp4        AS CHARACTER FORMAT "x(10)" LABEL "Spec Note 4 Group"
    FIELD spc-title4      AS CHARACTER FORMAT "x(30)" LABEL "Spec Note 4 Title"
    FIELD spc-note4       AS CHARACTER FORMAT "x(100)" LABEL "Spec Note 4 Note [Large]"    
    FIELD spc-grp5        AS CHARACTER FORMAT "x(10)" LABEL "Spec Note 5 Group"
    FIELD spc-title5      AS CHARACTER FORMAT "x(30)" LABEL "Spec Note 5 Title"
    FIELD spc-note5       AS CHARACTER FORMAT "x(100)" LABEL "Spec Note 5 Note [Large]"    
    FIELD setupBy         AS CHARACTER FORMAT "x(16)" LABEL "Setup By UserId"         
    FIELD setupDate       AS DATE      FORMAT "99/99/9999" LABEL "Setup Date"
    FIELD modifiedBy      AS CHARACTER FORMAT "x(16)" LABEL "Modified By"
    FIELD modifiedDate    AS DATE      FORMAT "99/99/9999" LABEL "Modified Date"
    FIELD pallet-qty      AS INTEGER   FORMAT ">>>,>>>,>>9" LABEL "Pallet Quantity"    
    FIELD fgcol1          AS CHARACTER FORMAT "x(10)" LABEL "FgItem Color1"
    FIELD fgcol1Desc      AS CHARACTER FORMAT "x(30)" LABEL "FgItem Color1 Desc"
    FIELD fgcol2          AS CHARACTER FORMAT "x(10)" LABEL "FgItem Color2"
    FIELD fgcol2Desc      AS CHARACTER FORMAT "x(30)" LABEL "FgItem Color2 Desc"
    FIELD fgcol3          AS CHARACTER FORMAT "x(10)" LABEL "FgItem Color3"
    FIELD fgcol3Desc      AS CHARACTER FORMAT "x(30)" LABEL "FgItem Color3 Desc"
    FIELD fgcol4          AS CHARACTER FORMAT "x(10)" LABEL "FgItem Color4"
    FIELD fgcol4Desc      AS CHARACTER FORMAT "x(30)" LABEL "FgItem Color4 Desc"
    FIELD fgcol5          AS CHARACTER FORMAT "x(10)" LABEL "FgItem Color5"
    FIELD fgcol5Desc      AS CHARACTER FORMAT "x(30)" LABEL "FgItem Color5 Desc"    
    FIELD fgcol6          AS CHARACTER FORMAT "x(10)" LABEL "FgItem Color6"
    FIELD fgcol6Desc      AS CHARACTER FORMAT "x(30)" LABEL "FgItem Color6 Desc"
    FIELD fgcol7          AS CHARACTER FORMAT "x(10)" LABEL "FgItem Color7"
    FIELD fgcol7Desc      AS CHARACTER FORMAT "x(30)" LABEL "FgItem Color7 Desc"
    FIELD fgcol8          AS CHARACTER FORMAT "x(10)" LABEL "FgItem Color8"
    FIELD fgcol8Desc      AS CHARACTER FORMAT "x(30)" LABEL "FgItem Color8 Desc"
    FIELD fgcol9          AS CHARACTER FORMAT "x(10)" LABEL "FgItem Color9"
    FIELD fgcol9Desc      AS CHARACTER FORMAT "x(30)" LABEL "FgItem Color9 Desc"
    FIELD fgcol10         AS CHARACTER FORMAT "x(10)" LABEL "FgItem Color10"
    FIELD fgcol10Desc     AS CHARACTER FORMAT "x(30)" LABEL "FgItem Color10 Desc"
    FIELD productTaxClass AS CHARACTER FORMAT "x(18)" LABEL "Product Tax Class"
    
    .

/* Parameters Definitions ---                                           */

&Scoped-define subjectID 84
{AOA/includes/subjectID{&subjectID}Defs.i}

{custom/globdefs.i}

/* Local Variable Definitions ---                                       */

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic:     
    DEFINE VARIABLE cTrNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTrName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCasNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCasName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBoard AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBoard2 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCaseCount AS INTEGER NO-UNDO.    
    DEFINE VARIABLE iPalletCount AS INTEGER NO-UNDO.
    DEFINE VARIABLE iCasesPerPallet AS INTEGER NO-UNDO.
    DEFINE VARIABLE dClip AS DECIMAL NO-UNDO.
    DEFINE VARIABLE iInkCnt AS INTEGER NO-UNDO.
    DEFINE VARIABLE cInks AS CHARACTER EXTENT 20 NO-UNDO.
    DEFINE VARIABLE cInksDesc AS CHARACTER EXTENT 20 NO-UNDO.
    DEFINE VARIABLE cCoat AS CHARACTER EXTENT 20 NO-UNDO.
    DEFINE VARIABLE iCoatCnt AS INTEGER NO-UNDO.
    DEFINE VARIABLE cSpecGroup AS CHAR EXTENT 7 NO-UNDO.
    DEFINE VARIABLE cSpecTitle AS CHAR EXTENT 7 NO-UNDO.
    DEFINE VARIABLE cSpecNote AS CHAR EXTENT 7 NO-UNDO.
    DEFINE VARIABLE cFGColor AS CHARACTER FORM "x(10)" EXTENT 10 NO-UNDO.
    DEFINE VARIABLE cFGColorDesc AS CHARACTER FORM "x(10)" EXTENT 10 NO-UNDO.
    DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
    
    FOR EACH itemfg WHERE itemfg.company = cCompany
        AND itemfg.i-no GE cStartFGItem
        AND itemfg.i-no LE cEndFGItem          
        AND itemfg.part-no GE cStartCustPart
        AND itemfg.part-no LE cEndCustPart
        AND itemfg.cust-no GE cStartCustNo
        AND itemfg.cust-no LE cEndCustNo
        AND itemfg.est-no GE cStartEstimateNo
        AND itemfg.est-no LE cEndEstimateNo
        AND itemfg.style GE cStartStyle
        AND itemfg.style LE cEndStyle
        AND itemfg.procat GE cStartProCats
        AND itemfg.procat LE cEndProCats
        AND ( ((itemfg.stat = "A" OR itemfg.stat = "" ) AND cItemStatus EQ "1" ) 
              OR (cItemStatus EQ "2" ) )
        NO-LOCK:
        
        CREATE ttFinishGoods.
        BUFFER-COPY itemfg TO ttFinishGoods.
        
       ASSIGN          
        cTrNo = "" 
        cTrName = ""
        cCasNo = ""
        cCasName = ""
        cBoard = ""
        cBoard2 = ""
        iCaseCount = 0
        iPalletCount = 0
        iCasesPerPallet = 0 
        dClip  =  0
        cInks = ""
        cCoat = ""
        cFGColor = ""
        cFGColorDesc = "".
        
              
     iCount = 0.
     cSpecGroup = "".
     cSpecTitle = "".
     cSpecNote = "".
     IF lAllSpecNote THEN do:
         MAIN-NOTES:
         FOR EACH notes WHERE notes.rec_key = itemfg.rec_key AND
         can-do(cSpecCode,notes.note_code) NO-LOCK:              
             iCount = iCount + 1.
             ASSIGN cSpecGroup[iCount] = notes.note_code
                 cSpecTitle[iCount] = notes.note_title
                 cSpecNote[iCount] = notes.note_text.
                 IF iCount GE 6 THEN LEAVE MAIN-NOTES.
         END.
     END.


     FIND FIRST est WHERE est.company = cCompany
                      AND est.est-no = itemfg.est-no NO-LOCK NO-ERROR.
    
     
     IF itemfg.ship-meth THEN
        ASSIGN iCaseCount = itemfg.case-count
               iPalletCount = 0.
     ELSE
        ASSIGN iCaseCount = 0
               iPalletCount = itemfg.case-count.
     
     FIND FIRST eb WHERE eb.company = cCompany
                     AND eb.est-no = itemfg.est-no 
                     AND eb.stock-no = itemfg.i-no NO-LOCK NO-ERROR.
     
     IF AVAIL eb THEN DO:
       FIND FIRST ef WHERE ef.company = cCompany
                        AND ef.est-no = itemfg.est-no 
                        AND ef.form-no = eb.form-no  NO-LOCK NO-ERROR.
        
        FIND FIRST style WHERE style.company = itemfg.company
                           AND style.style = eb.style NO-LOCK NO-ERROR. 
     
        ASSIGN ttFinishGoods.style =  IF AVAIL style THEN style.dscr ELSE eb.style
               cTrNo = eb.tr-no
               cCasNo = eb.cas-no
               cBoard = IF AVAIL ef THEN ef.board ELSE ""
               cBoard2 = IF AVAIL ef THEN ef.brd-dscr ELSE ""
               iCaseCount = eb.cas-cnt
               iPalletCount = eb.tr-cnt
               iCasesPerPallet = eb.cas-pal
               .  
        FIND FIRST ITEM WHERE ITEM.company = eb.company
                          AND ITEM.i-no = eb.tr-no NO-LOCK NO-ERROR.

        IF AVAIL ITEM THEN cTrName = ITEM.i-NAME.
        FIND FIRST ITEM WHERE ITEM.company = eb.company
                          AND ITEM.i-no = eb.cas-no NO-LOCK NO-ERROR.
       IF AVAIL ITEM THEN cCasName = ITEM.i-NAME.
        FIND FIRST ITEM WHERE ITEM.company = eb.company
                          AND ITEM.i-no = cBoard NO-LOCK NO-ERROR.         

        IF AVAIL ITEM THEN dClip = ITEM.cal . 
     
        ASSIGN iInkCnt = 1
               iCoatCnt = 1. 
     
        DO iCount = 1 TO 10:
          IF eb.est-type <= 4 THEN DO:
             IF eb.i-code2[iCount] <> "" THEN DO:
                FIND FIRST ITEM WHERE ITEM.company = cCompany
                             AND ITEM.i-no = eb.i-code2[iCount] NO-LOCK NO-ERROR.
                IF AVAIL ITEM AND ITEM.mat-type = "V" THEN 
                   ASSIGN cCoat[iCoatCnt] = eb.i-code2[iCount]                           
                          iCoatCnt = iCoatCnt + 1.
                ELSE ASSIGN cInks[iInkCnt] = eb.i-code2[iCount]
                            cInksDesc[iInkCnt] = eb.i-dscr2[iCount]
                            iInkCnt = iInkCnt + 1.
             END.
          END.
          else DO:
               IF eb.i-code[iCount] <> "" THEN DO:
                  FIND FIRST ITEM WHERE ITEM.company = cCompany
                                  AND ITEM.i-no = eb.i-code[iCount] NO-LOCK NO-ERROR.
                  IF AVAIL ITEM AND ITEM.mat-type = "V" THEN 
                        ASSIGN cCoat[iCoatCnt] = eb.i-code[iCount]
                           iCoatCnt = iCoatCnt + 1.
                  ELSE ASSIGN cInks[iInkCnt] = eb.i-code[iCount]
                              cInksDesc[iInkCnt] = eb.i-dscr[iCount]
                              iInkCnt = iInkCnt + 1.
               END.
          END.
        END. 
     
     END.  
     iInkCnt = 1.
     MAIN-LOOP-INK:
     FOR EACH itemfg-ink OF itemfg NO-LOCK:  
         cFGColor[iInkCnt] = itemfg-ink.rm-i-no. 
         cFGColorDesc[iInkCnt] = itemfg-ink.dscr.
          IF iInkCnt GE 10 THEN LEAVE MAIN-LOOP-INK.
          iInkCnt = iInkCnt + 1 .
     END.   
     
        ASSIGN
            ttFinishGoods.col1        = cInks[1]
            ttFinishGoods.col2        = cInks[2]
            ttFinishGoods.col3        = cInks[3]
            ttFinishGoods.col4        = cInks[4]
            ttFinishGoods.col5        = cInks[5]
            ttFinishGoods.col6        = cInks[6]
            ttFinishGoods.col7        = cInks[7]
            ttFinishGoods.col8        = cInks[8]
            ttFinishGoods.col9        = cInks[9]
            ttFinishGoods.col10       = cInks[10]
            ttFinishGoods.col1Desc    = cInksDesc[1]
            ttFinishGoods.col2Desc    = cInksDesc[2]
            ttFinishGoods.col3Desc    = cInksDesc[3]
            ttFinishGoods.col4Desc    = cInksDesc[4]
            ttFinishGoods.col5Desc    = cInksDesc[5]
            ttFinishGoods.col6Desc    = cInksDesc[6]
            ttFinishGoods.col7Desc    = cInksDesc[7]
            ttFinishGoods.col8Desc    = cInksDesc[8]
            ttFinishGoods.col9Desc    = cInksDesc[9]
            ttFinishGoods.col10Desc   = cInksDesc[10]      
            ttFinishGoods.cat1        = cCoat[1]
            ttFinishGoods.cat2        = cCoat[2]
            ttFinishGoods.cat3        = cCoat[3]
            ttFinishGoods.brd-cd      = cBoard
            ttFinishGoods.brd-nam     = cBoard2
            ttFinishGoods.calp        = dClip
            ttFinishGoods.cas-cd      = cCasNo
            ttFinishGoods.cas-nam     = cCasName
            ttFinishGoods.cas-qt      = iCaseCount
            ttFinishGoods.skid-cd     = cTrNo
            ttFinishGoods.skid-nam    = cTrName
            ttFinishGoods.skid-qt     = iPalletCount            
            ttFinishGoods.spc-grp1    = cSpecGroup[1]
            ttFinishGoods.spc-title1  = cSpecTitle[1]
            ttFinishGoods.spc-note1   = cSpecNote[1]
            ttFinishGoods.spc-grp2    = cSpecGroup[2]
            ttFinishGoods.spc-title2  = cSpecTitle[2]
            ttFinishGoods.spc-note2   = cSpecNote[2]
            ttFinishGoods.spc-grp3    = cSpecGroup[3]
            ttFinishGoods.spc-title3  = cSpecTitle[3]
            ttFinishGoods.spc-note3   = cSpecNote[3]
            ttFinishGoods.spc-grp4    = cSpecGroup[4]
            ttFinishGoods.spc-title4  = cSpecTitle[4]
            ttFinishGoods.spc-note4   = cSpecNote[4]
            ttFinishGoods.spc-grp5    = cSpecGroup[5]
            ttFinishGoods.spc-title5  = cSpecTitle[5]
            ttFinishGoods.spc-note5   = cSpecNote[5]      
            ttFinishGoods.pallet-qty  = (itemfg.case-count * itemfg.case-pall ) + itemfg.quantityPartial
            ttFinishGoods.fgcol1      = cFGColor[1]
            ttFinishGoods.fgcol2      = cFGColor[2]
            ttFinishGoods.fgcol3      = cFGColor[3]
            ttFinishGoods.fgcol4      = cFGColor[4]
            ttFinishGoods.fgcol5      = cFGColor[5]
            ttFinishGoods.fgcol6      = cFGColor[6]
            ttFinishGoods.fgcol7      = cFGColor[7]
            ttFinishGoods.fgcol8      = cFGColor[8]
            ttFinishGoods.fgcol9      = cFGColor[9]
            ttFinishGoods.fgcol10     = cFGColor[10]  
            ttFinishGoods.fgcol1Desc  = cFGColorDesc[1]
            ttFinishGoods.fgcol2Desc  = cFGColorDesc[2]
            ttFinishGoods.fgcol3Desc  = cFGColorDesc[3]
            ttFinishGoods.fgcol4Desc  = cFGColorDesc[4]
            ttFinishGoods.fgcol5Desc  = cFGColorDesc[5]
            ttFinishGoods.fgcol6Desc  = cFGColorDesc[6]
            ttFinishGoods.fgcol7Desc  = cFGColorDesc[7]
            ttFinishGoods.fgcol8Desc  = cFGColorDesc[8]
            ttFinishGoods.fgcol9Desc  = cFGColorDesc[9]
            ttFinishGoods.fgcol10Desc = cFGColorDesc[10] 
            ttFinishGoods.l-score50   = itemfg.l-score[50]
            ttFinishGoods.w-score50   = itemfg.w-score[50]
            ttFinishGoods.d-score50   = itemfg.d-score[50].
            
            CASE itemfg.alloc :
                WHEN YES THEN
                     ttFinishGoods.dfuncAlloc = "Unassembled".
                WHEN NO THEN
                    ttFinishGoods.dfuncAlloc = "Assembled".
                OTHERWISE
                    ttFinishGoods.dfuncAlloc = "Assembled w/Part Receipts".
            END CASE.           
            
            IF g_period NE 0 THEN ttFinishGoods.dfuncTotMSFPTD = itemfg.ptd-msf[g_period].         
                   
       
    END. /* each item */
END PROCEDURE.
