/*------------------------------------------------------------------------
  File:         AOA/dynBL/RmItemFolding.p
  Description:  Business Logic
  Author:       Sewa Singh
  Date Created: 6.15.2020
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

&Scoped-define ttTempTable ttRmItemFold
DEFINE TEMP-TABLE ttRmItemFold NO-UNDO
    FIELD i-no         AS CHARACTER FORMAT "x(10)"               LABEL "Item#"
    FIELD i-name       AS CHARACTER FORMAT "x(30)"               LABEL "Name"
    FIELD i-dscr       AS CHARACTER FORMAT "x(15)"               LABEL "Desc"
    FIELD est-dscr     AS CHARACTER FORMAT "x(30)"               LABEL "Est.Desc"
    FIELD i-code       AS CHARACTER FORMAT "x(15)"               LABEL "Item Code"
    FIELD tax-rcpt     AS LOGICAL   FORMAT "Yes/No"              LABEL "Taxable"
    FIELD mat-type     AS CHARACTER FORMAT "x(15)"               LABEL "Mat'l Type"
    FIELD cost-type    AS CHARACTER FORMAT "x(10)"               LABEL "Cost Type"
    FIELD procat       AS CHARACTER FORMAT "x(8)"                LABEL "Category"
    FIELD u-ptd        AS DECIMAL   FORMAT "->>,>>>,>>>,>>9.99"  LABEL "QTY Usage PTD"         
    FIELD u-ytd        AS DECIMAL   FORMAT "->>,>>>,>>>,>>9.99"  LABEL "Qty Usage YTD"
    FIELD u-lyr        AS DECIMAL   FORMAT "->>,>>>,>>>,>>9.99"  LABEL "Qty Usage Last YR"
    FIELD ink-type     AS CHARACTER FORMAT "x(15)"               LABEL "Ink Type"
    FIELD press-type   AS CHARACTER FORMAT "x(15)"               LABEL "Press Type"
    FIELD min-lbs      AS DECIMAL   FORMAT ">9.99"               LABEL "Min Lbs/Job"
    FIELD yield        AS INTEGER   FORMAT "->>,>>>,>>9"         LABEL "SI/Lb"
    FIELD weight-100   AS DECIMAL   FORMAT "->>,>>>,>>9.99"      LABEL "Wgt/100"
    FIELD cal          AS DECIMAL   FORMAT "->>,>>9.99"          LABEL "Caliper"
    FIELD shrink       AS DECIMAL   FORMAT ">>9.9999"            LABEL "Shrink%"
    FIELD basis-w      AS DECIMAL   FORMAT "->>,>>>,>>9.99"      LABEL "Basis Weight"
    FIELD s-wid        AS DECIMAL   FORMAT "->>,>>>,>>9.99"      LABEL "Sheet Width"    
    FIELD s-dep        AS DECIMAL   FORMAT "->>,>>>,>>9.99"      LABEL "Sheet Depth"
    FIELD s-len        AS DECIMAL   FORMAT "->>,>>>,>>9.99"      LABEL "Sheet Length"
    FIELD density      AS DECIMAL   FORMAT ">>,>>9.99<<<"        LABEL "Density"
    FIELD r-wid        AS DECIMAL   FORMAT "->>,>>>,>>9.99"      LABEL "Roll Width"
    FIELD color-1      AS CHARACTER FORMAT "x(15)"               LABEL "Color"   
    FIELD ect          AS INTEGER   FORMAT ">>>>>9"              LABEL "Core Dia"
    FIELD dept-name1   AS CHARACTER FORMAT "x(2)"                LABEL "Department 1"
    FIELD dept-name2   AS CHARACTER FORMAT "x(2)"                LABEL "Department 2"
    FIELD dept-name3   AS CHARACTER FORMAT "x(2)"                LABEL "Department 3"
    FIELD dept-name4   AS CHARACTER FORMAT "x(2)"                LABEL "Department 4"
    FIELD dept-name5   AS CHARACTER FORMAT "x(2)"                LABEL "Department 5"
    FIELD dept-name6   AS CHARACTER FORMAT "x(2)"                LABEL "Department 6"
    FIELD dept-name7   AS CHARACTER FORMAT "x(2)"                LABEL "Department 7"
    FIELD dept-name8   AS CHARACTER FORMAT "x(2)"                LABEL "Department 8"
    FIELD dept-name9   AS CHARACTER FORMAT "x(2)"                LABEL "Department 9"
    FIELD dept-name10  AS CHARACTER FORMAT "x(2)"                LABEL "Department 10"
    FIELD speed1       AS INTEGER   FORMAT ">>9"                 LABEL "Reduction% 1"
    FIELD speed2       AS INTEGER   FORMAT ">>9"                 LABEL "Reduction% 2"
    FIELD speed3       AS INTEGER   FORMAT ">>9"                 LABEL "Reduction% 3"    
    FIELD speed4       AS INTEGER   FORMAT ">>9"                 LABEL "Reduction% 4"
    FIELD speed5       AS INTEGER   FORMAT ">>9"                 LABEL "Reduction% 5"
    FIELD speed6       AS INTEGER   FORMAT ">>9"                 LABEL "Reduction% 6"
    FIELD speed7       AS INTEGER   FORMAT ">>9"                 LABEL "Reduction% 7"
    FIELD speed8       AS INTEGER   FORMAT ">>9"                 LABEL "Reduction% 8"    
    FIELD speed9       AS INTEGER   FORMAT ">>9"                 LABEL "Reduction% 9"
    FIELD speed10      AS INTEGER   FORMAT ">>9"                 LABEL "Reduction% 10"
    FIELD case-l       AS DECIMAL   FORMAT ">>9.9999"            LABEL "Length"
    FIELD case-w       AS DECIMAL   FORMAT ">>9.9999"            LABEL "Width"
    FIELD case-d       AS DECIMAL   FORMAT ">>9.9999"            LABEL "Depth"
    FIELD avg-w        AS DECIMAL   FORMAT ">>>9.99"             LABEL "Avg Wt"     
    FIELD box-case     AS INTEGER   FORMAT ">>>>9"               LABEL "Qty/Case"
    FIELD case-pall    AS INTEGER   FORMAT ">>>9"                LABEL "Case/Pallet"
    FIELD flute        AS CHARACTER FORMAT "x(15)"               LABEL "Flute"
    FIELD reg-no       AS CHARACTER FORMAT "x(3)"                LABEL "Reg. #"
    FIELD sqin-lb      AS DECIMAL   FORMAT ">>>,>>9"             LABEL "Sq In/Lb"
    FIELD linin-lb     AS DECIMAL   FORMAT ">>>,>>9"             LABEL "Lin In/UOM"
    FIELD loc          AS CHARACTER FORMAT "x(8)"                LABEL "Warehouse"
    FIELD loc-bin      AS CHARACTER FORMAT "x(8)"                LABEL "Bin"
    FIELD q-onh        AS DECIMAL   FORMAT "->>>,>>>,>>9.9<<<<<" LABEL "Qty On Hand"
    FIELD prep-cust    AS CHARACTER FORMAT "x(3)"                LABEL "Purchase UOM"
    FIELD prep-taxable AS CHARACTER FORMAT "x(3)"                LABEL "Consumption UOM"      
    .

/* Parameters Definitions ---                                           */

&Scoped-define subjectID 60
{AOA/includes/subjectID{&subjectID}Defs.i}

/* Local Variable Definitions ---                                       */

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic: 
    DEFINE VARIABLE h-field   AS HANDLE.
    DEFINE VARIABLE li-extent AS INTEGER                         NO-UNDO.
    DEFINE VARIABLE lc-return AS CHARACTER FORMAT "x(100)"       NO-UNDO.
    DEFINE VARIABLE test      AS INTEGER   FORMAT "->>>,>>>,>>9" NO-UNDO.
    DEFINE VARIABLE cTypelist AS CHARACTER FORMAT "x(30)"        NO-UNDO EXTENT 14.
    DEFINE VARIABLE iCount    AS INTEGER                         NO-UNDO.

    ASSIGN
        cTypelist[1]  = "I,V"
        cTypelist[2]  = "I,P,V"
        cTypelist[3]  = "A,B,P,D,C,Z,1,2,3,4"
        cTypelist[4]  = "D,C,Z,5,6"
        cTypelist[5]  = "A,B,P,R,1,2,3,4,D,C,Z,5,6"
        cTypelist[6]  = "A,B,P,1,2,3,4"
        cTypelist[7]  = "A,B,P,R,1,2,3,4,W"
        cTypelist[8]  = "A,B,P,R,1,2,3,4,W,D,C,Z,5,6"
        cTypelist[9]  = "A,B,P,R,1,2,3,4"
        cTypelist[10] = "A,B,P,1,2,3,4,W"
        cTypelist[11] = "G,S,L,F,T"
        cTypelist[12] = "G,W,S,L,F,T"
        cTypelist[13] = "D,C,Z"
        cTypelist[14] = "1,2,3,4"
        . 
    FOR EACH item NO-LOCK
        WHERE item.company  EQ cCompany
          AND item.i-no     GE cStartRMItem
          AND item.i-no     LE cEndRMItem
          AND item.industry EQ "1"
        :        
        CREATE ttRmItemFold.
        BUFFER-COPY item TO ttRmItemFold.
        ASSIGN
            ttRmItemFold.u-ytd = item.q-ytd * item.avg-cost
            ttRmItemFold.u-ptd = item.q-ptd * item.avg-cost
            ttRmItemFold.u-lyr = item.q-lyr * item.avg-cost
            .                  
        IF ttRmItemFold.u-ytd EQ ? THEN ttRmItemFold.u-ytd = 0.
        IF ttRmItemFold.u-ptd EQ ? THEN ttRmItemFold.u-ptd = 0.
        IF ttRmItemFold.u-lyr EQ ? THEN ttRmItemFold.u-lyr = 0.
        IF ttRmItemFold.q-onh EQ ? THEN ttRmItemFold.q-onh = 0.
        CASE ttRmItemFold.i-code:
            WHEN "R" THEN
            ttRmItemFold.i-code = "RM Stocked".
            WHEN "E" THEN
            ttRmItemFold.i-code = "Estimated Mat'1".
        END CASE.
        IF LOOKUP(ttRmItemFold.mat-type,cTypelist[2]) GT 0 THEN DO:
            CASE ttRmItemFold.ink-type:
                WHEN "I" THEN
                ttRmItemFold.ink-type = "Ink".
                WHEN "L" THEN
                ttRmItemFold.ink-type = "Lacquer".
                WHEN "U" THEN
                ttRmItemFold.ink-type = "Ultra Violet".
                WHEN "V" THEN
                ttRmItemFold.ink-type = "Varnish".
                WHEN "A" THEN
                ttRmItemFold.ink-type = "Aqueous".
            END CASE.
        END.
        ELSE ttRmItemFold.ink-type = "".                  
        IF  LOOKUP(ttRmItemFold.mat-type,cTypelist[2]) GT 0 THEN DO:
            CASE ttRmItemFold.press-type:
                WHEN "F" THEN
                ttRmItemFold.press-type = "Flexo".
                WHEN "G" THEN
                ttRmItemFold.press-type = "Gravure".
                WHEN "L" THEN
                ttRmItemFold.press-type = "Letterpress".
                WHEN "O" THEN
                ttRmItemFold.press-type = "Offset".
                WHEN "S" THEN
                ttRmItemFold.press-type = "Silkscreen".
            END CASE.
        END.
        ELSE ttRmItemFold.press-type = "".
        IF LOOKUP(ttRmItemFold.mat-type,cTypelist[4]) GT 0 THEN
        ASSIGN
            ttRmItemFold.flute     = item.flute
            ttRmItemFold.case-l    = item.case-l 
            ttRmItemFold.case-w    = item.case-w
            ttRmItemFold.case-d    = item.case-d
            ttRmItemFold.avg-w     = item.avg-w
            ttRmItemFold.box-case  = item.box-case
            ttRmItemFold.case-pall = item.case-pall
            .
        ELSE
        ASSIGN
            ttRmItemFold.flute     = ""
            ttRmItemFold.case-l    = 0
            ttRmItemFold.case-l    = 0 
            ttRmItemFold.case-w    = 0
            ttRmItemFold.case-d    = 0
            ttRmItemFold.avg-w     = 0
            ttRmItemFold.box-case  = 0
            ttRmItemFold.case-pall = 0
            .                
        IF LOOKUP(ttRmItemFold.mat-type,cTypelist[5]) GT 0 THEN
        ttRmItemFold.reg-no = item.reg-no.
        ELSE
        ttRmItemFold.reg-no = "".  
        IF LOOKUP(ttRmItemFold.mat-type,cTypelist[7]) GT 0 THEN
        ASSIGN
            ttRmItemFold.cal   = item.cal
            ttRmItemFold.s-len = item.s-len
            ttRmItemFold.r-wid = item.r-wid
            .
        ELSE
        ASSIGN
            ttRmItemFold.cal   = 0
            ttRmItemFold.s-len = 0 
            ttRmItemFold.s-wid = 0
            .
        IF LOOKUP(ttRmItemFold.mat-type,cTypelist[8]) GT 0 THEN
        ttRmItemFold.basis-w = item.basis-w.
        ELSE
        ttRmItemFold.basis-w  = 0. 
        IF LOOKUP(ttRmItemFold.mat-type,cTypelist[9]) GT 0 THEN
        ASSIGN 
            ttRmItemFold.ect   = item.ect
            ttRmItemFold.s-wid = item.s-wid
            .
        ELSE
        ASSIGN
            ttRmItemFold.ect   = 0
            ttRmItemFold.s-wid = 0
            .                
        IF LOOKUP(ttRmItemFold.mat-type,cTypelist[6]) GT 0 THEN
        ASSIGN
            ttRmItemFold.shrink      = item.shrink
            ttRmItemFold.dept-name1  = STRING(item.dept-name[1])
            ttRmItemFold.dept-name2  = STRING(item.dept-name[2])
            ttRmItemFold.dept-name3  = STRING(item.dept-name[3])
            ttRmItemFold.dept-name4  = STRING(item.dept-name[4])
            ttRmItemFold.dept-name5  = STRING(item.dept-name[5])
            ttRmItemFold.dept-name6  = STRING(item.dept-name[6])
            ttRmItemFold.dept-name7  = STRING(item.dept-name[7])
            ttRmItemFold.dept-name8  = STRING(item.dept-name[8])
            ttRmItemFold.dept-name9  = STRING(item.dept-name[9])
            ttRmItemFold.dept-name10 = STRING(item.dept-name[10])
            ttRmItemFold.speed1      = item.speed%[1]
            ttRmItemFold.speed2      = item.speed%[2]
            ttRmItemFold.speed3      = item.speed%[3]
            ttRmItemFold.speed4      = item.speed%[4]
            ttRmItemFold.speed5      = item.speed%[5]
            ttRmItemFold.speed6      = item.speed%[6]
            ttRmItemFold.speed7      = item.speed%[7]
            ttRmItemFold.speed8      = item.speed%[8]
            ttRmItemFold.speed9      = item.speed%[9]
            ttRmItemFold.speed10     = item.speed%[10]
            .
        ELSE
        ASSIGN
            ttRmItemFold.shrink      = 0
            ttRmItemFold.dept-name1  = ""
            ttRmItemFold.dept-name2  = ""
            ttRmItemFold.dept-name3  = ""
            ttRmItemFold.dept-name4  = ""
            ttRmItemFold.dept-name5  = ""
            ttRmItemFold.dept-name6  = ""
            ttRmItemFold.dept-name7  = ""
            ttRmItemFold.dept-name8  = ""
            ttRmItemFold.dept-name9  = ""
            ttRmItemFold.dept-name10 = ""
            ttRmItemFold.speed1      = 0
            ttRmItemFold.speed2      = 0
            ttRmItemFold.speed3      = 0
            ttRmItemFold.speed4      = 0
            ttRmItemFold.speed5      = 0
            ttRmItemFold.speed6      = 0
            ttRmItemFold.speed7      = 0
            ttRmItemFold.speed8      = 0
            ttRmItemFold.speed9      = 0
            ttRmItemFold.speed10     = 0
            . 
        IF LOOKUP(ttRmItemFold.mat-type,cTypelist[14]) GT 0 THEN 
        ASSIGN               
            ttRmItemFold.color-1 = item.color-1 
            ttRmItemFold.density = item.density
            .
        ELSE
        ASSIGN                
            ttRmItemFold.color-1 = ""
            ttRmItemFold.density = 0
            .                
        IF LOOKUP(ttRmItemFold.mat-type,cTypelist[2]) GT 0 THEN 
        ASSIGN
            ttRmItemFold.min-lbs = item.min-lbs
            ttRmItemFold.yield   = item.yield
            .
        ELSE
        ASSIGN
            ttRmItemFold.min-lbs = 0                 
            ttRmItemFold.yield   = 0
            .    
        IF LOOKUP(ttRmItemFold.mat-type,cTypelist[12]) GT 0 THEN 
        ASSIGN
            ttRmItemFold.linin-lb = item.linin-lb
            ttRmItemFold.sqin-lb  = item.sqin-lb
            .
        ELSE
        ASSIGN
            ttRmItemFold.linin-lb = 0
            ttRmItemFold.sqin-lb  = 0
            .
            
        iCount = iCount + 1.
        IF lProgressBar THEN
            RUN spProgressBar (cProgressBar, iCount, ?).
    END. /* each item */
END PROCEDURE.
