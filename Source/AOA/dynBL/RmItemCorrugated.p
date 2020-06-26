/*------------------------------------------------------------------------
  File:         AOA/dynBL/RmItemCorrugated.p
  Description:  Business Logic
  Author:       Sewa Singh
  Date Created: 6.15.2020
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

&Scoped-define ttTempTable ttRmItemCorrugated
DEFINE TEMP-TABLE ttRmItemCorrugated NO-UNDO
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
    FIELD basis-w      AS DECIMAL   FORMAT "->>,>>>,>>9.99"      LABEL "Weight/MSF"
    FIELD s-wid        AS DECIMAL   FORMAT "->>,>>>,>>9.99"      LABEL "Width"    
    FIELD s-dep        AS DECIMAL   FORMAT "->>,>>>,>>9.99"      LABEL "Depth"
    FIELD s-len        AS DECIMAL   FORMAT "->>,>>>,>>9.99"      LABEL "Length"
    FIELD density      AS DECIMAL   FORMAT ">>,>>9.99<<<"        LABEL "Density"
    FIELD r-wid        AS DECIMAL   FORMAT "->>,>>>,>>9.99"      LABEL "Roll Width"
    FIELD color-1      AS CHARACTER FORMAT "x(15)"               LABEL "Color"    
    FIELD ect          AS INTEGER   FORMAT ">>>>>9"              LABEL "ECT"
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
    FIELD case-l       AS DECIMAL   FORMAT ">>9.9999"            LABEL "Case Length"
    FIELD case-w       AS DECIMAL   FORMAT ">>9.9999"            LABEL "Case Width"
    FIELD case-d       AS DECIMAL   FORMAT ">>9.9999"            LABEL "Case Depth"
    FIELD avg-w        AS DECIMAL   FORMAT ">>>9.99"             LABEL "Avg Wt"     
    FIELD box-case     AS INTEGER   FORMAT ">>>>9"               LABEL "Boxes/Bundle"
    FIELD case-pall    AS INTEGER   FORMAT ">>>9"                LABEL "Bundle/Pallet"
    FIELD flute        AS CHARACTER FORMAT "x(15)"               LABEL "Flute"
    FIELD reg-no       AS CHARACTER FORMAT "x(3)"                LABEL "Test"
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
        CREATE ttRmItemCorrugated.
        BUFFER-COPY ITEM TO ttRmItemCorrugated.
        ASSIGN
            ttRmItemCorrugated.u-ytd = item.q-ytd * item.avg-cost
            ttRmItemCorrugated.u-ptd = item.q-ptd * item.avg-cost
            ttRmItemCorrugated.u-lyr = item.q-lyr * item.avg-cost
            .
        IF ttRmItemCorrugated.u-ytd EQ ? THEN ttRmItemCorrugated.u-ytd = 0.
        IF ttRmItemCorrugated.u-ptd EQ ? THEN ttRmItemCorrugated.u-ptd = 0.
        IF ttRmItemCorrugated.u-lyr EQ ? THEN ttRmItemCorrugated.u-lyr = 0.
        IF ttRmItemCorrugated.q-onh EQ ? THEN ttRmItemCorrugated.q-onh = 0.
        CASE ttRmItemCorrugated.i-code:
            WHEN "R" THEN
            ttRmItemCorrugated.i-code = "RM Stocked".
            WHEN "E" THEN
            ttRmItemCorrugated.i-code = "Estimated Mat'1".
        END CASE.
        IF LOOKUP(ttRmItemCorrugated.mat-type,cTypelist[2]) GT 0 THEN DO:
            CASE ttRmItemCorrugated.ink-type:
                WHEN "I" THEN
                ttRmItemCorrugated.ink-type = "Ink".
                WHEN "L" THEN
                ttRmItemCorrugated.ink-type = "Lacquer".
                WHEN "U" THEN
                ttRmItemCorrugated.ink-type = "Ultra Violet".
                WHEN "V" THEN
                ttRmItemCorrugated.ink-type = "Varnish".
                WHEN "A" THEN
                ttRmItemCorrugated.ink-type = "Aqueous".
            END CASE.
        END.
        ELSE  ttRmItemCorrugated.ink-type = "".
        IF LOOKUP(ttRmItemCorrugated.mat-type,cTypelist[2]) GT 0 THEN DO:
            CASE ttRmItemCorrugated.press-type:
                WHEN "F" THEN
                ttRmItemCorrugated.press-type = "Flexo".
                WHEN "G" THEN
                ttRmItemCorrugated.press-type = "Gravure".
                WHEN "L" THEN
                ttRmItemCorrugated.press-type = "Letterpress".
                WHEN "O" THEN
                ttRmItemCorrugated.press-type = "Offset".
                WHEN "S" THEN
                ttRmItemCorrugated.press-type = "Silkscreen".
            END CASE.
        END.
        ELSE ttRmItemCorrugated.press-type = "".
        IF LOOKUP(ttRmItemCorrugated.mat-type,cTypelist[3]) GT 0 THEN
        ASSIGN
            ttRmItemCorrugated.flute   = item.flute
            ttRmItemCorrugated.reg-no  = item.reg-no
            ttRmItemCorrugated.basis-w = item.basis-w
            .
        ELSE
        ASSIGN
            ttRmItemCorrugated.flute = ""
            ttRmItemCorrugated.reg-no = ""
            ttRmItemCorrugated.basis-w = 0
            .          
        IF LOOKUP(ttRmItemCorrugated.mat-type,cTypelist[13]) GT 0 THEN
        ASSIGN
            ttRmItemCorrugated.case-l    = item.case-l
            ttRmItemCorrugated.case-w    = item.case-w
            ttRmItemCorrugated.case-d    = item.case-d
            ttRmItemCorrugated.avg-w     = item.avg-w
            ttRmItemCorrugated.box-case  = item.box-case
            ttRmItemCorrugated.case-pall = item.case-pall
            .
        ELSE
        ASSIGN
            ttRmItemCorrugated.case-l    = 0
            ttRmItemCorrugated.case-w    = 0
            ttRmItemCorrugated.case-d    = 0
            ttRmItemCorrugated.avg-w     = 0
            ttRmItemCorrugated.box-case  = 0
            ttRmItemCorrugated.case-pall = 0
            .
        IF LOOKUP(ttRmItemCorrugated.mat-type,cTypelist[6]) GT 0 THEN
        ASSIGN
            ttRmItemCorrugated.cal   = item.cal
            ttRmItemCorrugated.s-len = item.s-len
            ttRmItemCorrugated.s-wid = item.s-wid
            ttRmItemCorrugated.r-wid = item.r-wid
            ttRmItemCorrugated.ect   = item.ect
            ttRmItemCorrugated.s-wid = item.s-wid
            .
        ELSE
        ASSIGN
            ttRmItemCorrugated.cal   = 0
            ttRmItemCorrugated.s-len = 0
            ttRmItemCorrugated.s-wid = 0
            ttRmItemCorrugated.r-wid = 0
            ttRmItemCorrugated.ect   = 0
            ttRmItemCorrugated.s-wid = 0
            .                
        IF LOOKUP(ttRmItemCorrugated.mat-type,cTypelist[10]) GT 0 THEN
        ttRmItemCorrugated.shrink = (item.shrink).
        ELSE ttRmItemCorrugated.shrink = 0.                
        IF LOOKUP(ttRmItemCorrugated.mat-type,cTypelist[6]) GT 0 THEN
        ASSIGN                                
            ttRmItemCorrugated.dept-name1  = STRING(item.dept-name[1])
            ttRmItemCorrugated.dept-name2  = STRING(item.dept-name[2])
            ttRmItemCorrugated.dept-name3  = STRING(item.dept-name[3])
            ttRmItemCorrugated.dept-name4  = STRING(item.dept-name[4])
            ttRmItemCorrugated.dept-name5  = STRING(item.dept-name[5])
            ttRmItemCorrugated.dept-name6  = STRING(item.dept-name[6])
            ttRmItemCorrugated.dept-name7  = STRING(item.dept-name[7])
            ttRmItemCorrugated.dept-name8  = STRING(item.dept-name[8])
            ttRmItemCorrugated.dept-name9  = STRING(item.dept-name[9])
            ttRmItemCorrugated.dept-name10 = STRING(item.dept-name[10])
            ttRmItemCorrugated.speed1      = item.speed%[1]
            ttRmItemCorrugated.speed2      = item.speed%[2]
            ttRmItemCorrugated.speed3      = item.speed%[3]
            ttRmItemCorrugated.speed4      = item.speed%[4]
            ttRmItemCorrugated.speed5      = item.speed%[5]
            ttRmItemCorrugated.speed6      = item.speed%[6]
            ttRmItemCorrugated.speed7      = item.speed%[7]
            ttRmItemCorrugated.speed8      = item.speed%[8]
            ttRmItemCorrugated.speed9      = item.speed%[9]
            ttRmItemCorrugated.speed10     = item.speed%[10]
            .
        ELSE
        ASSIGN                  
            ttRmItemCorrugated.dept-name1  = ""
            ttRmItemCorrugated.dept-name2  = ""
            ttRmItemCorrugated.dept-name3  = ""
            ttRmItemCorrugated.dept-name4  = ""
            ttRmItemCorrugated.dept-name5  = ""
            ttRmItemCorrugated.dept-name6  = ""
            ttRmItemCorrugated.dept-name7  = ""
            ttRmItemCorrugated.dept-name8  = ""
            ttRmItemCorrugated.dept-name9  = ""
            ttRmItemCorrugated.dept-name10 = ""
            ttRmItemCorrugated.speed1      = 0
            ttRmItemCorrugated.speed2      = 0
            ttRmItemCorrugated.speed3      = 0
            ttRmItemCorrugated.speed4      = 0
            ttRmItemCorrugated.speed5      = 0
            ttRmItemCorrugated.speed6      = 0
            ttRmItemCorrugated.speed7      = 0
            ttRmItemCorrugated.speed8      = 0
            ttRmItemCorrugated.speed9      = 0
            ttRmItemCorrugated.speed10     = 0
            .  
        IF LOOKUP(ttRmItemCorrugated.mat-type,cTypelist[1]) GT 0 THEN 
        ASSIGN
            ttRmItemCorrugated.min-lbs = item.min-lbs
            ttRmItemCorrugated.yield   = item.yield
            .
        ELSE
        ASSIGN
            ttRmItemCorrugated.min-lbs = 0
            ttRmItemCorrugated.yield   = 0
            .
        IF LOOKUP(ttRmItemCorrugated.mat-type,cTypelist[11]) GT 0 THEN 
        ASSIGN
            ttRmItemCorrugated.linin-lb = item.linin-lb
            ttRmItemCorrugated.sqin-lb  = item.sqin-lb
            ttRmItemCorrugated.density  = item.density
            ttRmItemCorrugated.color-1  = item.color-1
            .
        ELSE
        ASSIGN
            ttRmItemCorrugated.linin-lb = 0
            ttRmItemCorrugated.sqin-lb  = 0
            ttRmItemCorrugated.density  = 0
            ttRmItemCorrugated.color-1  = ""
            .               
    END. /* each item */
END PROCEDURE.
