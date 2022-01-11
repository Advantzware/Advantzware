
/*------------------------------------------------------------------------
    File        : ttJobReport.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Thur Jan 06 2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

define temp-table ttJob no-undo
     FIELD cJobNo              AS CHARACTER FORMAT "x(8)" LABEL "Job Number"
     FIELD iJobNo2             AS INTEGER FORMAT ">>9" LABEL "Job Number2"
     FIELD cCustName           AS CHARACTER FORMAT "X(30)" LABEL "Cust Name"
     FIELD dtCloseDate         AS DATE      FORMAT "99/99/9999" LABEL "Close Date"
     FIELD cEstimate           AS CHARACTER FORMAT "x(8)" LABEL "Estimate"
     FIELD dTotStdMachineCost  AS DECIMAL FORMAT "->>,>>>,>>9.9<<<" LABEL "Total Standard Machine Cost"
     FIELD dTotActMachineCost  AS DECIMAL FORMAT "->>,>>>,>>9.9<<<" LABEL "Total Actual Machine Cost"
     FIELD dTotStdMaterialCost AS DECIMAL FORMAT "->>,>>>,>>9.9<<<" LABEL "Total Standard Material Cost"
     FIELD dTotActMaterialCost AS DECIMAL FORMAT "->>,>>>,>>9.9<<<" LABEL "Total Actual Material Cost"
     FIELD dTotStdCost         AS DECIMAL     FORMAT "->>,>>>,>>9.9<<<" LABEL "Total Standard Cost"
     FIELD dTotActCost         AS DECIMAL     FORMAT "->>,>>>,>>9.9<<<" LABEL "Total Actual Cost" .

define temp-table ttDepartment no-undo
     FIELD cJobNo         AS CHARACTER FORMAT "x(8)" LABEL "Job Number"
     FIELD iJobNo2        AS INTEGER FORMAT ">>9" LABEL "Job Number2"
     FIELD cDept          AS CHARACTER FORMAT "x(10)" LABEL "Department"
     FIELD dRunQty        AS DECIMAL   FORMAT "->>>,>>>,>>9" LABEL "Run Qty"
     FIELD dRunQtyVar     AS DECIMAL   FORMAT "->>>,>>>,>>9" LABEL "Var Run Qty"
     FIELD dSetupHrs      AS DECIMAL   FORMAT ">>9.99" LABEL "Setup Hours"
     FIELD dSetupHrsVar   AS DECIMAL   FORMAT ">>9.99" LABEL "Var Setup Hours"
     FIELD dRunHrs        AS DECIMAL   FORMAT ">>9.99" LABEL "Run Hours"
     FIELD dRunHrsVar     AS DECIMAL   FORMAT ">>9.99" LABEL "Var Run Hours"
     FIELD dSpeed         AS DECIMAL   FORMAT ">>>>9" LABEL "Speed"
     FIELD dSpeedVar      AS DECIMAL   FORMAT ">>>>9" LABEL "Var Speed"
     FIELD dCost          AS DECIMAL   FORMAT "->,>>>,>>9.9999" LABEL "Cost" 
     FIELD dCostVar       AS DECIMAL   FORMAT "->,>>>,>>9.9999" LABEL "Var Cost" 
     FIELD dSetupWaste    AS DECIMAL   FORMAT ">>>>>>9.99" LABEL "Setup Waste"
     FIELD dSetupWasteVar AS DECIMAL   FORMAT ">>>>>>9.99" LABEL "Var Setup Waste"
     FIELD dRunWaste      AS DECIMAL   FORMAT ">>>>9" LABEL "Run Waste"
     FIELD dRunWasteVar   AS DECIMAL   FORMAT ">>>>9" LABEL "Var Run Waste"
     FIELD cDownTimeCode  AS CHARACTER FORMAT "x(5)" LABEL "DownTime Code"
     FIELD dDownTimeHrs   AS DECIMAL   FORMAT ">>>>9.99" LABEL "DownTime Hrs" .

define temp-table ttOperation no-undo
     FIELD cJobNo         AS CHARACTER FORMAT "x(8)" LABEL "Job Number"
     FIELD iJobNo2        AS INTEGER FORMAT ">>9" LABEL "Job Number2"
     FIELD cMachine       AS CHARACTER FORMAT "x(10)" LABEL "Machine"
     FIELD iFormNo         AS INTEGER   FORMAT ">>9" LABEL "Form"
     FIELD iBlankNo        AS INTEGER   FORMAT ">>9" LABEL "Blank"
     FIELD cDept          AS CHARACTER FORMAT "x(10)" LABEL "Department"
     FIELD cStdAct        AS CHARACTER FORMAT "x(10)" LABEL "Std/Act"
     FIELD dRunQty        AS DECIMAL   FORMAT "->>>,>>>,>>9" LABEL "Run Qty"
     FIELD dSetupHrs      AS DECIMAL   FORMAT ">>9.99" LABEL "Setup Hours"
     FIELD dRunHrs        AS DECIMAL   FORMAT ">>9.99" LABEL "Run Hours"
     FIELD dSpeed         AS DECIMAL   FORMAT ">>>>9" LABEL "Speed"
     FIELD dCost          AS DECIMAL   FORMAT "->,>>>,>>9.9999" LABEL "Cost" 
     FIELD dSetupWaste    AS DECIMAL   FORMAT ">>>>>>9.99" LABEL "Setup Waste"
     FIELD dRunWaste      AS DECIMAL   FORMAT ">>>>9" LABEL "Run Waste"
     FIELD cDownTimeCode  AS CHARACTER FORMAT "x(5)" LABEL "DownTime Code"
     FIELD dDownTimeHrs   AS DECIMAL   FORMAT ">>>>9.99" LABEL "DownTime Hrs" .

define temp-table ttMaterial no-undo
     FIELD cJobNo          AS CHARACTER FORMAT "x(8)" LABEL "Job Number"
     FIELD iJobNo2         AS INTEGER FORMAT ">>9" LABEL "Job Number2"
     FIELD iFormNo         AS INTEGER   FORMAT ">>9" LABEL "Form"
     FIELD iBlankNo        AS INTEGER   FORMAT ">>9" LABEL "Blank"
     FIELD cMaterial       AS CHARACTER  FORMAT "x(15)" LABEL "Material"
     FIELD dItemQty        AS DECIMAL    FORMAT "->>>,>>9.99<<" LABEL "Item Qty"  
     FIELD dItemQtyVar     AS DECIMAL    FORMAT "->>>,>>9.99<<" LABEL "Var Item Qty" 
     FIELD dItemCost       AS DECIMAL    FORMAT "->,>>>,>>>,>>9.99" LABEL "Item Cost"
     FIELD dItemCostVar    AS DECIMAL    FORMAT "->,>>>,>>>,>>9.99" LABEL "Var Item Cost".


define temp-table ttItem no-undo
     FIELD cJobNo         AS CHARACTER FORMAT "x(8)" LABEL "Job Number"
     FIELD iJobNo2        AS INTEGER FORMAT ">>9" LABEL "Job Number2"
     FIELD iFormNo         AS INTEGER   FORMAT ">>9" LABEL "Form"
     FIELD iBlankNo        AS INTEGER   FORMAT ">>9" LABEL "Blank"
     FIELD cFGItem         AS CHARACTER FORMAT "x(15)" LABEL "FG Item"
     FIELD cFGName         AS CHARACTER FORMAT "x(30)" LABEL "FG Item Name"
     FIELD cSellingPrice AS DECIMAL   FORMAT "->>,>>>,>>9.99<<<<" LABEL "Selling Price"
     FIELD dJobQty       AS DECIMAL   FORMAT "->>>,>>>,>>9" LABEL "Job Quantity"
     FIELD dProduced     AS DECIMAL   FORMAT "->>>,>>>,>>9" LABEL "Produced"
     FIELD dOnHand       AS DECIMAL   FORMAT "->>>,>>>,>>9" LABEL "On Hand".



/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
