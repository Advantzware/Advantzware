
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
DEFINE TEMP-TABLE ttJobReport NO-UNDO
    FIELD typeItem       AS CHARACTER FORMAT "x(10)" LABEL "Item Type"
    FIELD JobNo          AS CHARACTER FORMAT "x(8)" LABEL "Job Number"
    FIELD JobNo2         AS INTEGER FORMAT ">>9" LABEL "Job Number2"
    FIELD Job            AS CHARACTER FORMAT "x(12)" LABEL "Job Number" 
    FIELD custName       AS CHARACTER FORMAT "X(30)" LABEL "Cust Name"
    FIELD closeDate      AS DATE      FORMAT "99/99/9999" LABEL "Close Date"
    FIELD formNo         AS INTEGER   FORMAT ">>9" LABEL "Form"
    FIELD blankNo        AS INTEGER   FORMAT ">>9" LABEL "Blank"
    FIELD fgItem         AS CHARACTER FORMAT "x(15)" LABEL "FG Item"
    FIELD fgName         AS CHARACTER FORMAT "x(30)" LABEL "FG Item Name"
    FIELD fgSellingPrice AS DECIMAL   FORMAT "->>,>>>,>>9.99<<<<" LABEL "Selling Price"
    FIELD fgJobQty       AS DECIMAL   FORMAT "->>>,>>>,>>9" LABEL "Job Quantity"
    FIELD fgProduced     AS DECIMAL   FORMAT "->>>,>>>,>>9" LABEL "Produced"
    FIELD fgOnHand       AS DECIMAL   FORMAT "->>>,>>>,>>9" LABEL "On Hand"     
    FIELD mDept          AS CHARACTER FORMAT "x(10)" LABEL "Department"
    FIELD mMachine       AS CHARACTER FORMAT "x(10)" LABEL "Machine"
    FIELD mStdAct        AS CHARACTER FORMAT "x(10)" LABEL "Std/Act"
    FIELD mRunQty        AS DECIMAL   FORMAT "->>>,>>>,>>9" LABEL "Run Qty"
    FIELD mSetupHrs      AS DECIMAL   FORMAT ">>9.99" LABEL "Setup Hours"
    FIELD mRunHrs        AS DECIMAL   FORMAT ">>9.99" LABEL "Run Hours"
    FIELD mSpeed         AS DECIMAL   FORMAT ">>>>9" LABEL "Speed"
    FIELD mCost          AS DECIMAL   FORMAT "->,>>>,>>9.9999" LABEL "Cost" 
    FIELD mSetupWaste    AS DECIMAL   FORMAT ">>>>>>9.99" LABEL "Setup Waste"
    FIELD mRunWaste      AS DECIMAL   FORMAT ">>>>9" LABEL "Run Waste"
    FIELD mDownTimeCode  AS CHARACTER FORMAT "x(5)" LABEL "DownTime Code"
    FIELD mDownTimeHrs   AS DECIMAL   FORMAT ">>>>9.99" LABEL "DownTime Hrs"
    FIELD itemCode       AS CHARACTER  FORMAT "x(15)" LABEL "Material"
    FIELD itemQty        AS DECIMAL    FORMAT "->>>,>>9.99<<" LABEL "Item Qty"  
    FIELD itemCost       AS DECIMAL    FORMAT "->,>>>,>>>,>>9.99" LABEL "Item Cost"

    FIELD totStdMachineCost  AS DECIMAL FORMAT "->>,>>>,>>9.9<<<" LABEL "Total Standard Machine Cost"
    FIELD totActMachineCost  AS DECIMAL FORMAT "->>,>>>,>>9.9<<<" LABEL "Total Actual Machine Cost"
    FIELD totStdMaterialCost AS DECIMAL FORMAT "->>,>>>,>>9.9<<<" LABEL "Total Standard Material Cost"
    FIELD totActMaterialCost AS DECIMAL FORMAT "->>,>>>,>>9.9<<<" LABEL "Total Actual Material Cost"
    FIELD totStdCost         AS DECIMAL     FORMAT "->>,>>>,>>9.9<<<" LABEL "Total Standard Cost"
    FIELD totActCost         AS DECIMAL     FORMAT "->>,>>>,>>9.9<<<" LABEL "Total Actual Cost"
         
    .

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
