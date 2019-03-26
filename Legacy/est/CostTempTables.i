
/*------------------------------------------------------------------------
    File        : CostTempTables.i
    Purpose     : 

    Syntax      :

    Description : Definitions for Cost Temp Tables

    Author(s)   : BV
    Created     : Wed Feb 07 08:12:55 EST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE {1} TEMP-TABLE ttCostMaster  /*Store estimate level overrides or options*/
    FIELD cCompany        AS CHARACTER 
    FIELD cEstimateNo     LIKE est.est-no
    FIELD dMasterQuantity AS DECIMAL  
    FIELD dMarginPct AS DECIMAL 
    FIELD cMarginOn AS CHARACTER 
    FIELD dWarehouseMarkupPct AS DECIMAL 
    FIELD dHandlingChargePct AS DECIMAL 
    FIELD dHandlingRatePerCWTRMPct AS DECIMAL 
    FIELD dSpecial1MarkupPct AS DECIMAL 
    FIELD dSpecial2MarkupPct AS DECIMAL 
    FIELD dSpecial3MarkupPct AS DECIMAL 
    FIELD lShowCommissoins AS LOGICAL
    FIELD lShowLaborRates AS LOGICAL
    FIELD lAddToFactCostSpecial1 AS LOGICAL
    FIELD lAddToFactCostSpecial2 AS LOGICAL 
    FIELD lAddToFactCostSpecial3 AS LOGICAL 
    FIELD lAddToFactCostFreight AS LOGICAL 
    FIELD lAddToFactCostGSA AS LOGICAL 
    FIELD lAddToFactCostRoyalty AS LOGICAL 
    FIELD lAddToFactCostComm AS LOGICAL 
    FIELD dFoldPct AS DECIMAL
    FIELD dHandlingRatePerCWTFGPct AS DECIMAL 
    FIELD dHandlingRatePerCWTRMFarmPct AS DECIMAL 
    FIELD dHandlingRatePerCWTFGFarmPct AS DECIMAL 
    FIELD dHandlingChargeFarmPct AS DECIMAL
    .
    
DEFINE {1} TEMP-TABLE ttCostHeader
    LIKE costHeader.
/*    FIELD company AS CHARACTER                                       */
/*        FORMAT "x(3)"                                                */
/*        INITIAL ""                                                   */
/*        LABEL "Company"                                              */
/*        COLUMN-LABEL "Company"                                       */
/*        HELP "Enter the company code."                               */
/*    FIELD rec_key AS CHARACTER                                       */
/*        FORMAT "X(20)"                                               */
/*        INITIAL ""                                                   */
/*        LABEL "Rec Key"                                              */
/*        COLUMN-LABEL "Rec Key"                                       */
/*        HELP "Enter record key"                                      */
/*    FIELD rec_keyParentForm AS CHARACTER                             */
/*        FORMAT "X(20)"                                               */
/*        INITIAL ""                                                   */
/*        LABEL "Rec Key Form"                                         */
/*        COLUMN-LABEL "Rec Key Form"                                  */
/*        HELP "Enter form record key"                                 */
/*    FIELD rec_keyParentSet AS CHARACTER                              */
/*        FORMAT "X(20)"                                               */
/*        INITIAL ""                                                   */
/*        LABEL "Rec Key Set"                                          */
/*        COLUMN-LABEL "Rec Key Set"                                   */
/*        HELP "Enter set record key"                                  */
/*    FIELD isItem AS LOGICAL                                          */
/*        FORMAT "yes/no"                                              */
/*        INITIAL "no"                                                 */
/*        LABEL "Is FG Item"                                           */
/*        COLUMN-LABEL "Is FG Item"                                    */
/*        HELP "Select Yes if this is FG Item"                         */
/*    FIELD estimateNo AS CHARACTER                                    */
/*        FORMAT "x(8)"                                                */
/*        INITIAL ""                                                   */
/*        LABEL "Estimate #"                                           */
/*        COLUMN-LABEL "Estimate #"                                    */
/*        HELP "Enter estimate number."                                */
/*    FIELD industry AS INTEGER                                        */
/*        FORMAT "9"                                                   */
/*        INITIAL "0"                                                  */
/*        LABEL "Industry"                                             */
/*        COLUMN-LABEL "Industry"                                      */
/*        HELP "Enter whether header is used for 1=Fold 2=Corr 3=Foam" */
/*    FIELD estimateType AS INTEGER                                    */
/*        FORMAT "9"                                                   */
/*        INITIAL "0"                                                  */
/*        LABEL "Estimate Type"                                        */
/*        COLUMN-LABEL "Estimate Type"                                 */
/*        HELP "Enter estimate type (1-Single 2-2pc. 3-Tandem 4-Combo)"*/
/*    FIELD quantityMaster AS INTEGER                                  */
/*        FORMAT ">>>>>>>9"                                            */
/*        INITIAL "0"                                                  */
/*        LABEL "Quantity"                                             */
/*        COLUMN-LABEL "Quantity"                                      */
/*        HELP "Enter master quantity for cost calculation"            */
/*    FIELD quantityPerSet AS DECIMAL                                  */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "Qty Per Set"                                          */
/*        COLUMN-LABEL "Qty Per Set"                                   */
/*        DECIMALS 6                                                   */
/*    FIELD quantityRequest AS DECIMAL                                 */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "Request Qty"                                          */
/*        COLUMN-LABEL "Req Qty"                                       */
/*        DECIMALS 6                                                   */
/*    FIELD quantityYield AS DECIMAL                                   */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "Yield Qty"                                            */
/*        COLUMN-LABEL "Yld Qty"                                       */
/*        DECIMALS 6                                                   */
/*    FIELD headerDescription AS CHARACTER                             */
/*        FORMAT "x(30)"                                               */
/*        INITIAL ""                                                   */
/*        LABEL "Header Description"                                   */
/*        COLUMN-LABEL "Header Desc"                                   */
/*        HELP "Enter header description"                              */
/*    FIELD formNo AS INTEGER                                          */
/*        FORMAT ">9"                                                  */
/*        INITIAL "1"                                                  */
/*        LABEL "Form"                                                 */
/*        COLUMN-LABEL "Form"                                          */
/*        HELP "Enter form/sheet number"                               */
/*    FIELD blankNo AS INTEGER                                         */
/*        FORMAT ">9"                                                  */
/*        INITIAL "1"                                                  */
/*        LABEL "Blank"                                                */
/*        COLUMN-LABEL "Blank"                                         */
/*        HELP "Enter blank number"                                    */
/*    FIELD jobNo AS CHARACTER                                         */
/*        FORMAT "x(8)"                                                */
/*        INITIAL ""                                                   */
/*        LABEL "Job #"                                                */
/*        COLUMN-LABEL "Job #"                                         */
/*        HELP "Enter job number"                                      */
/*    FIELD jobNo2 AS INTEGER                                          */
/*        FORMAT ">9"                                                  */
/*        INITIAL "0"                                                  */
/*        LABEL "Job Run #"                                            */
/*        COLUMN-LABEL "Job Run #"                                     */
/*        HELP "Enter job sub-number"                                  */
/*    FIELD calculationTime AS DATETIME                                */
/*        FORMAT "99/99/9999 HH:MM:SS.SSS"                             */
/*        INITIAL ?                                                    */
/*        LABEL "Calculation Date-time"                                */
/*        COLUMN-LABEL "Calculation Date-time"                         */
/*        HELP "Calculation date-time"                                 */
/*    FIELD calculatedBy AS CHARACTER                                  */
/*        FORMAT "x(8)"                                                */
/*        INITIAL ""                                                   */
/*        LABEL "Calculated By"                                        */
/*        COLUMN-LABEL "Calculated By"                                 */
/*        HELP "User ID the started calculation"                       */
/*    FIELD customerPartID AS CHARACTER                                */
/*        FORMAT "x(15)"                                               */
/*        INITIAL ""                                                   */
/*        LABEL "Customer Part #"                                      */
/*        COLUMN-LABEL "Customer Part #"                               */
/*        HELP "Enter customer's part number for this item"            */
/*    FIELD fgItemID AS CHARACTER                                      */
/*        FORMAT "x(15)"                                               */
/*        INITIAL ""                                                   */
/*        LABEL "FG Item #"                                            */
/*        COLUMN-LABEL "FG Item #"                                     */
/*        HELP "Enter finished good item number"                       */
/*    FIELD stdCostBoard AS DECIMAL                                    */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "Board Cost - Standard"                                */
/*        COLUMN-LABEL "Board-Std"                                     */
/*        DECIMALS 6                                                   */
/*    FIELD stdCostPrepLabor AS DECIMAL                                */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "Prep Labor Cost - Standard"                           */
/*        COLUMN-LABEL "Prep Lab-Std"                                  */
/*        DECIMALS 6                                                   */
/*    FIELD stdCostPrepMaterial AS DECIMAL                             */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "Prep Material Cost - Standard"                        */
/*        COLUMN-LABEL "Prep Mat-Std"                                  */
/*        DECIMALS 6                                                   */
/*    FIELD stdCostMiscLabor AS DECIMAL                                */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "Misc Labor Cost - Standard"                           */
/*        COLUMN-LABEL "Misc Lab-Std"                                  */
/*        DECIMALS 6                                                   */
/*    FIELD stdCostMiscMaterial AS DECIMAL                             */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "Misc Material Cost - Standard"                        */
/*        COLUMN-LABEL "Misc Mat-Std"                                  */
/*        DECIMALS 6                                                   */
/*    FIELD stdCostDirectMaterial AS DECIMAL                           */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "Direct Material Cost - Standard"                      */
/*        COLUMN-LABEL "Dir Mat-Std"                                   */
/*        DECIMALS 6                                                   */
/*    FIELD stdCostDirectLabor AS DECIMAL                              */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "Direct Labor Cost - Standard"                         */
/*        COLUMN-LABEL "Dir Lab-Std"                                   */
/*        DECIMALS 6                                                   */
/*    FIELD stdCostVariableOverhead AS DECIMAL                         */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "Variable Overhead Cost - Standard"                    */
/*        COLUMN-LABEL "Var OH-Std"                                    */
/*        DECIMALS 6                                                   */
/*    FIELD stdCostDirectFactory AS DECIMAL                            */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "Direct Factory Cost - Standard"                       */
/*        COLUMN-LABEL "Dir Fact-Std"                                  */
/*        DECIMALS 6                                                   */
/*    FIELD stdCostFixedOverhead AS DECIMAL                            */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "Fixed Overhead Cost - Standard"                       */
/*        COLUMN-LABEL "Fix OH-Std"                                    */
/*        DECIMALS 6                                                   */
/*    FIELD stdCostTotalFactory AS DECIMAL                             */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "Total Factory Cost - Standard"                        */
/*        COLUMN-LABEL "Tot Fact-Std"                                  */
/*        DECIMALS 6                                                   */
/*    FIELD stdCostFreight AS DECIMAL                                  */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "Freight Cost - Standard"                              */
/*        COLUMN-LABEL "Freight-Std"                                   */
/*        DECIMALS 6                                                   */
/*    FIELD stdCostGSABoard AS DECIMAL                                 */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "GS&A Board Cost - Standard"                           */
/*        COLUMN-LABEL "GSA Board-Std"                                 */
/*        DECIMALS 6                                                   */
/*    FIELD stdCostGSALabor AS DECIMAL                                 */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "GS&A Labor Cost - Standard"                           */
/*        COLUMN-LABEL "GSA Lab-Std"                                   */
/*        DECIMALS 6                                                   */
/*    FIELD stdCostGSAMaterial AS DECIMAL                              */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "GS&A Material Cost - Standard"                        */
/*        COLUMN-LABEL "GSA Mat-Std"                                   */
/*        DECIMALS 6                                                   */
/*    FIELD stdCostTotalGSA AS DECIMAL                                 */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "Total GS&A Cost - Standard"                           */
/*        COLUMN-LABEL "Total GSA-Std"                                 */
/*        DECIMALS 6                                                   */
/*    FIELD stdCostRoyalty AS DECIMAL                                  */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "Royalty Cost - Standard"                              */
/*        COLUMN-LABEL "Royalty-Std"                                   */
/*        DECIMALS 6                                                   */
/*    FIELD stdCostSpecial1 AS DECIMAL                                 */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "Special Cost 1 - Standard"                            */
/*        COLUMN-LABEL "Spec1-Std"                                     */
/*        DECIMALS 6                                                   */
/*    FIELD stdCostSpecial2 AS DECIMAL                                 */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "Special Cost 2 - Standard"                            */
/*        COLUMN-LABEL "Spec2-Std"                                     */
/*        DECIMALS 6                                                   */
/*    FIELD stdCostSpecial3 AS DECIMAL                                 */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "Special Cost 3 - Standard"                            */
/*        COLUMN-LABEL "Spec3-Std"                                     */
/*        DECIMALS 6                                                   */
/*    FIELD stdCostBrokerComm AS DECIMAL                               */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "Broker Comm Cost - Standard"                          */
/*        COLUMN-LABEL "Broker Comm-Std"                               */
/*        DECIMALS 6                                                   */
/*    FIELD stdCostWarehousing AS DECIMAL                              */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "Warehousing Cost - Standard"                          */
/*        COLUMN-LABEL "Warehousing-Std"                               */
/*        DECIMALS 6                                                   */
/*    FIELD stdCostCommission AS DECIMAL                               */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "Commission Cost - Standard"                           */
/*        COLUMN-LABEL "Comm-Std"                                      */
/*        DECIMALS 6                                                   */
/*    FIELD stdCostTotalOther AS DECIMAL                               */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "Total Other Cost - Standard"                          */
/*        COLUMN-LABEL "Tot Oth-Std"                                   */
/*        DECIMALS 6                                                   */
/*    FIELD stdCostFull AS DECIMAL                                     */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "Full Cost - Standard"                                 */
/*        COLUMN-LABEL "Full-Std"                                      */
/*        DECIMALS 6                                                   */
/*    FIELD stdProfitGross AS DECIMAL                                  */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "Gross Profit - Standard"                              */
/*        COLUMN-LABEL "Gross Profit-Std"                              */
/*        DECIMALS 6                                                   */
/*    FIELD stdProfitNet AS DECIMAL                                    */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "Net Profit - Standard"                                */
/*        COLUMN-LABEL "Net Profit-Std"                                */
/*        DECIMALS 6                                                   */
/*    FIELD stdSellPrice AS DECIMAL                                    */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "Sell Price - Standard"                                */
/*        COLUMN-LABEL "Sell Price-Std"                                */
/*        DECIMALS 6                                                   */
/*    FIELD actCostBoard AS DECIMAL                                    */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "Board Cost - Actual"                                  */
/*        COLUMN-LABEL "Board-Act"                                     */
/*        DECIMALS 6                                                   */
/*    FIELD actCostPrepLabor AS DECIMAL                                */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "Prep Labor Cost - Actual"                             */
/*        COLUMN-LABEL "Prep Lab-Act"                                  */
/*        DECIMALS 6                                                   */
/*    FIELD actCostPrepMaterial AS DECIMAL                             */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "Prep Material Cost - Actual"                          */
/*        COLUMN-LABEL "Prep Mat-Act"                                  */
/*        DECIMALS 6                                                   */
/*    FIELD actCostMiscLabor AS DECIMAL                                */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "Misc Labor Cost - Actual"                             */
/*        COLUMN-LABEL "Misc Lab-Act"                                  */
/*        DECIMALS 6                                                   */
/*    FIELD actCostMiscMaterial AS DECIMAL                             */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "Misc Material Cost - Actual"                          */
/*        COLUMN-LABEL "Misc Mat-Act"                                  */
/*        DECIMALS 6                                                   */
/*    FIELD actCostDirectMaterial AS DECIMAL                           */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "Direct Material Cost - Actual"                        */
/*        COLUMN-LABEL "Dir Mat-Act"                                   */
/*        DECIMALS 6                                                   */
/*    FIELD actCostDirectLabor AS DECIMAL                              */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "Direct Labor Cost - Actual"                           */
/*        COLUMN-LABEL "Dir Lab-Act"                                   */
/*        DECIMALS 6                                                   */
/*    FIELD actCostVariableOverhead AS DECIMAL                         */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "Variable Overhead Cost - Actual"                      */
/*        COLUMN-LABEL "Var OH-Act"                                    */
/*        DECIMALS 6                                                   */
/*    FIELD actCostFixedOverhead AS DECIMAL                            */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "Fixed Overhead Cost - Actual"                         */
/*        COLUMN-LABEL "Fix OH-Act"                                    */
/*        DECIMALS 6                                                   */
/*    FIELD actCostDirectFactory AS DECIMAL                            */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "Direct Factory Cost - Actual"                         */
/*        COLUMN-LABEL "Dir Fact-Act"                                  */
/*        DECIMALS 6                                                   */
/*    FIELD actCostTotalFactory AS DECIMAL                             */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "Total Factory Cost - Actual"                          */
/*        COLUMN-LABEL "Tot Fact-Act"                                  */
/*        DECIMALS 6                                                   */
/*    FIELD actCostWarehousing AS DECIMAL                              */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "Warehousing Cost - Actual"                            */
/*        COLUMN-LABEL "Warehousing-Act"                               */
/*        DECIMALS 6                                                   */
/*    FIELD actCostFreight AS DECIMAL                                  */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "Freight Cost - Actual"                                */
/*        COLUMN-LABEL "Freight-Act"                                   */
/*        DECIMALS 6                                                   */
/*    FIELD actCostRoyalty AS DECIMAL                                  */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "Royalty Cost - Actual"                                */
/*        COLUMN-LABEL "Royalty-Act"                                   */
/*        DECIMALS 6                                                   */
/*    FIELD actCostGSABoard AS DECIMAL                                 */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "GS&A Board Cost - Actual"                             */
/*        COLUMN-LABEL "GSA Board-Act"                                 */
/*        DECIMALS 6                                                   */
/*    FIELD actCostGSALabor AS DECIMAL                                 */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "GS&A Labor Cost - Actual"                             */
/*        COLUMN-LABEL "GSA Lab-Act"                                   */
/*        DECIMALS 6                                                   */
/*    FIELD actCostGSAMaterial AS DECIMAL                              */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "GS&A Material Cost - Actual"                          */
/*        COLUMN-LABEL "GSA Mat-Act"                                   */
/*        DECIMALS 6                                                   */
/*    FIELD actCostTotalGSA AS DECIMAL                                 */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "Total GS&A Cost - Actual"                             */
/*        COLUMN-LABEL "Total GSA-Act"                                 */
/*        DECIMALS 6                                                   */
/*    FIELD actCostSpecial1 AS DECIMAL                                 */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "Special Cost 1 - Actual"                              */
/*        COLUMN-LABEL "Spec1-Act"                                     */
/*        DECIMALS 6                                                   */
/*    FIELD actCostSpecial2 AS DECIMAL                                 */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        COLUMN-LABEL "Spec2-Act"                                     */
/*        DECIMALS 6                                                   */
/*    FIELD actCostSpecial3 AS DECIMAL                                 */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "Special Cost 3 - Actual"                              */
/*        COLUMN-LABEL "Spec3-Act"                                     */
/*        DECIMALS 6                                                   */
/*    FIELD actCostBrokerComm AS DECIMAL                               */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "Broker Comm Cost - Actual"                            */
/*        COLUMN-LABEL "Broker Comm-Act"                               */
/*        DECIMALS 6                                                   */
/*    FIELD actCostCommission AS DECIMAL                               */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "Commission Cost - Actual"                             */
/*        COLUMN-LABEL "Comm-Act"                                      */
/*        DECIMALS 6                                                   */
/*    FIELD actCostTotalOther AS DECIMAL                               */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "Total Other Cost - Actual"                            */
/*        COLUMN-LABEL "Tot Oth-Act"                                   */
/*        DECIMALS 6                                                   */
/*    FIELD actCostFull AS DECIMAL                                     */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "Full Cost - Actual"                                   */
/*        COLUMN-LABEL "Full-Act"                                      */
/*        DECIMALS 6                                                   */
/*    FIELD actProfitGross AS DECIMAL                                  */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "Gross Profit - Actual"                                */
/*        COLUMN-LABEL "Gross Profit-Act"                              */
/*        DECIMALS 6                                                   */
/*    FIELD actProfitNet AS DECIMAL                                    */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "Net Profit - Actual"                                  */
/*        COLUMN-LABEL "Net Profit-Act"                                */
/*        DECIMALS 6                                                   */
/*    FIELD actSellPrice AS DECIMAL                                    */
/*        FORMAT "->,>>>,>>9.99"                                       */
/*        INITIAL "0"                                                  */
/*        LABEL "Sell Price - Actual"                                  */
/*        COLUMN-LABEL "Sell Price-Act"                                */
/*        DECIMALS 6                                                   */
/*    FIELD lengthGross AS DECIMAL                                     */
/*        FORMAT ">>9.9999"                                            */
/*        INITIAL "0"                                                  */
/*        LABEL "Gross Sheet Length"                                   */
/*        COLUMN-LABEL "Gross Sht L"                                   */
/*        HELP "Length of gross sheet"                                 */
/*        DECIMALS 6                                                   */
/*    FIELD widthGross AS DECIMAL                                      */
/*        FORMAT ">>9.9999"                                            */
/*        INITIAL "0"                                                  */
/*        COLUMN-LABEL "Gross Sht W"                                   */
/*        HELP "Width of gross sheet"                                  */
/*        DECIMALS 6                                                   */
/*    FIELD depthGross AS DECIMAL                                      */
/*        FORMAT ">>9.9999"                                            */
/*        INITIAL "0"                                                  */
/*        LABEL "Gross Sheet Depth"                                    */
/*        COLUMN-LABEL "Gross Sht D"                                   */
/*        HELP "Depth of gross sheet"                                  */
/*        DECIMALS 6                                                   */
/*    FIELD depthNet AS DECIMAL                                        */
/*        FORMAT ">>9.9999"                                            */
/*        INITIAL "0"                                                  */
/*        LABEL "Net Sheet Depth"                                      */
/*        COLUMN-LABEL "Net Sht D"                                     */
/*        HELP "Depth of net sheet"                                    */
/*        DECIMALS 6                                                   */
/*    FIELD lengthNet AS DECIMAL                                       */
/*        FORMAT ">>9.9999"                                            */
/*        INITIAL "0"                                                  */
/*        LABEL "Net Sheet Length"                                     */
/*        COLUMN-LABEL "Net Sht L"                                     */
/*        HELP "Length of net sheet"                                   */
/*        DECIMALS 6                                                   */
/*    FIELD widthNet AS DECIMAL                                        */
/*        FORMAT ">>9.9999"                                            */
/*        INITIAL "0"                                                  */
/*        LABEL "Net Sheet Width"                                      */
/*        COLUMN-LABEL "Net Sht W"                                     */
/*        HELP "Width of net sheet"                                    */
/*        DECIMALS 6                                                   */
/*    FIELD lengthBlank AS DECIMAL                                     */
/*        FORMAT ">>9.9999"                                            */
/*        INITIAL "0"                                                  */
/*        LABEL "Blank Length"                                         */
/*        COLUMN-LABEL "Blank L"                                       */
/*        HELP "Length of blank"                                       */
/*        DECIMALS 6                                                   */
/*    FIELD widthBlank AS DECIMAL                                      */
/*        FORMAT ">>9.9999"                                            */
/*        INITIAL "0"                                                  */
/*        LABEL "Blank Width"                                          */
/*        COLUMN-LABEL "Blank W"                                       */
/*        HELP "Width of blank"                                        */
/*        DECIMALS 6                                                   */
/*    FIELD blankSquareFeet AS DECIMAL                                 */
/*        FORMAT ">>>9.999<<"                                          */
/*        INITIAL "0"                                                  */
/*        LABEL "Blank Square Feet"                                    */
/*        COLUMN-LABEL "Blank Sq Ft"                                   */
/*        HELP "Blank square feet"                                     */
/*        DECIMALS 6                                                   */
/*    FIELD factorForm AS DECIMAL                                      */
/*        FORMAT "->>,>>9.99"                                          */
/*        INITIAL "0"                                                  */
/*        LABEL "Factor - Form"                                        */
/*        COLUMN-LABEL "Factor Form"                                   */
/*        HELP "Factor for blank area on combo form"                   */
/*        DECIMALS 6                                                   */
/*    FIELD factorSet AS DECIMAL                                       */
/*        FORMAT "->>,>>9.99"                                          */
/*        INITIAL "0"                                                  */
/*        LABEL "Factor - Set"                                         */
/*        COLUMN-LABEL "Factor - Set"                                  */
/*        HELP "Factor for item within set"                            */
/*        DECIMALS 6                                                   */
/*.                                                                    */


DEFINE {1} TEMP-TABLE ttCostDetail
    FIELD company               AS CHARACTER 
    FIELD rec_key               LIKE probe.rec_key
    FIELD rec_keyHeader         LIKE probe.rec_key
    FIELD rec_KeyActivity       LIKE probe.rec_key
    FIELD rec_keyModel          LIKE probe.rec_key
    FIELD modelType             AS CHARACTER 
    FIELD stdCost               AS DECIMAL 
    FIELD stdCostPerUOM         AS DECIMAL 
    FIELD stdCostUOM            AS CHARACTER 
    FIELD stdQuantityRequired   AS DECIMAL 
    FIELD stdQuantityUOM        AS CHARACTER 
    FIELD actCost               AS DECIMAL 
    FIELD actCostPerUOM         AS DECIMAL 
    FIELD actCostUOM            AS CHARACTER 
    FIELD actQuantityRequired   AS DECIMAL 
    FIELD actQuantityUOM        AS CHARACTER 
    FIELD costDetailDescription AS CHARACTER 
    .

DEFINE {1} TEMP-TABLE ttCostDetailMarkup
    FIELD company              AS CHARACTER 
    FIELD rec_key              LIKE probe.rec_key
    FIELD rec_keyHeader        LIKE probe.rec_key
    FIELD stdCostDescription   AS CHARACTER /*Commission, Margin, GS&A Board, [Special 1 Custom Description], etc.*/
    FIELD stdCostVariable      AS DECIMAL 
    FIELD stdCostFixed         AS DECIMAL 
    FIELD markupType           AS CHARACTER /*COM,GSAB,GSAM,GSAL,S1,S2,S3,ROY,BC,FRT,MAR*/ 
    FIELD stdCost              AS DECIMAL 
    FIELD markupPercent        AS DECIMAL 
    FIELD markupPercentOf      AS CHARACTER 
    FIELD markupFlatAmount     AS DECIMAL
    FIELD markupIsMargin       AS LOGICAL /*% of Sell Price vs. Cost Markup*/ 
    FIELD lookupValue          AS DECIMAL /* eg. Factory cost for Margin lookup*/
    FIELD lookupValueType      AS CHARACTER   /*eg "Factory Cost or Board Cost" for Margin Lookup*/
    FIELD lookupResult         AS DECIMAL  /*to determine overall markup in case there is Alt*/
    FIELD lookupAltValue       AS DECIMAL /*eg. value for Board % of Total Cost for Margin Reduction*/
    FIELD lookupAltValueType   AS CHARACTER /*eg "Board % of Total Cost" for Margin Reduction %*/
    FIELD lookupAltResult      AS DECIMAL /*to determine overall markup %*/
    FIELD includeInFactoryCost AS LOGICAL 
    . 
DEFINE {1} TEMP-TABLE ttCostDetailMisc
    FIELD company                        AS CHARACTER 
    FIELD rec_key                        LIKE probe.rec_key
    FIELD rec_keyHeader                  LIKE probe.rec_key
    FIELD stdCostDescription             AS CHARACTER
    FIELD stdCostVariable                AS DECIMAL
    FIELD stdCostFixed                   AS DECIMAL 
    FIELD materialID                     LIKE item.i-no
    FIELD materialType                   LIKE item.mat-type
    FIELD prepID                         LIKE prep.code
    FIELD vendorID                       LIKE vend.vend-no
    FIELD quantity                       AS DECIMAL 
    FIELD quantityUOM                    AS CHARACTER 
    FIELD stdCostBasePerUOM              AS DECIMAL 
    FIELD stdCostBaseUOM                 AS CHARACTER
    FIELD stdCostSetup                   AS DECIMAL 
    FIELD simonCode                      AS CHARACTER
    FIELD markupPercent                  AS DECIMAL 
    FIELD markupIsMargin                 AS LOGICAL
    FIELD sellPrice                      AS DECIMAL 
    FIELD profit                         AS DECIMAL  
    FIELD excludeSellPriceFromCommission AS LOGICAL 
    FIELD excludeChargeFromMarginLookup  AS LOGICAL   
    FIELD excludeProfitFromCost          AS LOGICAL 
    .
    
DEFINE {1} TEMP-TABLE ttCostDetailOperations
    FIELD company       AS CHARACTER
    FIELD rec_key       LIKE probe.rec_key
    FIELD rec_keyHeader LIKE probe.rec_key
    .
    
DEFINE {1} TEMP-TABLE ttCostActivity
    FIELD company                AS CHARACTER 
    FIELD rec_key                LIKE probe.rec_key
    FIELD activityID             AS INTEGER 
    FIELD activityDescription    AS CHARACTER 
    FIELD profitLossCategory     AS CHARACTER  //COGS - Material, COGS - Labor, Prep Labor (e.g. the estimate summary bins)
    FIELD produtionCategory      AS CHARACTER //Setup,Run, Cleanup, Downtime etc.
    FIELD costSource             AS CHARACTER //Vendor,Production,Admin,Carrier, etc.
    FIELD calculationModel       AS CHARACTER //Board,Misc,Ink,Glue,Production,etc.
    FIELD calculationDescription AS CHARACTER //Description of the calculation formula/process for this cost center
    .
    
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
