/*------------------------------------------------------------------------
  File: rd-fgexp.p
  Description: Business Logic
*/

/* ***************************  Definitions  ***************************/

/* Finished Goods Export.rpa */
{aoa/tempTable/ttFinishedGoodsExport.i}

{sys/ref/CustList.i NEW}

/* Parameters Definitions ---                                           */
DEFINE OUTPUT PARAMETER TABLE FOR ttFinishedGoodsExport.
{aoa/includes/pFinishedGoodsExport.i}
    /* set parameter values here if not running from parameter screen */

/* local variables */
    /* define local variable here */

/* subject business logic */
    /* code business logic here */

/* code procedures here */

{aoa/BL/pBuildCustList.i}



/**************** use "aoa/BL/template.p" as an example ***************/
