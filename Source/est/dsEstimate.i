
/*------------------------------------------------------------------------
    File        : dsEstimate.i
    Purpose     : 

    Syntax      :

    Description : Dataset definition for estimate

    Author(s)   : BV
    Created     : Tue Jun 02 19:25:48 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{est\dsEstimateTT.i}

DEFINE DATASET dsEstimate FOR 
    ttEstimate,
    ttEstimateQuantity,
    ttEstimateItem,
    ttEstimateForm,
    ttEstimateBlank,
    ttEstimateOperation,
    ttEstimateMaterial
    DATA-RELATION estQuantity FOR ttEstimate, ttEstimateQuantity
        RELATION-FIELDS (estimateID, estimateID) NESTED
    DATA-RELATION estItem FOR ttEstimate, ttEstimateItem
        RELATION-FIELDS (estimateID, estimateID) NESTED
    DATA-RELATION estForm FOR ttEstimate, ttEstimateForm
        RELATION-FIELDS (estimateID, estimateID) NESTED
    DATA-RELATION estBlank FOR ttEstimateForm, ttEstimateBlank
        RELATION-FIELDS (estimateFormID, estimateFormID) NESTED
    DATA-RELATION estOperation FOR ttEstimateQuantity, ttEstimateOperation
        RELATION-FIELDS (estimateQuantityID, estimateQuantityID) NESTED
    DATA-RELATION estMaterial FOR ttEstimate, ttEstimateMaterial
        RELATION-FIELDS (estimateID, estimateID) NESTED
    . 

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
