
/*------------------------------------------------------------------------
    File        : EstimateSpecificationTester.p
    Purpose     : 

    Syntax      :

    Description : Tests the Public Methods and Properties of the EstimateSpecification Class			

    Author(s)   : BV
    Created     : Mon Sep 28 21:54:49 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
USING est.EstimateSpecification.

DEFINE VARIABLE iFormCount AS INTEGER NO-UNDO.
DEFINE VARIABLE iBlankCount AS INTEGER NO-UNDO.
DEFINE VARIABLE oEstimateSpecification AS est.EstimateSpecification NO-UNDO. 

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

oEstimateSpecification = NEW est.EstimateSpecification().
oEstimateSpecification:InitializeEstimate(INPUT "001", "    3093").
MESSAGE oEstimateSpecification:FormCount SKIP 
    oEstimateSpecification:BlankCount
VIEW-AS ALERT-BOX.