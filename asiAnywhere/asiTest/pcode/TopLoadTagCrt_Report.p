/*------------------------------------------------------------------------
    File        : TopLoadTagCrt_Report.p
    Purpose     : Load Tag Creation Report

    Syntax      :

    Description : Return a Dataset of all Load Tag Creation Report

    Author(s)   : 
    Created     : july 27 2009
    Notes       : 
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttLoadTagCrtRep NO-UNDO
    FIELD vFile AS CHAR
    FIELD vpsduds AS CHAR.
DEFINE DATASET dsLoadTagCrtRep FOR ttLoadTagCrtRep.

    DEFINE INPUT PARAMETER prmUser          AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmAction        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmOut           AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmJobOrdRec     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmPurItemRec    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmRePrntTag     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmScanLabel     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmOrders        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmJobs          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmFromOrder     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmToOrder       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmFromJob1      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmFromJob2      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmToJob1        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmToJob2        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmFromItem      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmToItem        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmAutoPrntLbl   AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmFreezeLbl     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmLblMatrix     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmPrntPoFrom    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmFromDate      AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER prmToDate        AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER prmJobStatus     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmSetComponent  AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmPrntPostRel   AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmShowLWD       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmPrintForm     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmReturns       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmIncOverRun    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmLabelPallet   AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmTxtFilePath   AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmPrntDeptNote  AS CHAR NO-UNDO.

  DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsLoadTagCrtRep.
  DEFINE OUTPUT PARAMETER cError AS CHAR NO-UNDO.


  IF prmUser          = ?  THEN ASSIGN prmUser          = "".
  IF prmAction        = ?  THEN ASSIGN prmAction        = "".
  IF prmOut           = ?  THEN ASSIGN prmOut           = "".
  IF prmJobOrdRec     = ?  THEN ASSIGN prmJobOrdRec     = "".
  IF prmPurItemRec    = ?  THEN ASSIGN prmPurItemRec    = "".
  IF prmRePrntTag     = ?  THEN ASSIGN prmRePrntTag     = "".
  IF prmScanLabel     = ?  THEN ASSIGN prmScanLabel     = "".
  IF prmOrders        = ?  THEN ASSIGN prmOrders        = "".
  IF prmJobs          = ?  THEN ASSIGN prmJobs          = "".
  IF prmFromOrder     = ?  THEN ASSIGN prmFromOrder     = "".
  IF prmToOrder       = ?  THEN ASSIGN prmToOrder       = "".
  IF prmFromJob1      = ?  THEN ASSIGN prmFromJob1      = "".
  IF prmFromJob2      = ?  THEN ASSIGN prmFromJob2      = "".
  IF prmToJob1        = ?  THEN ASSIGN prmToJob1        = "".
  IF prmToJob2        = ?  THEN ASSIGN prmToJob2        = "".
  IF prmFromItem      = ?  THEN ASSIGN prmFromItem      = "".
  IF prmToItem        = ?  THEN ASSIGN prmToItem        = "".
  IF prmAutoPrntLbl   = ?  THEN ASSIGN prmAutoPrntLbl   = "".
  IF prmFreezeLbl     = ?  THEN ASSIGN prmFreezeLbl     = "".
  IF prmLblMatrix     = ?  THEN ASSIGN prmLblMatrix     = "".
  IF prmPrntPoFrom    = ?  THEN ASSIGN prmPrntPoFrom    = "".
  IF prmJobStatus     = ?  THEN ASSIGN prmJobStatus     = "".
  IF prmSetComponent  = ?  THEN ASSIGN prmSetComponent  = "".
  IF prmPrntPostRel   = ?  THEN ASSIGN prmPrntPostRel   = "".
  IF prmShowLWD       = ?  THEN ASSIGN prmShowLWD       = "".
  IF prmPrintForm     = ?  THEN ASSIGN prmPrintForm     = "".
  IF prmReturns       = ?  THEN ASSIGN prmReturns       = "".
  IF prmIncOverRun    = ?  THEN ASSIGN prmIncOverRun    = "".
  IF prmLabelPallet   = ?  THEN ASSIGN prmLabelPallet   = "".
  IF prmTxtFilePath   = ?  THEN ASSIGN prmTxtFilePath   = "".
  IF prmPrntDeptNote  = ?  THEN ASSIGN prmPrntDeptNote  = "".

def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.
DEFINE VAR vPdfFile AS CHAR NO-UNDO.
DEF VAR vcDefaultForm AS CHAR NO-UNDO.
