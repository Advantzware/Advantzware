/* sharedVars.i - used in jobBrowse.w, resourceDetail.w & pending.w */

DEFINE {1} SHARED VARIABLE cellColumn AS WIDGET-HANDLE NO-UNDO EXTENT {&cellColumn}.

DEFINE TEMP-TABLE browseColumn NO-UNDO
  FIELD colOrder AS INTEGER FORMAT 'zz9' LABEL 'Order'
  FIELD colHidden AS LOGICAL FORMAT 'yes/' LABEL 'Hidden'
  FIELD colLabel AS CHARACTER FORMAT 'X(20)' LABEL 'Label'
  FIELD colName AS CHARACTER FORMAT 'X(20)' LABEL 'Name'
  FIELD rptLine AS INTEGER FORMAT '9' LABEL 'Line'
  FIELD rptCol AS INTEGER FORMAT '>>9' LABEL 'Col'
  FIELD excelCol AS INTEGER FORMAT '>>9' LABEL 'Excel'
  FIELD colLocked AS LOGICAL FORMAT 'yes/' LABEL 'Locked'
  FIELD colMove AS INTEGER
    INDEX browseColumn1 IS PRIMARY colOrder
    INDEX browseColumn2 colLabel colName.
