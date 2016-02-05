/* conflictScop.i - used in checkJobConflict.i & moveExisting.i */

&GLOBAL-DEFINE addtnlWhere AND ROWID(~{1}) NE ipRowID
&GLOBAL-DEFINE completedHide AND (completedHide EQ NO OR ~{1}.jobCompleted EQ NO)
