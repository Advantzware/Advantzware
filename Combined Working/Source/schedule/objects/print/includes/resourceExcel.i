/* resourceExcel.i */

  IF ipExcel THEN DO:
    CREATE wrkExcel.
    ASSIGN
      excelCol# = 1
      wrkExcel.excelCol = excelCol#
      wrkExcel.excelLabel = 'Resource'
      wrkExcel.excelField = ttblJob.resource.
    CREATE wrkExcel.
    ASSIGN
      excelCol# = 2
      wrkExcel.excelCol = excelCol#
      wrkExcel.excelLabel = 'Description'
      wrkExcel.excelField = ttblJob.resourceDescription.
  END. /* if ipexcel */
