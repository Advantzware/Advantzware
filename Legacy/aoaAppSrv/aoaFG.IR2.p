/* aoaFG.IR2.p */

/* ***************************  Definitions  ************************** */

/* Inventory Value.rpa */
DEFINE TEMP-TABLE ttInventoryValue NO-UNDO
    {aoaAppSrv/ttFields.i}
    FIELD xxSort AS CHARACTER LABEL "Sort" FORMAT "x(500)"
        INDEX sortBy IS PRIMARY rowType xxSort
        .
{sys/ref/CustList.i NEW}
/* Inventory Value.rpa */

/* ***************************  Main Block  *************************** */
RUN pInventoryValue ("001",0,"asi").
FOR EACH ttInventoryValue:
    DISPLAY
        ttInventoryValue.rowType    FORMAT "x(10)"
        ttInventoryValue.parameters FORMAT "x(10)"
        ttInventoryValue.xxSort     FORMAT "x(10)"
        .
END.

PROCEDURE pInventoryValue:
/*------------------------------------------------------------------------------
  Purpose:     Inventory Value.rpa
  Parameters:  Company, Batch Seq, User ID
  Notes:       
------------------------------------------------------------------------------*/
    {aoaAppSrv/pInventoryValue.i}

    /* local variables */

    /* subject business logic */

END PROCEDURE.
