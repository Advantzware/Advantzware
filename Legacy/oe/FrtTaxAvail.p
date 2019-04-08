DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO .
DEFINE INPUT PARAMETER ipcTaxGroup AS CHARACTER NO-UNDO .
DEFINE OUTPUT PARAMETER oplFrtTax AS LOGICAL NO-UNDO .
DEFINE VARIABLE i AS INTEGER NO-UNDO .

 FIND FIRST stax NO-LOCK
        WHERE stax.company   EQ ipcCompany
        AND stax.tax-group EQ ipcTaxGroup 
        NO-ERROR .
    IF AVAIL stax THEN
    do i = 1 to EXTENT(stax.tax-code1):
        if stax.tax-code1[i] eq ipcTaxGroup then do:
            if stax.tax-frt1[i] then
                oplFrtTax = TRUE .
        end.
    end. 
