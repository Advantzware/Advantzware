WHEN {1} THEN 
    ASSIGN 
        ttCustomerItemByMonth.invTotal{1}    = ipcInvTotal
        ttCustomerItemByMonth.qtyInvoiced{1} = ipcQtyInvoiced
        ttCustomerItemByMonth.qtyShipped{1}  = ipcQtyShipped
        .
