def var itemlist as cha no-undo.

 FOR EACH emplogin NO-LOCK WHERE emplogin.company = "ABC"
                              AND emplogin.machine GT ''
                              AND emplogin.end_time = 0
                              AND emplogin.total_time = 0:
    FIND employee OF emplogin NO-LOCK.
    itemlist = IF itemlist = '' THEN employee.last_name + ', ' + 
                                     employee.first_name + ' (' +
                                     employee.employee + ')'
               ELSE itemlist + '@' + employee.last_name + ', ' + 
                                     employee.first_name + ' (' +
                                     employee.employee + ')'
                                     .
  END.
