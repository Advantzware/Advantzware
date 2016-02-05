/* reset employee id for Fibre
  10/12/01  YSK */
  
  
def var ii as int no-undo.
def var ls-zero as cha no-undo.

/* dump records 
    output to c:\tmp\employee.dat.
    for each employee : 
        export recid(employee) employee.employee.
    end.
    output to c:\tmp\emplogin.dat.
    for each emplogin : 
        export recid(emplogin) emplogin.employee.
    end.
    output to c:\tmp\empmach.dat.
    for each empmach : 
        export recid(empmach) empmach.employee.
    end.
    output to c:\tmp\machemp.dat.
    for each machemp : 
        export recid(machemp) machemp.employee.
    end.
    output to c:\tmp\rate.dat.
    for each rate : 
        export recid(rate) rate.employee.
    end.


*/
for each employee where employee.company = "001" and length(employee.employee) < 4 :
    ii = int(employee.employee) no-error.
    if error-status:error then next.
       
    for each emplogin of employee :
        ls-zero = if length(employee.employee) = 3 then "0"
                  else if length(employee.employee) = 2 then "00"
                  else "000".
        emplogin.employee = substring(ls-zero,1,4 - length(employee.employee) ) + emplogin.employee.
             
    end.
    for each empmach of employee :
        ls-zero = if length(employee.employee) = 3 then "0"
                  else if length(employee.employee) = 2 then "00"
                  else "000".
        empmach.employee = substring(ls-zero,1,4 - length(employee.employee) ) + employee.employee.
             
    end.
    for each machemp where machemp.employee = employee.employee :
        ls-zero = if length(employee.employee) = 3 then "0"
                  else if length(employee.employee) = 2 then "00"
                  else "000".
        machemp.employee = substring(ls-zero,1,4 - length(employee.employee) ) + employee.employee.
             
    end.
    for each rate of employee :
        ls-zero = if length(employee.employee) = 3 then "0"
                  else if length(employee.employee) = 2 then "00"
                  else "000".
        rate.employee = substring(ls-zero,1,4 - length(employee.employee) ) + employee.employee.
             
    end.
 
    ls-zero = if length(employee.employee) = 3 then "0"
              else if length(employee.employee) = 2 then "00"
              else "000".
    employee.employee = substring(ls-zero,1,4 - length(employee.employee) ) + employee.employee.

end.  
