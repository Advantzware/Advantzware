/* emplogin.i */
def buffer bf-machemp for machemp.

/*  can not delete machtran, machemp when emplogin is deleted.  */

IF CAN-FIND(FIRST machemp where /*machemp.company = emplogin.company and */
                                machemp.employee = emplogin.employee and
                                machemp.start_date >= emplogin.start_date and
                                machemp.start_time >= emplogin.start_time and
                                machemp.end_date <= emplogin.end_date and
                                machemp.end_time <= emplogin.end_time
) THEN
FOR EACH bf-machemp where /*machemp.company = emplogin.company and */
                       bf-machemp.employee = emplogin.employee and
                       bf-machemp.start_date >= emplogin.start_date and
                       bf-machemp.start_time >= emplogin.start_time and
                       bf-machemp.end_date <= emplogin.end_date and
                       bf-machemp.end_time <= emplogin.end_time
EXCLUSIVE-LOCK:
  DELETE bf-machemp.
END.



