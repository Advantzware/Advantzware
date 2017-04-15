
{custom/globdefs.i}

{sys/inc/var.i new SHARED}

DEF VAR li AS INT NO-UNDO.


SESSION:SET-WAIT-STATE("general").

ASSIGN
 cocode = g_company
 locode = g_loc.

FOR EACH account
    WHERE account.company            EQ cocode
      AND INDEX("ALCT",account.type) GT 0
    TRANSACTION:

  account.lyr-open = account.cyr-open.
   
  DO li = 1 TO EXTENT(account.lyr):
    account.lyr-open = account.lyr-open - account.lyr[li].
  END.
END.

SESSION:SET-WAIT-STATE("").
