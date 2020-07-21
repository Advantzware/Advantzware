/* DateRules.i */

DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
        skipHour:SENSITIVE              = YES
        skipMinute:SENSITIVE            = YES
        skipampm:SENSITIVE              = YES
        dbTables:SENSITIVE              = YES
        dbFields:SENSITIVE              = YES
        lSunday:SENSITIVE               = YES
        lMonday:SENSITIVE               = YES
        lTuesday:SENSITIVE              = YES
        lWednesday:SENSITIVE            = YES
        lThursday:SENSITIVE             = YES
        lFriday:SENSITIVE               = YES
        lSaturday:SENSITIVE             = YES
        lHoliday:SENSITIVE              = YES
        skipampm:HIDDEN                 = iHourMax EQ 24
        dateRules.baseTable:SENSITIVE   = lSuperAdmin
        dateRules.baseField:SENSITIVE   = lSuperAdmin
        dateRules.resultTable:SENSITIVE = lSuperAdmin
        dateRules.resultField:SENSITIVE = lSuperAdmin
        dbTables:SENSITIVE              = lSuperAdmin
        dbFields:SENSITIVE              = lSuperAdmin
        .
END.
