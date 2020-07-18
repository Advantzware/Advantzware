/* DateRules.i */

DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
        skipHour:SENSITIVE              = NO
        skipMinute:SENSITIVE            = NO
        skipampm:SENSITIVE              = NO
        dbTables:SENSITIVE              = NO
        dbFields:SENSITIVE              = NO
        lSunday:SENSITIVE               = NO
        lMonday:SENSITIVE               = NO
        lTuesday:SENSITIVE              = NO
        lWednesday:SENSITIVE            = NO
        lThursday:SENSITIVE             = NO
        lFriday:SENSITIVE               = NO
        lSaturday:SENSITIVE             = NO
        lHoliday:SENSITIVE              = NO
        skipampm:HIDDEN                 = iHourMax EQ 24
        dateRules.baseTable:SENSITIVE   = NO
        dateRules.baseField:SENSITIVE   = NO
        dateRules.resultTable:SENSITIVE = NO
        dateRules.resultField:SENSITIVE = NO
        dbTables:SENSITIVE              = NO
        dbFields:SENSITIVE              = NO
        .
END.
