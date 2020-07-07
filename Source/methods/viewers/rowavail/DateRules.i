/* DateRules.i */

DO WITH FRAME {&FRAME-NAME}:
    IF AVAILABLE DateRules THEN DO:
        IF iHourMax EQ 24 THEN
        ASSIGN
            skipHour:SCREEN-VALUE   = SUBSTRING(STRING(DateRules.skipTime,"HH:MM"),1,2)
            skipMinute:SCREEN-VALUE = SUBSTRING(STRING(DateRules.skipTime,"HH:MM"),4,2)
            .
        ELSE
        ASSIGN
            skipHour:SCREEN-VALUE   = SUBSTRING(STRING(DateRules.skipTime,"HH:MM AM"),1,2)
            skipMinute:SCREEN-VALUE = SUBSTRING(STRING(DateRules.skipTime,"HH:MM AM"),4,2)
            .
        ASSIGN
            skipampm:SCREEN-VALUE   = SUBSTRING(STRING(DateRules.skipTime,"HH:MM AM"),7,2)
            lSunday:SCREEN-VALUE    = STRING(SUBSTRING(DateRules.skipDays,1,1) EQ "Y")
            lMonday:SCREEN-VALUE    = STRING(SUBSTRING(DateRules.skipDays,2,1) EQ "Y")
            lTuesday:SCREEN-VALUE   = STRING(SUBSTRING(DateRules.skipDays,3,1) EQ "Y")
            lWednesday:SCREEN-VALUE = STRING(SUBSTRING(DateRules.skipDays,4,1) EQ "Y")
            lThursday:SCREEN-VALUE  = STRING(SUBSTRING(DateRules.skipDays,5,1) EQ "Y")
            lFriday:SCREEN-VALUE    = STRING(SUBSTRING(DateRules.skipDays,6,1) EQ "Y")
            lSaturday:SCREEN-VALUE  = STRING(SUBSTRING(DateRules.skipDays,7,1) EQ "Y")
            lHoliday:SCREEN-VALUE   = STRING(SUBSTRING(DateRules.skipDays,8,1) EQ "Y")
            .
    END. /* if avail */
END. /* do frame */
