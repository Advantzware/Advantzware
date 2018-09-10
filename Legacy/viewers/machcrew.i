FIND CURRENT MACH exclusive-lock no-error .
IF AVAILABLE mach THEN DO:
    IF "{1}" = "RUN" THEN ASSIGN mach.run-crusiz-qty[1] = 1
                                 mach.run-crusiz-cst[1] = DEC(SELF:SCREEN-VALUE)
                                 .
    ELSE IF "{1}" = "M R" THEN ASSIGN mach.mr-crusiz-qty[1] = 1
                                      mach.mr-crusiz-cst[1] = DEC(SELF:SCREEN-VALUE)
                                      .
END.

RUN cec/d-refestmach.w (RECID(mach), "{1}").

FIND CURRENT MACH no-lock no-error .

IF "{1}" = "RUN" THEN ASSIGN SELF:SCREEN-VALUE = STRING(mach.run-crusiz-cst[1]).
ELSE IF "{1}" = "M R" THEN ASSIGN SELF:SCREEN-VALUE = STRING(mach.mr-crusiz-cst[1]).

