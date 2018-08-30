
RUN cec/d-refestmach.w (RECID(mach), "{1}").


IF "{1}" = "RUN" THEN ASSIGN SELF:SCREEN-VALUE = STRING(mach.run-crusiz-cst[1]).
ELSE IF "{1}" = "M R" THEN ASSIGN SELF:SCREEN-VALUE = STRING(mach.mr-crusiz-cst[1]).

