 
IF "{1}" = "RUN" THEN ASSIGN mach.run-crusiz-qty[1] = 1
                             mach.run-crusiz-cst[1] = DEC(SELF:SCREEN-VALUE)
                             .
ELSE IF "{1}" = "M R" THEN ASSIGN mach.mr-crusiz-qty[1] = 1
                                  mach.mr-crusiz-cst[1] = DEC(SELF:SCREEN-VALUE)
                                  .


RUN cec/d-refestmach.w (RECID(mach), "{1}").


IF "{1}" = "RUN" THEN ASSIGN SELF:SCREEN-VALUE = STRING(mach.run-crusiz-cst[1]).
ELSE IF "{1}" = "M R" THEN ASSIGN SELF:SCREEN-VALUE = STRING(mach.mr-crusiz-cst[1]).

