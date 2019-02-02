&SCOPED-DEFINE cDir N:\Repositories\Advantzware
&SCOPED-DEFINE cTarget N:\Repositories\Advantzware\Template\Patch\Admin\EnvAdmin
ASSIGN 
    propath = "{&cDir}\Legacy,{&cDir}\Resources," + propath.

COMPILE {&cDir}\Template\BuildScript\ToCOMPILE\asiLogin.w SAVE INTO {&cTarget}.
COMPILE {&cDir}\Template\BuildScript\ToCOMPILE\asiUpdate.w SAVE INTO {&cTarget}.
COMPILE {&cDir}\Template\BuildScript\ToCOMPILE\asiUpdateDb.w SAVE INTO {&cTarget}.

CONNECT -db asiDevel -H localhost -S 2821 -ld asi.
COMPILE {&cDir}\Template\BuildScript\ToCOMPILE\prerun.p SAVE INTO {&cTarget}.

CONNECT -db audDevel -H localhost -S 2831 -ld audit.
COMPILE {&cDir}\Template\BuildScript\ToCOMPILE\asiUpdateEnv.w SAVE INTO {&cTarget}.

RUN {&cDir}\Template\BuildScript\buildDumpFiles.p.
