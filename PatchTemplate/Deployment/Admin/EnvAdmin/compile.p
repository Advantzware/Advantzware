compile N:\Repositories\Advantzware\PatchTemplate\Deployment\Admin\EnvAdmin\asiLogin.w save.
compile N:\Repositories\Advantzware\PatchTemplate\Deployment\Admin\EnvAdmin\asiUpdate.w save.
compile N:\Repositories\Advantzware\PatchTemplate\Deployment\Admin\EnvAdmin\asiUpdateDb.w save.
connect -db asiTest168 -H localhost -S 2866 -ld asi.
compile N:\Repositories\Advantzware\PatchTemplate\Deployment\Admin\EnvAdmin\prerun.p save.
connect -db audTest168 -H localhost -S 2867 -ld audit.
compile N:\Repositories\Advantzware\PatchTemplate\Deployment\Admin\EnvAdmin\asiUpdateEnv.w save.
