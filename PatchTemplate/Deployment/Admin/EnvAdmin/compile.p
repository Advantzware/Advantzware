compile N:\Repositories\Advantzware\PatchTemplate\Deployment\Admin\EnvAdmin\asiLogin.w save.
compile N:\Repositories\Advantzware\PatchTemplate\Deployment\Admin\EnvAdmin\asiUpdate.w save.
compile N:\Repositories\Advantzware\PatchTemplate\Deployment\Admin\EnvAdmin\asiUpdateDb.w save.
connect -db asiTest167 -H localhost -S 2856 -ld asi.
compile N:\Repositories\Advantzware\PatchTemplate\Deployment\Admin\EnvAdmin\prerun.p save.
connect -db audTest167 -H localhost -S 2828 -ld audit.
compile N:\Repositories\Advantzware\PatchTemplate\Deployment\Admin\EnvAdmin\asiUpdateEnv.w save.
