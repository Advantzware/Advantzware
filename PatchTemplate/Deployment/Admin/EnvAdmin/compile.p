compile N:\Repositories\Advantzware\PatchTemplate\Deployment\Admin\EnvAdmin\asiLogin.w save.
compile N:\Repositories\Advantzware\PatchTemplate\Deployment\Admin\EnvAdmin\asiUpdate.w save.
compile N:\Repositories\Advantzware\PatchTemplate\Deployment\Admin\EnvAdmin\asiUpdateDb.w save.
connect -db asiTest168 -H localhost -S 2827 -ld asi.
compile N:\Repositories\Advantzware\PatchTemplate\Deployment\Admin\EnvAdmin\prerun.p save.
connect -db audTest168 -H localhost -S 2837 -ld audit.
compile N:\Repositories\Advantzware\PatchTemplate\Deployment\Admin\EnvAdmin\asiUpdateEnv.w save.
