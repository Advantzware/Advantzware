SET patchDir=P:\asi16ship\stdPatch

@echo off
for /F %%a IN (%patchDir%\potentialMissing.txt) do (
    IF NOT EXIST %%a (
        Echo ERROR %%a Does Not Exist!!!!
    )
)