  @echo off
  for /F %%a IN (P:\asi16ship\stdPatch\potentialMissing.txt) do (
    IF NOT EXIST %%a (
      Echo ERROR %%a Does Not Exist!!!!
    )

  )