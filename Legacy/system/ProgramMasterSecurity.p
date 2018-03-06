PROCEDURE getSecurity:
  DEFINE INPUT PARAMETER ipProgname    AS CHARACTER.
  DEFINE INPUT PARAMETER ipUsername   AS CHARACTER.
  DEFINE INPUT PARAMETER ipOperation  AS CHARACTER.
  
  DEFINE OUTPUT PARAMETER opCanAccess AS LOGICAL.
  
  ASSIGN opCanAccess = TRUE.
  FIND FIRST users NO-LOCK WHERE 
         users.user_id EQ ipUsername 
         NO-ERROR.
  IF AVAIL users THEN DO:
      CASE ipProgname:
          WHEN "browsers/rm-ibin.w" OR
          WHEN "oeinq/b-ordfgi.w" THEN
             ASSIGN opCanAccess = IF users.securityLevel LE 900 THEN TRUE ELSE FALSE.
          WHEN "jcinq/b-updmat.w" THEN
             ASSIGN opCanAccess = IF users.securityLevel GT 899 THEN TRUE ELSE FALSE.                     
          WHEN "methods/template/viewer4.i" OR
          WHEN "viewers/sys-ctrl.w" OR
          WHEN "viewers/vend.w" THEN
             ASSIGN opCanAccess = IF users.securityLevel GE 1000 THEN TRUE ELSE FALSE.            
          WHEN "sys/ref/hlp-ctrl.w" THEN DO:
              IF ipOperation = "EnableUpdateButton" THEN
                 ASSIGN opCanAccess = IF users.securityLevel GE 900 THEN TRUE ELSE FALSE.
              IF ipOperation = "EnableHelpUpdate" THEN
                 ASSIGN opCanAccess = IF users.securityLevel LE 999 THEN TRUE ELSE FALSE.
          END. /*WHEN "sys/ref/hlp-ctrl.w"*/
          WHEN "sys/ref/hlp.w" THEN DO:
              IF ipOperation = "Access1" THEN
                 ASSIGN opCanAccess = IF users.securityLevel GE 900 THEN TRUE ELSE FALSE.
              IF ipOperation = "Access2" THEN
                 ASSIGN opCanAccess = IF users.securityLevel LE 999 THEN TRUE ELSE FALSE.
          END. /*WHEN "sys/ref/hlp.w"*/
          WHEN "viewers/p-fg-bj-l.w" THEN
             ASSIGN opCanAccess = IF users.securityLevel GE 900 THEN TRUE ELSE FALSE.
           
          WHEN "viewers/userControl.w" THEN DO:
              IF ipOperation = "Access1" THEN
                 ASSIGN opCanAccess = IF users.securityLevel GE 1000 THEN TRUE ELSE FALSE.
              IF ipOperation = "Access2" THEN
                 ASSIGN opCanAccess = IF users.securityLevel GT 900 THEN TRUE ELSE FALSE.
          END. /*WHEN "viewers/userControl.w"*/
          WHEN "windows/prgrms.w" THEN
             ASSIGN opCanAccess = IF users.securityLevel LE 899 THEN TRUE ELSE FALSE.
          WHEN "windows/uom.w" OR
          WHEN "windows/xref.w" OR
          WHEN "windows/std-code.w" OR
          WHEN "windows/matprep.w" OR
          WHEN "windows/mat.w" OR
          WHEN "windows/job-cat.w" OR
          WHEN "windows/file.w" OR 
          WHEN "windows/dept.w" OR
          WHEN "windows/company.w" OR 
          WHEN "viewers/vp-rmov.w" OR
          WHEN "viewers/file-seq.w" OR
          WHEN "viewers/company.w" OR
          WHEN "sys/help/w-head.w" THEN
             ASSIGN opCanAccess = IF users.securityLevel LE 999 THEN TRUE ELSE FALSE.             
      END CASE.
  END.  /*IF AVAIL users THEN DO:*/  
  ELSE ASSIGN opCanAccess = FALSE.

END PROCEDURE.
