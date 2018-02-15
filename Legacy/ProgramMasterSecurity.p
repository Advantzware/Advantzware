PROCEDURE getSecurity:
  DEFINE INPUT PARAMETER ipProname    AS CHARACTER.
  DEFINE INPUT PARAMETER ipUsername   AS CHARACTER.
  DEFINE INPUT PARAMETER ipOperation  AS CHARACTER.
  
  DEFINE OUTPUT PARAMETER opCanAccess AS LOGICAL.
  
  FIND FIRST users NO-LOCK WHERE 
         users.user_id EQ ipUsername 
         NO-ERROR.
  IF AVAIL users THEN DO:
      IF users.securityLevel LE 900 AND ipProname = "browsers/rm-ibin.w" THEN
          ASSIGN opCanAccess = TRUE.
      ELSE IF users.securityLevel GT 899 AND ipProname = "jcinq/b-updmat.w" THEN
          ASSIGN opCanAccess = TRUE.      
      ELSE IF users.securityLevel GE 1000 AND ipProname = "methods/template/viewer4.i" THEN
          ASSIGN opCanAccess = TRUE.
      ELSE IF users.securityLevel LE 900 AND ipProname = "oeinq/b-ordfgi.w" THEN
          ASSIGN opCanAccess = TRUE.
      ELSE IF users.securityLevel LE 999 AND ipProname = "sys/help/w-head.w" THEN
          ASSIGN opCanAccess = TRUE.
      ELSE IF users.securityLevel GE 900 AND ipProname = "sys/ref/hlp-ctrl.w" AND ipOperation = "EnableUpdateButton" THEN
          ASSIGN opCanAccess = TRUE.
      ELSE IF users.securityLevel LE 999 AND ipProname = "sys/ref/hlp-ctrl.w" AND ipOperation = "EnableHelpUpdate" THEN
          ASSIGN opCanAccess = TRUE.    
      ELSE IF users.securityLevel GE 900 AND ipProname = "sys/ref/hlp.w" AND ipOperation = "Access1" THEN
          ASSIGN opCanAccess = TRUE.
      ELSE IF users.securityLevel LE 999 AND ipProname = "sys/ref/hlp.w" AND ipOperation = "Access2" THEN
          ASSIGN opCanAccess = TRUE.    
      ELSE IF users.securityLevel LE 999 AND ipProname = "viewers/company.w" THEN
          ASSIGN opCanAccess = TRUE. 
      ELSE IF users.securityLevel LE 999 AND ipProname = "viewers/file-seq.w" THEN
          ASSIGN opCanAccess = TRUE.
      ELSE IF users.securityLevel GE 900 AND ipProname = "viewers/p-fg-bj-l.w" THEN
          ASSIGN opCanAccess = TRUE.
      ELSE IF users.securityLevel GE 1000 AND ipProname = "viewers/sys-ctrl.w" THEN
          ASSIGN opCanAccess = TRUE.
      ELSE IF users.securityLevel GE 1000 AND ipProname = "viewers/userControl.w" AND ipOperation = "Access1" THEN
          ASSIGN opCanAccess = TRUE.
      ELSE IF users.securityLevel GT 900 AND ipProname = "viewers/userControl.w" AND ipOperation = "Access2" THEN
          ASSIGN opCanAccess = TRUE.      
      ELSE IF users.securityLevel GE 1000 AND ipProname = "viewers/vend.w" THEN
          ASSIGN opCanAccess = TRUE.
      ELSE IF users.securityLevel LE 999 AND ipProname = "viewers/vp-rmov.w" THEN
          ASSIGN opCanAccess = TRUE.
      ELSE IF users.securityLevel LE 999 AND ipProname = "windows/company.w" THEN
          ASSIGN opCanAccess = TRUE.
      ELSE IF users.securityLevel LE 999 AND ipProname = "windows/dept.w" THEN
          ASSIGN opCanAccess = TRUE.         
      ELSE IF users.securityLevel LE 999 AND ipProname = "windows/file.w" THEN
          ASSIGN opCanAccess = TRUE.
      ELSE IF users.securityLevel LE 999 AND ipProname = "windows/job-cat.w" THEN
          ASSIGN opCanAccess = TRUE.
      ELSE IF users.securityLevel LE 999 AND ipProname = "windows/mat.w" THEN
          ASSIGN opCanAccess = TRUE.                                
      ELSE IF users.securityLevel LE 999 AND ipProname = "windows/matprep.w" THEN
          ASSIGN opCanAccess = TRUE.
      ELSE IF users.securityLevel LE 899 AND ipProname = "windows/prgrms.w" THEN
          ASSIGN opCanAccess = TRUE.         
      ELSE IF users.securityLevel LE 999 AND ipProname = "windows/std-code.w" THEN
          ASSIGN opCanAccess = TRUE.
      ELSE IF users.securityLevel LE 999 AND ipProname = "windows/uom.w" THEN
          ASSIGN opCanAccess = TRUE.
      ELSE IF users.securityLevel LE 999 AND ipProname = "windows/xref.w" THEN
          ASSIGN opCanAccess = TRUE.
      ELSE opCanAccess = FALSE.
  END.  /*IF AVAIL users THEN DO:*/  
     

END PROCEDURE.
