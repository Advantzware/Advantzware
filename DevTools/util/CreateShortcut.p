    DEF VAR first-part AS CHAR .
    DEF VAR second-part AS CHAR.
    DEF VAR third-part AS CHAR.
    DEF VAR fourth-part AS CHAR .
    DEF VAR i AS INT.
    def buffer bimage for z_image.
    def var j as int.
    for each z_shortcut where user-id = "" exclusive-lock:
    delete z_shortcut.
    end.
    FOR EACH z_image WHERE CAN-DO("*inquiry,*Maintenance,*report,*system,*transaction",z_image.Menu-name):
        IF z_image.menu-name BEGINS "edi" THEN NEXT.
        IF z_image.menu-name BEGINS "cp" THEN NEXT.
        IF z_image.menu-name BEGINS "hr" THEN NEXT.
        IF z_image.MENU-name BEGINS "tqm" THEN NEXT.
        IF z_image.menu-name BEGINS "tpm" THEN NEXT.
        IF z_image.menu-name BEGINS "py" THEN NEXT.
        IF z_image.menu-name BEGINS "wb" THEN NEXT.
   second-part = "  ".
    IF z_image.menu-name MATCHES "*inquiry" THEN second-part = " 6 ".
    IF z_image.menu-name MATCHES "*maintenance" then second-part = " 3 ". 
    IF z_image.menu-name MATCHES "*report" THEN second-part = " 5 ".
    IF z_image.menu-name MATCHES "*system" THEN second-part = " 7 ".
    IF z_image.menu-name MATCHES "*transaction"  THEN second-part = " 4 ".
    FIRST-part = SUBSTRING(z_image.menu-name,1,INDEX(z_image.menu-name,".") - 1).
    DO i = 1 TO 32:
        IF z_image.entry-desc[i] <> "" THEN DO:
            third-part = SUBSTRING(z_image.entry-desc[i],1,3).
            third-part = TRIM(third-part).
            if z_image.type-list[i] = "m" then do:
               find bimage where bimage.menu-name = z_image.funct-list[i]
                  and bimage.user-id = "" no-lock no-error.
               if not avail bimage then next.
               do j = 1 to 32:
                 if bimage.entry-desc[j] <> "" then do:
                   fourth-part = substring(bimage.entry-desc[j],1,3).
                   fourth-part = trim(fourth-part).
            CREATE z_shortcut.
            ASSIGN z_shortcut.short-cut = first-part + second-part +  third-part + " " + fourth-part
                   z_shortcut.funct-name = bimage.funct-list[j].
            DISPLAY  z_shortcut.SHORT-cut z_shortcut.funct-name .
            DOWN .
                 end.
               end.    
            end.
            else do:
            CREATE z_shortcut.
            ASSIGN z_shortcut.short-cut = first-part + second-part +  third-part
                   z_shortcut.funct-name = z_image.funct-list[i].
            DISPLAY  z_shortcut.SHORT-cut z_shortcut.funct-name .
            DOWN .
            end.
        END.
    END.
    END.
  
