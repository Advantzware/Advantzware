/*---------------------------------------------------------------------------
    File:           util\fixUsers.p
    Copyright:      (c)2017 Advanced Software Services, Inc.All rights rsrvd
    Description:    utility to fix user (and related) table values AFTER 16.6.0
                    upgrade is complete
    Included files:     (none)

    External RUN/CALL:  (none)

    External files:     (none)

    Revision history:   MM/DD/YY    INIT    TKT     Description
                        unknown                     original version
                        10/01/17    MYT     21774   16.6.0 revision
-----------------------------------------------------------------------------*/

DISABLE TRIGGERS FOR LOAD OF users.
DISABLE TRIGGERS FOR LOAD OF user-print.
DISABLE TRIGGERS FOR LOAD OF usercomp.
DISABLE TRIGGERS FOR LOAD OF userEula.
DISABLE TRIGGERS FOR LOAD OF userlog.
DISABLE TRIGGERS FOR LOAD OF usersman.
DISABLE TRIGGERS FOR LOAD OF uservend.
DISABLE TRIGGERS FOR LOAD OF usr.
DISABLE TRIGGERS FOR LOAD OF usr-grp.
DISABLE TRIGGERS FOR LOAD OF usr-menu.
DISABLE TRIGGERS FOR LOAD OF usrx.
DISABLE TRIGGERS FOR LOAD OF reftable.
DEF TEMP-TABLE tempUser NO-UNDO LIKE _User.

/* Set user "asi" data */
FIND users EXCLUSIVE WHERE 
    users.user_id = "asi"
    NO-ERROR.
IF NOT AVAIL users THEN DO:
    CREATE users.
    ASSIGN
        users.user_id = "asi"
        users.createDate = today
        users.createTime = time
        users.createUser = "asi"
        .
END.
ASSIGN
    users.developer = true
    users.fax = ""
    users.fax-cnty = ""
    users.image_filename = "asiHelp@advantzware.com"
    users.isActive = true
    users.isLocked = false
    users.phone = "2153697800"
    users.phone-cnty = "1"
    users.securityLevel = 1000
    users.showOnAck = false
    users.showOnBol = false
    users.showOnInv = false
    users.showOnPO = false
    users.showOnQuote = false
    users.track_usage = true
    users.updateDate = today
    users.updateTime = time
    users.updateUser = "asi"
    users.userType = "Administrator"
    users.user_language = "English"
    users.user_name = "Advantzware System Admin"
    users.user_program[1] = ""
    users.user_program[2] = ""
    users.user_program[3] = ""
    users.use_colors = false
    users.use_ctrl_keys = false
    users.use_fonts = false
    users.widget_bgc = 0
    users.widget_fgc = 0
    users.widget_font = 0
    . 
RUN ipSetASIPwd.
    
/* Set user "admin" data */
FIND users EXCLUSIVE WHERE 
    users.user_id = "Admin"
    NO-ERROR.
IF NOT AVAIL users THEN DO:
    CREATE users.
    ASSIGN
        users.user_id = "Admin"
        users.createDate = today
        users.createTime = time
        users.createUser = "asi"
        .
END.
ASSIGN
    users.developer = true
    users.fax = ""
    users.fax-cnty = ""
    users.image_filename = ""
    users.isActive = true
    users.isLocked = false
    users.phone = ""
    users.phone-cnty = ""
    users.securityLevel = 700
    users.showOnAck = false
    users.showOnBol = false
    users.showOnInv = false
    users.showOnPO = false
    users.showOnQuote = false
    users.track_usage = true
    users.updateDate = today
    users.updateTime = time
    users.updateUser = "asi"
    users.userType = "Administrator"
    users.user_language = "English"
    users.user_name = "Local System Admin"
    users.user_program[1] = ""
    users.user_program[2] = ""
    users.user_program[3] = ""
    users.use_colors = false
    users.use_ctrl_keys = false
    users.use_fonts = false
    users.widget_bgc = 0
    users.widget_fgc = 0
    users.widget_font = 0
    . 
RUN ipSetAdminPwd.

/* Add/convert data for new users table fields */
FOR EACH users NO-LOCK:
    IF users.userType = "" OR users.userType = ? THEN DO:
        CASE users.user_id:
            WHEN "ASI" OR
            WHEN "Administrator" OR
            WHEN "Admin"THEN ASSIGN users.userType = "Administrator".
            OTHERWISE ASSIGN users.userType = "Full User".
        END CASE.
    END.
    IF users.securityLevel = 0 THEN DO:
        CASE users.user_id:
            WHEN "ASI" THEN ASSIGN users.securityLevel = 1000.
            WHEN "Administrator" OR
            WHEN "Admin"THEN ASSIGN users.securityLevel = 700.
            OTHERWISE ASSIGN users.securityLevel = 100.
        END CASE.
    END.
    IF NOT CAN-FIND(FIRST usr WHERE 
        usr.uid EQ users.user_id) THEN DO:
        CREATE usr.
        ASSIGN
            usr.uid = users.user_id
            usr.usr-lang = 'English'.
    END.
    FOR EACH reftable EXCLUSIVE WHERE 
        reftable.reftable EQ "users.user-docs" AND
        reftable.company EQ users.user_id:
        ASSIGN
            users.showOnPO = IF users.showOnPO = TRUE OR reftable.val[1] = 1 THEN TRUE ELSE FALSE
            users.showOnBOL = IF users.showOnBOL = TRUE OR reftable.val[2] = 1 THEN TRUE ELSE FALSE
            users.showOnInv = IF users.showOnInv = TRUE OR reftable.val[3] = 1 THEN TRUE ELSE FALSE
            users.showOnAck = IF users.showOnAck = TRUE OR reftable.val[4] = 1 THEN TRUE ELSE FALSE
            users.showOnQuote = IF users.showOnQuote = TRUE OR reftable.val[5] = 1 THEN TRUE ELSE FALSE
            .
        DELETE reftable.
    END.
    FOR EACH reftable EXCLUSIVE WHERE
        reftable.reftable EQ "users.phone-no" AND
        reftable.company EQ users.user_id:
        ASSIGN
            users.phone = IF users.phone = "" AND reftable.CODE = "" THEN reftable.CODE ELSE users.phone
            .
        DELETE reftable.
    END.
    FOR EACH reftable EXCLUSIVE WHERE
        reftable.reftable EQ "users.fax-no" AND
        reftable.company EQ users.user_id:
        ASSIGN
            users.fax = IF users.fax = "" AND reftable.CODE NE "" THEN reftable.CODE ELSE users.fax
            .
        DELETE reftable.
    END.
    FOR EACH reftable EXCLUSIVE WHERE
        reftable.reftable EQ "users.phone-cnty" AND
        reftable.company EQ users.user_id:
        ASSIGN
            users.phone-cnty = IF users.phone-cnty = "" AND reftable.CODE NE "" THEN reftable.CODE ELSE users.phone-cnty
            .
        DELETE reftable.
    END.
    FOR EACH reftable EXCLUSIVE WHERE
        reftable.reftable EQ "users.fax-cnty" AND
        reftable.company EQ users.user_id:
        ASSIGN
            users.fax-cnty = IF users.fax-cnty = "" AND reftable.CODE NE "" THEN reftable.CODE ELSE users.fax-cnty
            .
        DELETE reftable.
    END.
END. /* each users */

/* Clean up remnant records for any deleted users */
&SCOPED-DEF cFileName usr
&SCOPED-DEF cFieldName uid
FOR EACH {&cFileName} EXCLUSIVE:
    IF NOT CAN-FIND(FIRST users WHERE 
                    users.user_id EQ {&cFileName}.{&cFieldName}) THEN 
        DELETE {&cFileName}.
END. 

&SCOPED-DEF cFileName user-print
&SCOPED-DEF cFieldName user-id
FOR EACH {&cFileName} EXCLUSIVE:
    IF NOT CAN-FIND(FIRST users WHERE 
                    users.user_id EQ {&cFileName}.{&cFieldName}) THEN 
        DELETE {&cFileName}.
END. 

&SCOPED-DEF cFileName usercomp
&SCOPED-DEF cFieldName user_id
FOR EACH {&cFileName} EXCLUSIVE:
    IF NOT CAN-FIND(FIRST users WHERE 
                    users.user_id EQ {&cFileName}.{&cFieldName}) THEN 
        DELETE {&cFileName}.
END. 

&SCOPED-DEF cFileName usercust
&SCOPED-DEF cFieldName user_id
FOR EACH {&cFileName} EXCLUSIVE:
    IF NOT CAN-FIND(FIRST users WHERE 
                    users.user_id EQ {&cFileName}.{&cFieldName}) THEN 
        DELETE {&cFileName}.
END. 

&SCOPED-DEF cFileName userEula
&SCOPED-DEF cFieldName user_id
FOR EACH {&cFileName} EXCLUSIVE:
    IF NOT CAN-FIND(FIRST users WHERE 
                    users.user_id EQ {&cFileName}.{&cFieldName}) THEN 
        DELETE {&cFileName}.
END. 

&SCOPED-DEF cFileName userLog
&SCOPED-DEF cFieldName user_id
FOR EACH {&cFileName} EXCLUSIVE:
    IF NOT CAN-FIND(FIRST users WHERE 
                    users.user_id EQ {&cFileName}.{&cFieldName}) THEN 
        DELETE {&cFileName}.
END. 

&SCOPED-DEF cFileName usersman
&SCOPED-DEF cFieldName user_id
FOR EACH {&cFileName} EXCLUSIVE:
    IF NOT CAN-FIND(FIRST users WHERE 
                    users.user_id EQ {&cFileName}.{&cFieldName}) THEN 
        DELETE {&cFileName}.
END. 

&SCOPED-DEF cFileName userVend
&SCOPED-DEF cFieldName user_id
FOR EACH {&cFileName} EXCLUSIVE:
    IF NOT CAN-FIND(FIRST users WHERE 
                    users.user_id EQ {&cFileName}.{&cFieldName}) THEN 
        DELETE {&cFileName}.
END. 

&SCOPED-DEF cFileName usr
&SCOPED-DEF cFieldName uid
FOR EACH {&cFileName} EXCLUSIVE:
    IF NOT CAN-FIND(FIRST users WHERE 
                    users.user_id EQ {&cFileName}.{&cFieldName}) THEN 
        DELETE {&cFileName}.
END. 

&SCOPED-DEF cFileName usr-grp
&SCOPED-DEF cFieldName uid
FOR EACH {&cFileName} EXCLUSIVE:
    IF NOT CAN-FIND(FIRST users WHERE 
                    users.user_id EQ {&cFileName}.{&cFieldName}) THEN 
        DELETE {&cFileName}.
END. 

&SCOPED-DEF cFileName usr-menu
&SCOPED-DEF cFieldName user_id
FOR EACH {&cFileName} EXCLUSIVE:
    IF NOT CAN-FIND(FIRST users WHERE 
                    users.user_id EQ {&cFileName}.{&cFieldName}) THEN 
        DELETE {&cFileName}.
END. 

&SCOPED-DEF cFileName usrx
&SCOPED-DEF cFieldName uid
FOR EACH {&cFileName} EXCLUSIVE:
    IF NOT CAN-FIND(FIRST users WHERE 
                    users.user_id EQ {&cFileName}.{&cFieldName}) THEN 
        DELETE {&cFileName}.
END. 

FOR EACH reftable EXCLUSIVE WHERE reftable.reftable EQ "users.user-docs":
    IF NOT CAN-FIND(FIRST users WHERE 
                    users.user_id EQ reftable.company) THEN
        DELETE reftable.
END.

FOR EACH reftable EXCLUSIVE WHERE reftable.reftable EQ "users.phone-no":
    IF NOT CAN-FIND(FIRST users WHERE 
                    users.user_id EQ reftable.company) THEN
        DELETE reftable.
END.

FOR EACH reftable EXCLUSIVE WHERE reftable.reftable EQ "users.fax-no":
    IF NOT CAN-FIND(FIRST users WHERE 
                    users.user_id EQ reftable.company) THEN
        DELETE reftable.
END.

FOR EACH reftable EXCLUSIVE WHERE reftable.reftable EQ "users.phone-cnty":
    IF NOT CAN-FIND(FIRST users WHERE 
                    users.user_id EQ reftable.company) THEN
        DELETE reftable.
END.

FOR EACH reftable EXCLUSIVE WHERE reftable.reftable EQ "users.fax-cnty":
    IF NOT CAN-FIND(FIRST users WHERE 
                    users.user_id EQ reftable.company) THEN
        DELETE reftable.
END.

MESSAGE 'Users Fix Complete!' VIEW-AS ALERT-BOX.

PROCEDURE ipSetASIPwd :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FIND FIRST _User WHERE 
        _User._UserId = "asi" 
        EXCLUSIVE-LOCK NO-ERROR.

    IF AVAIL (_User) THEN DO:
        BUFFER-COPY _User EXCEPT _tenantID _User._Password TO tempUser.
        ASSIGN 
            tempUser._Password = "ifaOfSAcSdialAkd".
        DELETE _User.
        CREATE _User.
        BUFFER-COPY tempUser EXCEPT _tenantid TO _User.
    END.
    ELSE DO:
        CREATE _User.
        ASSIGN
            _User._UserId = "asi"
            _User._Password = "ifaOfSAcSdialAkd".
    END. 
END PROCEDURE.

PROCEDURE ipSetAdminPwd :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FIND FIRST _User WHERE 
        _User._UserId = "Admin" 
        EXCLUSIVE-LOCK NO-ERROR.

    IF AVAIL (_User) THEN DO:
        BUFFER-COPY _User EXCEPT _tenantID _User._Password TO tempUser.
        ASSIGN 
            tempUser._Password = ENCODE("admin").
        DELETE _User.
        CREATE _User.
        BUFFER-COPY tempUser EXCEPT _tenantid TO _User.
    END.
    ELSE DO:
        CREATE _User.
        ASSIGN
            _User._UserId = "Admin"
            _User._Password = Encode("admin").
    END. 
END PROCEDURE.
