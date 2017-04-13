/*********************************************************************
* Copyright (C) 2000,2011 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*******************************************************************************
*
*   PROGRAM:  _login.p
*
*   PROGRAM SUMMARY:
*       Prompt user for userid and password and set the userid.
*       Caller must include login.i NEW.
*
*   RUN/CALL SYNTAX:
*       { login.i NEW }
*       RUN login.p
*
*   PARAMETERS/ARGUMENTS LIST:
*       None
*
*******************************************************************************/

USING Consultingwerk.Windows.OpenEdgeLogin.* FROM PROPATH .

DEFINE INPUT PARAMETER viewAsDialog AS LOGICAL NO-UNDO.

{Consultingwerk/products.i}
{ login.i }

DEFINE VARIABLE tries     AS INTEGER NO-UNDO.
DEFINE VARIABLE lFound    AS LOGICAL NO-UNDO.
DEFINE VARIABLE hCP       AS HANDLE  NO-UNDO.
DEFINE VARIABLE setdbclnt AS LOGICAL NO-UNDO.
define variable cUserid   as character no-undo.

DEFINE VARIABLE oForm          AS Consultingwerk.Windows.OpenEdgeLogin.DatabaseLoginForm NO-UNDO .
DEFINE VARIABLE oDialogResult  AS System.Windows.Forms.DialogResult                      NO-UNDO .
DEFINE VARIABLE oOptOutService AS IDatabaseLoginPreferences                              NO-UNDO .

oOptOutService = {Consultingwerk/get-service.i Consultingwerk.Windows.OpenEdgeLogin.IDatabaseLoginPreferences
                                               "NEW Consultingwerk.Windows.OpenEdgeLogin.DatabaseLoginPreferences()"} .

IF USERID("DICTDB":U) <> "":U  THEN
    RETURN.

IF oOptOutService:IsDisabled (DBPARAM ("DICTDB":U)) THEN
    RETURN .

if integer(dbversion("DICTDB":U)) > 10 then
do:
    find first  dictdb._sec-authentication-domain
         where (dictdb._sec-authentication-domain._Domain-enabled = yes
           and  dictdb._sec-authentication-domain._Domain-id > -1 and
                dictdb._sec-authentication-domain._Domain-type = "_oeusertable":U) NO-LOCK no-error.
    if available(dictdb._sec-authentication-domain) then
    do on error undo, throw:
        lFound = buffer dictdb._user:find-first("WHERE dictdb._User._sql-only-user = FALSE":U) .
        catch e as Progress.Lang.Error :
            /* 138 means no ABL user record found.
               Otherwise we assume readpermission error due to disallow blank user access in which
               case we assume a prompt is needed (disallow blank user access cannot be set by blank uer) */
            lFound = e:GetMessageNum(1) <> 138.
        end catch.
    end.
    if not lFound then
    do on error undo, throw:
        for each dictdb._sec-authentication-domain
                 where (dictdb._sec-authentication-domain._Domain-enabled = yes
                   and  dictdb._sec-authentication-domain._Domain-type <> "_oeusertable":U
                   and  dictdb._sec-authentication-domain._Domain-id > -1 ) NO-LOCK:
            find dictdb._sec-authentication-system of dictdb._sec-authentication-domain
                 where (dictdb._sec-authentication-system._PAM-plug-in = yes ) NO-LOCK.
            lFound = yes.
            leave.
            catch e as Progress.Lang.Error :
                /* 138 means no record, we could get readpermission error on which case we
                   assume a prompt is needed  */
                if e:GetMessageNum(1) <> 138 then
                do:
                    lFound = yes.
                    leave.
                end.
            end catch.
        end.
        catch e as Progress.Lang.Error :
            /* 138 means no ABL user record found.
               Otherwise we assume readpermission error due to disallow blank user access in which
               case we assume a prompt is needed (disallow blank user access cannot be set by blank uer) */
            lFound = e:GetMessageNum(1) <> 138.
        end catch.
    end.
    if lFound = false then
        return.
end.
else if not can-find(first DICTDB._User) then
    return.

create client-principal hCP. /* create a CLIENT-PRINCIPAL only once during login*/

DO ON ENDKEY UNDO, LEAVE:
    currentdb = LDBNAME("DICTDB":U).

    /* reset id and password to blank in case of retry */
    ASSIGN id = ""
           domain = ""
           password = "".

/*    if viewAsDialog then                                               */
/*    do:                                                                */
/*                                                                       */
/*      DISPLAY currentdb WITH FRAME logindb_frame view-as dialog-box.   */
/*                                                                       */
/*      UPDATE id password domain ok_btn cancel_btn help_btn {&WHEN_HELP}*/
/*             WITH FRAME logindb_frame view-as dialog-box.              */
/*    end.                                                               */
/*    else do:                                                           */

      oForm = NEW Consultingwerk.Windows.OpenEdgeLogin.DatabaseLoginForm () .

      ASSIGN
        oForm:DatabaseName = currentdb
        oForm:UserId       = id
        oForm:DomainName   = domain
        oForm:Password     = password .

/*      DISPLAY currentdb WITH FRAME login_frame.*/

      WAIT-FOR oForm:ShowDialog () SET oDialogResult .

      IF oForm:OptOut THEN
          oOptOutService:OptOut (DBPARAM ("DICTDB":U)) .

      {Consultingwerk/check-dialogresult-ok.i oDialogResult} .

      ASSIGN id       = oForm:UserId
             domain   = oForm:DomainName
             password = oForm:Password .

/*      UPDATE id password domain ok_btn cancel_btn help_btn {&WHEN_HELP}*/
/*             WITH FRAME login_frame.                                   */
/*    end.*/
    cUserid = id
            + if domain = "":U then "":U else "@":U + domain.

    /* Use SET-DB-CLIENT instead of SETUSERID */
    hCP:initialize(cUserid,?,?,password).

    setdbclnt = set-db-client(hCP,currentdb) NO-ERROR.
    if NOT setdbclnt THEN
    DO:
        MESSAGE "Userid/Password is incorrect."{&TRAN}
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        IF tries > 1 THEN
            QUIT. /* only allow 3 tries*/
        tries = tries + 1.
        UNDO, RETRY.
    END.

END.

FINALLY:
    IF VALID-HANDLE (hCP) THEN
        delete object hCP.
    hcp = ?.

/*    HIDE FRAME login_frame.*/
END FINALLY.


