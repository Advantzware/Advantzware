/* .profile should include EDIDIR=whatever,export EDIDIR */

/*
if opsys = "unix" then do:
INPUT THROUGH echo $EDIDIR no-echo.
end.
else do:
input from edidir.dat no-echo.
end.
import ws_edi_path.
input from terminal.
*/

/*
edierr-name = ws_edi_path + "/edierr." + SUBSTRING(STRING(TODAY),1,2) +
  SUBSTRING(STRING(TODAY),4,2).
/* Find full path of editdfo.fil */
OUTPUT STREAM edierr TO VALUE(edierr-name) APPEND NO-ECHO.
OUTPUT STREAM edimsg TO /usr4/dprept/edimsg.out APPEND PAGED.
editdfo-name = ws_edi_path + "/editdfo.fil".
*/

FIND edcode WHERE RECID(edcode) = ws_edcode_rec NO-LOCK NO-ERROR.
IF NOT AVAIL edcode THEN
do:
    ws_erc = -1.
    RETURN.
end.

/*find edmast                                            */
/*where edmast.partner = edcode.partner no-lock no-error.*/
find edco where edco.company = ws_company no-lock no-error.
IF NOT AVAIL edmast THEN
do: ws_erc = -2. RETURN. end.

IF NOT AVAIL edco THEN find first edco no-lock no-error.
if not avail edco then do:
    ws_erc = -3. return. end.


assign
    ws_edmast_rec   = recid(edmast)
    ws_edco_rec     = recid(edco)
    ws_partner      = edmast.partner
    ws_setid        = edcode.setid
    ws_version      = string(edcode.version,"9999")
    ws_direction    = edcode.direction
    .

if edcode.direction = "I" then do:
    ws_edi_path =
        if edcode.path-in > "" then edcode.path-in else
        if edmast.path-in > "" then edmast.path-in else edco.path-in.
    if ws_edi_path = "" then do:
        bell.
        RETURN.
    end.
end.
else do:
    ws_edi_path =
        if edcode.path-out > "" then edcode.path-out else
        if edmast.path-out > "" then edmast.path-out else edco.path-out.
		
    if ws_edi_path = "" then do:
        bell.
        RETURN.
    end.
end.
