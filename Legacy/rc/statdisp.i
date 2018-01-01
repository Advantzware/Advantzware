if ws_userid = ? then ws_userid = substring(userid('dictdb'),1,3).
DISPLAY ws_co ws_dept ws_app ws_per ws_userid today @ fill_date
    string(time,'HH:MMam') @ fill_time
WITH FRAME f-stat /* stream-io */.
