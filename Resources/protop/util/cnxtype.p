

for each _connect no-lock where _connect-usr <> ?
    break by _connect-type
&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) >= 10.0 &THEN
          by _connect-clientType
&ENDIF
  :

  if   first-of( _connect-type )
&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) >= 10.0 &THEN
    or first-of( _connect-clientType )
&ENDIF
    then

    display
      _connect-id
      _connect-usr
      _connect-type
&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) >= 10.0 &THEN
      _connect-clientType
&ENDIF
    .

end.

