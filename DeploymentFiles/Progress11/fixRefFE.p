output to propath.log.
propath = session:param + "," + propath.
put unformatted propath skip.


CONNECT -pf value(session:param + "\" + "asi.pf") .
run fixRef.p.
