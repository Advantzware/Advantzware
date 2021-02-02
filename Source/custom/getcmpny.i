/* getcmpny.i */

IF NOT VALID-HANDLE(Persistent-Handle) THEN
RUN nosweat/persist.p PERSISTENT SET Persistent-Handle.

RUN Get-Company IN Persistent-Handle (OUTPUT gcompany).
