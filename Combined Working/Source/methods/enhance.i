/* enhance.i */

&IF "{&ENHANCE}" NE "no" &THEN
RUN Enhance IN Persistent-Handle (FRAME {&FRAME-NAME}:HANDLE).
&ENDIF
