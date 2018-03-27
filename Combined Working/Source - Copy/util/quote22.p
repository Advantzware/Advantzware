/* util/quote22.p  Create dummy quotehd # 23 to get the error around */

DEF BUFFER bf-q FOR quotehd.

FIND last quotehd.

CREATE bf-q.

BUFFER-COPY quotehd EXCEPT q-no TO bf-q.
bf-q.q-no = 23.

MESSAGE "Completed. " VIEW-AS ALERT-BOX.
