/* custom/asimailr2.i  include file for reports using txt extention
                       added &group-title instead of using email and RUN xpmail2.p */

&SCOPED-DEFINE type {&type}
&SCOPED-DEFINE group-title {&group-title}
&SCOPED-DEFINE begin_cust {&begin_cust}
&SCOPED-DEFINE end_cust {&end_cust}
&SCOPED-DEFINE mail-subject {&mail-subject}
&SCOPED-DEFINE mail-body {&mail-body}
&SCOPED-DEFINE mail-file {&mail-file}

{custom/asimail.i &two=2}
