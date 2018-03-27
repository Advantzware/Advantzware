/* mattypes.i */
&if defined(item-corr) ne 0 &then
    /* include file for corrugate item maintenance */
    {cec/mattypes.i}
&else 
    {ce/mattypes.i}
&endif
/* ======== moved to ce/mattypes.i } 
&Scoped-define col-coord 1
&Scoped-define row-coord 6.24

&Scoped-define group# 1
&Scoped-define group{&group#}-misc RECT-{&group#} group{&group#}-text
&Scoped-define group{&group#}-fields item.cal item.basis-w item.reg-no ~
item.shrink item.s-wid item.s-len item.r-wid item.ect item.dept-name[1] ~
item.dept-name[2] item.dept-name[3] item.dept-name[4] item.dept-name[5] ~
item.dept-name[6] item.dept-name[7] item.dept-name[8] item.dept-name[9] ~
item.dept-name[10] item.speed%[1] item.speed%[2] item.speed%[3] item.speed%[4] ~
item.speed%[5] item.speed%[6] item.speed%[7] item.speed%[8] item.speed%[9] ~
item.speed%[10] 
&Scoped-define group{&group#} {&group{&group#}-misc} {&group{&group#}-fields}

&Scoped-define group# 2
&Scoped-define group{&group#}-misc RECT-{&group#} group{&group#}-text ~
ink-type-label press-type-label
&Scoped-define group{&group#}-fields item.ink-type item.press-type item.min-lbs ~
item.yield
&Scoped-define group{&group#} {&group{&group#}-misc} {&group{&group#}-fields}

&Scoped-define group# 3
&Scoped-define group{&group#}-misc RECT-{&group#} group{&group#}-text
&Scoped-define group{&group#}-fields item.sqin-lb item.linin-lb
&Scoped-define group{&group#} {&group{&group#}-misc} {&group{&group#}-fields}

&Scoped-define group# 4
&Scoped-define group{&group#}-misc RECT-{&group#} group{&group#}-text
&Scoped-define group{&group#}-fields item.case-l item.case-w item.case-d ~
item.avg-w item.box-case item.case-pall
&Scoped-define group{&group#} {&group{&group#}-misc} {&group{&group#}-fields}

&Scoped-define mat-types {&group1} {&group2} {&group3} {&group4}

DEFINE VARIABLE col-coord AS INTEGER NO-UNDO.
DEFINE VARIABLE row-coord AS INTEGER NO-UNDO.
DEFINE VARIABLE current-widget AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE label-widget AS WIDGET-HANDLE NO-UNDO.


&IF DEFINED(first-time) NE 0 &THEN
&Scoped-define group# 2
{custom/mattypmv.i} 
&Scoped-define group# 3
{custom/mattypmv.i}
&Scoped-define group# 4
{custom/mattypmv.i} 
&UNDEFINE first-time
&ENDIF

HIDE {&mat-types} NO-PAUSE.
CASE {&FIRST-EXTERNAL-TABLE}.mat-type:SCREEN-VALUE:
  &Scoped-define group# 1
  WHEN 'A' OR WHEN 'B' OR WHEN 'P' THEN
  DO:
    
    DISPLAY {&group{&group#}}.
    &IF '{&mat-types-enable}' = 'yes' &THEN
    ENABLE {&group{&group#}-fields}.
    &ENDIF
  END.
  &Scoped-define group# 2
  WHEN 'I' OR WHEN 'P' THEN
  DO:
    DISPLAY {&group{&group#}}.
    &IF '{&mat-types-enable}' = 'yes' &THEN
    ENABLE {&group{&group#}-fields}.
    &ENDIF
  END.
  &Scoped-define group# 3
  WHEN 'G' OR WHEN 'W' OR WHEN 'S' OR WHEN 'L' OR WHEN 'F' OR WHEN 'T' THEN
  DO:
    DISPLAY {&group{&group#}}.
    &IF '{&mat-types-enable}' = 'yes' &THEN
    ENABLE {&group{&group#}-fields}.
    &ENDIF
  END.
  &Scoped-define group# 4
  WHEN 'D' OR WHEN 'C' OR WHEN 'Z' THEN
  DO:
    DISPLAY {&group{&group#}}.
    &IF '{&mat-types-enable}' = 'yes' &THEN
    ENABLE {&group{&group#}-fields}.
    &ENDIF
  END.
END CASE.
&UNDEFINE group#
============================*/
