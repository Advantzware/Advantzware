/* cec/mattypes.i   like  custom/mattypes.i but for corrugate box*/

&Scoped-define col-coord 1
&Scoped-define row-coord 6.24

&Scoped-define group# 1
&Scoped-define group{&group#}-misc RECT-{&group#} group{&group#}-text
&Scoped-define group{&group#}-fields item.flute item.reg-no item.cal ~
item.basis-w fi_ect item.shrink ~
item.s-wid item.s-len item.r-wid item.dept-name[1] ~
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
item.avg-w item.box-case item.case-pall fi_cas-pal-w fi_flute fi_reg-no
&Scoped-define group{&group#} {&group{&group#}-misc} {&group{&group#}-fields}

&Scoped-define group# 5
&Scoped-define group{&group#}-misc RECT-{&group#} group{&group#}-text
&Scoped-define group{&group#}-FIELDS item.flute item.reg-no item.cal ~
item.basis-w fi_ect item.shrink ~
item.s-dep item.s-wid item.s-len item.r-wid ~
item.density item.color-1 item.dept-name[1] item.dept-name[2] item.dept-name[3] ~
item.dept-name[4] item.dept-name[5] item.dept-name[6] item.dept-name[7] ~
item.dept-name[8] item.dept-name[9] item.dept-name[10] item.speed%[1] ~
item.speed%[2] item.speed%[3] item.speed%[4] item.speed%[5] item.speed%[6] ~
item.speed%[7] item.speed%[8] item.speed%[9] item.speed%[10] 

&Scoped-define group{&group#} {&group{&group#}-misc} {&group{&group#}-fields}

&Scoped-define group# 6
/*&Scoped-define group{&group#}-wax group{&group#}-text */
&Scoped-define group{&group#}-fields item.shrink
&Scoped-define group{&group#} {&group{&group#}-wax} {&group{&group#}-fields}

&Scoped-define mat-types {&group1} {&group2} {&group3} {&group4} {&group5} {&group6}

DEFINE VARIABLE col-coord AS INTEGER NO-UNDO.
DEFINE VARIABLE row-coord AS INTEGER NO-UNDO.
DEFINE VARIABLE current-widget AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE label-widget AS WIDGET-HANDLE NO-UNDO.
define var group6-text as widget-handle no-undo.
&IF DEFINED(first-time) NE 0 &THEN
&Scoped-define group# 2
{custom/mattypmv.i}       
&Scoped-define group# 3
{custom/mattypmv.i} 
&Scoped-define group# 4
{custom/mattypmv.i}      
&Scoped-define group# 5
{custom/mattypmv.i}       
&Scoped-define group# 6  
/*{custom/mattypmv.i}  */
&UNDEFINE first-time
&ENDIF

do with frame {&frame-name}: 
HIDE {&mat-types} NO-PAUSE.

CASE fi_mat-type:SCREEN-VALUE:
  &Scoped-define group# 1
  WHEN 'A' OR WHEN 'B' OR WHEN 'P' THEN
  DO:
    DISPLAY {&group{&group#}}.
    assign
       item.s-wid:screen-value in frame {&frame-name} = string({sys/inc/k16v.i item.s-wid})
       item.s-len:screen-value in frame {&frame-name} = string({sys/inc/k16v.i item.s-len})
       item.r-wid:screen-value in frame {&frame-name} = string({sys/inc/k16v.i item.r-wid})
       fi_ect:screen-value in frame {&frame-name}     = string(item.ect / IF fi_mat-type EQ "P" THEN 10000 ELSE 1).

    &IF '{&mat-types-enable}' = 'yes' &THEN do:
      ENABLE {&group{&group#}-fields}.
    END.
    &ENDIF
  END. 
  &Scoped-define group# 2
  WHEN 'I' OR WHEN 'V' THEN
  DO:
    DISPLAY {&group{&group#}}.
    &IF '{&mat-types-enable}' = 'yes' &THEN
    ENABLE {&group{&group#}-fields}.
    &ENDIF
  END.
  &Scoped-define group# 3
  WHEN 'G' OR WHEN 'S' OR WHEN 'L' OR WHEN 'F' OR WHEN 'T' THEN
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
    assign
       item.case-w:screen-value in frame {&frame-name}  = string({sys/inc/k16v.i item.case-w})
       item.case-l:screen-value in frame {&frame-name}  = string({sys/inc/k16v.i item.case-l})
       item.case-d:screen-value in frame {&frame-name}  = string({sys/inc/k16v.i item.case-d})
       fi_cas-pal-w:screen-value in frame {&frame-name} = string(item.basis-w)
       fi_flute:screen-value in frame {&frame-name}     = item.flute
       fi_reg-no:screen-value in frame {&frame-name}    = item.reg-no.

    &IF '{&mat-types-enable}' = 'yes' &THEN
    ENABLE {&group{&group#}-fields}.
    &ENDIF
  END.
  &Scoped-define group# 5
  WHEN '1' OR WHEN '2' OR WHEN '3' or when '4' THEN
  DO:
    DISPLAY {&group{&group#}}.
    assign
       item.s-wid:screen-value in frame {&frame-name} = string({sys/inc/k16v.i item.s-wid})
       item.s-len:screen-value in frame {&frame-name} = string({sys/inc/k16v.i item.s-len})
       item.s-dep:screen-value in frame {&frame-name} = string({sys/inc/k16v.i item.s-dep})
       item.r-wid:screen-value in frame {&frame-name} = string({sys/inc/k16v.i item.r-wid})
       fi_ect:screen-value in frame {&frame-name}     = string(item.ect / IF fi_mat-type EQ "P" THEN 10000 ELSE 1).

    &IF '{&mat-types-enable}' = 'yes' &THEN
    ENABLE {&group{&group#}-fields}.
    &ENDIF
  END.
  &Scoped-define group# 6
  WHEN 'W' THEN
  DO:
    DISPLAY {&group{&group#}}.
    assign
       item.shrink:label = "Pickup %" 
       item.s-wid:screen-value in frame {&frame-name} = string({sys/inc/k16v.i item.s-wid})
       item.s-len:screen-value in frame {&frame-name} = string({sys/inc/k16v.i item.s-len})
       item.s-dep:screen-value in frame {&frame-name} = string({sys/inc/k16v.i item.s-dep}).

    &IF '{&mat-types-enable}' = 'yes' &THEN
    ENABLE {&group{&group#}-fields}.
    &ENDIF
  END.
  
END CASE.
&UNDEFINE group#

end. /* do with frame */
