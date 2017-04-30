/* menuitem.i */

&IF "{&ITEM{1}}" NE "" &THEN
       MENU-ITEM {2}_{&ITEM{1}} LABEL "{&LABEL{1}}"
&ENDIF
