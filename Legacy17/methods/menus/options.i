/* options.i */

&IF "{&ITEMS}" = "yes" &THEN
DEFINE SUB-MENU m_Options
       {methods/menus/menuitem.i 1 m}
       {methods/menus/menuitem.i 2 m}
       {methods/menus/menuitem.i 3 m}
       {methods/menus/menuitem.i 4 m}
       {methods/menus/menuitem.i 5 m}
       {methods/menus/menuitem.i 6 m}
       {methods/menus/menuitem.i 7 m}
       {methods/menus/menuitem.i 8 m}
       .

DEFINE SUB-MENU p_Options
       {methods/menus/menuitem.i 1 p}
       {methods/menus/menuitem.i 2 p}
       {methods/menus/menuitem.i 3 p}
       {methods/menus/menuitem.i 4 p}
       {methods/menus/menuitem.i 5 p}
       {methods/menus/menuitem.i 6 p}
       {methods/menus/menuitem.i 7 p}
       {methods/menus/menuitem.i 8 p}
       .
&ENDIF

&IF "{&ITEMS}" = "no" &THEN
{methods/menus/menutrig.i 1}
{methods/menus/menutrig.i 2}
{methods/menus/menutrig.i 3}
{methods/menus/menutrig.i 4}
{methods/menus/menutrig.i 5}
{methods/menus/menutrig.i 6}
{methods/menus/menutrig.i 7}
{methods/menus/menutrig.i 8}
&ENDIF
