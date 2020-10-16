&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*--------------------------------------------------------------------------
    File        : browsercustom.i
    Purpose     : Browser customization based on New Designes

    Syntax      : {methods/template/brwcustom.i}

    Description : Browser customization as per new enhanced UI 


    Author(s)   : Anjly
    Created     : 08/10/20
    Notes       :
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{methods/template/brwcustomdef.i}

frame {&frame-name}:BGCOLOR = 15.
{&BROWSE-NAME}:bgcolor = 25.
{&BROWSE-NAME}:fgcolor = 0.
{&BROWSE-NAME}:separator-fgcolor = 15.
{&BROWSE-NAME}:row-height-chars = 0.84.
{&BROWSE-NAME}:font = 22.


colHand = {&BROWSE-NAME}:first-column.
do while valid-handle(colHand).
    colHandList =  colHandList + ","  + string(colHand).
    colHand = colHand:next-column.
END.
colHandList = trim(colHandList, ",").
 
&if defined(exclude-row-display) = 0 &then
    on 'row-display' of {&BROWSE-NAME} do:
        {methods/template/brwrowdisplay.i}
    end.
&endif




