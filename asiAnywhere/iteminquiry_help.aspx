<%@ Page Language="C#" AutoEventWireup="true" Inherits="order_help" Codebehind="order_help.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head runat="server">
    <title>Order Help</title>
</head>
<body>
    <form id="form1" runat="server">
    <div>
        &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
        <b>Help for Item Inquiry Browser</b>
    <br />
        <p>
            ABOUT THE FINISHED GOODS ITEM INVENTORY BROWSER - The browser screen shows 
all the existing item numbers, names, CAD numbers, estimate numbers, item codes, cat, SPC number and style in the system starting with the most recent and descending to the oldest. 
            To view the first/last, press the END key or HOME key or click the icon with the circling arrows to sort ascending, then descending.
        </p>
        <p>
            SCROLL BARS - Information is displayed to the right and below the normal 
screen size, hence the up/down and left/right scroll bars will move the screen 
to the far right or back to the left and also further down the screen. 
Alternatively, you may press the arrow keys to move up or down the screen. 
You may also, press the page down or page up keys, home key, end key or the 
icon with the arrows.  
         </p>
         <p>

            RADIO BUTTONS - The bottom of the screen has various empty circles, which are 
known as radio buttons for sorting the item numbers, names, CAD numbers, 
estimate numbers, item codes, cat, SPC number and style. Simply click the 
field that you wish to sort upon, then press the type a few letters and the 
program will limit the search based on the exact match of the keystrokes 
entered. For Example, click the item number radio button, then type the item 
number. A list of item numbers starting with that item number will appear in 
the browser. Double click a particular item number or press the down ARROW 
key to select a company. Click on any option and the list will be sorted by 
that field value.
        </p>
        <p>
           AUTO FIND - This is an alpha-numeric search block. Once a radio button has 
been selected to sort the list, simply type a few letters and the list will 
continue to reduce the list. To select a particular item, just double click 
one of the lines listed on the screen. For example, click the item number 
radio button, then type the item number and only the item numbers will appear. 
If you back space then all item numbers beginning with that item will 
reappear. To clear out the characters that were typed, press the Clear Find 
button on the bottom right or delete all the characters.
        </p>
        <p>
            FOUNTAIN PEN ICON - This icon looks like a fountain pen and is used for adding 
notes for each order. Each note is date and time stamped and notes may be 
entered for each order number.
        </p>
        <p>
            BOOK ICON - The fountain pen icon is used for adding notes for the particular 
item specification codes. Codes such as CUSTOMER SERVICE NOTES must be added 
to the system administration spec file codes file and the finished goods item 
must be created before this icon will allow note by spec.
        </p>
        <p>
            COMPUTER LIGHTING BOLT ICON - This allows access to any other menu option in 
the Advantzware system. Each user may define their own unique menu in the 
system administration user file maintenance file.

        </p>
        <p>
            QUESTION MARK ICON -  This is our online help for each screen and each field 
in our system. The [F3] key will also provide the same help key. To view help 
on a particular screen, make sure that you mouse click inside the screen that 
you want help. This will ensure that the focus is pointing to that screen.

        </p>
        <%--<p>
            ARROWS ICON - Same function as the Home and End keys, which display the last 
            record or first record respectively for the information listed on the browser.
        </p>--%>
        <p>
            ARROWS ICON - Same function as the Home and End keys, which display the last 
record or first record respectively.

        </p>
    </div>
    </form>
</body>
</html>
