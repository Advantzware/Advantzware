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
        <b>Help for Order Entry Browser</b>
    <br />
        <p>
            ABOUT THE ORDER MAINTENANCE BROWSER - The browser screen will import the last thirty open orders sorted by the due date. 
            The customer service representative has the option to search by Order number, Customer number, FG Item number, CustPart number, 
            Customre PO number, Estimate# and Job number. The browsers shows order numbers, order date due date, customer numbers, estimates 
            numbers, job numbers, item numbers and status in the system starting with the most recent and descending to the oldest.
             To view the first/last, click in the order section, then press the END key or HOME key or click the icon with the circling
              arrows to sort ascending, then descending. Alternatively, click any yellow column heading to sort the list of orders by that 
              field.
        </p>
        <p>
            SORT OPTIONS - The order browser works just like the order inquiry whereby the user may sort on any heading listed on the browser.
             Once the browser lists the orders, click on any column heading. Each click will sort the file alternating between ascending then
              descending. To limit the search criteria, enter information in one of the sort fields including Order number, Customer number,
               FG Item number, CustPart number, Customre PO number, Estimate number and Job number. The order number requires an exact match.
                If you enter an invalid number, nothing will appear on the browser. The other field such as FG Item# and Customer Part# will
                 limit the search as you type more characters. Type in 8x8 and all part numbers startin with 8x8 will appear. Type 8x8x8 and 
                 only 8x8x8 items will appear. To further limit the search, you many enter multiple fields. To limit the search to a single
                  customer and a part number that begins with a few letters, enter information in both fields before press ENTER or GO. 
                  Every time you press the ENTER or GO key, the system will import the new search criteria.  
         </p>
         <p>

            SHOW ALL - This button will show all orders in the system from the begining of time. For large databases, this could take a 
            minute or more. For a faster search, limit the search by entering data in one of the sort options.
        </p>
        <p>
            SCROLL BARS - Information is displayed to the right and below the normal screen size, hence the up/down and left/right scroll
             bars will move the screen to the far right or back to the left and also further down the screen. Alternatively, you may press
              the arrow keys to move up or down the screen. You may also, press the page down or page up keys, home key, end key or the icon
               with the arrows. 
        </p>
        <p>
            FOUNTAIN PEN ICON - This icon looks like a fountain pen and is used for adding 
                notes for each order. Each note is date and time stamped and notes may be 
                entered for each order number.
        </p>
        <%--<p>
            BOOK ICON - The fountain pen icon is used for adding notes specifically for a finished goods item.  Secification codes such as
             CUSTOMER SERVICE NOTES must be added to the system administration spec file codes file and the finished goods item must be 
             created before this icon will allow notes by item.
        </p>
        <p>
            COMPUTER LIGHTING BOLT ICON - This allows access to any other menu option in 
                the Advantzware system. Each user may define their own unique menu in the 
                system administration user file maintenance file.
        </p>--%>
        <p>
            QUESTION MARK ICON - This is our on line help for each screen and each field 
            in our system. The  key will also provide the same help key. To view help 
            on a particular screen, make sure that you mouse click inside the screen that 
            you want help. This will ensure that the focus is pointing to that screen.
        </p>
        <%--<p>
            ARROWS ICON - Same function as the Home and End keys, which display the last 
            record or first record respectively for the information listed on the browser.
        </p>--%>
        <p>
            REGARDING CUSTOMER PURCHASE ORDER NUMBERS
                We can have multiple Purchase Orders for each item on an order. We have the Header PO number which becomes a default for
                 each line item added to the order. However, each line item could be a different PO# and may be changed when adding line
                  items. The Line Item PO number serves as a default to the Release PO number, but may be changed on the Release Folder.
                   Finally, each shipment, which we call a release could be a different PO number. In effect, our order entry system 
                   supports blanket orders, whereby the customer may provide a separate PO number for each delivery, which we call a 
                   Release in our system. The RELEASE folder for each line item can house a separate PO number for each delivery. 
                   Please note, If the user changes the release PO number, the system will prompt to change all PO number's for all items.
                    This would make them all the same. 

        </p>
    </div>
    </form>
</body>
</html>
