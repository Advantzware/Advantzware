<%@ Page Language="C#" AutoEventWireup="true" Debug="true" Inherits="add_rfqapp" Codebehind="add_rfqapp.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>RFQ  : Add RFQ</title>    
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
     <link href="include/tree.css" rel="stylesheet" type="text/css" />
    
    <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
     
     <link rel="stylesheet" type="text/css" href="include/help.css">         
    <script language="javascript" src="include/date.js"></script>
    <script language="javascript" src="include/event.js"></script>
    <script language="javascript" src="include/insert.js"></script>
    <script language = "JavaScript" src="include/CalendarControl.js"> 
      
    </script>
    
    
               
      
    
    <script language="javascript">

        var check = "";
        function customerlook(obj) {
            check = obj;
            var NewWindow = window.open("customer_lookup.aspx", "CustomerLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }

        function CustomerLookup(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5, ReturnObj6, ReturnObj7, ReturnObj8, ReturnObj9, ReturnObj10, ReturnObj11) {
            document.forms[0].FormView1$cust_noTextBox.value = ReturnObj1;
            document.forms[0].FormView1$ship_nameTextBox.value = ReturnObj2;
            //document.forms[0].FormView1$fob_codeTextBox.value = ReturnObj3;
            document.forms[0].FormView1$smanNameTextBox.value = ReturnObj4;
            document.forms[0].FormView1$ship_cityTextBox.value = ReturnObj5;
            document.forms[0].FormView1$ship_stateTextBox.value = ReturnObj6;
            document.forms[0].FormView1$ship_zipTextBox.value = ReturnObj7;
            document.forms[0].FormView1$smanTextBox.value = ReturnObj8;
            document.forms[0].FormView1$commTextBox.value = ReturnObj9;
            document.forms[0].HiddenFieldComm.value = ReturnObj9;
            document.forms[0].FormView1$shipAddrTextBox.value = ReturnObj10;
            document.forms[0].FormView1$shipAddr2TextBox.value = ReturnObj11;
            //document.forms[0].FormView1$fob_codeTextBox.value = ReturnObj12;

            if (check == "2") {
                if (ReturnObj3 == "DEST") {
                    document.forms[0].FormView1_RD5.checked = true;
                }
                else {
                    document.forms[0].FormView1_RD6.checked = true;
                }
            }
            else {
                /*if (ReturnObj3 == "DEST") sss{
                document.forms[0].FormView1_RadioButtonList1.SelectedValue = "D";
                }
                else {
                document.forms[0].FormView1_RadioButtonList1.SelectedValue = "O";
                }*/
            }
            document.forms[0].FormView1$cust_noTextBox.onchange();

        }



        function customer2look() {
            var NewWindow = window.open("customer2_lookup.aspx", "CustomerLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }

        function customer2look() {
            var NewWindow = window.open("customer2_lookup.aspx", "CustomerLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }

        function Customer2Lookup(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5, ReturnObj6, ReturnObj7, ReturnObj8, ReturnObj9, ReturnObj10) {
            if (document.forms[0].FormView2$cust_noTextBox) {
                document.forms[0].FormView2$cust_noTextBox.value = ReturnObj1;
            }
            else {
                document.forms[0].FormView1$cust_noTextBox.value = ReturnObj1;
            }
            if (document.forms[0].FormView2$ship_nameTextBox) {
                document.forms[0].FormView2$ship_nameTextBox.value = ReturnObj2;
            }
            else {
                document.forms[0].FormView1$ship_nameTextBox.value = ReturnObj2;
            }
            //document.forms[0].FormView1$fob_codeTextBox.value = ReturnObj3;
            if (document.forms[0].FormView2$smanNameTextBox) {
                document.forms[0].FormView2$smanNameTextBox.value = ReturnObj4;
            }
            else {
                document.forms[0].FormView1$smanNameTextBox.value = ReturnObj4;
            }
            if (document.forms[0].FormView2$ship_cityTextBox) {
                document.forms[0].FormView2$ship_cityTextBox.value = ReturnObj5;
            }
            else {
                document.forms[0].FormView1$ship_cityTextBox.value = ReturnObj5;
            }

            if (document.forms[0].FormView2$ship_stateTextBox) {
                document.forms[0].FormView2$ship_stateTextBox.value = ReturnObj6;
            }
            else {
                document.forms[0].FormView1$ship_stateTextBox.value = ReturnObj6;
            }

            if (document.forms[0].FormView2$ship_zipTextBox) {
                document.forms[0].FormView2$ship_zipTextBox.value = ReturnObj7;
            }
            else {
                document.forms[0].FormView1$ship_zipTextBox.value = ReturnObj7;
            }

            if (document.forms[0].FormView2$smanTextBox) {
                document.forms[0].FormView2$smanTextBox.value = ReturnObj8;
            }
            else {
                document.forms[0].FormView1$smanTextBox.value = ReturnObj8;
            }

            if (document.forms[0].FormView2$shipAddrTextBox) {
                document.forms[0].FormView2$shipAddrTextBox.value = ReturnObj9;
            }
            else {
                document.forms[0].FormView1$shipAddrTextBox.value = ReturnObj9;
            }

            if (document.forms[0].FormView2$shipAddr2TextBox) {
                document.forms[0].FormView2$shipAddr2TextBox.value = ReturnObj10;
            }
            else {
                document.forms[0].FormView1$shipAddr2TextBox.value = ReturnObj10;
            }


            if (ReturnObj3 == "DEST") {
                document.forms[0].FormView1_RD5.checked = true;
            }
            else {
                document.forms[0].FormView1_RD6.checked = true;
            }
        }


        function customer3look() {
            var NewWindow = window.open("customer3_lookup.aspx", "CustomerLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }

        function Customer3Lookup(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5, ReturnObj6, ReturnObj7, ReturnObj8, ReturnObj9, ReturnObj10) {
            document.forms[0].FormView2$cust_noTextBox.value = ReturnObj1;
            document.forms[0].FormView2$ship_nameTextBox.value = ReturnObj2;
            //document.forms[0].HiddenField1.value = ReturnObj3;
            document.forms[0].FormView2$smanNameTextBox.value = ReturnObj4;
            document.forms[0].FormView2$ship_cityTextBox.value = ReturnObj5;
            document.forms[0].FormView2$ship_stateTextBox.value = ReturnObj6;
            document.forms[0].FormView2$ship_zipTextBox.value = ReturnObj7;
            document.forms[0].FormView2$smanTextBox.value = ReturnObj8;
            document.forms[0].FormView2$shipAddrTextBox.value = ReturnObj9;
            document.forms[0].FormView2$shipAddr2TextBox.value = ReturnObj10;
            document.forms[0].FormView2$cust_noTextBox.onchange();

        }


        function smanlook() {
            var NewWindow = window.open("sman_lookup.aspx", "SalesmanLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }

        function SalesManLookup(ReturnObj1, ReturnObj2, ReturnObj3) {
            document.forms[0].FormView1$smanTextBox.value = ReturnObj1;
            document.forms[0].FormView1$smanNameTextBox.value = ReturnObj2;
            if (document.forms[0].FormView1$commTextBox) {
                document.forms[0].FormView1$commTextBox.value = ReturnObj3;
                document.forms[0].HiddenFieldComm.value = ReturnObj3;
            }
            document.getElementById("FormView1_smanTextBox").focus();
        }

        function smanlook2() {
            var NewWindow = window.open("sman_lookup2.aspx", "SalesmanLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }

        function SalesManLookup2(ReturnObj1, ReturnObj2, ReturnObj3) {
            document.forms[0].FormView2$smanTextBox.value = ReturnObj1;
            document.forms[0].FormView2$smanNameTextBox.value = ReturnObj2;

        }

        function Datelook() {
            var NewWindow = window.open("date_lookup.aspx", "DateLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        function Datelookup(obj) {
            document.forms[0].FormView1_req_dateTextBox.value = obj;
        }

        function Datelook2() {
            var NewWindow = window.open("date_lookup2.aspx", "DateLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        function Datelookup2(obj) {
            document.forms[0].FormView1_due_dateTextBox.value = obj;
        }

        function setestdate() {

            var da = document.getElementById("FormView1_req_dateTextBox");
            da.focus();

        }

        function setestdate2() {

            var da = document.getElementById("FormView1_due_dateTextBox");
            da.focus();

        }

        function Datelook4() {
            var NewWindow = window.open("date_lookup4.aspx", "DateLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        function Datelookup4(obj) {
            document.forms[0].FormView2_req_dateTextBox.value = obj;
        }

        function Datelook5() {
            var NewWindow = window.open("date_lookup5.aspx", "DateLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        function Datelookup5(obj) {
            document.forms[0].FormView2_due_dateTextBox.value = obj;
        }

        function Datelook1() {
            document.forms[0].FormView1_req_dateTextBox.value = "";
            Datelook();
        }
        function Datelook3() {
            document.forms[0].FormView1_due_dateTextBox.value = "";
            Datelook2();
        }

        function Datelook6() {
            document.forms[0].FormView2_req_dateTextBox.value = "";
            Datelook4();
        }
        function Datelook7() {
            document.forms[0].FormView2_due_dateTextBox.value = "";
            Datelook5();
        }


</script>
<script type="text/javascript">
    function showQty() {
        var show = document.getElementById("Qtyhelp");
        show.style.display = 'inline';
        var originalqty = document.getElementById("FormView2_RfqQtyTextBox");
        var qty = document.getElementById("FormView2_TextBox1");
        var style = document.getElementById("FormView2_RfqstyleTextBox");

        if (originalqty.value <= 0 || isNaN(originalqty.value)) {
            alert("Qty must be greater than 0");
            originalqty.focus();
        }
        else {
            qty.value = originalqty.value;
            document.forms[0].FormView2$lv_delivery_1.value = "1";
            document.forms[0].FormView2$lv_uom_1.value = "M";
            qty.focus();
        }
    }
    function openstyle() {
        var NewWindow = window.open("style_bro.aspx", "StyleBroWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    function defaultVal(val) {
        var str = "FormView2$lv_qty_" + val;
        qtyval = eval("document.forms[0]." + str + ".value");

        if (parseFloat(qtyval) > 0) {
            eval("document.forms[0].FormView2$lv_delivery_" + val + ".value=1");
            var uomval = eval("document.forms[0].FormView2$lv_uom_" + val);
            uomval.value = "M";
        }
        else if (parseFloat(qtyval) <= 0) {
            eval("document.forms[0].FormView2$lv_delivery_" + val + ".value=");
            var uomval = eval("document.forms[0].FormView2$lv_uom_" + val);
            uomval.value = "";
        }
    }

    function reverseqty() {

        var originalqty = document.getElementById("FormView2_RfqQtyTextBox");
        var qty = document.getElementById("FormView2_TextBox1");
        originalqty.value = qty.value;
    }

    function QPartlook() {
        var NewWindow = window.open("QFgItemLook.aspx?from=custpart", "FgItemWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    function QPartLookup(ReturnObj1) {
        document.forms[0].FormView2$RfqPartnoTextBox.value = ReturnObj1;
        document.getElementById("FormView2_RfqPartnoTextBox").focus();

    }
    function QFgItemlook() {
        var NewWindow = window.open("QFgItemLook.aspx?from=fgitm", "FgItemWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    function QFgItemLookUp(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5, ReturnObj6, ReturnObj7, ReturnObj8, ReturnObj9, ReturnObj10, ReturnObj11, ReturnObj12, ReturnObj13) {
        document.forms[0].FormView2$RfqStockTextBox.value = ReturnObj1;
        document.forms[0].FormView2$RfqNameTextBox.value = ReturnObj2;
        document.forms[0].FormView2$RfqPartnoTextBox.value = ReturnObj3;
        document.forms[0].FormView2$RfqstyleTextBox.value = ReturnObj4;
        document.forms[0].FormView2$RfqProcatTextBox.value = ReturnObj5;
        document.forms[0].FormView2$RfqColTextBox.value = ReturnObj6;
        document.forms[0].FormView2$RfqCoatTextBox.value = ReturnObj7;
        document.forms[0].FormView2$RfqLengthTextBox.value = ReturnObj8;
        document.forms[0].FormView2$RfqWidthTextBox.value = ReturnObj9;
        document.forms[0].FormView2$RfqDepthTextBox.value = ReturnObj10;

        document.forms[0].FormView2$HiddenField1.value = ReturnObj11;
        document.forms[0].FormView2$RfqBoardTextBox.value = ReturnObj12;
        document.forms[0].FormView2$RfqCalTextBox.value = ReturnObj13;
        document.getElementById("FormView2_RfqPartnoTextBox").focus();

    }
    function stylelook() {
        var NewWindow = window.open("StyleLookup.aspx", "StyleWindow", "width=600,height=650,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    function styleLookUp(ReturnObj1) {
        document.forms[0].FormView2$RfqstyleTextBox.value = ReturnObj1;
        document.getElementById("FormView2_RfqstyleTextBox").focus();

    }
    function Boardlook1() {
        var NewWindow = window.open("BoardLook1.aspx", "BoardWindow", "width=500,height=300,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    function BoardLookUp1(ReturnObj1, ReturnObj2) {
        document.forms[0].FormView2$RfqBoardTextBox.value = ReturnObj1;
        document.forms[0].FormView2$RfqCalTextBox.value = ReturnObj2;
        document.getElementById("FormView2_RfqBoardTextBox").focus();
    }
    function categorylookup() {
        var NewWindow = window.open("CategoryLookup.aspx", "CategoryWindow", "width=600,height=420,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    function categoryLookUp(ReturnObj1) {
        document.forms[0].FormView2$RfqProcatTextBox.value = ReturnObj1;
        document.getElementById("FormView2_RfqProcatTextBox").focus();
    }

    function validate(evt) {
        var charcode = (evt.which) ? evt.which : event.keyCode
        if (charcode > 31 && (charcode < 48 || charcode > 57))
            return false;

        return true;


    }
    var prevval = "";
    function getprevval(obj) {
        prevval = obj.value;
    }

    function checkNumVal(obj, tval) {
        if (isNaN(obj.value)) {
            if (tval == "q") {
                alert("Please Enter Valid Quantity");
                //obj.value = prevval;
                obj.focus();
                return;
            }
            if (tval == "d") {
                alert("Please Enter Valid Delivery");
                obj.value = prevval;
            }
            if (tval == "p") {
                alert("Please Enter Valid Price");
                obj.value = prevval;
            }
        }


    }

    function displayqty(obj, tval) {

        if (parseInt(obj.value) <= 0 || obj.value == "") {

            var show = document.getElementById("Qtyhelp");
            var desc = document.getElementById("FormView2_RfqNameTextBox");
            var fgitem = document.getElementById("FormView2_RfqStockTextBox");
            show.style.display = 'none';

            if (tval == "1") {
                desc.focus();
            }
            else fgitem.focus();

        }
    }

    function checkUom(obj) {
        var uomval = (obj.value).toUpperCase();

        if (uomval != "" && uomval != "M" && uomval != "EA" && uomval != "L" && uomval != "CS" && uomval != "C" && uomval != "LB" && uomval != "DRM" && uomval != "ROL" && uomval != "PKG" && uomval != "SET" && uomval != "DOZ" && uomval != "BDL") {
            alert("Invalid UOM");
            obj.value = prevval;
            return;
        }
    }

    function checkNumeric(objName, comma, period) {
        var numberfield = objName;
        if (chkNumeric(objName, comma, period) == false) {
            numberfield.select();
            numberfield.focus();
            return false;
        }
        else {
            return true;
        }
    }

    function chkNumeric(objName, comma, period) {
        var checkOK = "0123456789" + comma + period;
        var checkStr = objName;
        var allValid = true;
        var decPoints = 0;
        var allNum = "";

        for (i = 0; i < checkStr.value.length; i++) {
            ch = checkStr.value.charAt(i);
            for (j = 0; j < checkOK.length; j++)
                if (ch == checkOK.charAt(j))
                break;
            if (j == checkOK.length) {
                allValid = false;
                break;
            }
            if (ch != ",")
                allNum += ch;
        }
        if (!allValid) {
            alertsay = "Please enter only these values \""
            alertsay = alertsay + checkOK + "\" in the \"" + checkStr.name + "\" field."
            alert(alertsay);
            return (false);
        }
    }
    //if(checkStr.value<=0)
    //{
    //document.getElementById("FormView2_Label7").innerHTML="Must have some value";

    //checkStr.focus();
    //}
    //else
    //{
    //document.getElementById("FormView2_Label7").innerHTML="";
    //}
    //if(checkStr.value<=0)
    //{
    //document.getElementById("FormView2_Label8").innerHTML="Must have some value";
    //checkStr.focus();
    //}
    //else
    //{
    //document.getElementById("FormView2_Label8").innerHTML="";
    //}

    var val1 = "";

    function uomlook(value) {
        val1 = value;
        var NewWindow = window.open("Uom_lookup.aspx", "UomLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function UomLookup(ReturnObj1, ReturnObj2) {
        var uomval = eval("document.forms[0].FormView2$lv_uom_" + val1);
        uomval.value = ReturnObj1;
        document.getElementById("FormView2_lv_uom_" + val1).focus();
    }


    function decimalval() {

        var positionOfC = document.forms[0].FormView2$RfqLengthTextBox.value;
        var positionOfC1 = document.forms[0].FormView2$RfqWidthTextBox.value;
        var positionOfC2 = document.forms[0].FormView2$RfqDepthTextBox.value;

        if (positionOfC > 1) {

            var val = positionOfC.indexOf(".");
            if (val == -1) {
                if (positionOfC.length > 1 && positionOfC.length < 3)
                    document.forms[0].FormView2$RfqLengthTextBox.value = positionOfC + ".";
            }
        }

        if (positionOfC1 > 1) {
            var val = positionOfC1.indexOf(".");
            if (val == -1) {
                if (positionOfC1.length > 1 && positionOfC1.length < 3)
                    document.forms[0].FormView2$RfqWidthTextBox.value = positionOfC1 + ".";
            }
        }

        if (positionOfC2 > 1) {
            var val = positionOfC2.indexOf(".");
            if (val == -1) {
                if (positionOfC2.length > 1 && positionOfC2.length < 3)
                    document.forms[0].FormView2$RfqDepthTextBox.value = positionOfC2 + ".";
            }
        }
    }
</script>

    </head>
<body>
    <form id="form1" runat="server">
    <hd:header id="Header1" runat="server"></hd:header>
    <asp:ScriptManager ID="ScriptManager1" runat="server">
            </asp:ScriptManager>
     <asp:HiddenField ID="HiddenField1" runat="server" />
    <asp:HiddenField ID="HiddenField2" runat="server" />
    <asp:HiddenField ID="HiddenField3" runat="server" />
    <asp:HiddenField ID="HiddenField4" runat="server" />
    <asp:HiddenField ID="HiddenField5" runat="server" />
    <asp:HiddenField ID="HiddenField6" runat="server" />
    <asp:HiddenField ID="HiddenField7" runat="server" />
    <asp:HiddenField ID="HiddenField8" runat="server" />
    <asp:HiddenField ID="HiddenField9" runat="server" />
    <asp:HiddenField ID="HiddenField10" runat="server" />
    <asp:HiddenField ID="HiddenField11" runat="server" />
    <asp:HiddenField ID="HiddenCarr" runat="server" />
    <asp:HiddenField ID="HiddenTerm" runat="server" />
    <asp:HiddenField ID="HiddenTermdesc" runat="server" />
    <asp:HiddenField ID="Hiddensold" runat="server" />
    
    <asp:HiddenField ID="HiddenQuoteQty" runat="server" />
    <asp:HiddenField ID="HiddenQuotePrice" runat="server" />
    <asp:HiddenField ID="HiddenQuoteUom" runat="server" />
    <asp:HiddenField ID="HiddenQuoteNum" runat="server" />  
    <asp:HiddenField ID="HiddenFieldFgItem" runat="server" />  
    <asp:HiddenField ID="TaxesHiddenField" runat="server" />
<asp:HiddenField ID="TotHiddenField" runat="server" />
<asp:HiddenField ID="CosHiddenField" runat="server" />
    
        
            
     <table width="100%">
    <tr><td>
    <div>
     <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
          
          <TD align=left nowrap><font size=+0><b>Add RFQ  &nbsp; </b></font></TD>
          <TD vAlign="middle">
            <asp:linkbutton id="LinkButton1" runat="server" OnClick="LinkButton1_Click"></asp:linkbutton>
          </TD>
          <TD align="right"><font size=+0><b>Users&nbsp;&nbsp;</b></font></TD>
          <TD vAlign="middle" align="left">Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
                        
            Company:&nbsp;
            <asp:label id="lblComp" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
          </TD>
          
                    
          <TD vAlign="middle" width="20">&nbsp;</TD>
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>   
    
   <%-- <table>
    <tr style="background-color:Gray">
    <td>
    <asp:LinkButton ID="lnk_list" runat="server" OnClick="lnk_list_click"><img src="Images/list_receipt_0.jpg" border="0" alt="List Receipt" /></asp:LinkButton>
    <asp:LinkButton ID="lnk_view" runat="server" OnClick="lnk_view_click"><img src="Images/view_reciept_1.jpg" border="0" alt="View Receipt" /></asp:LinkButton>
    </td>
    </tr></table>--%>
    
     <asp:HiddenField ID="HiddenFieldComm" runat="server" />
    <asp:HiddenField ID="HiddenFieldsman" runat="server" />
    <asp:HiddenField ID="HiddenFieldsname" runat="server" />                
      
    
    

    <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource1" OnDataBound="FormView1_DataBound" EmptyDataText="No Record Found" OnUnload="FormView1_Unload" OnPreRender="FormView1_PreRender">
        <ItemTemplate>
         <asp:Panel ID="Edit_Panel" runat="server" DefaultButton="UpdatButton">
        <fieldset style="background-color:#EFF3FB;">
        <table>
        <tr>
        <td align="right" style="padding-right:5px;"><b>RFQ#:</b></td>
        <td style="width: 178px"><b><asp:Label ID="Label1" BackColor="Turquoise" Width="170px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("rfq_no") %>'></asp:Label></b></td>
        <td align="right" style="width: 115px;padding-right:5px;"><b>Requested Date:</b></td>
        <td><b><asp:Label ID="Label2" runat="server" BackColor="Turquoise" Width="80px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("req_date","{0:MM/dd/yyyy}") %>'></asp:Label></b></td>
        
        </tr>
        <tr>
        <td align="right" style="padding-right:5px;" ><b>Cust#:</b></td>
        <td style="width: 178px;"><b><asp:Label ID="Label4" BackColor="Turquoise" Width="170px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("cust_no") %>'></asp:Label></b></td>
        <td align="right" style="padding-right:5px;"><b>Due Date:</b></td>
        <td><b><asp:Label ID="Label3" runat="server" BackColor="Turquoise" Width="80px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("due_date","{0:MM/dd/yyyy}") %>'></asp:Label></b></td>
        
        </tr>
        <tr>
        <td align="right" style="padding-right:5px;"><b>Cust Name:</b></td>
        <td style="width: 178px"><b><asp:Label ID="Label6" BackColor="Turquoise" Width="170px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("ship_name") %>'></asp:Label>
         </b></td>
        <td align="right" style="padding-right:5px;"><b>FOB:</b></td>
        <td style="height: 23px"><b><asp:RadioButton ID="RD5" Enabled="false" runat="server" />Destination
            <asp:Label ID="Label5" Visible="false" runat="server" Text='<%# Bind("fob_code") %>'></asp:Label>
            <asp:RadioButton ID="RD6" Enabled="false" runat="server" />Origin</b></td>       
        
        </tr>
        <tr>
        <td align="right" style="padding-right:5px;"><b>Address:</b></td>
        <td style="width: 178px"><b><asp:Label ID="Label8" BackColor="Turquoise" Width="170px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("shipAddr") %>'></asp:Label></b></td>
        <td align="right" style="padding-right:5px;"><b>Freight Charge:</b></td>
        <td><b><asp:Label ID="Label7" BackColor="Turquoise" Width="140px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("chg_method") %>'></asp:Label></b></td>
        
        </tr>
        <tr>
        <td><b></b></td>
        <td style="width: 178px"><b><asp:Label ID="Label10" runat="server" BackColor="Turquoise" Width="170px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("shipAddr2") %>'></asp:Label></b></td>
        <td align="right" style="padding-right:5px;"><b>Warehouse Month:</b></td>
        <td><b><asp:Label ID="Label9" runat="server" BackColor="Turquoise" Width="140px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("wh_month") %>'></asp:Label></b></td>
       
        </tr>
        <tr>
        <td nowrap  align="right" style="padding-right:5px;"><b>City:</b></td>
        <td style="width: 178px"><b><asp:Label ID="Label11" runat="server" BackColor="Turquoise" Width="70px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("ship_city") %>'></asp:Label>
         <asp:Label ID="Label12" BackColor="Turquoise" Width="55px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("ship_state") %>'>
            </asp:Label>
           <asp:Label ID="Label13" runat="server" BackColor="Turquoise" Width="35px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("ship_zip") %>'></asp:Label> 
        </b></td>
        <td align="right" style="padding-right:5px; "><b><asp:Label ID="commlabel" runat="server" Text="Comm%:"></asp:Label></b></td>
        <td ><b><asp:Label ID="Label15" runat="server" BackColor="Turquoise" Width="140px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("comm","{0:###,###,##0.00}") %>'></asp:Label></b></td>
        
        </tr>
        <tr>
        <td align="right" style="padding-right:5px; height: 44px;"><b>Sales Rep:</b></td>
        <td style="width: 178px; height: 44px;"><b><asp:Label ID="Label14" BackColor="Turquoise" Width="115px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("sman") %>'></asp:Label>
        <asp:Label ID="Label17" BackColor="Turquoise" Width="115px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("smanName") %>'></asp:Label>
        </b></td>
                
        </tr></table>
        <table>
        <tr>
        <td style="width: 338px"><b>Special Instructions:</b></td></tr>
        <tr>
        <td><b><asp:TextBox ID="Label16" Font-Bold="true" Height="50px" runat="server" TextMode="MultiLine" ReadOnly="true" BackColor="Turquoise" Width="450px" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("inst") %>'></asp:TextBox></b></td>
        </tr>
        </table>
        <br />
        <br />
        &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
        &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
        <asp:Button ID="UpdatButton" runat="server" OnClick="F1UpdateButton_Click" CommandName="Edit" CssClass="buttonM" Text="Update" />       
        <asp:Button ID="NewButton" runat="server" CausesValidation="False" CssClass="buttonM" CommandName="New" Text="Add">
        </asp:Button> 
                <asp:Button ID="DeleteButton" runat="server"  CssClass="buttonM" OnClick="Delete_Rfqview"
                    Text="Delete" OnClientClick="return confirm('Are you sure you want to delete this record')"> </asp:Button> 
        <asp:Button ID="ViewButton" runat="server" CssClass="buttonM" Text="View Rfq" OnClick="View_Rfqview"/>
        </fieldset>
        
        </asp:Panel>
          
        </ItemTemplate>
        <EditItemTemplate>
         <asp:Panel ID="Edit_Panel" runat="server" DefaultButton="UpdateButton">
        <fieldset style="background-color:#EFF3FB">
        <table>
        <tr>
        <td><b>RFQ#:</b></td>
        <td width="180px"><b><asp:Label ID="rfq_noTextBox" runat="server" Text='<%# Bind("rfq_no") %>'>
            </asp:Label></b></td>
        <td><b>Requested Date:</b></td>
        <td width="140px"><b><asp:TextBox ID="req_dateTextBox"  onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );"  MaxLength="10" Width="70px" runat="server" Text='<%# Bind("req_date","{0:MM/dd/yyyy}") %>'>
            </asp:TextBox>
            <a href="#" onblur="setestdate()" tabindex="1" onClick="showCalendarControl(FormView1_req_dateTextBox); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            
            </b>
            </td>
        
        </tr>
        <tr>
        <td><b>Cust#:</b></td>       
        <td nowrap><b><asp:TextBox ID="cust_noTextBox" AutoPostBack="true" OnTextChanged="cust_change_textbox" MaxLength="8" runat="server" Text='<%# Bind("cust_no") %>'>
            </asp:TextBox>
            <a href="#" tabindex="1" onClick="customerlook('2'); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a></b></td>            
        <td><b>Due Date:</b></td>
        <td><b><asp:TextBox ID="due_dateTextBox" runat="server" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );"  MaxLength="10" Width="70px" Text='<%# Bind("due_date","{0:MM/dd/yyyy}") %>'>
            </asp:TextBox></b>
            <a href="#" onblur="setestdate2()" tabindex="1" onClick="showCalendarControl(FormView1_due_dateTextBox); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            
            </td>
        
        
        </tr>
        <tr>
        <td><b>Cust Name:</b></td>
        <td><b><asp:TextBox ID="ship_nameTextBox" runat="server" Text='<%# Bind("ship_name") %>'>
            </asp:TextBox></b></td>
        <td><b>FOB:</b></td>
        <td><b><asp:TextBox ID="fob_codeTextBox" Visible="false" runat="server" Text='<%# Bind("fob_code") %>'></asp:TextBox>
           <asp:RadioButton ID="RD5" GroupName="editstatus" runat="server" />Destination
           <asp:RadioButton ID="RD6" GroupName="editstatus" runat="server" />Origin</b></td>
        
        </tr>
        <tr>
        <td><b>Address:</b></td>
        <td><b><asp:TextBox ID="shipAddrTextBox" runat="server" Text='<%# Bind("shipAddr") %>'>
            </asp:TextBox></b></td>
        <td><b>Freight Charge:</b></td>
        <td><b><asp:DropDownList ID="chg_methodTextBox" runat="server" SelectedValue='<%# Bind("chg_method") %>' DataTextField='<%# Bind("chg_method") %>'>
         <asp:ListItem>Prepaid</asp:ListItem>
        <asp:ListItem>Collect</asp:ListItem>
        <asp:ListItem>Bill</asp:ListItem>
        <asp:ListItem>Third Party</asp:ListItem>
            </asp:DropDownList></b></td>
        
        </tr>
        <tr>
        <td><b></b></td>
        
        <td><b><asp:TextBox ID="shipAddr2TextBox" runat="server" Text='<%# Bind("shipAddr2") %>'>
            </asp:TextBox></b></td>
            <td><b>Warehouse Month:</b></td>
        <td><b><asp:DropDownList ID="wh_monthTextBox" runat="server" SelectedValue='<%# Bind("wh_month") %>' DataTextField='<%# Bind("wh_month") %>'>
        <asp:ListItem>1</asp:ListItem>
        <asp:ListItem>2</asp:ListItem>
        <asp:ListItem>3</asp:ListItem>
        <asp:ListItem>4</asp:ListItem>
        <asp:ListItem>5</asp:ListItem>
        <asp:ListItem>6</asp:ListItem>
        <asp:ListItem>7</asp:ListItem>
        <asp:ListItem>8</asp:ListItem>
        <asp:ListItem>9</asp:ListItem>
        <asp:ListItem>10</asp:ListItem>
        <asp:ListItem>11</asp:ListItem>
        <asp:ListItem>12</asp:ListItem>
        <asp:ListItem>13</asp:ListItem>
        <asp:ListItem>14</asp:ListItem>
        <asp:ListItem>15</asp:ListItem>
        <asp:ListItem>16</asp:ListItem>
        <asp:ListItem>17</asp:ListItem>
        <asp:ListItem>18</asp:ListItem>
        <asp:ListItem>19</asp:ListItem>
        <asp:ListItem>20</asp:ListItem>
        <asp:ListItem>21</asp:ListItem>
        <asp:ListItem>22</asp:ListItem>
        <asp:ListItem>23</asp:ListItem>
        <asp:ListItem>24</asp:ListItem>
            </asp:DropDownList></b></td>
        </tr>
        <tr>
        <td><b>City:</b></td>
        <td nowrap><b> <asp:TextBox ID="ship_cityTextBox" Width="100px" runat="server" Text='<%# Bind("ship_city") %>'>
            </asp:TextBox>
           <asp:TextBox ID="ship_stateTextBox" runat="server" Width="50px" Text='<%# Bind("ship_state") %>'>
            </asp:TextBox>
            <asp:TextBox ID="ship_zipTextBox" runat="server" Width="30px" Text='<%# Bind("ship_zip") %>'>
            </asp:TextBox></b></td>
        <td><b><asp:Label ID="commlabel" runat="server" Text="Comm%:"></asp:Label></b></td>
        <td><b><asp:TextBox ID="commTextBox" MaxLength="6" Width="50px" runat="server" Text='<%# Bind("comm") %>'>
            </asp:TextBox></b>
            <asp:CompareValidator ID="CompareValidator1" runat="server" ErrorMessage="Only decimal values" ControlToValidate="commTextBox" SetFocusOnError="true" Display="dynamic" Operator="DataTypeCheck" Type="double"></asp:CompareValidator>
            </td>
        </tr>
        <tr>
        <td><b>Sales Rep:</b></td>
        <td><b> <asp:TextBox ID="smanTextBox" MaxLength="3" Width="40px" runat="server" Text='<%# Bind("sman") %>'>
            </asp:TextBox><a href="#" tabindex="1" onClick="smanlook(); return false"><asp:Image ID="SalesRep" runat="server" ImageUrl="images/lookup_icon.gif" /></a></b>
           <asp:TextBox ID="smanNameTextBox" Width="110px" runat="server" Text='<%# Bind("smanName") %>'>
            </asp:TextBox> </b></td>
        <td><b></b></td>
        <td><b></b></td>
        
        </tr>
       
        </table>
        <table>
        <tr>
        <td><b>Special Instruction:</b></td>
        <td><b></b></td>
        <td><b></b></td>
        <td><b></b></td>
        <td><b></b></td>
        <td><b></b></td>
        </tr>
        <tr>
        <td><b><asp:TextBox ID="instTextBox" Font-Bold="true" Height="50px" Width="450px" TextMode="multiline" runat="server" Text='<%# Bind("inst") %>'>
            </asp:TextBox></b></td>
        </tr>
        </table>
        </fieldset>
<input type="hidden"  name="VRowid" value='<%# Server.UrlEncode(Convert.ToString(DataBinder.Eval(Container,"DataItem.VRowid"))) %>' />
            
            <asp:Button ID="UpdateButton" runat="server" Font-Bold="true" CssClass="buttonM" OnClick="UpdateButon_click"
                Text="Save">
            </asp:Button>
            <asp:Button ID="UpdateCancelButton" Font-Bold="true" CssClass="buttonM" runat="server"  CausesValidation="False" CommandName="Cancel"
                Text="Cancel" OnClick="UpdateButton_Cancel_Click">
            </asp:Button>
             </asp:Panel>
        </EditItemTemplate>
        <InsertItemTemplate>
        <asp:Panel ID="Insert_Panel" runat="server" DefaultButton="InsertButton">
        <fieldset style="background-color:#EFF3FB;">
        <table>
        <tr>
        <td ><b>RFQ:</b></td>
        <td><b><asp:Label ID="rfq_noTextBox" BackColor="PaleTurquoise" Width="50px" BorderColor="White" BorderStyle="Solid" BorderWidth="1px"  runat="server" Text='<%# Bind("rfq_no") %>'>
            </asp:Label></b></td>
        
        <td><b>Requested Date:</b></td>
        <td width="180px"><b><asp:TextBox ID="req_dateTextBox" Width="80px" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );"  MaxLength="10" runat="server" Text='<%# Bind("req_date") %>'>
            </asp:TextBox></b>
            <a href="#" onblur="setestdate()" tabindex="1" onClick="showCalendarControl(FormView1_req_dateTextBox); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            
            </td>
        
        </tr>
        <tr>
        <td><b>Cust#:</b></td>
        <td><b><asp:TextBox ID="cust_noTextBox" AutoPostBack="true" OnTextChanged="cust_change_textbox" runat="server" Text='<%# Bind("cust_no") %>'>
            </asp:TextBox></b><a href="#" tabindex="1" onClick="customerlook('1'); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            <a href="#" tabindex="1" onClick="customer2look(); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
        <td><b>Due Date:</b></td>
        <td nowrap><b><asp:TextBox ID="due_dateTextBox" Width="80px" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );"  MaxLength="10" runat="server" Text='<%# Bind("due_date") %>'>
            </asp:TextBox></b>
            <a href="#" onblur="setestdate2()" tabindex="1" onClick="showCalendarControl(FormView1_due_dateTextBox); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            
            </td>
        
        
        </tr>
        <tr>
        <td><b>Cust Name:</b></td>
        <td><b><asp:TextBox ID="ship_nameTextBox" runat="server" Text='<%# Bind("ship_name") %>'>
            </asp:TextBox></b></td>
        <td><b>Fob:</b></td>
        <td><b>
            <asp:TextBox ID="fob_codeTextBox" Visible="false" runat="server" Text='<%# Bind("fob_code") %>'></asp:TextBox>
           <asp:RadioButtonList ID="RadioButtonList1"   RepeatLayout="Flow" CellSpacing="1" RepeatColumns="2" SelectedValue='<%# Bind("fob_code") %>' runat="server">
                <asp:ListItem Value="D" Text="Destination"></asp:ListItem>
                <asp:ListItem Value="O" Text="Origin"></asp:ListItem>                             
                </asp:RadioButtonList>
           
            </b></td>
        
        </tr>
        <tr>
        <td><b>Address:</b></td>
        <td><b><asp:TextBox ID="shipAddrTextBox" runat="server" Text='<%# Bind("shipAddr") %>'>
            </asp:TextBox></b></td>
        <td><b>Freight Charge:</b></td>
        <td><b>
            <asp:DropDownList ID="chg_methodTextBox" runat="server" SelectedValue='<%# Bind("chg_method") %>' DataTextField='<%# Bind("chg_method") %>'>
         <asp:ListItem>Prepaid</asp:ListItem>
        <asp:ListItem>Collect</asp:ListItem>
        <asp:ListItem>Bill</asp:ListItem>
        <asp:ListItem>Third Party</asp:ListItem>
            </asp:DropDownList>
            </b></td>        
        </tr>
        <tr>
        <td><b></b></td>
        <td><b><asp:TextBox ID="shipAddr2TextBox" runat="server" Text='<%# Bind("shipAddr2") %>'>
            </asp:TextBox></b></td>
        <td><b>Warehouse Month:</b></td>
        <td><b>
            <asp:DropDownList ID="wh_monthTextBox" runat="server" SelectedValue='<%# Bind("wh_month") %>' DataTextField='<%# Bind("wh_month") %>'>
        <asp:ListItem>1</asp:ListItem>
        <asp:ListItem>2</asp:ListItem>
        <asp:ListItem>3</asp:ListItem>
        <asp:ListItem>4</asp:ListItem>
        <asp:ListItem>5</asp:ListItem>
        <asp:ListItem>6</asp:ListItem>
        <asp:ListItem>7</asp:ListItem>
        <asp:ListItem>8</asp:ListItem>
        <asp:ListItem>9</asp:ListItem>
        <asp:ListItem>10</asp:ListItem>
        <asp:ListItem>11</asp:ListItem>
        <asp:ListItem>12</asp:ListItem>
        <asp:ListItem>13</asp:ListItem>
        <asp:ListItem>14</asp:ListItem>
        <asp:ListItem>15</asp:ListItem>
        <asp:ListItem>16</asp:ListItem>
        <asp:ListItem>17</asp:ListItem>
        <asp:ListItem>18</asp:ListItem>
        <asp:ListItem>19</asp:ListItem>
        <asp:ListItem>20</asp:ListItem>
        <asp:ListItem>21</asp:ListItem>
        <asp:ListItem>22</asp:ListItem>
        <asp:ListItem>23</asp:ListItem>
        <asp:ListItem>24</asp:ListItem>
            </asp:DropDownList>
            </b></td>
        </tr>
        <tr>
        <td><b>City:</b></td>
        <td ><b><asp:TextBox ID="ship_cityTextBox" Width="100px" runat="server" Text='<%# Bind("ship_city") %>'>
            </asp:TextBox>
            <asp:TextBox ID="ship_stateTextBox" Width="50px" runat="server" Text='<%# Bind("ship_state") %>'>
            </asp:TextBox>
            <asp:TextBox ID="ship_zipTextBox" Width="30px" runat="server" Text='<%# Bind("ship_zip") %>'>
            </asp:TextBox></b></td>
        
        
        <td><b><asp:Label ID="commlabel" runat="server" Text="Comm%:"></asp:Label></b></td>
        <td><b><asp:TextBox ID="commTextBox" Width="50px" runat="server" Text='<%# Bind("comm") %>'>
            </asp:TextBox></b>
            <asp:CompareValidator ID="CompareValidator1" runat="server" ErrorMessage="Only decimal values" ControlToValidate="commTextBox" SetFocusOnError="true" Display="dynamic" Operator="DataTypeCheck" Type="double"></asp:CompareValidator>
            </td>
        </tr>
        <tr>
        <td><b>Sales Rep:</b></td>
        <td colspan="2"><b><asp:TextBox ID="smanTextBox" Width="70px" runat="server" Text='<%# Bind("sman") %>'>
            </asp:TextBox><a href="#" tabindex="1" onClick="smanlook(); return false"><asp:Image ID="SalesRep" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            <asp:TextBox ID="smanNameTextBox" Width="110px" runat="server" Text='<%# Bind("smanName") %>'>
            </asp:TextBox></b></td>
        
        <td><b></b></td>
        
        </tr></table>
        <table>
        <tr>
        <td><b>Special Instruction:</b></td>
        
        </tr>
        <tr>
        <td><b><asp:TextBox ID="instTextBox" TextMode="multiline" Height="30px" Width="370px" runat="server" Text='<%# Bind("inst") %>'>
            </asp:TextBox></b></td>
        </tr>
        </table>
        </fieldset>
            
            <asp:Button ID="InsertButton" runat="server" CssClass="buttonM"
                Text="Save" OnClick="InsertButton_click">
            </asp:Button>
            <asp:Button ID="InsertCancelButton" runat="server" CssClass="buttonM" CausesValidation="False"  CommandName="Cancel"
                Text="Cancel" >
            </asp:Button>
            </asp:Panel>
        </InsertItemTemplate>
        <EmptyDataRowStyle Font-Bold="True" />
    </asp:FormView>
    
    <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="ViewRfq" TypeName="rfqs">
        <SelectParameters>
        <asp:Parameter Name="prmUser" Type="string" />
        <asp:Parameter Name="prmAction" DefaultValue="Select" Type="string" />
        <asp:Parameter Name="prmExt" Type="string" />
            <asp:SessionParameter Name="prmRfqNo" SessionField="Rfqview_app" Type="Int32" />
            <asp:Parameter Name="prmReqdate" Type="DateTime" />
            <asp:Parameter Name="prmDuedate" Type="DateTime" />
            <asp:Parameter Name="prmCustno" Type="string" />
            <asp:Parameter Name="prmShipname" Type="string" />
            <asp:Parameter Name="prmShipAddr" Type="string" />
            <asp:Parameter Name="prmShipAddr2" Type="string" />
            <asp:Parameter Name="prmShipcity" Type="string" />
            <asp:Parameter Name="prmShipstate" Type="string" />
            <asp:Parameter Name="prmShipzip" Type="string" />
            <asp:Parameter Name="prmSman" Type="string" />
            <asp:Parameter Name="prmSmanName" Type="string" />
            <asp:Parameter Name="prmComm" Type="decimal" />
            <asp:Parameter Name="prmFobcode" Type="string" />
            <asp:Parameter Name="prmChgmethod" Type="string" />
            
            <asp:Parameter Name="prmWhmonth" Type="Int32" />
            <asp:Parameter Name="prmInst" Type="string" />
            <asp:Parameter Name="VRowid" Type="Int64" />
            
        </SelectParameters>
    </asp:ObjectDataSource>
    &nbsp; 
    
    
</div>


<div><br />
<asp:FormView ID="FormView2" runat="server" DataSourceID="ObjectDataSource4" OnDataBound="FormView2_DataBound" OnPreRender="FormView2_PreRender" OnUnload="FormView2_Unload">
      <EditItemTemplate>
       <asp:Panel ID="Panel1" runat="server" DefaultButton="Button1">
        <asp:HiddenField ID="HiddenField1" runat="server" />        
        <fieldset style="background-color:#EFF3FB">  
        <table><tr><td>     
                           
       <table class="shade">    
        <tr><td nowrap  align="right" style="padding-right:5px"><b>Seq No:</b></td>
        <td><b><asp:Label ID="Label1" runat="server" Text='<%# Bind("RfqSeqNo") %>'></asp:Label></b></td>
        <td nowrap  align="right" style="padding-right:5px"><b>Qty:</b</td>
        <td><b><asp:TextBox ID="RfqQtyTextBox" Width="50px" MaxLength="8" onkeypress="return validate(event)" onblur="showQty()" runat="server" Text='<%# Bind("RfqQty","{0:#########}") %>'></asp:TextBox></b></td>
        <td nowrap  align="right" style="padding-right:5px"><b>Fg Item#:</b></td>
        <td><b><asp:TextBox ID="RfqStockTextBox" Width="90px" MaxLength="15" runat="server" Text='<%# Bind("RfqStock") %>'>        
            </asp:TextBox><a href="#" tabindex="1" onClick="QFgItemlook(); return false" ><asp:Image ID="Image35" runat="server" ImageUrl="images/lookup_icon.gif" /></a></b></td></tr>        
        
        <tr><td nowrap  align="right" style="padding-right:5px"><b>Item Name:</b></td>
        <td><b><asp:TextBox ID="RfqNameTextBox" MaxLength="30" Width="160px" runat="server" Text='<%# Bind("RfqName") %>'>
            </asp:TextBox>  <asp:RequiredFieldValidator ID="RequiredFieldValidator4" runat="server" ErrorMessage="Cannot be empty" ControlToValidate="RfqNameTextBox" Display="dynamic" SetFocusOnError="true"></asp:RequiredFieldValidator></b></td>
        <td nowrap  align="right" style="padding-right:5px"><b>Cust Part#:</b</td>
        <td><b><asp:TextBox MaxLength="15" Width="90px"  ID="RfqPartnoTextBox" runat="server" Text='<%# Bind("RfqPartno") %>'>
            </asp:TextBox><a href="#" tabindex="1" onClick="QPartlook(); return false" ><asp:Image ID="Image36" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            <asp:RequiredFieldValidator ID="RequiredFieldValidator9" runat="server" ErrorMessage="Cannot be empty" ControlToValidate="RfqPartnoTextBox" Display="dynamic" SetFocusOnError="true"></asp:RequiredFieldValidator></b></td>
        <td nowrap  align="right" style="padding-right:5px"><b>Category:</b></td>
        <td><b><asp:TextBox Width="50px" MaxLength="6" ID="RfqProcatTextBox" AutoPostBack="true" OnTextChanged="CategoryTextChanged" runat="server" Text='<%# Bind("RfqProcat") %>'></asp:TextBox>
            <a href="#" tabindex="1" onClick="categorylookup(); return false" ><asp:Image ID="Image37" runat="server" ImageUrl="images/lookup_icon.gif" />
            <asp:RequiredFieldValidator ID="RequiredFieldValidator10" runat="server" ErrorMessage="Cannot be empty" ControlToValidate="RfqProcatTextBox" Display="dynamic" SetFocusOnError="true"></asp:RequiredFieldValidator></b></td></tr>        
        <tr><td nowrap  align="right" style="padding-right:5px"><b>Color:</b></TD>
        <Td><b><asp:TextBox ID="RfqColTextBox" Width="50px" MaxLength="2" onkeypress="return validate(event)" runat="server" Text='<%# Bind("RfqCol") %>'></asp:TextBox></b>
            <asp:CompareValidator ID="CompareValidator8" runat="server" ControlToValidate="RfqColTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Invalid Color"></asp:CompareValidator></b></td>
        <td nowrap align="right" style="padding-right:5px"><b>Coating:</b></TD>
        <td><b><asp:TextBox MaxLength="2" Width="50px" onkeypress="return validate(event)" ID="RfqCoatTextBox" runat="server" Text='<%# Bind("RfqCoat") %>'></asp:TextBox></b>
            <asp:CompareValidator ID="CompareValidator9" runat="server" ControlToValidate="RfqCoatTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Invalid Coating"></asp:CompareValidator></b></td>
        <td nowrap align="right" style="padding-right:5px"><b>Style:</b></td>
        <td><b><asp:TextBox ID="RfqstyleTextBox" Width="50px" MaxLength="6" AutoPostBack="true" OnTextChanged="StyleTextChanged" runat="server" Text='<%# Bind("Rfqstyle") %>'>
            </asp:TextBox>
            <a href="#" tabindex="1" onClick="stylelook(); return false" ><asp:Image ID="Image38" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            <input id="Button4" type="button" onclick="openstyle()" value="Style Image" />
            <asp:RequiredFieldValidator ID="RequiredFieldValidator11" runat="server" ErrorMessage="Cannot be empty" ControlToValidate="RfqstyleTextBox" Display="dynamic" SetFocusOnError="true"></asp:RequiredFieldValidator></b></td></tr>
        <TR><td nowrap align="right" style="padding-right:5px"><b>Length:</b></td>
        <td><b><asp:TextBox ID="RfqLengthTextBox" Width="50px" onBlur="checkNumeric(this,',','.');" onkeyup="javascript:decimalval();" MaxLength="8" runat="server" Text='<%# Bind("RfqLength","{0:###,##0.00}") %>'>
            </asp:TextBox>
            <asp:RequiredFieldValidator ID="RequiredFieldValidator12" runat="server" ErrorMessage="Cannot be empty" ControlToValidate="RfqLengthTextBox" Display="dynamic" SetFocusOnError="true"></asp:RequiredFieldValidator>
            <asp:RangeValidator ID="RangeValidator3" runat="server" ControlToValidate="RfqLengthTextBox" SetFocusOnError="true" Display="dynamic" MinimumValue="00000.01" MaximumValue="99999.99" ErrorMessage="Must have some value"></asp:RangeValidator>
            <asp:CompareValidator ID="CompareValidator10" runat="server" ErrorMessage="Only single decimal" ControlToValidate="RfqLengthTextBox" Display="dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="double"></asp:CompareValidator></b></td>
        <td nowrap align="right" style="padding-right:5px"><b>Width:</b></td>
        <td><b><asp:TextBox ID="RfqWidthTextBox" Width="50px" onBlur="checkNumeric(this,',','.');" onkeyup="javascript:decimalval();" MaxLength="8" runat="server" Text='<%# Bind("RfqWidth","{0:###,##0.00}") %>'>
            </asp:TextBox>
            <asp:RequiredFieldValidator ID="RequiredFieldValidator13" runat="server" ErrorMessage="Cannot be empty" ControlToValidate="RfqWidthTextBox" Display="dynamic" SetFocusOnError="true"></asp:RequiredFieldValidator>
            <asp:RangeValidator ID="RangeValidator4" runat="server" ControlToValidate="RfqWidthTextBox" SetFocusOnError="true" Display="dynamic" MinimumValue="00000.01" MaximumValue="99999.99" ErrorMessage="Must have some value"></asp:RangeValidator>
            <asp:CompareValidator ID="CompareValidator11" runat="server" ErrorMessage="Only single decimal" ControlToValidate="RfqWidthTextBox" Display="dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="double"></asp:CompareValidator></b></td>
        <td nowrap align="right" style="padding-right:5px"><b>Depth:</b></td>
        <td><b><asp:TextBox ID="RfqDepthTextBox" Width="50px" MaxLength="8" onkeyup="javascript:decimalval();" onBlur="checkNumeric(this,',','.');" runat="server" Text='<%# Bind("RfqDepth","{0:###,##0.00}") %>'>
            </asp:TextBox>
            <asp:CompareValidator ID="CompareValidator12" runat="server" ErrorMessage="Only single decimal" ControlToValidate="RfqDepthTextBox" Display="dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="double"></asp:CompareValidator></b></td></TR>
        <tr><td nowrap align="right" style="padding-right:5px"><b>Board:</b></td>
        <td><b><asp:TextBox ID="RfqBoardTextBox" Width="65px" MaxLength="10" AutoPostBack="true" OnTextChanged="BoardTextChanged" runat="server" Text='<%# Bind("RfqBoard") %>'>
            </asp:TextBox><a href="#" tabindex="1" onClick="Boardlook1(); return false"><asp:Image ID="Image39" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            <asp:RequiredFieldValidator ID="RequiredFieldValidator14" runat="server" ErrorMessage="Cannot be empty" ControlToValidate="RfqBoardTextBox" Display="dynamic" SetFocusOnError="true"></asp:RequiredFieldValidator></b></td>
        <td nowrap align="right" style="padding-right:5px"><b>Caliper:</b></td>
        <td><b><asp:TextBox ID="RfqCalTextBox" Enabled="false" Width="50px" MaxLength="1" runat="server" Text='<%# Bind("RfqCal","{0:###,##0.00000}") %>'>
            </asp:TextBox></b></td>        
        <td nowrap align="right" style="padding-right:5px"><b>Qty/Set:</b></td>
        <td><b><asp:Label ID="qtysetLabel" runat="server" BackColor="PaleTurquoise" Width="60px" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("qtyset") %>'></asp:Label></b></td>              
        </tr>        
        </table>    
            </td></tr><tr><td>
            <table id="Qtyhelp" align="left" width="400px" style="display:none;">
            <tr>
            <td><b>Quantity</b></td>
            <td><b>Deliveries</b></td>
            <td><b>Price</b></td>
            <td><b>UOM</b></td>
            <td><b>Date</b></td>
            </tr>
            <tr>
            <td Width="70px"><b><asp:TextBox ID="TextBox1" onfocus="getprevval(this)" onblur="reverseqty();checkNumVal(this, 'q')" MaxLength="8" Width="50px" runat="server"></asp:TextBox></b></td>
            <td Width="30px"><b><asp:TextBox ID="lv_delivery_1" MaxLength="3" Width="30px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'd')" Text='<%# Bind("lv_delivery_1") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_price_1" MaxLength="5" Width="40px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'p')" Text='<%# Bind("lv_price_1") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_uom_1" MaxLength="3" Width="35px" onfocus="getprevval(this)" onblur="checkUom(this)" Text='<%# Bind("lv_uom_1") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onClick="uomlook('1'); return false"><asp:Image ID="Image34" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            <td Width="100px" nowrap><b><asp:TextBox ID="lv_date_1" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" MaxLength="10" Width="70px" Text='<%# Bind("lv_date_1","{0:MM/dd/yyyy}") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onblur="FormView1$lv_date_1.focus()" onClick="showCalendarControl(FormView1$lv_date_1); return false"><asp:Image ID="Image25" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            </tr>
            <tr>
            <td Width="70px"><b><asp:TextBox ID="lv_qty_2" MaxLength="8" Width="50px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'q');displayqty(this, '1')" Text='<%# Bind("lv_qty_2") %>' runat="server"></asp:TextBox></b></td>
            <td Width="30px" ><b><asp:TextBox ID="lv_delivery_2" MaxLength="3" Width="30px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'd')" Text='<%# Bind("lv_delivery_2") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_price_2" MaxLength="5" Width="40px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'p')" Text='<%# Bind("lv_price_2") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_uom_2" MaxLength="3" Width="35px" onfocus="getprevval(this)" onblur="checkUom(this)" Text='<%# Bind("lv_uom_2") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onClick="uomlook('2'); return false"><asp:Image ID="Image33" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            <td Width="100px" nowrap><b><asp:TextBox ID="lv_date_2" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" MaxLength="10" Width="70px" Text='<%# Bind("lv_date_2","{0:MM/dd/yyyy}") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onblur="FormView1$lv_date_2.focus()" onClick="showCalendarControl(FormView1$lv_date_2); return false"><asp:Image ID="Image24" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            </tr>
            <tr>
            <td Width="70px"><b><asp:TextBox ID="lv_qty_3"  MaxLength="8" Width="50px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'q');displayqty(this, '1')" Text='<%# Bind("lv_qty_3") %>' runat="server"></asp:TextBox></b></td>
            <td Width="30px"><b><asp:TextBox ID="lv_delivery_3" MaxLength="3" Width="30px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'd')" Text='<%# Bind("lv_delivery_3") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_price_3" MaxLength="5" Width="40px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'p')" Text='<%# Bind("lv_price_3") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_uom_3" MaxLength="3" Width="35px" onfocus="getprevval(this)" onblur="checkUom(this)" Text='<%# Bind("lv_uom_3") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onClick="uomlook('3'); return false"><asp:Image ID="Image32" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            <td Width="100px" nowrap><b><asp:TextBox ID="lv_date_3" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" MaxLength="10" Width="70px" Text='<%# Bind("lv_date_3","{0:MM/dd/yyyy}") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onblur="FormView1$lv_date_3.focus()" onClick="showCalendarControl(FormView1$lv_date_3); return false"><asp:Image ID="Image23" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            </tr>
            <tr>
            <td Width="70px"><b><asp:TextBox ID="lv_qty_4" MaxLength="8" Width="50px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'q');displayqty(this, '1')" Text='<%# Bind("lv_qty_4") %>' runat="server"></asp:TextBox></b></td>
            <td Width="30px"><b><asp:TextBox ID="lv_delivery_4" MaxLength="3" Width="30px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'd')" Text='<%# Bind("lv_delivery_4") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_price_4" MaxLength="5" Width="40px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'p')" Text='<%# Bind("lv_price_4") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_uom_4" MaxLength="3" Width="35px" onfocus="getprevval(this)" onblur="checkUom(this)" Text='<%# Bind("lv_uom_4") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onClick="uomlook('4'); return false"><asp:Image ID="Image31" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            <td Width="100px" nowrap><b><asp:TextBox ID="lv_date_4" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" MaxLength="10" Width="70px" Text='<%# Bind("lv_date_4","{0:MM/dd/yyyy}") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onblur="FormView1$lv_date_4.focus()" onClick="showCalendarControl(FormView1$lv_date_4); return false"><asp:Image ID="Image22" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            </tr>
            <tr>
            <td Width="70px"><b><asp:TextBox ID="lv_qty_5" MaxLength="8" Width="50px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'q');displayqty(this, '1')" Text='<%# Bind("lv_qty_5") %>' runat="server"></asp:TextBox></b></td>
            <td Width="30px"><b><asp:TextBox ID="lv_delivery_5" Width="30px" MaxLength="3" onfocus="getprevval(this)" onblur="checkNumVal(this, 'd')" Text='<%# Bind("lv_delivery_5") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_price_5" MaxLength="5" Width="40px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'p')" Text='<%# Bind("lv_price_5") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_uom_5" MaxLength="3" Width="35px" onfocus="getprevval(this)" onblur="checkUom(this)" Text='<%# Bind("lv_uom_5") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onClick="uomlook('5'); return false"><asp:Image ID="Image30" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            <td Width="100px" nowrap><b><asp:TextBox ID="lv_date_5" MaxLength="10" Width="70px" Text='<%# Bind("lv_date_5","{0:MM/dd/yyyy}") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onblur="FormView1$lv_date_5.focus()" onClick="showCalendarControl(FormView1$lv_date_5); return false"><asp:Image ID="Image21" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            </tr>
            <tr>
            <td Width="70px"><b><asp:TextBox ID="lv_qty_6" MaxLength="8" Width="50px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'q');displayqty(this, '1')" Text='<%# Bind("lv_qty_6") %>' runat="server"></asp:TextBox></b></td>
            <td Width="30px"><b><asp:TextBox ID="lv_delivery_6" MaxLength="3" Width="30px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'd')" Text='<%# Bind("lv_delivery_6") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_price_6" MaxLength="5" Width="40px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'p')" Text='<%# Bind("lv_price_6") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_uom_6" MaxLength="3" Width="35px" onfocus="getprevval(this)" onblur="checkUom(this)" Text='<%# Bind("lv_uom_6") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onClick="uomlook('6'); return false"><asp:Image ID="Image29" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            <td Width="100px" nowrap><b><asp:TextBox ID="lv_date_6" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" MaxLength="10" Width="70px" Text='<%# Bind("lv_date_6","{0:MM/dd/yyyy}") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onblur="FormView1$lv_date_6.focus()" onClick="showCalendarControl(FormView1$lv_date_6); return false"><asp:Image ID="Image20" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            </tr>
            <tr>
            <td Width="70px"><b><asp:TextBox ID="lv_qty_7" MaxLength="8" Width="50px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'q');displayqty(this, '1')" Text='<%# Bind("lv_qty_7") %>' runat="server"></asp:TextBox></b></td>
            <td Width="30px"><b><asp:TextBox ID="lv_delivery_7" MaxLength="3" Width="30px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'd')" Text='<%# Bind("lv_delivery_7") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_price_7" MaxLength="5" Width="40px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'p')" Text='<%# Bind("lv_price_7") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_uom_7" MaxLength="3" Width="35px" onfocus="getprevval(this)" onblur="checkUom(this)" Text='<%# Bind("lv_uom_7") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1"  onClick="uomlook('7'); return false"><asp:Image ID="Image28" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            <td Width="100px" nowrap><b><asp:TextBox ID="lv_date_7" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" MaxLength="10" Width="70px" Text='<%# Bind("lv_date_7","{0:MM/dd/yyyy}") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onblur="FormView1$lv_date_7.focus()" onClick="showCalendarControl(FormView1$lv_date_7); return false"><asp:Image ID="Image19" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            </tr>
            <tr>
            <td Width="70px"><b><asp:TextBox ID="lv_qty_8" MaxLength="8" Width="50px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'q');displayqty(this, '1')" Text='<%# Bind("lv_qty_8") %>' runat="server"></asp:TextBox></b></td>
            <td Width="30px"><b><asp:TextBox ID="lv_delivery_8" MaxLength="3" Width="30px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'd')" Text='<%# Bind("lv_delivery_8") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_price_8" MaxLength="5" Width="40px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'p')" Text='<%# Bind("lv_price_8") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_uom_8" MaxLength="3" Width="35px" onfocus="getprevval(this)" onblur="checkUom(this)" Text='<%# Bind("lv_uom_8") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onClick="uomlook('8'); return false"><asp:Image ID="Image27" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            <td Width="100px" nowrap><b><asp:TextBox ID="lv_date_8" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" MaxLength="10" Width="70px" Text='<%# Bind("lv_date_8","{0:MM/dd/yyyy}") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onblur="FormView1$lv_date_8.focus()" onClick="showCalendarControl(FormView1$lv_date_8); return false"><asp:Image ID="Image18" runat="server" ImageUrl="images/lookup_icon.gif" /></a>                
            </td>
            </tr>
            <tr>
            <td Width="70px"><b><asp:TextBox ID="lv_qty_9" MaxLength="8" Width="50px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'q');displayqty(this, '1')" Text='<%# Bind("lv_qty_9") %>' runat="server"></asp:TextBox></b></td>
            <td Width="30px"><b><asp:TextBox ID="lv_delivery_9" MaxLength="3" Width="30px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'd')" Text='<%# Bind("lv_delivery_9") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_price_9" MaxLength="5" Width="40px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'p')" Text='<%# Bind("lv_price_9") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_uom_9" MaxLength="3" Width="35px" onfocus="getprevval(this)" onblur="checkUom(this)" Text='<%# Bind("lv_uom_9") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onClick="uomlook('9'); return false"><asp:Image ID="Image26" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            <td Width="100px" nowrap><b><asp:TextBox ID="lv_date_9" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" MaxLength="10" Width="70px" Text='<%# Bind("lv_date_9","{0:MM/dd/yyyy}") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onblur="FormView1$lv_date_9.focus()" onClick="showCalendarControl(FormView1$lv_date_9); return false"><asp:Image ID="Image17" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            </tr>
            <tr>
            <td Width="70px"><b><asp:TextBox ID="lv_qty_10" MaxLength="8" Width="50px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'q');displayqty(this, '1')" Text='<%# Bind("lv_qty_10") %>' runat="server"></asp:TextBox></b></td>
            <td Width="30px"><b><asp:TextBox ID="lv_delivery_10" MaxLength="3" Width="30px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'd')" Text='<%# Bind("lv_delivery_10") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_price_10" Width="40px" MaxLength="5" onfocus="getprevval(this)" onblur="checkNumVal(this, 'p')" Text='<%# Bind("lv_price_10") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_uom_10" MaxLength="3" Width="35px" onfocus="getprevval(this)" onblur="checkUom(this)" Text='<%# Bind("lv_uom_10") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onClick="uomlook('10'); return false"><asp:Image ID="Image15" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            <td Width="100px" nowrap><b><asp:TextBox ID="lv_date_10" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" MaxLength="10" Width="70px" Text='<%# Bind("lv_date_10","{0:MM/dd/yyyy}") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onblur="FormView1$lv_date_10.focus()" onClick="showCalendarControl(FormView1$lv_date_10); return false"><asp:Image ID="Image16" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            </tr>
            
            
            <tr><td colspan="2">
            <input type="hidden"  name="HiddenRowid" value='<%# Server.UrlEncode(Convert.ToString(DataBinder.Eval(Container,"DataItem.RfqRowid"))) %>' />
            <input type="hidden"  name="ARowid" value='<%# Server.UrlEncode(Convert.ToString(DataBinder.Eval(Container,"DataItem.ARowid"))) %>' />
            <br />           
            
            </td></tr></table>
            </td></tr>
            <tr><td>
            <asp:Button ID="Button1" runat="server"   CssClass="buttonM" onclick="updateRfqitem"
                Text="Save" >
            </asp:Button>
            <asp:Button ID="Button2" runat="server" CausesValidation="False" CommandName="Cancel" CssClass="buttonM"
                Text="Cancel">
            </asp:Button>
            </td></tr>
            </table>
            </fieldset>         
           </asp:Panel>
        </EditItemTemplate>
        
        
        <InsertItemTemplate>
        <asp:Panel ID="Panel1" runat="server" DefaultButton="Button1">
         <asp:HiddenField ID="HiddenField1" runat="server" />
        <fieldset  style="background-color:#EFF3FB">  
        <table><tr><td>
        <table class="shade">    
        <tr><td nowrap  align="right" style="padding-right:5px"><b>Seq No:</b></td>
        <td><b><asp:Label ID="RfqSeqNoLabel" BackColor="PaleTurquoise" Width="50px" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("RfqSeqNo") %>'></asp:Label></b></td>
        <td nowrap  align="right" style="padding-right:5px"><b>Qty:</b</td>
        <td><b><asp:TextBox ID="RfqQtyTextBox" Width="50px" MaxLength="8" onkeypress="return validate(event)" onblur="showQty()" runat="server" Text='<%# Bind("RfqQty","{0:#########}") %>'></asp:TextBox></b></td>
        <td nowrap  align="right" style="padding-right:5px"><b>Fg Item#:</b></td>
        <td><b><asp:TextBox ID="RfqStockTextBox" Width="90px" MaxLength="15" runat="server" Text='<%# Bind("RfqStock") %>'>        
            </asp:TextBox><a href="#" tabindex="1" onClick="QFgItemlook(); return false" ><asp:Image ID="Image35" runat="server" ImageUrl="images/lookup_icon.gif" /></a></b></td></tr>        
        <tr>  
        <tr><td nowrap  align="right" style="padding-right:5px"><b>Item Name:</b></td>
        <td><b><asp:TextBox ID="RfqNameTextBox" MaxLength="30" Width="160px" runat="server" Text='<%# Bind("RfqName") %>'>
            </asp:TextBox>  <asp:RequiredFieldValidator ID="RequiredFieldValidator4" runat="server" ErrorMessage="Cannot be empty" ControlToValidate="RfqNameTextBox" Display="dynamic" SetFocusOnError="true"></asp:RequiredFieldValidator></b></td>
        <td nowrap  align="right" style="padding-right:5px"><b>Cust Part#:</b</td>
        <td><b><asp:TextBox MaxLength="15" Width="90px"  ID="RfqPartnoTextBox" runat="server" Text='<%# Bind("RfqPartno") %>'>
            </asp:TextBox><a href="#" tabindex="1" onClick="QPartlook(); return false" ><asp:Image ID="Image36" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            <asp:RequiredFieldValidator ID="RequiredFieldValidator9" runat="server" ErrorMessage="Cannot be empty" ControlToValidate="RfqPartnoTextBox" Display="dynamic" SetFocusOnError="true"></asp:RequiredFieldValidator></b></td>
        <td nowrap  align="right" style="padding-right:5px"><b>Category:</b></td>
        <td><b><asp:TextBox Width="50px" MaxLength="6" ID="RfqProcatTextBox" AutoPostBack="true" OnTextChanged="CategoryTextChanged" runat="server" Text='<%# Bind("RfqProcat") %>'></asp:TextBox>
            <a href="#" tabindex="1" onClick="categorylookup(); return false" ><asp:Image ID="Image37" runat="server" ImageUrl="images/lookup_icon.gif" />
            <asp:RequiredFieldValidator ID="RequiredFieldValidator10" runat="server" ErrorMessage="Cannot be empty" ControlToValidate="RfqProcatTextBox" Display="dynamic" SetFocusOnError="true"></asp:RequiredFieldValidator></b></td></tr>        
        <tr><td nowrap  align="right" style="padding-right:5px"><b>Color:</b></TD>
        <Td><b><asp:TextBox ID="RfqColTextBox" Width="50px" MaxLength="2" onkeypress="return validate(event)" runat="server" Text='<%# Bind("RfqCol") %>'></asp:TextBox></b>
            <asp:CompareValidator ID="CompareValidator8" runat="server" ControlToValidate="RfqColTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Invalid Color"></asp:CompareValidator></b></td>
        <td nowrap align="right" style="padding-right:5px"><b>Coating:</b></TD>
        <td><b><asp:TextBox MaxLength="2" Width="50px" onkeypress="return validate(event)" ID="RfqCoatTextBox" runat="server" Text='<%# Bind("RfqCoat") %>'></asp:TextBox></b>
            <asp:CompareValidator ID="CompareValidator9" runat="server" ControlToValidate="RfqCoatTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Invalid Coating"></asp:CompareValidator></b></td>
        <td nowrap align="right" style="padding-right:5px"><b>Style:</b></td>
        <td><b><asp:TextBox ID="RfqstyleTextBox" Width="50px" MaxLength="6" AutoPostBack="true" OnTextChanged="StyleTextChanged" runat="server" Text='<%# Bind("Rfqstyle") %>'>
            </asp:TextBox>
            <a href="#" tabindex="1" onClick="stylelook(); return false" ><asp:Image ID="Image38" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            <input id="Button4" type="button" onclick="openstyle()" value="Style Image" />
            <asp:RequiredFieldValidator ID="RequiredFieldValidator11" runat="server" ErrorMessage="Cannot be empty" ControlToValidate="RfqstyleTextBox" Display="dynamic" SetFocusOnError="true"></asp:RequiredFieldValidator></b></td></tr>
        <TR><td nowrap align="right" style="padding-right:5px"><b>Length:</b></td>
        <td><b><asp:TextBox ID="RfqLengthTextBox" Width="50px" onBlur="checkNumeric(this,',','.');" onkeyup="javascript:decimalval();" MaxLength="8" runat="server" Text='<%# Bind("RfqLength","{0:###,##0.00}") %>'>
            </asp:TextBox>
            <asp:RequiredFieldValidator ID="RequiredFieldValidator12" runat="server" ErrorMessage="Cannot be empty" ControlToValidate="RfqLengthTextBox" Display="dynamic" SetFocusOnError="true"></asp:RequiredFieldValidator>
            <asp:RangeValidator ID="RangeValidator3" runat="server" ControlToValidate="RfqLengthTextBox" SetFocusOnError="true" Display="dynamic" MinimumValue="00000.01" MaximumValue="99999.99" ErrorMessage="Must have some value"></asp:RangeValidator>
            <asp:CompareValidator ID="CompareValidator10" runat="server" ErrorMessage="Only single decimal" ControlToValidate="RfqLengthTextBox" Display="dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="double"></asp:CompareValidator></b></td>
        <td nowrap align="right" style="padding-right:5px"><b>Width:</b></td>
        <td><b><asp:TextBox ID="RfqWidthTextBox" Width="50px" onBlur="checkNumeric(this,',','.');" onkeyup="javascript:decimalval();" MaxLength="8" runat="server" Text='<%# Bind("RfqWidth","{0:###,##0.00}") %>'>
            </asp:TextBox>
            <asp:RequiredFieldValidator ID="RequiredFieldValidator13" runat="server" ErrorMessage="Cannot be empty" ControlToValidate="RfqWidthTextBox" Display="dynamic" SetFocusOnError="true"></asp:RequiredFieldValidator>
            <asp:RangeValidator ID="RangeValidator4" runat="server" ControlToValidate="RfqWidthTextBox" SetFocusOnError="true" Display="dynamic" MinimumValue="00000.01" MaximumValue="99999.99" ErrorMessage="Must have some value"></asp:RangeValidator>
            <asp:CompareValidator ID="CompareValidator11" runat="server" ErrorMessage="Only single decimal" ControlToValidate="RfqWidthTextBox" Display="dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="double"></asp:CompareValidator></b></td>
        <td nowrap align="right" style="padding-right:5px"><b>Depth:</b></td>
        <td><b><asp:TextBox ID="RfqDepthTextBox" Width="50px" MaxLength="8" onkeyup="javascript:decimalval();" onBlur="checkNumeric(this,',','.');" runat="server" Text='<%# Bind("RfqDepth","{0:###,##0.00}") %>'>
            </asp:TextBox>
            <asp:CompareValidator ID="CompareValidator12" runat="server" ErrorMessage="Only single decimal" ControlToValidate="RfqDepthTextBox" Display="dynamic" SetFocusOnError="true" Operator="DataTypeCheck" Type="double"></asp:CompareValidator></b></td></TR>
        <tr><td nowrap align="right" style="padding-right:5px"><b>Board:</b></td>
        <td><b><asp:TextBox ID="RfqBoardTextBox" Width="65px" MaxLength="10" AutoPostBack="true" OnTextChanged="BoardTextChanged" runat="server" Text='<%# Bind("RfqBoard") %>'>
            </asp:TextBox><a href="#" tabindex="1" onClick="Boardlook1(); return false"><asp:Image ID="Image39" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            <asp:RequiredFieldValidator ID="RequiredFieldValidator14" runat="server" ErrorMessage="Cannot be empty" ControlToValidate="RfqBoardTextBox" Display="dynamic" SetFocusOnError="true"></asp:RequiredFieldValidator></b></td>
        <td nowrap align="right" style="padding-right:5px"><b>Caliper:</b></td>
        <td><b><asp:TextBox ID="RfqCalTextBox" Enabled="false" Width="50px" MaxLength="1" runat="server" Text='<%# Bind("RfqCal","{0:###,##0.00000}") %>'>
            </asp:TextBox></b></td>        
        <td nowrap align="right" style="padding-right:5px"><b>Qty/Set:</b></td>
        <td><b><asp:Label ID="qtysetLabel" runat="server" BackColor="PaleTurquoise" Width="60px" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("qtyset") %>'></asp:Label></b></td>              
        </tr>        
        </table>
              
        
            
            <table id="Qtyhelp" align="left" width="400px" style="display:none;">
            <tr>
            <td><b>Quantity</b></td>
            <td><b>Deliveries</b></td>
            <td><b>Price</b></td>
            <td><b>UOM</b></td>
            <td><b>Date</b></td>
            </tr>
            <tr>
            <td Width="70px"><b><asp:TextBox ID="TextBox1" onfocus="getprevval(this)" onblur="reverseqty(); checkNumVal(this, 'q')" MaxLength="8" Width="50px" runat="server"></asp:TextBox></b></td>
            <td Width="30px"><b><asp:TextBox ID="lv_delivery_1" MaxLength="3" Width="30px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'd')" Text='<%# Bind("lv_delivery_1") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_price_1" MaxLength="5" Width="40px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'p')" Text='<%# Bind("lv_price_1") %>' runat="server"></asp:TextBox></b></td>
            <td Width="100px" nowrap><b><asp:TextBox ID="lv_uom_1" MaxLength="3" Width="35px" onfocus="getprevval(this)" onblur="checkUom(this)" Text='<%# Bind("lv_uom_1") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1"  onClick="uomlook('1'); return false"><asp:Image ID="Image15" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            <td Width="100px" nowrap><b><asp:TextBox ID="lv_date_1" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" MaxLength="10" Width="70px" Text='<%# Bind("lv_date_1","{0:MM/dd/yyyy}") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onblur="FormView1$lv_date_1.focus()" onClick="showCalendarControl(FormView1$lv_date_1); return false"><asp:Image ID="Image25" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            </tr>
            <tr>
            <td Width="70px"><b><asp:TextBox ID="lv_qty_2" MaxLength="8" Width="50px" onfocus="getprevval(this)" onblur="defaultVal('2');checkNumVal(this, 'q');displayqty(this, '2')" Text='<%# Bind("lv_qty_2") %>' runat="server"></asp:TextBox></b></td>
            <td Width="30px" ><b><asp:TextBox ID="lv_delivery_2" MaxLength="3" Width="30px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'd')" Text='<%# Bind("lv_delivery_2") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_price_2" MaxLength="5" Width="40px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'p')" Text='<%# Bind("lv_price_2") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_uom_2" MaxLength="3" Width="35px" onfocus="getprevval(this)" onblur="checkUom(this)" Text='<%# Bind("lv_uom_2") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onClick="uomlook('2'); return false"><asp:Image ID="Image14" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            <td Width="100px" nowrap><b><asp:TextBox ID="lv_date_2" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" MaxLength="10" Width="70px" Text='<%# Bind("lv_date_2","{0:MM/dd/yyyy}") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onblur="FormView1$lv_date_2.focus()" onClick="showCalendarControl(FormView1$lv_date_2); return false"><asp:Image ID="Image24" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            </tr>
            <tr>
            <td Width="70px"><b><asp:TextBox ID="lv_qty_3"  MaxLength="8" Width="50px" onfocus="getprevval(this)" onblur="defaultVal('3');checkNumVal(this, 'q');displayqty(this, '2')" Text='<%# Bind("lv_qty_3") %>' runat="server"></asp:TextBox></b></td>
            <td Width="30px"><b><asp:TextBox ID="lv_delivery_3" MaxLength="3" Width="30px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'd')" Text='<%# Bind("lv_delivery_3") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_price_3" MaxLength="5" Width="40px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'p')" Text='<%# Bind("lv_price_3") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_uom_3" MaxLength="3" Width="35px" onfocus="getprevval(this)" onblur="checkUom(this)" Text='<%# Bind("lv_uom_3") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onClick="uomlook('3'); return false"><asp:Image ID="Image13" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            <td Width="100px" nowrap><b><asp:TextBox ID="lv_date_3" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" MaxLength="10" Width="70px" Text='<%# Bind("lv_date_3","{0:MM/dd/yyyy}") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onblur="FormView1$lv_date_3.focus()" onClick="showCalendarControl(FormView1$lv_date_3); return false"><asp:Image ID="Image23" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            </tr>
            <tr>
            <td Width="70px"><b><asp:TextBox ID="lv_qty_4" MaxLength="8" Width="50px" onfocus="getprevval(this)" onblur="defaultVal('4');checkNumVal(this, 'q');displayqty(this, '2')" Text='<%# Bind("lv_qty_4") %>' runat="server"></asp:TextBox></b></td>
            <td Width="30px"><b><asp:TextBox ID="lv_delivery_4" MaxLength="3" Width="30px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'd')" Text='<%# Bind("lv_delivery_4") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_price_4" MaxLength="5" Width="40px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'p')" Text='<%# Bind("lv_price_4") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_uom_4" MaxLength="3" Width="35px" onfocus="getprevval(this)" onblur="checkUom(this)" Text='<%# Bind("lv_uom_4") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onClick="uomlook('4'); return false"><asp:Image ID="Image12" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            <td Width="100px" nowrap><b><asp:TextBox ID="lv_date_4" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" MaxLength="10" Width="70px" Text='<%# Bind("lv_date_4","{0:MM/dd/yyyy}") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onblur="FormView1$lv_date_4.focus()" onClick="showCalendarControl(FormView1$lv_date_4); return false"><asp:Image ID="Image22" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            </tr>
            <tr>
            <td Width="70px"><b><asp:TextBox ID="lv_qty_5" MaxLength="8" Width="50px" onfocus="getprevval(this)" onblur="defaultVal('5');checkNumVal(this, 'q');displayqty(this, '2')" Text='<%# Bind("lv_qty_5") %>' runat="server"></asp:TextBox></b></td>
            <td Width="30px"><b><asp:TextBox ID="lv_delivery_5" Width="30px" MaxLength="3" onfocus="getprevval(this)" onblur="checkNumVal(this, 'd')" Text='<%# Bind("lv_delivery_5") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_price_5" MaxLength="5" Width="40px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'p')" Text='<%# Bind("lv_price_5") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_uom_5" MaxLength="3" Width="35px" onfocus="getprevval(this)" onblur="checkUom(this)" Text='<%# Bind("lv_uom_5") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onClick="uomlook('5'); return false"><asp:Image ID="Image11" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            <td Width="100px" nowrap><b><asp:TextBox ID="lv_date_5" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" MaxLength="10" Width="70px" Text='<%# Bind("lv_date_5","{0:MM/dd/yyyy}") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onblur="FormView1$lv_date_5.focus()" onClick="showCalendarControl(FormView1$lv_date_5); return false"><asp:Image ID="Image21" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            </tr>
            <tr>
            <td Width="70px"><b><asp:TextBox ID="lv_qty_6" MaxLength="8" Width="50px" onfocus="getprevval(this)" onblur="defaultVal('6');checkNumVal(this, 'q');displayqty(this, '2')" Text='<%# Bind("lv_qty_6") %>' runat="server"></asp:TextBox></b></td>
            <td Width="30px"><b><asp:TextBox ID="lv_delivery_6" MaxLength="3" Width="30px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'd')" Text='<%# Bind("lv_delivery_6") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_price_6" MaxLength="5" Width="40px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'p')" Text='<%# Bind("lv_price_6") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_uom_6" MaxLength="3" Width="35px" onfocus="getprevval(this)" onblur="checkUom(this)" Text='<%# Bind("lv_uom_6") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onClick="uomlook('6'); return false"><asp:Image ID="Image10" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            <td Width="100px" nowrap><b><asp:TextBox ID="lv_date_6" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" MaxLength="10" Width="70px" Text='<%# Bind("lv_date_6","{0:MM/dd/yyyy}") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onblur="FormView1$lv_date_6.focus()" onClick="showCalendarControl(FormView1$lv_date_6); return false"><asp:Image ID="Image20" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            </tr>
            <tr>
            <td Width="70px"><b><asp:TextBox ID="lv_qty_7" MaxLength="8" Width="50px" onfocus="getprevval(this)" onblur="defaultVal('7');checkNumVal(this, 'q');displayqty(this, '2')" Text='<%# Bind("lv_qty_7") %>' runat="server"></asp:TextBox></b></td>
            <td Width="30px"><b><asp:TextBox ID="lv_delivery_7" MaxLength="3" Width="30px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'd')" Text='<%# Bind("lv_delivery_7") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_price_7" MaxLength="5" Width="40px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'p')" Text='<%# Bind("lv_price_7") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_uom_7" MaxLength="3" Width="35px" onfocus="getprevval(this)" onblur="checkUom(this)" Text='<%# Bind("lv_uom_7") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onClick="uomlook('7'); return false"><asp:Image ID="Image9" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            <td Width="100px" nowrap><b><asp:TextBox ID="lv_date_7" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" MaxLength="10" Width="70px" Text='<%# Bind("lv_date_7","{0:MM/dd/yyyy}") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onblur="FormView1$lv_date_7.focus()" onClick="showCalendarControl(FormView1$lv_date_7); return false"><asp:Image ID="Image19" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            </tr>
            <tr>
            <td Width="70px"><b><asp:TextBox ID="lv_qty_8" MaxLength="8" Width="50px" onfocus="getprevval(this)" onblur="defaultVal('8');checkNumVal(this, 'q');displayqty(this, '2')"  Text='<%# Bind("lv_qty_8") %>' runat="server"></asp:TextBox></b></td>
            <td Width="30px"><b><asp:TextBox ID="lv_delivery_8" MaxLength="3" Width="30px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'd')" Text='<%# Bind("lv_delivery_8") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_price_8" MaxLength="5" Width="40px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'p')" Text='<%# Bind("lv_price_8") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_uom_8" MaxLength="3" Width="35px" onfocus="getprevval(this)" onblur="checkUom(this)" Text='<%# Bind("lv_uom_8") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1"  onClick="uomlook('8'); return false"><asp:Image ID="Image8" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            <td Width="100px" nowrap><b><asp:TextBox ID="lv_date_8" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" MaxLength="10" Width="70px" Text='<%# Bind("lv_date_8","{0:MM/dd/yyyy}") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onblur="FormView1$lv_date_8.focus()" onClick="showCalendarControl(FormView1$lv_date_8); return false"><asp:Image ID="Image18" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            </tr>
            <tr>
            <td Width="70px"><b><asp:TextBox ID="lv_qty_9" MaxLength="8" Width="50px" onfocus="getprevval(this)" onblur="defaultVal('9');checkNumVal(this, 'q');displayqty(this, '2')" Text='<%# Bind("lv_qty_9") %>' runat="server"></asp:TextBox></b></td>
            <td Width="30px"><b><asp:TextBox ID="lv_delivery_9" MaxLength="3" Width="30px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'd')" Text='<%# Bind("lv_delivery_9") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_price_9" MaxLength="5" Width="40px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'p')" Text='<%# Bind("lv_price_9") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_uom_9" MaxLength="3" Width="35px" onfocus="getprevval(this)" onblur="checkUom(this)" Text='<%# Bind("lv_uom_9") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onClick="uomlook('9'); return false"><asp:Image ID="Image7" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            <td Width="100px" nowrap><b><asp:TextBox ID="lv_date_9" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" MaxLength="10" Width="70px" Text='<%# Bind("lv_date_9","{0:MM/dd/yyyy}") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onblur="FormView1$lv_date_9.focus()" onClick="showCalendarControl(FormView1$lv_date_9); return false"><asp:Image ID="Image17" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            </tr>
            <tr>
            <td Width="70px"><b><asp:TextBox ID="lv_qty_10" MaxLength="8" Width="50px" onfocus="getprevval(this)" onblur="defaultVal('10');checkNumVal(this, 'q');displayqty(this, '2')" Text='<%# Bind("lv_qty_10") %>' runat="server"></asp:TextBox></b></td>
            <td Width="30px"><b><asp:TextBox ID="lv_delivery_10" MaxLength="3" Width="30px" onfocus="getprevval(this)" onblur="checkNumVal(this, 'd')" Text='<%# Bind("lv_delivery_10") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_price_10" Width="40px" MaxLength="5" onfocus="getprevval(this)" onblur="checkNumVal(this, 'p')" Text='<%# Bind("lv_price_10") %>' runat="server"></asp:TextBox></b></td>
            <td Width="70px"><b><asp:TextBox ID="lv_uom_10" MaxLength="3" Width="35px" onfocus="getprevval(this)" onblur="checkUom(this)" Text='<%# Bind("lv_uom_10") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onClick="uomlook('10'); return false"><asp:Image ID="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            <td Width="100px" nowrap><b><asp:TextBox ID="lv_date_10" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" MaxLength="10" Width="70px" Text='<%# Bind("lv_date_10","{0:MM/dd/yyyy}") %>' runat="server"></asp:TextBox></b>
                <a href="#" tabindex="1" onblur="FormView1$lv_date_10.focus()" onClick="showCalendarControl(FormView1$lv_date_10); return false"><asp:Image ID="Image16" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            </tr>
            
            
            <tr><td colspan="2">
            
            
            <input type="hidden"  name="HiddenRowid" value='<%# Server.UrlEncode(Convert.ToString(DataBinder.Eval(Container,"DataItem.RfqRowid"))) %>' />
            <input type="hidden"  name="ARowid" value='<%# Server.UrlEncode(Convert.ToString(DataBinder.Eval(Container,"DataItem.ARowid"))) %>' />
            <br />
            
           
            
            </td></tr></table>
            
           </td></tr>
           <tr><td>
            <asp:Button ID="Button1" runat="server"   CssClass="buttonM" onclick="Insertbutton_Click" Text="Save" >
            </asp:Button>
            <asp:Button ID="Button2" runat="server" CausesValidation="False" CommandName="Cancel" CssClass="buttonM" Text="Cancel">
            </asp:Button>
           </td></tr>
           </table>
           
            </fieldset>
          </asp:Panel>  
        </InsertItemTemplate>
        <ItemTemplate>
         <fieldset style="background-color:#EFF3FB">       
        
        <table class="shade">    
        <tr><td nowrap  align="right" style="padding-right:5px"><b>Seq No:</b></td>
        <td><b><asp:Label ID="RfqSeqNoLabel" BackColor="PaleTurquoise" Width="50px" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("RfqSeqNo") %>'></asp:Label></b></td>
        <td nowrap  align="right" style="padding-right:5px"><b>Qty:</b</td>
        <td><asp:Label ID="RfqQtyLabel" BackColor="PaleTurquoise" Width="60px" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" runat="server" Text='<%# Bind("RfqQty") %>'></asp:Label></b></td>
        <td nowrap  align="right" style="padding-right:5px"><b>Fg Item#:</b></td>
        <td><b><asp:Label ID="RfqStockLabel" runat="server" BackColor="PaleTurquoise" Width="120px" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("RfqStock") %>'></asp:Label></b></td></tr>        
        <tr>  
        <tr><td nowrap  align="right" style="padding-right:5px"><b>Item Name:</b></td>
        <td><asp:Label ID="RfqNameLabel" runat="server" BackColor="PaleTurquoise" Width="120px" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("RfqName") %>'></asp:Label></b></td>
        <td nowrap  align="right" style="padding-right:5px"><b>Cust Part#:</b</td>
        <td><b><asp:Label ID="RfqPartnoLabel" runat="server" BackColor="PaleTurquoise" Width="120px" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("RfqPartno") %>'></asp:Label></b></td>
        <td nowrap  align="right" style="padding-right:5px"><b>Category:</b></td>
        <td><b><asp:Label ID="RfqProcatLabel" runat="server" BackColor="PaleTurquoise" Width="120px" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("RfqProcat") %>'></asp:Label></b></td></tr>        
        <tr><td nowrap  align="right" style="padding-right:5px"><b>Color:</b></TD>
        <Td><b><asp:Label ID="RfqColLabel" runat="server" BackColor="PaleTurquoise" Width="60px" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("RfqCol") %>'></asp:Label></b></td>
        <td nowrap align="right" style="padding-right:5px"><b>Coating:</b></TD>
        <td><b><asp:Label ID="RfqCoatLabel" runat="server" BackColor="PaleTurquoise" Width="60px" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("RfqCoat") %>'></asp:Label></b></td>
        <td nowrap align="right" style="padding-right:5px"><b>Style:</b></td>
        <td><b><asp:Label ID="RfqstyleLabel" runat="server" BackColor="PaleTurquoise" Width="120px" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("Rfqstyle") %>'></asp:Label></b></td></tr>
        <TR><td nowrap align="right" style="padding-right:5px"><b>Length:</b></td>
        <td><b><asp:Label ID="RfqLengthLabel" runat="server" BackColor="PaleTurquoise" Width="60px" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("RfqLength") %>'></asp:Label></b></td>
        <td nowrap align="right" style="padding-right:5px"><b>Width:</b></td>
        <td><b><asp:Label ID="RfqWidthLabel" runat="server" BackColor="PaleTurquoise" Width="60px" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("RfqWidth") %>'></asp:Label></b></td>
        <td nowrap align="right" style="padding-right:5px"><b>Depth:</b></td>
        <td><b><asp:Label ID="RfqDepthLabel" runat="server" BackColor="PaleTurquoise" Width="60px" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("RfqDepth") %>'></asp:Label></b></td></TR>
        <tr><td nowrap align="right" style="padding-right:5px"><b>Board:</b></td>
        <td><b><asp:Label ID="RfqBoardLabel" runat="server" BackColor="PaleTurquoise" Width="120px" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("RfqBoard") %>'></asp:Label></b></td>
        <td nowrap align="right" style="padding-right:5px"><b>Caliper:</b></td>
        <td><b><asp:Label ID="RfqCalLabel" runat="server" BackColor="PaleTurquoise" Width="60px" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("RfqCal") %>'></asp:Label></b></td>        
        <td nowrap align="right" style="padding-right:5px"><b>Qty/Set:</b></td>
        <td><b><asp:Label ID="qtysetLabel" runat="server" BackColor="PaleTurquoise" Width="60px" BorderColor="White" BorderStyle="Solid" BorderWidth="1px" Text='<%# Bind("qtyset") %>'></asp:Label></b></td>              
        </tr>
        
        </table>
           
           
           <br />
                <asp:Button ID="EditButton" runat="server" CausesValidation="False" CommandName="Edit" CssClass="buttonM" 
                   Text="Update">
                </asp:Button>
                <asp:Button ID="DeleteButton" runat="server"  CssClass="buttonM" OnClick="Delete_RfqItem" OnClientClick="return confirm('Are you sure you want to delete this record')"
                    Text="Delete">
                </asp:Button>                                
                
                <asp:Button ID="NewButton" runat="server" CausesValidation="False" CommandName="New" CssClass="buttonM" 
                   Text="Add">
                </asp:Button> 
                
                </fieldset> 
        </ItemTemplate>
    </asp:FormView>
    
    <asp:ObjectDataSource ID="ObjectDataSource4" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="Rfqitem" TypeName="rfqs">
        <SelectParameters>
            <asp:Parameter Name="prmComp" Type="String" />
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:Parameter Name="prmAction" DefaultValue="select" Type="String" />
            <asp:SessionParameter Name="prmRfqNo" SessionField="Rfqview_app" Type="Int32" />
            <asp:SessionParameter SessionField="my_new_seq_no_gen_app" Name="RfqSeqNo" Type="Int32" />
            <asp:Parameter Name="RfqQty" Type="Int32" />
            <asp:Parameter Name="RfqStock" Type="String" />
            <asp:Parameter Name="RfqName" Type="String" />
            <asp:Parameter Name="RfqPartno" Type="String" />
            <asp:Parameter Name="Rfqstyle" Type="String" />
            <asp:Parameter Name="RfqProcat" Type="String" />
            <asp:Parameter Name="RfqCol" Type="Int32" />
            <asp:Parameter Name="RfqCoat" Type="Int32" />
            <asp:Parameter Name="RfqLength" Type="Decimal" />
            <asp:Parameter Name="RfqWidth" Type="Decimal" />
            <asp:Parameter Name="RfqDepth" Type="Decimal" />
            <asp:Parameter Name="RfqBoard" Type="String" />
            <asp:Parameter Name="RfqCal" Type="Decimal" />
            <asp:Parameter Name="RfqQuantity" Type="Int32" />
            <asp:Parameter Name="RfqRowid" Type="Int64" />
            
            <asp:Parameter Name="lv_qty2" Type="Int32" />
            <asp:Parameter Name="lv_qty3" Type="Int32" />
            <asp:Parameter Name="lv_qty4" Type="Int32" />
            <asp:Parameter Name="lv_qty5" Type="Int32" />
            <asp:Parameter Name="lv_qty6" Type="Int32" />
            <asp:Parameter Name="lv_qty7" Type="Int32" />
            <asp:Parameter Name="lv_qty8" Type="Int32" />
            <asp:Parameter Name="lv_qty9" Type="Int32" />
            <asp:Parameter Name="lv_qty10" Type="Int32" />
            <asp:Parameter Name="lv_price_1" Type="Decimal" />
            <asp:Parameter Name="lv_price_2" Type="Decimal" />
            <asp:Parameter Name="lv_price_3" Type="Decimal" />
            <asp:Parameter Name="lv_price_4" Type="Decimal" />
            <asp:Parameter Name="lv_price_5" Type="Decimal" />
            <asp:Parameter Name="lv_price_6" Type="Decimal" />
            <asp:Parameter Name="lv_price_7" Type="Decimal" />
            <asp:Parameter Name="lv_price_8" Type="Decimal" />
            <asp:Parameter Name="lv_price_9" Type="Decimal" />
            <asp:Parameter Name="lv_price_10" Type="Decimal" />
            <asp:Parameter Name="lv_uom_1" Type="String" />
            <asp:Parameter Name="lv_uom_2" Type="String" />
            <asp:Parameter Name="lv_uom_3" Type="String" />
            <asp:Parameter Name="lv_uom_4" Type="String" />
            <asp:Parameter Name="lv_uom_5" Type="String" />
            <asp:Parameter Name="lv_uom_6" Type="String" />
            <asp:Parameter Name="lv_uom_7" Type="String" />
            <asp:Parameter Name="lv_uom_8" Type="String" />
            <asp:Parameter Name="lv_uom_9" Type="String" />
            <asp:Parameter Name="lv_uom_10" Type="String" />
            <asp:Parameter Name="lv_date_1" Type="DateTime" />
            <asp:Parameter Name="lv_date_2" Type="DateTime" />
            <asp:Parameter Name="lv_date_3" Type="DateTime" />
            <asp:Parameter Name="lv_date_4" Type="DateTime" />
            <asp:Parameter Name="lv_date_5" Type="DateTime" />
            <asp:Parameter Name="lv_date_6" Type="DateTime" />
            <asp:Parameter Name="lv_date_7" Type="DateTime" />
            <asp:Parameter Name="lv_date_8" Type="DateTime" />
            <asp:Parameter Name="lv_date_9" Type="DateTime" />
            <asp:Parameter Name="lv_date_10" Type="DateTime" />
            <asp:Parameter Name="lv_delivery_1" Type="Int32" />
            <asp:Parameter Name="lv_delivery_2" Type="Int32" />
            <asp:Parameter Name="lv_delivery_3" Type="Int32" />
            <asp:Parameter Name="lv_delivery_4" Type="Int32" />
            <asp:Parameter Name="lv_delivery_5" Type="Int32" />
            <asp:Parameter Name="lv_delivery_6" Type="Int32" />
            <asp:Parameter Name="lv_delivery_7" Type="Int32" />
            <asp:Parameter Name="lv_delivery_8" Type="Int32" />
            <asp:Parameter Name="lv_delivery_9" Type="Int32" />
            <asp:Parameter Name="lv_delivery_10" Type="Int32" />
            <asp:Parameter Name="RfqEstNo" Type="string" />
            </SelectParameters>
    </asp:ObjectDataSource>

</div>

        
        
   
   
  
  
  
           
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
</body>
</html>