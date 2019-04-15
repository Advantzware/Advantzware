<%@ Page Language="C#" AutoEventWireup="true" Inherits="quote_item_popup" Title="Customer Item Lookup" Codebehind="quote_item_popup.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head runat="server">
    <title>Customer Lookup</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
<link rel="stylesheet" href="include/dhtmlwindow.css" type="text/css" />

</head>
<script type="text/javascript" src="include/dhtmlwindow.js"></script>
<script >
    var counter = 0;
    var pattern = '^GridView1';
    function Check(parentChk) {
        var elements = document.getElementsByTagName("INPUT");

        for (i = 0; i < elements.length; i++) {
            if (parentChk.checked == true) {
                if (IsCheckBox(elements[i]) && IsMatch(elements[i].id)) {
                    elements[i].checked = true;
                }
            }
            else {
                elements[i].checked = false;
            }
        }

    }

    function IsMatch(id) {
        var regularExpresssion = new RegExp(pattern);
        if (id.match(regularExpresssion)) return true;
        else return false;
    }
    function IsCheckBox(chk) {
        if (chk.type == 'checkbox') return true;
        else return false;
    }

    function AddEvent(obj, evType, fn) {
        if (obj.addEventListener) {
            obj.addEventListener(evType, fn, true);
            return true;
        }

        else if (obj.attachEvent) {
            var r = obj.attachEvent("on" + evType, fn);
            return r;
        }
        else {
            return false;
        }
    }

    function AttachListener() {

        var elements = document.getElementsByTagName("INPUT");



        for (i = 0; i < elements.length; i++) {

            if (IsCheckBox(elements[i]) && IsMatch(elements[i].id)) {

                AddEvent(elements[i], 'click', CheckChild);

            }

        }

    }

    function CheckChild(e) {

        var evt = e || window.event;



        var obj = evt.target || evt.srcElement



        if (obj.checked) {

            if (counter < GetChildCheckBoxCount())

            { counter++; }

        }



        else {

            if (counter > 0) { counter--; }

        }



        if (counter == GetChildCheckBoxCount())

        { document.getElementById("chkAll").checked = true; }

        else if (counter < GetChildCheckBoxCount()) { document.getElementById("chkAll").checked = false; }



    }

    function Check(parentChk) {

        var elements = document.getElementsByTagName("INPUT");



        for (i = 0; i < elements.length; i++) {

            if (parentChk.checked == true) {

                if (IsCheckBox(elements[i]) && IsMatch(elements[i].id)) {

                    elements[i].checked = true;

                }

            }

            else {

                elements[i].checked = false;

                // reset the counter

                counter = 0;

            }

        }



        if (parentChk.checked == true) {

            counter = GetChildCheckBoxCount();

        }



    }
</script>

<body>
    <form id="form1" runat="server">
   
 
    <div>
    <br />
    <br />
        <asp:GridView ID="GridView1" runat="server" AutoGenerateColumns="False" OnUnload="grid_unload" OnSelectedIndexChanged="GridView1_SelectedIndexChanged"
            DataSourceID="ObjectDataSource1" AllowPaging="True" AllowSorting="True"  
            EmptyDataText="No Records Found" Width="100%" BorderStyle="Dotted" CssClass="Grid">
            <SelectedRowStyle CssClass="GridSelected" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <RowStyle CssClass="shade"  />   
            <HeaderStyle CssClass="gridrowhdr" HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" BackColor="Gray" ForeColor="White" />
            <Columns > 
             <asp:TemplateField>
            <HeaderTemplate>
        <input type="checkbox" id="chkAll" name="chkAll" onclick="Check(this)" />

        </HeaderTemplate>
    <ItemTemplate>
    <asp:CheckBox ID="chk1" runat="server" />
    </ItemTemplate>
    </asp:TemplateField>
                <asp:TemplateField HeaderText="company" Visible="false" >
                    
                    <ItemTemplate>
                        <asp:Label ID="Label1" runat="server" Text='<%# Bind("[arinv-ino]") %>'></asp:Label>
                    </ItemTemplate>
                </asp:TemplateField> 
              
                <asp:BoundField DataField="arinv-ino" HeaderText="FG Item#" 
                      ItemStyle-Wrap="false" SortExpression="arinv-ino" >
                </asp:BoundField>
                <asp:BoundField DataField="arinv-name" HeaderText="FG Name" 
                      ItemStyle-Wrap="false" SortExpression="arinv-name" >
                </asp:BoundField>
                
                <asp:BoundField DataField="arinv-qty" HeaderText="Qty Invoiced" 
                      ItemStyle-Wrap="false" SortExpression="arinv-qty" >
                </asp:BoundField>
                <asp:BoundField DataField="arinv-unitpr" HeaderText="Last Price" 
                      ItemStyle-Wrap="false" SortExpression="arinv-unitpr" >
                </asp:BoundField>
                <asp:BoundField DataField="arinv-qtyuom" HeaderText="UOM" 
                      ItemStyle-Wrap="false" SortExpression="arinv-qtyuom" >
                </asp:BoundField>
               
                
                </Columns>
            
        </asp:GridView>
        <asp:Button ID="Select_button" runat="server" CssClass="button" Text="Ok" OnClick="Select_item" />
        <input type="button" name="close" class="buttonM" id="close" value="Close" onclick="javascript:window.opener.location.href='ViewQuote.aspx';self.close();" />
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="QuoteItemPupup" TypeName="Corrugated">
            <SelectParameters>
                <asp:Parameter DefaultValue="View" Name="prmAction" Type="String" />
            <asp:Parameter Name="prmUser" Type="String" />
                <asp:SessionParameter DefaultValue="" Name="prmQuote" SessionField="quote_no" Type="Int32" />
                <asp:Parameter Name="prmCondition" Type="String" />
                <asp:Parameter Name="prmText" Type="String" />
            </SelectParameters>
        </asp:ObjectDataSource>
        
    </div>
    </form>
</body>
</html>
