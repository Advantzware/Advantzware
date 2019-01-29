<%@ Page Language="C#" AutoEventWireup="true" Inherits="corr_vendor_cost" Title="Vendor Cost" Codebehind="corr_vendor_cost.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>Vendor Cost</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
<link rel="stylesheet" href="include/dhtmlwindow.css" type="text/css" />
</head>
<script type="text/javascript" src="include/dhtmlwindow.js"></script>
<script type="text/javascript">
    window.onunload = refreshParent;
        
    function refreshParent() {
        window.opener.location.href = window.opener.location.href;

        if (window.opener.progressWindow) {
            window.opener.progressWindow.close()
        }      
    }

    function addcostclick() {
        var logic = document.getElementById("HiddenField1").value;
        
        if (logic == "")
            logic = "no";
                    
        if (logic == "no") {
            var NewWindow = window.open("corr_vend_fgitem.aspx", "AddVendorCostWindow", "width=500,height=530,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
       
        if (logic == "yes") {
            var NewWindow = window.open("corr_vendor_item.aspx", "AddVendorCostWindow2", "width=500,height=530,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
    }
</script>
<body>
    <form id="form1" runat="server">
    <asp:HiddenField ID="HiddenField1" runat="server" />
     
    <div align="center">
    <fieldset>
        
        <table>
        <tr><td>
        &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;       
        <asp:GridView ID="GridView1" runat="server" AutoGenerateColumns="False" OnSelectedIndexChanged="GridView1_SelectedIndexChanged"
            DataSourceID="ObjectDataSource1" AllowPaging="True" AllowSorting="True"  DataKeyNames="vRMItem" Width="350px"
            EmptyDataText="No Records Found"  BorderStyle="Dotted" CssClass="Grid">
            <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <RowStyle CssClass="shade"  />   
            <HeaderStyle CssClass="gridrowhdr" HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" BackColor="Gray" ForeColor="White" />
            <Columns>
                <asp:CommandField ShowSelectButton="True" ButtonType="Image" SelectImageUrl="~/Images/sel.gif" SelectText="" > 
                    <ItemStyle Width="10px" />
                </asp:CommandField>                    
                <asp:BoundField DataField="vForm" HeaderText="Form" SortExpression="vForm" />
                <asp:BoundField DataField="vBlank" HeaderText="Blank" 
                    SortExpression="vBlank" />                
                
                <asp:BoundField DataField="vRMItem" ItemStyle-Wrap="false" HeaderText="RM Item" 
                    SortExpression="vRMItem" />
                <asp:BoundField DataField="vName" HeaderText="Name" ItemStyle-Wrap="false" SortExpression="vName" />
                <asp:BoundField DataField="vLength" HeaderText="Length" 
                    SortExpression="vLength" />
                <asp:BoundField DataField="vWidth" HeaderText="Width" 
                    SortExpression="vWidth" />
                <asp:BoundField DataField="vDepth" HeaderText="Depth" 
                    SortExpression="vDepth" />
                <asp:BoundField DataField="vUp" HeaderText="Up" SortExpression="vUp" />
                              
                 <asp:TemplateField HeaderText="logic" Visible="false">
                     <ItemTemplate>
                     <asp:Label runat="server" ID="recidlabel" Text='<%# Eval("vRecid") %>'></asp:Label>
                     </ItemTemplate>
                     </asp:TemplateField>
              
                                
            </Columns>
        </asp:GridView>
        <br /><br />    
         </td></tr>
        <tr><td> 
        <input type="button" id="addcostButton" value="Add Vendor Cost" class="button" onclick="addcostclick()" />
        <input type="button" id="CloseButton" value="Cancel" class="button" onclick="javascript:self.close();" />
        <br /><br />
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
         SelectMethod="VendorCost" TypeName="Corrugated">
            <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
                    
                <asp:Parameter DefaultValue="select" Name="prmAction" Type="String" />
                <asp:Parameter Name="prmComp" Type="String" />
                <asp:SessionParameter Name="prmEstNum" SessionField="corr_vendor_cost_est" Type="String" />
                <asp:Parameter Name="prmFormNo" Type="Int32" />
                <asp:Parameter Name="prmBlankNo" Type="Int32" />
                <asp:Parameter DefaultValue="" Name="prmItem" Type="Int32" />                
                 
            </SelectParameters>
        </asp:ObjectDataSource>
         &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
         </td></tr>
        <tr><td>
         <asp:GridView ID="GridView_item" runat="server" AutoGenerateColumns="False" 
            DataSourceID="ObjectDataSource2" AllowPaging="True" AllowSorting="True"  
            EmptyDataText="No Records Found" Width="350px"  BorderStyle="Dotted" CssClass="Grid">
             <Columns>
                 
                 <asp:BoundField DataField="vVander#" HeaderText="Vander#" 
                     SortExpression="vVander#" />
                 <asp:BoundField DataField="vVaName" HeaderText="Name" 
                     SortExpression="vVaName" />
                 <asp:BoundField DataField="vTons" HeaderText="Tons" SortExpression="vTons" />
                 <asp:BoundField DataField="vMsf" HeaderText="Msf" SortExpression="vMsf" />
                 <asp:BoundField DataField="vCost" HeaderText="Cost" SortExpression="vCost" />
                 <asp:BoundField DataField="vLead" HeaderText="Lead" SortExpression="vLead" />
                 <asp:BoundField DataField="vExtCost" HeaderText="ExtCost" 
                     SortExpression="vExtCost" />
                 <asp:BoundField Visible="false" DataField="vRecid" HeaderText="vRecid" 
                     SortExpression="vRecid" />
                     <asp:TemplateField >
                     <ItemTemplate>
                     <asp:Label runat="server" Visible="false"  ID="recidlabel" Text='<%# Eval("vRecid") %>'></asp:Label>
                     </ItemTemplate>
                     </asp:TemplateField>
             </Columns>
            <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <RowStyle CssClass="shade"  />   
            <HeaderStyle CssClass="gridrowhdr" HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" BackColor="Gray" ForeColor="White" />
            
        </asp:GridView>
        
         <asp:ObjectDataSource ID="ObjectDataSource2" runat="server" OldValuesParameterFormatString="original_{0}"
         SelectMethod="VendorCost" TypeName="Corrugated">
            <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />                    
                <asp:Parameter DefaultValue="select" Name="prmAction" Type="String" />
                <asp:Parameter Name="prmComp" Type="String" />
                <asp:SessionParameter Name="prmEstNum" SessionField="corr_vendor_cost_est" Type="String" />
                <asp:Parameter Name="prmFormNo" Type="Int32" />
                <asp:Parameter Name="prmBlankNo" Type="Int32" /> 
                <asp:ControlParameter ControlID="GridView1" DefaultValue="" Name="prmItem" 
                    PropertyName="SelectedValue" Type="String" />
            </SelectParameters>
        </asp:ObjectDataSource>
        </td></tr>
        </table>   
        </fieldset>
    </div>
    </form>
</body>
</html>
