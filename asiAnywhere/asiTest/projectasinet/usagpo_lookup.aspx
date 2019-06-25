<%@ Page Language="C#" AutoEventWireup="true" Inherits="usag_look" Codebehind="usagpo_lookup.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>Customer Po Lookup</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
<link rel="stylesheet" href="include/dhtmlwindow.css" type="text/css" />
</head>
<body>
    &nbsp;&nbsp;
    <form id="form1" runat="server" defaultfocus="txtSearchValue">
    <div>
    <asp:Panel ID="Panel1" runat="server" DefaultButton="Button1">
  <table id="tblSearch" cellSpacing="1" cellPadding="5" width="100%" border="0"  bgcolor=black>
  <tr>
  <td class="shade"><asp:button id="Button1" runat="server" Width="40px" CssClass="button" Text="Go" OnClick="btnSearch_Click"></asp:button>&nbsp;<br /><br />
                  <asp:button id="Button2" runat="server" Width="40px" CssClass="button" Text=" All" OnClick="btnShowAll_Click" ></asp:button>&nbsp;
 </td>
  <td id="tdSearch" runat="server" class="shade" vAlign="middle" align="center" width="800">&nbsp;                
                <B>Search for:&nbsp; </B>&nbsp;&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchField" runat="server">
                    <%--<asp:ListItem Value="Any">Any</asp:ListItem>--%>
                    <asp:ListItem Value="pono">PO. Num</asp:ListItem>  
                      <asp:ListItem Value="i-no">Item No</asp:ListItem>  
                     
                      
                  </asp:dropdownlist>&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchOperation" runat="server">
                    <%--<asp:ListItem Value="Contains">Contains</asp:ListItem>--%>
                    
                    <asp:ListItem Value="BEGIN">BEGIN</asp:ListItem>
                     <asp:ListItem Value="EQUAL">EQUAL</asp:ListItem>                 
                  </asp:dropdownlist>
                  <asp:textbox id="txtSearchValue" runat="server" Width="136px"></asp:textbox>
                  
 </td>
  </tr>
  </table>
  </asp:Panel>
  </div>
    <div>
        <asp:GridView ID="GridView1" AllowPaging="True" runat="server" AutoGenerateColumns="False" AllowSorting="true" DataSourceID="ObjectDataSource1"
            Style="position: static" OnSelectedIndexChanged="GridView1_SelectedIndexChanged" EmptyDataText="No Records Found" Width="100%" BorderStyle="Dotted" CssClass="Grid">
            <SelectedRowStyle CssClass="GridSelected" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <RowStyle CssClass="shade"  />   
            <HeaderStyle  HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" BackColor="Teal" ForeColor="White" />
            
            <Columns>
            <asp:TemplateField>
                  <ItemStyle HorizontalAlign=Center Wrap="False" />
                   <ItemTemplate>       
		    <a href="#" onClick="javascript:top.opener.window.UsagPoLookup('<%#DataBinder.Eval(Container,"DataItem.itemno")%>', '<%#DataBinder.Eval(Container,"DataItem.pono")%>', '<%#DataBinder.Eval(Container,"DataItem.orderno")%>', '<%#DataBinder.Eval(Container,"DataItem.lineno")%>', '<%#DataBinder.Eval(Container,"DataItem.usagQty")%>', '<%#DataBinder.Eval(Container,"DataItem.usagCustPart")%>', '<%#DataBinder.Eval(Container,"DataItem.VanderCode")%>', '<%#DataBinder.Eval(Container,"DataItem.PlantCode")%>', '<%#DataBinder.Eval(Container,"DataItem.DeptCode")%>', '<%#DataBinder.Eval(Container,"DataItem.JobNo")%>', '<%#DataBinder.Eval(Container,"DataItem.JobNo2")%>', '<%#DataBinder.Eval(Container,"DataItem.Tprice")%>');window.close();">Select</a>             	    
                   </ItemTemplate>
                </asp:TemplateField> 
                <asp:BoundField DataField="pono" HeaderText="Po. Num" SortExpression="pono" />
                <asp:BoundField DataField="itemno" HeaderText="Item No" SortExpression="itemno" />
                
                <asp:BoundField DataField="orderno" HeaderText="Order" SortExpression="orderno" />
                <asp:BoundField DataField="lineno" HeaderText="Line#" SortExpression="lineno" />
                <asp:BoundField DataField="usagQty" HeaderText="Quantity" SortExpression="usagQty" />
               <asp:BoundField DataField="usagCustPart" HeaderText="usagCustPart" SortExpression="usagCustPart" />
                <asp:BoundField DataField="VanderCode" HeaderText="VanderCode" SortExpression="VanderCode" />
                <asp:BoundField DataField="PlantCode" HeaderText="PlantCode" SortExpression="PlantCode" />
                <asp:BoundField DataField="DeptCode" HeaderText="DeptCode" SortExpression="DeptCode" />
                <asp:BoundField DataField="JobNo" HeaderText="JobNo" SortExpression="JobNo" />
                <asp:BoundField DataField="JobNo2" HeaderText="JobNo2" SortExpression="JobNo2" />
                <asp:BoundField DataField="Tprice" HeaderText="Tprice" SortExpression="Tprice" />
              
                
            </Columns>
        </asp:GridView>
        <input type="button" name="close" class="buttonM" id="close" value="Close" onclick="javascript:window.close()" />
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="UsagPoLook" TypeName="LookUp">
            <SelectParameters>               
                                <asp:Parameter Name="prmAction" Type="String" />
                                <asp:Parameter Name="prmUser"  Type="String" />
                <asp:Parameter Name="prmComp" Type="String" />
                <asp:Parameter Name="prmField" Type="String" />
                <asp:Parameter Name="prmCondition" Type="String" />
                <asp:Parameter Name="prmText" Type="String" />
            </SelectParameters>
        </asp:ObjectDataSource>
    
    </div>
    </form>
</body>
</html>

