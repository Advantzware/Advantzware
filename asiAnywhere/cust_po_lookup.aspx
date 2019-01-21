<%@ Page Language="C#" AutoEventWireup="true" Inherits="cust_po_lookup" Codebehind="cust_po_lookup.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>Customer PO Lookup</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
<link rel="stylesheet" href="include/dhtmlwindow.css" type="text/css" />
</head>
<body>
    &nbsp;&nbsp;
    <form id="form1" runat="server" defaultfocus="txtSearchValue" DefaultButton="Button1">
    <div>
  <table id="tblSearch" cellSpacing="1" cellPadding="5" width="100%" border="0"  bgcolor=black>
  <tr>
  <td class="shade"><asp:button id="Button1" runat="server" Width="40px" CssClass="button" Text="Go" OnClick="btnSearch_Click"></asp:button>&nbsp;<br /><br />
                  <asp:button id="Button2" runat="server" Width="40px" CssClass="button" Text=" All" OnClick="btnShowAll_Click" ></asp:button>&nbsp;
 </td>
  <td id="tdSearch" runat="server" class="shade" vAlign="middle" align="center" width="800">&nbsp;                
                <B>Search for:&nbsp; </B>&nbsp;&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchField" runat="server">
                    <%--<asp:ListItem Value="Any">Any</asp:ListItem>--%>
                      <asp:ListItem Value="Boardpo">Board Po</asp:ListItem>  
                     
                      
                  </asp:dropdownlist>&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchOperation" runat="server">
                    <%--<asp:ListItem Value="Contains">Contains</asp:ListItem>--%>
                    
                    <%--<asp:ListItem Value="BEGIN">BEGIN</asp:ListItem>--%>
                     <asp:ListItem Value="EQUAL">EQUAL</asp:ListItem>                 
                  </asp:dropdownlist>
                  <asp:textbox id="txtSearchValue" runat="server" Width="136px"></asp:textbox>
                  
 </td>
  </tr>
  </table>
  </div>
    <div>
        <asp:GridView ID="GridView1" AllowPaging="true" PageSize="10" runat="server" AllowSorting="true" AutoGenerateColumns="False" DataSourceID="ObjectDataSource1"
            Style="position: static" OnSelectedIndexChanged="GridView1_SelectedIndexChanged" EmptyDataText="No Records Found" Width="500px" BorderStyle="Dotted" CssClass="Grid">
            <SelectedRowStyle CssClass="GridSelected" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <RowStyle CssClass="shade"  />   
            <HeaderStyle  HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" BackColor="teal" ForeColor="White" />
            
            <Columns>
                <asp:TemplateField>
                  <ItemStyle HorizontalAlign=Center Wrap="False" />
                   <ItemTemplate>       
		    <a href="#" onClick ="javascript:top.opener.window.CustPoLookup('<%#DataBinder.Eval(Container,"DataItem.vpono")%>','<%#DataBinder.Eval(Container,"DataItem.vvendor")%>');window.close();">Select</a>             	    
                   </ItemTemplate>
                </asp:TemplateField>            

                 <asp:BoundField DataField="vpono" HeaderText="PO Number" SortExpression="vpono" />
                <asp:BoundField DataField="vpodate" HeaderText="Po Date" SortExpression="vpodate" />
                <asp:BoundField DataField="vvendor" HeaderText="Vendor" SortExpression="vvendor" />
                <asp:BoundField DataField="vprint" HeaderText="Print" SortExpression="vprint" />
                <asp:BoundField DataField="vstat" HeaderText="Status" SortExpression="vstat" />
                
               
                
                
            </Columns>
        </asp:GridView>
        <input type="button" name="close" class="buttonM" id="close" value="Close" onclick="javascript:window.close()" />
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="SelectBoardRepLookup" TypeName="LookUp">
            <SelectParameters>               
                                <asp:Parameter Name="prmAction" Type="String" />
                                <asp:Parameter Name="prmUser"  Type="String" />
                <asp:Parameter Name="prmField" Type="String" />
                <asp:Parameter Name="prmCondition" Type="String" />
                <asp:Parameter Name="prmText" Type="String" />
            </SelectParameters>
        </asp:ObjectDataSource>
    
    </div>
    </form>
</body>
</html>

