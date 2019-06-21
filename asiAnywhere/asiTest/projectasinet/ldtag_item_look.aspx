<%@ Page Language="C#" AutoEventWireup="true" Inherits="ldtag_item_look" Codebehind="ldtag_item_look.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>Finished Goods Information</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
<link rel="stylesheet" href="include/dhtmlwindow.css" type="text/css" />
</head>
<body>
    &nbsp;&nbsp;
    <form id="form1" runat="server" defaultfocus="txtSearchValue" >
    
  
    <div id="searchdiv" runat="server">
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
                      <asp:ListItem Value="i-no">Item </asp:ListItem>  
                     
                      
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
  
    <div id="gridviewdiv" runat="server">
        <asp:GridView ID="GridView1"  AllowPaging="True" runat="server" AllowSorting="True"  AutoGenerateColumns="False" DataSourceID="ObjectDataSource1"
            Style="position: static"  EmptyDataText="No Records Found" Width="100%" BorderStyle="Dotted" CssClass="Grid">
            <SelectedRowStyle CssClass="GridSelected" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <RowStyle CssClass="shade"  />   
            <HeaderStyle  HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" BackColor="Teal" ForeColor="White" />
            
            <Columns>
                <asp:TemplateField>
                  <ItemStyle HorizontalAlign=Center Wrap="False" />
                   <ItemTemplate>       
		    <a href="#" onClick="javascript:top.opener.window.LoadItemLookup('<%#DataBinder.Eval(Container,"DataItem.itemno")%>');window.close();">Select</a>             	    
                   </ItemTemplate>
                </asp:TemplateField> 
                <asp:BoundField DataField="itemno" HeaderText="Item#" SortExpression="itemno" />
                <asp:BoundField DataField="iname" HeaderText="Name" ItemStyle-Wrap="false" SortExpression="iname" />                
                <asp:BoundField DataField="cust" HeaderText="Customer" SortExpression="cust" />                
                <asp:BoundField DataField="part-dscr1" HeaderText="Item Description" SortExpression="part-dscr1" />
                
                 <%--<asp:BoundField DataField="actnum" HeaderText="actnum" SortExpression="actnum" />
                <asp:BoundField DataField="actdesc" HeaderText="actdesc" SortExpression="actdesc" />--%>
                
                
                
            </Columns>
        </asp:GridView>
        <input type="button" name="close" class="buttonM" id="close" value="Close" onclick="javascript:window.close()" />
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="SelectFgAllItemLook" TypeName="browspo">
            <SelectParameters>               
                <asp:Parameter Name="prmUser"  Type="String" />
                <asp:Parameter Name="prmAction" DefaultValue="Select" Type="String"  />
                <asp:Parameter Name="prmField" Type="String" />
                <asp:Parameter Name="prmCondition" Type="String" />
                <asp:Parameter Name="prmText" Type="String" />
                <asp:Parameter Name="prmIndu"  Type="String" />
                <asp:Parameter Name="prmMatType" Type="String"  />
                <asp:Parameter Name="prmPoNo" Type="String" />
                <asp:Parameter Name="prmJob" Type="String" />
                <asp:Parameter Name="prmJob2" Type="String" />
                <asp:Parameter Name="prmComp"  Type="String" />
                <asp:Parameter Name="prmType" DefaultValue="ItemFG" Type="String"  />
                <asp:Parameter Name="prmpoline" Type="Int32" />
                <asp:Parameter Name="prmino" Type="String" />
                <asp:Parameter Name="prmsno"  Type="Int32" />
                <asp:Parameter Name="prmbno" Type="Int32" />
                <asp:Parameter Name="prmqty" Type="Int32" />
                <asp:Parameter Name="prmpoitemtype"  Type="String" />
                <asp:Parameter Name="prmCust" Type="String" />
                <asp:Parameter Name="prmcost" Type="Decimal" />
                <asp:Parameter Name="prmconsuom"  Type="String" />
                <asp:Parameter Name="prmprqtyuom" Type="String" />
                <asp:Parameter Name="prmpruom" Type="String" />
                <asp:Parameter Name="prmdiscount"  Type="Decimal" />
                                
            </SelectParameters>
        </asp:ObjectDataSource>
    
    </div>
    </form>
</body>
</html>


