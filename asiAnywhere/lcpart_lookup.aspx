<%@ Page Language="C#" AutoEventWireup="true" Debug="true" Inherits="lcpart_lookup" Codebehind="lcpart_lookup.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>CustPart Lookup</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
<link rel="stylesheet" href="include/dhtmlwindow.css" type="text/css" />
</head>
<body>
    &nbsp;&nbsp;
    <form id="form1" runat="server" defaultfocus="txtSearchValue" defaultbutton="Button1">
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
                      <asp:ListItem Value="cust">Customer#</asp:ListItem>  
                      <asp:ListItem Value="partno">Customer Part#</asp:ListItem> 
                       <asp:ListItem Value="fgitem">FG Item#</asp:ListItem> 
                        <asp:ListItem Value="i-name">Name</asp:ListItem>  
                     
                      
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
  </div>
    <div>
        <asp:GridView ID="GridView1"  AllowPaging="True" runat="server" 
            AllowSorting="True" AutoGenerateColumns="False" DataSourceID="ObjectDataSource1"
            Style="position: static" EmptyDataText="No Records Found" Width="100%" 
            BorderStyle="Dotted" CssClass="Grid">
            <SelectedRowStyle CssClass="GridSelected" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <RowStyle CssClass="shade"  />   
            <HeaderStyle  HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" BackColor="teal" ForeColor="White" />
            
            <Columns>
             <asp:TemplateField>
                  <ItemStyle HorizontalAlign=Center Wrap="False" />
                   <ItemTemplate>       
		    <a href="#" onClick="javascript:top.opener.window.lcpartlook('<%#DataBinder.Eval(Container,"DataItem.lc-partno")%>','<%#DataBinder.Eval(Container,"DataItem.lc-ino")%>','<%#DataBinder.Eval(Container,"DataItem.lc-iname")%>','<%#DataBinder.Eval(Container,"DataItem.lc-style")%>','<%#DataBinder.Eval(Container,"DataItem.lc-price")%>','<%#DataBinder.Eval(Container,"DataItem.lc-uom")%>','<%#DataBinder.Eval(Container,"DataItem.lc-size")%>');window.close();">Select</a>             	    
                   </ItemTemplate>
                </asp:TemplateField> 
                 <asp:BoundField DataField="lc-custno" HeaderText="Customer#" 
                    SortExpression="lc-custno" />
                 <asp:BoundField DataField="lc-partno" HeaderText="Customer Part#"  SortExpression="lc-partno" />
                                 
                <asp:BoundField DataField="lc-ino" HeaderText="FG Item#"   SortExpression="lc-ino" ItemStyle-Wrap="false" />
                <asp:BoundField DataField="lc-iname" HeaderText="Name"  SortExpression="lc-iname" ItemStyle-Wrap="false"/>
                <asp:BoundField DataField="lc-style" HeaderText="Style" SortExpression="lc-style" ItemStyle-Wrap="false"/>
                <asp:BoundField DataField="lc-length" HeaderText="Length" SortExpression="lc-length" ItemStyle-Wrap="false"/>
                <asp:BoundField DataField="lc-width" HeaderText="Width" SortExpression="lc-width" ItemStyle-Wrap="false"/>
                <asp:BoundField DataField="lc-depth" HeaderText="Depth" SortExpression="lc-depth" ItemStyle-Wrap="false"/>
                <asp:BoundField DataField="lc-price" Visible="false" HeaderText="lc-price" SortExpression="lc-price" />
                <asp:BoundField DataField="lc-uom" Visible="false" HeaderText="lc-uom" SortExpression="lc-uom" />
                <asp:BoundField DataField="lc-size" Visible="false" HeaderText="lc-size" SortExpression="lc-size" />
                                 
            </Columns>
        </asp:GridView>
        <input type="button" name="close" class="buttonM" id="close" value="Close" onclick="javascript:window.close()" />
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="LcPartLookup" TypeName="LookUp">
            <SelectParameters>               
                 <asp:Parameter Name="prmAction" DefaultValue="jgh" Type="String" />
                 <asp:Parameter Name="prmUser"  Type="String" />
                <asp:Parameter Name="prmField" Type="String" />
                <asp:Parameter Name="prmCondition" Type="String" />
                <asp:Parameter Name="prmText" Type="String" />                
                 <asp:SessionParameter SessionField="quote_no" Name="prmQuote" Type="Int32" 
                     DefaultValue="" />
            </SelectParameters>
        </asp:ObjectDataSource>
    
    </div>
    </form>
</body>
</html>


