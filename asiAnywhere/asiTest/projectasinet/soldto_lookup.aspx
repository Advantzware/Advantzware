<%@ Page Language="C#" AutoEventWireup="true" Inherits="soldto_lookup" Codebehind="soldto_lookup.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>SoldTo Lookup</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
<link rel="stylesheet" href="include/dhtmlwindow.css" type="text/css" />
</head>
<body>
   
    <form id="form1" runat="server" defaultfocus="txtSearchValue">
    
    <div>
    <asp:Panel ID="searchpanel" runat="server" DefaultButton="Button1">
    <table id="tblSearch" runat="server" cellSpacing="1" cellPadding="5" width="100%" border="0"  bgcolor=black>
  <tr>
  <td class="shade"><asp:button id="Button1" runat="server" Width="40px" CssClass="button" Text="Go" OnClick="btnSearch_Click"></asp:button>&nbsp;<br /><br />
                  <asp:button id="Button2" runat="server" Width="40px" CssClass="button" Text=" All" OnClick="btnShowAll_Click" ></asp:button>&nbsp;
 </td>
  <td id="tdSearch" runat="server" class="shade" vAlign="middle" align="center" width="800">&nbsp;                
                <B>Search for:&nbsp; </B>&nbsp;&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchField" runat="server">
                    <asp:ListItem Value="soldid">Sold Id</asp:ListItem>  
                    <asp:ListItem Value="sold-name">Sold Name</asp:ListItem>  
                    
                                         
                  </asp:dropdownlist>&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchOperation" runat="server">
                     <asp:ListItem Value="BEGIN">BEGINS</asp:ListItem> 
                     <asp:ListItem Value="EQUAL">EQUAL</asp:ListItem>                                      
                  </asp:dropdownlist>
                  <asp:textbox id="txtSearchValue" runat="server" Width="136px"></asp:textbox>
                  
 </td>
  </tr>
  </table>
  </asp:Panel>
  </div>
  
    <div>
        <asp:GridView ID="GridView1" AllowPaging="true" PageSize="10" AllowSorting="true" runat="server" AutoGenerateColumns="False" DataSourceID="ObjectDataSource1"
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
		    <a href="#" onClick="javascript:top.opener.window.SoldToLookup('<%#DataBinder.Eval(Container,"DataItem.soldid")%>','<%#DataBinder.Eval(Container,"DataItem.Name")%>','<%#DataBinder.Eval(Container,"DataItem.Addr1")%>','<%#DataBinder.Eval(Container,"DataItem.Addr2")%>','<%#DataBinder.Eval(Container,"DataItem.City1")%>','<%#DataBinder.Eval(Container,"DataItem.State1")%>','<%#DataBinder.Eval(Container,"DataItem.Zip1")%>');window.close();">Select</a>             	    
                   </ItemTemplate>
                </asp:TemplateField>            

                <asp:BoundField DataField="soldid" HeaderText="Soldto" SortExpression="soldid" />
                <asp:BoundField DataField="Name" HeaderText="Name" SortExpression="Name" />
                <asp:BoundField DataField="Addr1" HeaderText="Addr1" SortExpression="Addr1" />
                <asp:BoundField DataField="Addr2" HeaderText="Addr2" SortExpression="Addr2" />
                <asp:BoundField DataField="City1" HeaderText="City" SortExpression="City1" />
                <asp:BoundField DataField="State1" HeaderText="State" SortExpression="State1" />
                <asp:BoundField DataField="Zip1" HeaderText="Zip" SortExpression="Zip1" />
               
            </Columns>
        </asp:GridView>
        <input type="button" name="close" class="buttonM" id="close" value="Close" onclick="javascript:window.close()" />
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="SoldToLookup" TypeName="LookUp">
            <SelectParameters>               
                                
                               <asp:Parameter Name="prmAction" DefaultValue = "" Type="String" />
                                <asp:Parameter Name="prmUser" Type="String" />
                                <asp:Parameter Name="prmField" Type="String" />
                                <asp:Parameter Name="prmCondition" Type="String" />
                                <asp:Parameter Name="prmText" Type="String" />
                                <asp:QueryStringParameter QueryStringField="look" Name="prmSold" Type="String" />
               
            </SelectParameters>
        </asp:ObjectDataSource>
    
    </div>
    </form>
</body>
</html>

