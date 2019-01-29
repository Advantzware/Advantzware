<%@ Page Language="C#" AutoEventWireup="true" Inherits="fgadjlookup" Codebehind="fgadjlookup.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>Item Fg Lookup</title>
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
                   <asp:ListItem Value="itemno">FG Item</asp:ListItem>
                      <asp:ListItem Value="partno">Cust Part</asp:ListItem>  
                     
                      
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
        <asp:GridView ID="GridView1" AllowPaging="true" AllowSorting="true" PageSize="10" runat="server" AutoGenerateColumns="False" DataSourceID="ObjectDataSource1"
            Style="position: static" OnSelectedIndexChanged="GridView1_SelectedIndexChanged" EmptyDataText="No Records Found" Width="100%" BorderStyle="Dotted" CssClass="Grid">
            <SelectedRowStyle CssClass="GridSelected" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <RowStyle CssClass="shade"  />   
            <HeaderStyle  HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" BackColor="teal" ForeColor="White" />
            
            <Columns>
                <asp:TemplateField>
                  <ItemStyle HorizontalAlign=Center Wrap="False" />
                   <ItemTemplate>       
		    <a href="#" onClick="javascript:top.opener.window.fgAdjLookup('<%#DataBinder.Eval(Container,"DataItem.vItem")%>', '<%#DataBinder.Eval(Container,"DataItem.vName")%>', '<%#DataBinder.Eval(Container,"DataItem.vPartno")%>', '<%#DataBinder.Eval(Container,"DataItem.vJob1")%>', '<%#DataBinder.Eval(Container,"DataItem.vJob2")%>', '<%#DataBinder.Eval(Container,"DataItem.vLoc")%>', '<%#DataBinder.Eval(Container,"DataItem.vLocbin")%>', '<%#DataBinder.Eval(Container,"DataItem.vtag")%>', '<%#DataBinder.Eval(Container,"DataItem.vcust")%>', '<%#DataBinder.Eval(Container,"DataItem.vqty")%>');window.close();">Select</a>             	    
                   </ItemTemplate>
                </asp:TemplateField>            

                 <asp:BoundField DataField="vItem" HeaderText="FG Item" SortExpression="vItem" />
                <asp:BoundField DataField="vName" HeaderText="Name" SortExpression="vName" />
                <asp:BoundField DataField="vPartno" HeaderText="Cust Part" SortExpression="vPartno" />
                <%--<asp:BoundField DataField="vJob1" HeaderText="vJob1" SortExpression="vJob1" />
                <asp:BoundField DataField="vJob2" HeaderText="vJob2" SortExpression="vJob2" />
                <asp:BoundField DataField="vLoc" HeaderText="vLoc" SortExpression="vLoc" />
                <asp:BoundField DataField="vLocbin" HeaderText="vLocbin" SortExpression="vLocbin" />
                <asp:BoundField DataField="vtag" HeaderText="vtag" SortExpression="vtag" />
                <asp:BoundField DataField="vcust" HeaderText="vcust" SortExpression="vcust" />
                <asp:BoundField DataField="vqty" HeaderText="vqty" SortExpression="vqty" />
                --%>
                
            </Columns>
        </asp:GridView>
        <input type="button" name="close" class="buttonM" id="close" value="Close" onclick="javascript:window.close()" />
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="SelectFGadjLook" TypeName="LookUp">
            <SelectParameters>               
                                <asp:Parameter Name="prmAction"  Type="String" />
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

