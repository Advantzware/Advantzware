<%@ Page Language="C#" AutoEventWireup="true" Inherits="die_lookup" Codebehind="die_lookup.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>Die Lookup</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
<link rel="stylesheet" href="include/dhtmlwindow.css" type="text/css" />
</head>
<body>
    &nbsp;&nbsp;
    <form id="form1" runat="server" defaultfocus="txtSearchValue" >
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
                      <asp:ListItem Value="code">Type </asp:ListItem>  
                     
                      
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
        <asp:GridView ID="GridView1"  AllowPaging="True" runat="server" AllowSorting="True" AutoGenerateColumns="False" DataSourceID="ObjectDataSource1"
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
		    <a href="#" onClick="javascript:top.opener.window.DieLookup('<%#DataBinder.Eval(Container,"DataItem.vspc-no")%>','<%#DataBinder.Eval(Container,"DataItem.vi-name")%>');window.close();">Select</a>             	    
                   </ItemTemplate>
                </asp:TemplateField> 
                <asp:BoundField DataField="vspc-no" HeaderText="Type" SortExpression="vspc-no" />
                <asp:BoundField DataField="vi-name" HeaderText="Description" SortExpression="vi-name" />
                <asp:BoundField DataField="vMat" HeaderText="Mat-Type" SortExpression="vMat" />
                
            </Columns>
        </asp:GridView>
        <input type="button" name="close" class="buttonM" id="close" value="Close" onclick="javascript:window.close()" />
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
            SelectMethod="SelectDieLook" TypeName="LookUp">
            <SelectParameters>               
                <asp:Parameter Name="prmUser"  Type="String" />
                <asp:Parameter Name="prmAction" Type="String"  />
                <asp:Parameter Name="prmField" Type="String" />
                <asp:Parameter Name="prmCondition" Type="String" />
                <asp:Parameter Name="prmText" Type="String" />
                 <asp:Parameter Name="prmPlateType" Type="string" />
            </SelectParameters>
        </asp:ObjectDataSource>
    
    </div>
    </form>
</body>
</html>


