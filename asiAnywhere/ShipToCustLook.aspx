<%@ Page Language="C#" AutoEventWireup="true" Inherits="shiptoLookup" Title="ShipIdLook" Codebehind="ShipToCustLook.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
<link rel="stylesheet" href="include/dhtmlwindow.css" type="text/css" />
</head>
<script type="text/javascript" src="include/dhtmlwindow.js"></script>
<body>
    <form id="form1" runat="server" defaultfocus="txtSearchValue">
    <asp:Panel ID="Panel1" runat="server" DefaultButton="Button1">
    <div>
  <table id="tblSearch" cellSpacing="1" cellPadding="5" width="100%" border="0"  bgcolor=black>
  <tr>
  <td class="shade">
  <asp:button id="Button1" runat="server"  width="50px" CssClass="button" Text="Go" OnClick="btnSearch_Click"></asp:button>
                  <asp:button id="Button2" runat="server"  width="50px" CssClass="button" Text="All" OnClick="btnShowAll_Click" ></asp:button>&nbsp;</td>
  <td id="tdSearch" runat="server" class="shade" vAlign="middle" align="center" width="800">&nbsp;                
                <B>Search for:&nbsp; </B>&nbsp;&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchField" runat="server">                                        
                      <%--<asp:ListItem Value="custnum">Cust#</asp:ListItem>--%>
                      <asp:ListItem Value="ship-id">Ship To</asp:ListItem>      
                      <asp:ListItem Value="ship-name">Ship Name</asp:ListItem>                                                             
                  </asp:dropdownlist>&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchOperation" runat="server">                   
                    <asp:ListItem Value="EQUAL">EQUAL</asp:ListItem>
                    <asp:ListItem Value="BEGIN">BEGIN</asp:ListItem>                                      
                  </asp:dropdownlist>
                  <asp:textbox id="txtSearchValue" runat="server" Width="136px"></asp:textbox>
                  
 </td>
  </tr>
  </table>
  </div>
  </asp:Panel>   
    <div>
        &nbsp;&nbsp;
        <asp:GridView ID="GridView1" runat="server" AutoGenerateColumns="False" OnSelectedIndexChanged="GridView1_SelectedIndexChanged"
            DataSourceID="ObjectDataSource1" AllowPaging="True" AllowSorting="True"  
            EmptyDataText="No Records Found" Width="100%" BorderStyle="Dotted" CssClass="Grid" OnUnload="GridView1_Unload">
            <SelectedRowStyle CssClass="GridSelected" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <RowStyle CssClass="shade"  />   
            <HeaderStyle CssClass="gridrowhdr" HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" BackColor="Gray" ForeColor="White" />
            <Columns>
                 <asp:TemplateField>
                  <ItemStyle HorizontalAlign=Center />
                   <ItemTemplate>       
		    <a href="#" onClick="javascript:top.opener.window.ShipTo2Lookup('<%#DataBinder.Eval(Container,"DataItem.Shipid")%>','<%#DataBinder.Eval(Container,"DataItem.custnum")%>' ,'<%#DataBinder.Eval(Container,"DataItem.Name")%>','<%#DataBinder.Eval(Container,"DataItem.Addr1")%>');window.close();">Select</a>             	    
                   </ItemTemplate>
                </asp:TemplateField>
                <asp:BoundField DataField="custnum" Visible="false" HeaderText="custnum" SortExpression="custnum" />
                <asp:BoundField DataField="Shipid" HeaderText="Shipid" SortExpression="Shipid" />
                <asp:BoundField DataField="Name" HeaderText="Name" SortExpression="Name" />
                <asp:BoundField DataField="Addr1" HeaderText="Address" SortExpression="Addr1" />
                <asp:BoundField DataField="City1" HeaderText="City" SortExpression="City1" />
                <asp:BoundField DataField="State1" HeaderText="State" SortExpression="State1" />
                <%--<asp:BoundField DataField="Zip1" HeaderText="Zip1" SortExpression="Zip1" />
                <asp:BoundField DataField="carrier1" HeaderText="carrier1" SortExpression="carrier1" />
                <asp:BoundField DataField="Loc1" HeaderText="Loc1" SortExpression="Loc1" />--%>
                
            </Columns>
        </asp:GridView>
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
         SelectMethod="SelectShipLook" TypeName="LookUp">
            <SelectParameters>                    
                <asp:Parameter DefaultValue="" Name="prmAction" Type="String" />
                <asp:Parameter Name="prmUser" Type="String" />
                <asp:Parameter Name="prmCondition" Type="String" />
                <asp:Parameter Name="prmText" Type="String" />
                <asp:Parameter Name="prmField" Type="String" />
                <asp:QueryStringParameter Name="prmShip" QueryStringField="look" Type="String" />                                 
            </SelectParameters>
        </asp:ObjectDataSource>
    </div>
    </form>
</body>
</html>
