<%@ Page Language="C#" AutoEventWireup="true" Inherits="BoardLook" Title="Board LookUp" Codebehind="BoardLook.aspx.cs" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>Category LookUp</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
<link rel="stylesheet" href="include/dhtmlwindow.css" type="text/css" />
</head>
<script type="text/javascript" src="include/dhtmlwindow.js"></script>
<body>
    <form id="form1" runat="server" defaultfocus="txtSearchValue">    
    <div>
    <asp:Panel ID="searchpanel" runat="server" DefaultButton="btnSearch">
  <table id="tblSearch" cellSpacing="1" cellPadding="5" width="100%" border="0"  bgcolor=black>
  <tr>
  <td class="shade" >
  <asp:button id="btnSearch" runat="server" Width="50px" CssClass="button" Text="Go" OnClick="btnSearch_Click"></asp:button>
                  <asp:button id="btnShowAll" runat="server" Width="50px" CssClass="button" Text="All" OnClick="btnShowAll_Click" ></asp:button>&nbsp;</td>
  <td id="tdSearch" runat="server" class="shade" vAlign="middle" align="center" width="800">&nbsp;                
                <B>Search for:&nbsp; </B>&nbsp;&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchField" runat="server">                    
                      <asp:ListItem Value="i-no">Item#</asp:ListItem>
                      <asp:ListItem Value="i-name">Name</asp:ListItem>
                      <asp:ListItem Value="i-code">E/R</asp:ListItem>                        
                  </asp:dropdownlist>&nbsp;&nbsp;
                  <asp:dropdownlist id="ddlSearchOperation" runat="server">                   
                    <asp:ListItem Value="EQUAL">EQUAL</asp:ListItem>
                    <asp:ListItem Value="BEGIN">BEGIN</asp:ListItem>                                     
                  </asp:dropdownlist>
                  <asp:textbox id="txtSearchValue" runat="server" Width="136px"></asp:textbox>
                  
 </td>
  </tr>
  </table>
  </asp:Panel>
  </div>
    
    <div>
        &nbsp;&nbsp;
        <asp:GridView ID="GridView1" runat="server" AutoGenerateColumns="False" OnSelectedIndexChanged="GridView1_SelectedIndexChanged"
            DataSourceID="ObjectDataSource1" AllowPaging="True" AllowSorting="True"  
            EmptyDataText="No Records Found" Width="100%" BorderStyle="Dotted" CssClass="Grid">
            <SelectedRowStyle CssClass="GridSelected" />
            <AlternatingRowStyle CssClass="GridItemOdd" />
            <EmptyDataRowStyle BorderStyle="Dotted" BorderColor="Gray" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
            <RowStyle CssClass="shade"  />   
            <HeaderStyle CssClass="gridrowhdr" HorizontalAlign="Center" VerticalAlign="Middle" Wrap="False" BackColor="Gray" ForeColor="White" />
            <Columns>
            <asp:TemplateField>
                  <ItemStyle HorizontalAlign=Center />
                   <ItemTemplate>       
		    
		    <a href="#" onClick="javascript:top.opener.window.BoardLookUp('<%#DataBinder.Eval(Container,"DataItem.QINum")%>', '<%#DataBinder.Eval(Container,"DataItem.QIName")%>' ,'<%#DataBinder.Eval(Container,"DataItem.QCal")%>' , '<%#DataBinder.Eval(Container,"DataItem.QWid")%>' , '<%#DataBinder.Eval(Container,"DataItem.QLen")%>' );window.close();">Select</a>             	    
                   </ItemTemplate>
                </asp:TemplateField>
                <asp:BoundField DataField="QINum" HeaderText="Item#" SortExpression="QINum" />
                <asp:BoundField DataField="QIName" HeaderText="Name" SortExpression="QIName" />
                <asp:BoundField DataField="QCode" HeaderText="E/R" SortExpression="QCode" />
                <asp:BoundField DataField="QCal" HeaderText="Caliper" SortExpression="QCal" />
                 <asp:BoundField DataField="QWid" HeaderText="Width" SortExpression="QWid" />
                <asp:BoundField DataField="QLen" HeaderText="Length" SortExpression="QLen" />
                <asp:BoundField DataField="QOnh" HeaderText="Qty On Hand" SortExpression="QOnh" />
                <asp:BoundField DataField="QComm" HeaderText="Committed" SortExpression="QComm" />
                 <asp:BoundField DataField="QAvail" HeaderText="Availbale" SortExpression="QAvail" />
                
                
                
            </Columns>
        </asp:GridView>
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
         SelectMethod="BoardLook" TypeName="LookUp">
            <SelectParameters>
                    
                <asp:Parameter DefaultValue="" Name="prmAction" Type="String" />
            
            <asp:Parameter Name="prmUser" Type="String" />
                 <asp:Parameter  Name="prmIndustry" Type="String" />
                <asp:Parameter Name="prmField" Type="String" />
                <asp:Parameter Name="prmCondition" Type="String" />
                <asp:Parameter Name="prmText" Type="String" />
                
                 
            </SelectParameters>
        </asp:ObjectDataSource>
    </div>
    </form>
</body>
</html>
