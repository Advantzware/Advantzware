<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="user_looksql" Codebehind="user_looksql.aspx.cs" %>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>User Lookup</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
     
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/dhtmlwindow.css" rel="stylesheet" type="text/css" />
    <script language = JavaScript>
    
//    var bSelected=false;
//    function ChSel()
//    {
//        var theForm = document.forms['frmList'];
//        if (!theForm) theForm = document.frmList;
//        bSelected = !bSelected; 
//        var i;
//        for (i=0;i<theForm.chDelete.length;++i) theForm.chDelete[i].checked=bSelected;
//    } 
//    
    function OnKeyDown()
    {
        e = window.event;
        if (e.keyCode == 13)
        {
            e.cancel = true;
            var theForm = document.forms['frmList'];
            if (!theForm) theForm = document.frmList;                
            theForm.btnSearch.click();              
        }
    }
    
    
     function showhide()
    {
    var show= document.getElementById("firstshow");
     show.style.display='inline';
     var hide=document.getElementById("secondshow");
     hide.style.display='none';
     
     
    }
    
    function showhide2()
    {
    var show= document.getElementById("firstshow");
     show.style.display='none';
     var hide=document.getElementById("secondshow");
     hide.style.display='inline';
    
     
    }
    </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='txtSearchValue' >   
        
      <div>      
       
   
      <TABLE id="tblMain" cellSpacing="1" cellPadding="1" border="0" Width="400px">
        <TR>
          <TD>
            <TABLE id="tblSearch" cellSpacing="1" cellPadding="5" border="0"  bgcolor=black Width="400px">
              <TR>
                <TD class="shade" align="center" width="45"><nobr>
                   <table cellspacing="2" cellpadding="1"  border="0" class="shade" bgcolor="gray">    		   
		            <tr><td>
		   
                  <asp:button id="btnSearch" runat="server" CssClass="button" Width="40px" Text="Go" OnClick="btnSearch_Click"></asp:button>
                  <br />
                
                  <asp:button id="btnShowAll" runat="server" CssClass="button" Width="40px" Text="All" OnClick="btnShowAll_Click"></asp:button>
                </TD>               
                          
                               
                     <td class="shade" align="center" nowrap>
                  <b>User </b></br>
                    <asp:TextBox ID="txt_fldname" Width="100px" runat="server"></asp:TextBox>
                    </td>
                     <td class="shade" align="center" nowrap>
                  <b>Name</b></br>
                    <asp:TextBox ID="txt_title" Width="100px" runat="server"></asp:TextBox>
                    </td>
                    
                   
              
                       
                
                <TD id="tdInfo" runat="server" class="shade" align="center">
                <table><tr><td align="center" nowrap>
                  <b>Records/Page</b><BR>
           
            
            <asp:FormView ID="FormView2" runat="server" DataSourceID="ObjectDataSource2">
                          <EditItemTemplate>
                              aLine:
                              <asp:TextBox ID="aLineTextBox" Width="100px" runat="server" Text='<%# Bind("aLine") %>'>
                              </asp:TextBox><br />
                              <asp:LinkButton ID="UpdateButton" runat="server" CausesValidation="True" CommandName="Update"
                                  Text="Update">
                              </asp:LinkButton>
                              <asp:LinkButton ID="UpdateCancelButton" runat="server" CausesValidation="False" CommandName="Cancel"
                                  Text="Cancel">
                              </asp:LinkButton>
                          </EditItemTemplate>
                          <InsertItemTemplate>
                              aLine:
                              <asp:TextBox ID="aLineTextBox" runat="server" Text='<%# Bind("aLine") %>'>
                              </asp:TextBox><br />
                              <asp:LinkButton ID="InsertButton" runat="server" CausesValidation="True" CommandName="Insert"
                                  Text="Insert">
                              </asp:LinkButton>
                              <asp:LinkButton ID="InsertCancelButton" runat="server" CausesValidation="False" CommandName="Cancel"
                                  Text="Cancel">
                              </asp:LinkButton>
                          </InsertItemTemplate>
                          <ItemTemplate>
                             
                              <asp:TextBox ID="aLineLabel" runat="server" Width="70px" OnTextChanged="ddl_display_TextChanged" Text='<%# Bind("aLine") %>'></asp:TextBox>
                              <asp:CompareValidator ID="CompareValidator4" runat="server" ControlToValidate="aLineLabel" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="Integer" ErrorMessage="Invalid Number"></asp:CompareValidator>
                          </ItemTemplate>
                      </asp:FormView>
                      <asp:ObjectDataSource ID="ObjectDataSource2" runat="server" OldValuesParameterFormatString="original_{0}"
                          SelectMethod="SelectRows" TypeName="Order">
                          <SelectParameters>
                              <asp:SessionParameter Name="prmUser" SessionField="Rowuser" Type="String" />
                              <asp:SessionParameter Name="vLine" Type="Int32" SessionField="gridsize" />
                          </SelectParameters>
                      </asp:ObjectDataSource>
                      </td></tr></table> 
                </TD>
                
                 </TR></table> 
              </TR>
            </TABLE>
            <asp:label id="lblMessage" runat="server" ForeColor="Red"></asp:label>
          </td>
        </tr>
        <tr>
          <td>

      <%--<p>
      <a href=# onClick = "ChSel()">Select/Unselect all</a>
      <asp:linkbutton id="btnDelete" runat="server" OnClick="btnDelete_Click">Delete selected</asp:linkbutton>
      </p>--%>

           
            <asp:GridView id="dbGrid_help_main" runat="server" CssClass="Grid" Width="400px"
                   
                  OnPageIndexChanging="dbGrid_help_main_PageIndexChanging"  
                  OnSorted="dbGrid_help_main_Sorted"
                    OnRowCommand="dbGrid_contact_RowCommand" 
                  OnRowDeleted="dbGrid_help_main_RowDeleted" OnSorting="GridView1_Sorting"
                  OnPreRender="dbGrid_help_main_PreRender"          

                   AllowPaging="True" AllowSorting="True" BorderStyle="Dotted" EmptyDataText="No records found" OnSelectedIndexChanged="dbGrid_help_main_SelectedIndexChanged">
        <SelectedRowStyle  BackColor="yellow" CssClass="GridSelected" />
        <AlternatingRowStyle CssClass="GridItemOdd" />
        <EmptyDataRowStyle BorderStyle="None" BorderWidth="0px" Font-Bold="True" HorizontalAlign="Center" VerticalAlign="Middle" />
        <RowStyle CssClass="shade" />
        <HeaderStyle  BackColor="teal" VerticalAlign="Middle"  ForeColor ="white"  HorizontalAlign="Center" Wrap="true"></HeaderStyle>
        
              <Columns>

                  <asp:TemplateField>
                  <ItemStyle HorizontalAlign=Center Wrap="False" />
                   <ItemTemplate>       
		    <a href="#" onClick ="javascript:top.opener.window.UserLookup('<%#DataBinder.Eval(Container,"DataItem.User#")%>','<%#DataBinder.Eval(Container,"DataItem.Name")%>');window.close();">Select</a>             	    
                   </ItemTemplate>
                </asp:TemplateField>               
              </Columns>
            </asp:GridView>
          </TD>
        </TR>
      </TABLE> 
      
    
    </div>
   
    </form>
  </body>
</HTML>