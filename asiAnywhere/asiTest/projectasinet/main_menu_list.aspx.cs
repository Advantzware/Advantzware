
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
#endregion

public partial class Cmain_menu_list : System.Web.UI.Page 
{
  
    private int iDefaultRecordsPerPage = 100;

    private bool bSort = true;

protected void Page_Load( object sender,  System.EventArgs e)  
{

    UserClass.CheckLogin(Page);

    if (! func.CheckUserPermissions("[dbo].[main_menu]", "SA") )
    {
      Response.Write("<p>" + "You don't have permissions to access this table" + "<a href=\"login.aspx\">&nbsp;" + "Back to login page" + "</a></p>");
      Response.End();
    }  

    lblMessage.Text = "";
    dbGrid_main_menu.Visible = true;

    string sCulture = ConfigurationManager.AppSettings["LCID"];
    if (!String.IsNullOrEmpty(sCulture)) 
    {
        int nCulture = int.Parse(sCulture);
        System.Threading.Thread.CurrentThread.CurrentCulture = new System.Globalization.CultureInfo(nCulture, false);
    } 
       
    if (! Page.IsPostBack )
    {    

        if (Session["User"] != null) 
        {
            UserClass UserLogin = (UserClass)Session["User"]; 
            lblUser.Text = UserLogin.UserName;

            hlnkChangePwd.Visible = (UserLogin.UserName != "Guest");

        }
   
        //func.GetMenu(ddlQuickJump, "Menu Maintenance");

        hlnkExport.Visible = (func.CheckUserPermissions("[dbo].[main_menu]", "p") && func.CheckUserPermissions("[dbo].[main_menu]", "s"));

        hlnkPrint.Visible = (func.CheckUserPermissions("[dbo].[main_menu]", "p") && func.CheckUserPermissions("[dbo].[main_menu]", "s"));
        hlnkPrintImg.Visible = hlnkPrint.Visible;

        btnAdd.Visible = func.CheckUserPermissions("[dbo].[main_menu]", "a");

        tdInfo.Visible = tdPageCount.Visible = func.CheckUserPermissions("[dbo].[main_menu]", "s");

        tdSearch.Visible = tdInfo.Visible;                
        hlnkAdvSearch.Visible = tdSearch.Visible;

        btnDelete.Attributes.Add("onclick", "return confirm('"+"Do you really want to delete these records?"+"');");

        if (func.CheckUserPermissions("[dbo].[main_menu]", "s"))
	
	if (Session["dbGrid_main_menu_SearchSQL"] == null) 
	{
		ddlSearchField.SelectedValue = "parent";
	    	ddlSearchOperation.SelectedValue = "Equals";
	    	txtSearchValue.Text = "TOP";
	    	Session["dbGrid_main_menu_SearchSQL"]= "[parent] = 'TOP'";
	    	Session["dbGrid_main_menu_CurrentPageIndex"] = 0;
	}
      
        BuildDataSource();
    } //  ! Page.IsPostBack
    
}

private void BuildDataSource()
{
    try
    {
        string sSqlWhere = "";
        main_menuSqlDataSource.SelectParameters.Clear();
        
        if (Session["dbGrid_main_menu_SearchSQL"] != null) 
            sSqlWhere =  func.WhereBuilder(sSqlWhere, "(" + Session["dbGrid_main_menu_SearchSQL"].ToString() + ")","And");
        if (Session["dbGrid_main_menu_SearchParams"] != null) 
        {
            ParameterCollection Params = (ParameterCollection)Session["dbGrid_main_menu_SearchParams"];
            foreach (Parameter p in Params) 
            {
                txtSearchValue.Text = p.DefaultValue;
                main_menuSqlDataSource.SelectParameters.Add(p);
            }
        }               

        if (Session["dbGrid_main_menu_AdvSearch"] != null)
            sSqlWhere = func.WhereBuilder( sSqlWhere , Session["dbGrid_main_menu_AdvSearch"].ToString());
        if (Session["dbGrid_main_menu_AdvParam"] != null)
        {
            ParameterCollection Params = (ParameterCollection)Session["dbGrid_main_menu_AdvParam"];
            foreach (Parameter p in Params) main_menuSqlDataSource.SelectParameters.Add(p);
        }    

        if ( func.GetsAdvSecurityMethod("[dbo].[main_menu]") == "1" && (! func.IsAdminUser()))
        {
            string sOwnerIDField = func.GetOwnerIDField("[dbo].[main_menu]");
            string userId = ((UserClass)Session["User"]).UserID;
            
            if (sOwnerIDField != string.Empty && userId != string.Empty)
            {
                sSqlWhere = func.WhereBuilder(sSqlWhere, "[" + sOwnerIDField + "]=@" + func.BuildParameterName(sOwnerIDField));            
                main_menuSqlDataSource.SelectParameters.Add(func.BuildParameterName(sOwnerIDField), GetFieldType(sOwnerIDField), userId);
            }
            else
            {
                return;
            }  
         }

        if(sSqlWhere != string.Empty) main_menuSqlDataSource.SelectCommand = func.SqlBuilder(main_menuSqlDataSource.SelectCommand, sSqlWhere);
        
        dbGrid_main_menu.DataBind();
       
        if (Session["dbGrid_main_menu_SortExpression"] != null)
        {
            bSort = false;
            dbGrid_main_menu.Sort((string)Session["dbGrid_main_menu_SortExpression"], (SortDirection)Session["dbGrid_main_menu_SortDirection"]);
            bSort = true;
        }
        
        if (Session["dbGrid_main_menu_CurrentPageCount"] != null )
        {
            ddlPagerCount.SelectedValue = (string)Session["dbGrid_main_menu_CurrentPageCount"];
            dbGrid_main_menu.PageSize = Convert.ToInt32(ddlPagerCount.SelectedValue);
        } 
        else
        {
            if (ddlPagerCount.Items.FindByValue(iDefaultRecordsPerPage.ToString()) == null) ddlPagerCount.Items.Add(iDefaultRecordsPerPage.ToString());
            ddlPagerCount.SelectedValue = iDefaultRecordsPerPage.ToString();
            dbGrid_main_menu.PageSize = iDefaultRecordsPerPage;
        }
        
        int iCurrentPageIndex = (Session["dbGrid_main_menu_CurrentPageIndex"] == null)?0:Convert.ToInt32(Session["dbGrid_main_menu_CurrentPageIndex"]);
        if (dbGrid_main_menu.PageCount >= iCurrentPageIndex)
            dbGrid_main_menu.PageIndex = iCurrentPageIndex;
        else 
        {
            dbGrid_main_menu.PageIndex = 0;
            Session["dbGrid_main_menu_CurrentPageIndex"] = 0;
        }
    }
    catch (Exception ex)
    {
        lblMessage.Text += "Error description" + ": " + ex.Message + "<p>";
        dbGrid_main_menu.Visible = false;
    }  
    finally
    {
/*#if(DEBUG)
{

    lblMessage.Text += "<p>SQL = " + main_menuSqlDataSource.SelectCommand  + "<p>";
    foreach (Parameter p in main_menuSqlDataSource.SelectParameters)
        lblMessage.Text += p.Name + "(" + p.Type.ToString()+")="+p.DefaultValue+"<br>";

}
#endif*/
    }
}

protected void dbGrid_main_menu_RowCreated(object sender,  GridViewRowEventArgs e)
{

    if (e.Row.RowType == DataControlRowType.Header) func.AddGlyph(dbGrid_main_menu, e.Row);

    if (e.Row.RowType == DataControlRowType.DataRow)
    { 

        bool bCheckSecurityD = func.CheckUserPermissions("[dbo].[main_menu]", "d");

        bool bCheckSecurityE = func.CheckUserPermissions("[dbo].[main_menu]", "e");

        try 
        {
            DataRowView rowData;
            rowData = (DataRowView)e.Row.DataItem;
            string  sOwnerIDField = func.GetOwnerIDField("[dbo].[main_menu]");
            if (! (sOwnerIDField == "" || (rowData == null)) ) 
            {
                string  sData = (rowData.Row[sOwnerIDField] == System.DBNull.Value)?"":Convert.ToString(rowData.Row[sOwnerIDField]);

                bCheckSecurityD = func.CheckSecurity("[dbo].[main_menu]", "d", sData);

                bCheckSecurityE = func.CheckSecurity("[dbo].[main_menu]", "e", sData);

            }
        } 
        catch
        {

                bCheckSecurityD = false;

                bCheckSecurityE = false;

        }
              
        if (!bCheckSecurityD) e.Row.Cells[0].Text = "";
                     
        TableCell myTableCellE;
        myTableCellE = (TableCell)e.Row.Cells[1];
        LinkButton myEditButton;
        myEditButton = (LinkButton)myTableCellE.Controls[0];

        myEditButton.Visible = bCheckSecurityE;

       }

   }
 
protected void dbGrid_main_menu_RowDataBound(object sender,  GridViewRowEventArgs e)
{
    if (e.Row.RowType == DataControlRowType.DataRow)
    {
        DataRowView rowData;
        rowData = (DataRowView)e.Row.DataItem;

    if (e.Row.Cells[2].Text.Length > 80)
        e.Row.Cells[2].Text = func.ProcessLargeText(e.Row.Cells[2].Text, 80, "main_menu", "field=menu_id&key="  + "menu_id=" + Convert.ToString(rowData["menu_id"]) + ";" );   

    if (e.Row.Cells[3].Text.Length > 80)
        e.Row.Cells[3].Text = func.ProcessLargeText(e.Row.Cells[3].Text, 80, "main_menu", "field=menu_label&key="  + "menu_id=" + Convert.ToString(rowData["menu_id"]) + ";" );   

    if (e.Row.Cells[4].Text.Length > 80)
        e.Row.Cells[4].Text = func.ProcessLargeText(e.Row.Cells[4].Text, 80, "main_menu", "field=order_num&key="  + "menu_id=" + Convert.ToString(rowData["menu_id"]) + ";" );   

    if (e.Row.Cells[5].Text.Length > 80)
        e.Row.Cells[5].Text = func.ProcessLargeText(e.Row.Cells[5].Text, 80, "main_menu", "field=target_page&key="  + "menu_id=" + Convert.ToString(rowData["menu_id"]) + ";" );   

    if (e.Row.Cells[6].Text.Length > 80)
        e.Row.Cells[6].Text = func.ProcessLargeText(e.Row.Cells[6].Text, 80, "main_menu", "field=parent&key="  + "menu_id=" + Convert.ToString(rowData["menu_id"]) + ";" );   

        if (rowData["parent"] != System.DBNull.Value) e.Row.Cells[6].Text = func.GetLookupValue("[menu_label]", "[menu_id]", "[dbo].[main_menu]", Convert.ToString(rowData["parent"]), TypeCode.String);

    if (e.Row.Cells[7].Text.Length > 80)
        e.Row.Cells[7].Text = func.ProcessLargeText(e.Row.Cells[7].Text, 80, "main_menu", "field=security&key="  + "menu_id=" + Convert.ToString(rowData["menu_id"]) + ";" );   

    }

}    

protected void dbGrid_main_menu_RowCommand(object source,  GridViewCommandEventArgs e)
{        
    if (e.CommandName == "Page") return;
    if (e.CommandName == "Sort")  return;
    
    //int index = Convert.ToInt32(e.CommandArgument);
    //Response.Write(index);
      //DataKey dkKeys = dbGrid_main_menu.DataKeys[0];
    
    //string sKeysArg = "";   
    //foreach (string s in dkKeys.Values.Keys)    
        //sKeysArg += s + "=" + Convert.ToString(dkKeys[s])+"&";    
    //if (sKeysArg == String.Empty) return;

    if (e.CommandName == "cmdEdit")
    {
        int index = Convert.ToInt32(e.CommandArgument);
        //Response.Write(index);
        DataKey dkKeys = dbGrid_main_menu.DataKeys[index];

        string sKeysArg = "";
        foreach (string s in dkKeys.Values.Keys)
            sKeysArg += s + "=" + Convert.ToString(dkKeys[s]) + "&";
        if (sKeysArg == String.Empty) return;
        Response.Redirect("main_menu_Edit.aspx?" + sKeysArg);
    }
  if ( e.CommandName == "Submenu" ) 
  {         
     btnSubMenu_Click(Server.UrlDecode((string)e.CommandArgument));
    
  }

}

protected void dbGrid_main_menu_RowDeleted(object sender, GridViewDeletedEventArgs e)
{
    lblMessage.Text = "<b>Record has been deleted!</b><br>";
}

protected void dbGrid_main_menu_PageIndexChanged(object source, EventArgs e)
{
    Session["dbGrid_main_menu_CurrentPageIndex"] = dbGrid_main_menu.PageIndex;
    BuildDataSource();

}

protected void dbGrid_main_menu_Sorted(object sender, EventArgs e)
{
    Session["dbGrid_main_menu_SortExpression"] = null;
    if (bSort) BuildDataSource();
    Session["dbGrid_main_menu_SortExpression"] = dbGrid_main_menu.SortExpression;
    Session["dbGrid_main_menu_SortDirection"] = dbGrid_main_menu.SortDirection;
}

protected void hlnkLogOut_Click(object sender,  EventArgs e) 
{

    string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
    if ( sLoginURL == "" ) 
    {
        Response.Write("<script language=javascript>alert('"+"Login page isn’t set"+"!');</script>");
        return;
    }

    Page.Session.Clear();
    Response.Redirect(sLoginURL);
}
 
protected void ddlPagerCount_SelectedIndexChanged(object sender,  EventArgs e)  
{
    Session["dbGrid_main_menu_CurrentPageCount"] = ddlPagerCount.SelectedValue;
    Session["dbGrid_main_menu_CurrentPageIndex"] = 0;
    BuildDataSource();
}

protected void btnShowAll_Click( object sender,  System.EventArgs e) 
{
    ViewState["bNoRecords"] = false;
    txtSearchValue.Text = "";
    ddlSearchOperation.SelectedIndex = 0;
    ddlSearchField.SelectedIndex = 0;

    Session["dbGrid_main_menu_CurrentPageIndex"] = 0;
    Session["dbGrid_main_menu_SearchSQL"]= null;
    Session["dbGrid_main_menu_SearchParams"]= null;           
    Session["dbGrid_main_menu_AdvSearch"] = null;
    Session["dbGrid_main_menu_AdvParam"] = null;
    Session["htPerammain_menu"] = null;

    Session["htPerammain_menu"] = null;
    BuildDataSource();
}


protected void btnTopMenu_Click( object sender,  System.EventArgs e) 
{
    btnSubMenu_Click("top");
}

protected void btnSubMenu_Click(string sValue) 
{
    ddlSearchField.SelectedValue = "parent";
    ddlSearchOperation.SelectedValue = "Equals";
    txtSearchValue.Text = sValue;
    
    
    Session["dbGrid_main_menu_SearchSQL"]= "[parent] = '" + sValue + "'";
    Session["dbGrid_main_menu_CurrentPageIndex"] = 0;
    BuildDataSource();  
}

protected string GetText(string inpText) 
{
    if (inpText == "menuhead") 
      {
	return "Submenu";
      }
   else {
       return "";
   }
}

protected void btnSearch_Click( object sender,  System.EventArgs e) 
{
  if (txtSearchValue.Text.Trim() == String.Empty && ddlSearchOperation.SelectedValue.ToString() != "IsNull") return;

  string  sSqlWhere = "";
  ParameterCollection Params = new ParameterCollection();

if (ddlSearchField.SelectedValue.ToString() == "Any Field")
  {

        sSqlWhere =  func.WhereBuilder(sSqlWhere, WhereOneField("menu_id", "Searchmenu_id", Params), "Or");

        sSqlWhere =  func.WhereBuilder(sSqlWhere, WhereOneField("menu_label", "Searchmenu_label", Params), "Or");

        sSqlWhere =  func.WhereBuilder(sSqlWhere, WhereOneField("target_page", "Searchtarget_page", Params), "Or");

        sSqlWhere =  func.WhereBuilder(sSqlWhere, WhereOneField("parent", "Searchparent", Params), "Or");

        sSqlWhere =  func.WhereBuilder(sSqlWhere, WhereOneField("security", "Searchsecurity", Params), "Or");

        sSqlWhere =  func.WhereBuilder(sSqlWhere, WhereOneField("description", "Searchdescription", Params), "Or");

  }
  else
  {
      sSqlWhere = WhereOneField(ddlSearchField.SelectedValue.ToString(), func.BuildParameterName("Search" + ddlSearchField.SelectedValue.ToString()), Params);
  }
  
  if ( sSqlWhere.Trim() != "" )
  {    
    Session["dbGrid_main_menu_SearchSQL"]= sSqlWhere;
    Session["dbGrid_main_menu_SearchParams"]= Params;   
  }
  else
  {
    Session["dbGrid_main_menu_SearchSQL"] = "2<>2";
    Session["dbGrid_main_menu_SearchParams"] = null;
  }  
  Session["dbGrid_main_menu_CurrentPageIndex"] = 0;
  BuildDataSource();

}

private string WhereOneField(string  sSearchField, string sParamName, ParameterCollection Params) 
{
    string  sSearchOperation = ddlSearchOperation.SelectedValue;
    string  sValue = txtSearchValue.Text.TrimEnd();
    string  sReturn = "";
    string  sSearchType = "";
    TypeCode  fieldType = GetFieldType(sSearchField);
    
    sSearchField = "[" + sSearchField + "]";
    
    if (sSearchOperation == "IsNull") return sSearchField + " is null";
    
    try{object o = Convert.ChangeType(sValue, fieldType);}
    catch { return String.Empty; }

    if (!(func.IsDate(sValue) || func.IsNumeric(sValue))) 
    {

    sSearchType = "upper";

    sValue = sValue.ToUpper();
  }
    
  switch (sSearchOperation)
    { 
            case "Contains":
                sReturn = sSearchType + "(" + sSearchField + ") like '%" + sValue + "%'";break;
            case "Starts with ...":              
                sReturn = sSearchType + "(" + sSearchField + ") like '" + sValue + "%'";break;            
            case "Equals":
                sReturn = sSearchType + "(" + sSearchField + ") = @" + sParamName;break;
            case "More than ...":
                sReturn = sSearchField + ">@" + sParamName;break;
            case "Less than ...":
                sReturn = sSearchField + "<@" + sParamName;break;
            case "Equal or more than ...":
                sReturn = sSearchField + ">=@" + sParamName;break;
            case "Equal or less than ...":
                sReturn = sSearchField + "<=@" + sParamName;break;
            default:
            sReturn = String.Empty; break;
  }

  if (sReturn != string.Empty && (sSearchOperation != "Contains" && sSearchOperation != "Starts with ..."))
  {  
      if (func.IsNumeric(sValue)) Params.Add(sParamName, sValue);
      else Params.Add(sParamName, fieldType, sValue);
  }
  return sReturn;
}

protected TypeCode GetFieldType(string  sField) 
{
    switch (sField)
    {
  
        case "group_id": return TypeCode.String;
  
        case "parent": return TypeCode.String;
  
        case "menu_label": return TypeCode.String;
  
        case "destination": return TypeCode.String;
  
        case "security": return TypeCode.String;
  
        case "description": return TypeCode.String;
  
        case "menu_id": return TypeCode.Int32;
  
        case "order_num": return TypeCode.Int32;
  
        case "allowedidtypes": return TypeCode.String;
  
        case "long_desc": return TypeCode.String;
  
        case "target_page": return TypeCode.String;
  
        default: return TypeCode.String;
    }    
}

protected void btnDelete_Click(object sender,  EventArgs e)  
{
    string sWhere = Request.Form["chDelete"];
    if (!string.IsNullOrEmpty(sWhere))
    {
    try
    {
        foreach (string s in sWhere.Split(','))
        {
            int iRowIndex = Convert.ToInt32(s);
     
            dbGrid_main_menu.DeleteRow(iRowIndex);
        }
        
    }
    catch (Exception ex)
    {
      lblMessage.Text += "Error description" + ": " + ex.Message + "<p>";     
    }  
    }
    BuildDataSource();
}
protected void main_menuSqlDataSource_Deleting(object sender, SqlDataSourceCommandEventArgs e)
{
    
}

protected void btnAdd_Click(object sender,  EventArgs e)  
{

    Response.Redirect("main_menu_add.aspx");

}
 
protected void hlkBackToMenu_Click(object sender,  EventArgs e)
{
    string sMenuURL = ConfigurationManager.AppSettings["MenuFile"];
    if ( sMenuURL == String.Empty) 
    {
        Response.Write("<script language=javascript>alert('Menu page isn't set');</script>");
        return;
    }
        
    ClearSession();
    Response.Redirect(sMenuURL);
}

protected void ddlQuickJump_SelectedIndexChanged(object sender, EventArgs e)
{
    ClearSession();
    //Response.Redirect(ddlQuickJump.SelectedItem.Value);
}

private void ClearSession() 
{
    Session["dbGrid_main_menu_Sort"] = null;

    Session["dbGrid_main_menu_SearchSQL"]= null;
    Session["dbGrid_main_menu_SearchParams"]= null;   

    Session["dbGrid_main_menu_AdvSearch"] = null;
    Session["dbGrid_main_menu_AdvParam"] = null;
    Session["htPerammain_menu"] = null;   

    Session["htPerammain_menu"] = null;
    Session["dbGrid_main_menu_CurrentPageIndex"] = null;
    Session["dbGrid_main_menu_CurrentPageCount"] = null;

    Session["dbGrid_main_menu_SortExpression"] = null;
    Session["dbGrid_main_menu_SortDirection"] = null;
}

protected void ShowWait() 
{
    Response.Write("<div id='mydiv' align=center>&nbsp;</div>");
    Response.Write("<script>mydiv.innerText = '';</script>");
    Response.Write("<script language=javascript>;");
    Response.Write("var dots = 0;var dotmax = 10;function ShowWait()");
    Response.Write("{var output; output = '"+"Please wait"+"';dots++;if(dots>=dotmax)dots=1;");
    Response.Write("for(var x = 0;x < dots;x++){output += '.';}mydiv.innerText =  output;}");
    Response.Write("function StartShowWait(){mydiv.style.visibility = 'visible'; window.setInterval('ShowWait()',500);}");
    Response.Write("function HideWait(){mydiv.style.visibility = 'hidden';window.clearInterval();}");
    Response.Write("StartShowWait();</script>");
    Response.Flush();
}

protected void main_menuSqlDataSource_Selected(object sender, SqlDataSourceStatusEventArgs e)
{
    lblCount.Text = "Details found" +": " + e.AffectedRows.ToString();
}

}
