
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
#endregion

public partial class status_copylookup : System.Web.UI.Page
{

    private int iDefaultRecordsPerPage = 20;

    private bool bSort = true;

    protected void Page_Load(object sender, System.EventArgs e)
    {

        //UserClass.CheckLogin(Page);

        if (!func.CheckUserPermissions("[dbo].[status]", "SA"))
        {
            Response.Write("<p>" + "You don't have permissions to access this table" + "<a href=\"login.aspx\">&nbsp;" + "Back to login page" + "</a></p>");
            Response.End();
        }
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];




        dbGrid_status.Visible = true;

        string sCulture = ConfigurationManager.AppSettings["LCID"];
        if (!String.IsNullOrEmpty(sCulture))
        {
            int nCulture = int.Parse(sCulture);
            System.Threading.Thread.CurrentThread.CurrentCulture = new System.Globalization.CultureInfo(nCulture, false);
        }

        if (!Page.IsPostBack)
        {
            ddlSearchOperation.SelectedIndex = 1;
            if (Session["User"] != null)
            {
                //UserClass UserLogin = (UserClass)Session["User"];
                //lblUser.Text = UserLogin.UserName;

            }


            

                BuildDataSource();
        }

    }



       
       

    private void BuildDataSource()
    {
        try
        {
            string sSqlWhere = "";
            statusSqlDataSource.SelectParameters.Clear();

            if (Session["dbGrid_status_SearchSQL"] != null)
                sSqlWhere = func.WhereBuilder(sSqlWhere, "(" + Session["dbGrid_status_SearchSQL"].ToString() + ")", "And");
            if (Session["dbGrid_status_SearchParams"] != null)
            {
                ParameterCollection Params = (ParameterCollection)Session["dbGrid_status_SearchParams"];
                foreach (Parameter p in Params)
                {
                    txtSearchValue.Text = p.DefaultValue;
                    statusSqlDataSource.SelectParameters.Add(p);
                }
            }

            if (func.GetsAdvSecurityMethod("[dbo].[status]") == "1" && (!func.IsAdminUser()))
            {
                string sOwnerIDField = func.GetOwnerIDField("[dbo].[status]");
                string userId = ((UserClass)Session["User"]).UserID;

                if (sOwnerIDField != string.Empty && userId != string.Empty)
                {
                    sSqlWhere = func.WhereBuilder(sSqlWhere, "[" + sOwnerIDField + "]=@" + func.BuildParameterName(sOwnerIDField));
                    statusSqlDataSource.SelectParameters.Add(func.BuildParameterName(sOwnerIDField), GetFieldType(sOwnerIDField), userId);
                }
                else
                {
                    return;
                }
            }

            if (sSqlWhere != string.Empty) statusSqlDataSource.SelectCommand = func.SqlBuilder(statusSqlDataSource.SelectCommand, sSqlWhere);

            dbGrid_status.DataBind();

            if (Session["dbGrid_status_SortExpression"] != null)
            {
                bSort = false;
                dbGrid_status.Sort((string)Session["dbGrid_status_SortExpression"], (SortDirection)Session["dbGrid_status_SortDirection"]);
                bSort = true;
            }

            if (Session["dbGrid_status_CurrentPageCount"] != null)
            {
                //ddlPagerCount.SelectedValue = (string)Session["dbGrid_status_CurrentPageCount"];
                //dbGrid_status.PageSize = Convert.ToInt32(ddlPagerCount.SelectedValue);
            }
            else
            {
                //if (ddlPagerCount.Items.FindByValue(iDefaultRecordsPerPage.ToString()) == null) ddlPagerCount.Items.Add(iDefaultRecordsPerPage.ToString());
                //ddlPagerCount.SelectedValue = iDefaultRecordsPerPage.ToString();
                dbGrid_status.PageSize = iDefaultRecordsPerPage;
            }

            int iCurrentPageIndex = (Session["dbGrid_status_CurrentPageIndex"] == null) ? 0 : Convert.ToInt32(Session["dbGrid_status_CurrentPageIndex"]);
            if (dbGrid_status.PageCount >= iCurrentPageIndex)
                dbGrid_status.PageIndex = iCurrentPageIndex;
            else
            {
                dbGrid_status.PageIndex = 0;
                Session["dbGrid_status_CurrentPageIndex"] = 0;
            }
        }
        catch (Exception ex)
        {
            //lblMessage.Text += "Error description" + ": " + ex.Message + "<p>";
            dbGrid_status.Visible = false;
        }
        finally
        {
/*#if(DEBUG)
{

    //lblMessage.Text += "<p>SQL = " + statusSqlDataSource.SelectCommand  + "<p>";
    foreach (Parameter p in statusSqlDataSource.SelectParameters)
        //lblMessage.Text += p.Name + "(" + p.Type.ToString()+")="+p.DefaultValue+"<br>";

}
#endif*/
        }
    }

    protected void dbGrid_status_RowCreated(object sender, GridViewRowEventArgs e)
    {

        if (e.Row.RowType == DataControlRowType.Header) func.AddGlyph(dbGrid_status, e.Row);

        if (e.Row.RowType == DataControlRowType.DataRow)
        {

            bool bCheckSecurityD = func.CheckUserPermissions("[dbo].[status]", "d");

            bool bCheckSecurityE = func.CheckUserPermissions("[dbo].[status]", "e");

            try
            {
                DataRowView rowData;
                rowData = (DataRowView)e.Row.DataItem;
                string sOwnerIDField = func.GetOwnerIDField("[dbo].[status]");
                if (!(sOwnerIDField == "" || (rowData == null)))
                {
                    string sData = (rowData.Row[sOwnerIDField] == System.DBNull.Value) ? "" : Convert.ToString(rowData.Row[sOwnerIDField]);

                    bCheckSecurityD = func.CheckSecurity("[dbo].[status]", "d", sData);

                    bCheckSecurityE = func.CheckSecurity("[dbo].[status]", "e", sData);

                }
            }
            catch
            {

                bCheckSecurityD = false;

                bCheckSecurityE = false;

            }

            if (!bCheckSecurityD) e.Row.Cells[0].Text = "";

        }

    }

    protected void dbGrid_status_RowDataBound(object sender, GridViewRowEventArgs e)
    {
        if (e.Row.RowType == DataControlRowType.DataRow)
        {
            DataRowView rowData;
            rowData = (DataRowView)e.Row.DataItem;

        }

    }

    protected void dbGrid_status_RowCommand(object source, GridViewCommandEventArgs e)
    {
        if (e.CommandName == "Page") return;
        if (e.CommandName == "Sort") return;

        int index = Convert.ToInt32(e.CommandArgument);
        DataKey dkKeys = dbGrid_status.DataKeys[index];

        string sKeysArg = "";
        foreach (string s in dkKeys.Values.Keys)
            sKeysArg += s + "=" + Convert.ToString(dkKeys[s]) + "&";
        if (sKeysArg == String.Empty) return;

        if (e.CommandName == "cmdEdit") Response.Redirect("status_Edit.aspx?" + sKeysArg);

    }

    protected void dbGrid_status_RowDeleted(object sender, GridViewDeletedEventArgs e)
    {
        //lblMessage.Text = "<b>Record has been deleted!</b><br>";
    }

    protected void dbGrid_status_PageIndexChanged(object source, EventArgs e)
    {
        Session["dbGrid_status_CurrentPageIndex"] = dbGrid_status.PageIndex;
        BuildDataSource();

    }

    protected void dbGrid_status_Sorted(object sender, EventArgs e)
    {
        Session["dbGrid_status_SortExpression"] = null;
        if (bSort) BuildDataSource();
        Session["dbGrid_status_SortExpression"] = dbGrid_status.SortExpression;
        Session["dbGrid_status_SortDirection"] = dbGrid_status.SortDirection;
    }

   

    

    protected void btnShowAll_Click(object sender, System.EventArgs e)
    {
        ViewState["bNoRecords"] = false;
        txtSearchValue.Text = "";
        ddlSearchOperation.SelectedIndex = 0;
        ddlSearchField.SelectedIndex = 0;

        Session["dbGrid_status_CurrentPageIndex"] = 0;
        Session["dbGrid_status_SearchSQL"] = null;
        Session["dbGrid_status_SearchParams"] = null;
        Session["dbGrid_status_AdvSearch"] = null;
        Session["dbGrid_status_AdvParam"] = null;
        Session["htPeramstatus"] = null;

        Session["htPeramstatus"] = null;
        BuildDataSource();
    }

    protected void btnSearch_Click(object sender, System.EventArgs e)
    {
        if (txtSearchValue.Text.Trim() == String.Empty && ddlSearchOperation.SelectedValue.ToString() != "IsNull") return;

        string sSqlWhere = "";
        ParameterCollection Params = new ParameterCollection();

        if (ddlSearchField.SelectedValue.ToString() == "Any Field")
        {

            sSqlWhere = func.WhereBuilder(sSqlWhere, WhereOneField("code", "Searchcode", Params), "Or");

            sSqlWhere = func.WhereBuilder(sSqlWhere, WhereOneField("description", "Searchdescription", Params), "Or");

        }
        else
        {
            sSqlWhere = WhereOneField(ddlSearchField.SelectedValue.ToString(), func.BuildParameterName("Search" + ddlSearchField.SelectedValue.ToString()), Params);
        }

        if (sSqlWhere.Trim() != "")
        {
            Session["dbGrid_status_SearchSQL"] = sSqlWhere;
            Session["dbGrid_status_SearchParams"] = Params;
        }
        else
        {
            Session["dbGrid_status_SearchSQL"] = "2<>2";
            Session["dbGrid_status_SearchParams"] = null;
        }
        Session["dbGrid_status_CurrentPageIndex"] = 0;
        BuildDataSource();

    }

    private string WhereOneField(string sSearchField, string sParamName, ParameterCollection Params)
    {
        string sSearchOperation = ddlSearchOperation.SelectedValue;
        string sValue = txtSearchValue.Text.TrimEnd();
        string sReturn = "";
        string sSearchType = "";
        TypeCode fieldType = GetFieldType(sSearchField);

        sSearchField = "[" + sSearchField + "]";

        if (sSearchOperation == "IsNull") return sSearchField + " is null";

        try { object o = Convert.ChangeType(sValue, fieldType); }
        catch { return String.Empty; }

        if (!(func.IsDate(sValue) || func.IsNumeric(sValue)))
        {

            sSearchType = "upper";

            sValue = sValue.ToUpper();
        }

        switch (sSearchOperation)
        {
            case "Contains":
                sReturn = sSearchType + "(" + sSearchField + ") like '%" + sValue + "%'"; break;
            case "Starts with ...":
                sReturn = sSearchType + "(" + sSearchField + ") like '" + sValue + "%'"; break;
            case "Equals":
                sReturn = sSearchType + "(" + sSearchField + ") = @" + sParamName; break;
            case "More than ...":
                sReturn = sSearchField + ">@" + sParamName; break;
            case "Less than ...":
                sReturn = sSearchField + "<@" + sParamName; break;
            case "Equal or more than ...":
                sReturn = sSearchField + ">=@" + sParamName; break;
            case "Equal or less than ...":
                sReturn = sSearchField + "<=@" + sParamName; break;
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

    protected TypeCode GetFieldType(string sField)
    {
        switch (sField)
        {

            case "code": return TypeCode.String;

            case "description": return TypeCode.String;

            default: return TypeCode.String;
        }
    }

    

    

    

    private void ClearSession()
    {
        Session["dbGrid_status_Sort"] = null;

        Session["dbGrid_status_SearchSQL"] = null;
        Session["dbGrid_status_SearchParams"] = null;

        Session["htPeramstatus"] = null;
        Session["dbGrid_status_CurrentPageIndex"] = null;
        Session["dbGrid_status_CurrentPageCount"] = null;

        Session["dbGrid_status_SortExpression"] = null;
        Session["dbGrid_status_SortDirection"] = null;
    }

    protected void ShowWait()
    {
        Response.Write("<div id='mydiv' align=center>&nbsp;</div>");
        Response.Write("<script>mydiv.innerText = '';</script>");
        Response.Write("<script language=javascript>;");
        Response.Write("var dots = 0;var dotmax = 10;function ShowWait()");
        Response.Write("{var output; output = '" + "Please wait" + "';dots++;if(dots>=dotmax)dots=1;");
        Response.Write("for(var x = 0;x < dots;x++){output += '.';}mydiv.innerText =  output;}");
        Response.Write("function StartShowWait(){mydiv.style.visibility = 'visible'; window.setInterval('ShowWait()',500);}");
        Response.Write("function HideWait(){mydiv.style.visibility = 'hidden';window.clearInterval();}");
        Response.Write("StartShowWait();</script>");
        Response.Flush();
    }

    protected void statusSqlDataSource_Selected(object sender, SqlDataSourceStatusEventArgs e)
    {
        //lblCount.Text = "Details found" + ": " + e.AffectedRows.ToString();

    }

    protected void dbGrid_status_SelectedIndexChanged(object sender, EventArgs e)
    {
        //Session["status_list_index"] = dbGrid_status.SelectedIndex;
        //Session["status_list_code"] = dbGrid_status.SelectedRow.Cells[2].Text;
    }
    
   
    
   
}
