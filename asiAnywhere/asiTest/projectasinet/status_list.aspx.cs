
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
using System.Data.SqlClient;
#endregion

public partial class Cstatus_list : System.Web.UI.Page
{



    private const string ASCENDING = " ASC";
    private const string DESCENDING = " DESC";



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
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "status_list.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            lblComp.Text = PrmComp;
            if (aUsers == "external")
            {
                //txt_customer.Visible = false;
                //CustomerLook.Visible = false;
            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }



        //lblMessage.Text = "";
        dbGrid_status.Visible = true;

        string sCulture = ConfigurationManager.AppSettings["LCID"];
        if (!String.IsNullOrEmpty(sCulture))
        {
            int nCulture = int.Parse(sCulture);
            System.Threading.Thread.CurrentThread.CurrentCulture = new System.Globalization.CultureInfo(nCulture, false);
        }

        if (!Page.IsPostBack)
        {

            //ddlSearchOperation.SelectedIndex = 1;
            if (Session["User"] != null)
            {
                //UserClass UserLogin = (UserClass)Session["User"];
                lblUser.Text = UserLogin.UserName;

            }



            if (func.CheckUserPermissions("[dbo].[status]", "s"))

                BuildDataSource();
        } //  ! Page.IsPostBack





        Session["Rowuser"] = UserLogin.UserName;
        dbGrid_status.SelectedIndex = Convert.ToInt32(Session["status_list_index"]);
        try
        {
            if (Session["status_list_index"] == null)
            {
                dbGrid_status.SelectedIndex = 0;
                //Session["cust_type_comp"] = Session["cust_type_comp"];
                Session["status_list_code"] = dbGrid_status.SelectedRow.Cells[1].Text;




            }
        }
        catch
        {
            return;
        }

        try
        {
            TextBox ddl_display = (TextBox)FormView2.FindControl("aLineLabel");
            //ddl_display.Text = Convert.ToString(Session["gridsize"]);
            Session["size"] = Convert.ToInt32(ddl_display.Text);
            dbGrid_status.PageSize = Convert.ToInt32(Session["size"]);
        }
        catch
        {
            return;
        }

    }

    private void BuildDataSource()
    {
        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();
            string cmd = "select status_code as 'Status Code', description as 'Description' from status";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            dbGrid_status.DataSource = ds;
            dbGrid_status.DataBind();

            conn.Close();
        }
        catch
        { return; }
        finally
        {
            conn.Close();
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



    protected void dbGrid_status_RowDeleted(object sender, GridViewDeletedEventArgs e)
    {
        // lblMessage.Text = "<b>Record has been deleted!</b><br>";
    }

    protected void dbGrid_status_PageIndexChanged(object source, EventArgs e)
    {
        Session["dbGrid_status_CurrentPageIndex"] = dbGrid_status.PageIndex;
        BuildDataSource();

    }

    protected void dbGrid_status_Sorted(object sender, EventArgs e)
    {
        //Session["dbGrid_status_SortExpression"] = null;
        //if (bSort) BuildDataSource();
        //Session["dbGrid_status_SortExpression"] = dbGrid_status.SortExpression;
        //Session["dbGrid_status_SortDirection"] = dbGrid_status.SortDirection;
    }

    protected void hlnkLogOut_Click(object sender, EventArgs e)
    {

        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        Response.Redirect(sLoginURL);
    }



    protected void btnSearch_Click(object sender, System.EventArgs e)
    {
        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();
            string cmd = "";
            if (txt_statuscode.Text != "")
            {
                cmd = "select status_code as 'Status Code', description as 'Description' from status where status_code LIKE '" + txt_statuscode.Text + "%'";
            }
            if (txt_description.Text != "")
            {
                cmd = "select status_code as 'Status Code', description as 'Description' from status where description LIKE '" + txt_description.Text + "%'";
            }

            if (txt_description.Text != "" && txt_statuscode.Text != "")
            {
                cmd = "select status_code as 'Status Code', description as 'Description' from  status where description LIKE '" + txt_description.Text + "%' and status_code LIKE '" + txt_statuscode.Text + "%'";
            }

            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            dbGrid_status.DataSource = ds;
            dbGrid_status.DataBind();

            conn.Close();
        }
        catch
        { return; }
        finally
        {
            dbGrid_status.PageSize = Convert.ToInt32(Session["size"]);
            conn.Close();
        }

    }
    protected void dbGrid_industry_sic_PageIndexChanging(object source, GridViewPageEventArgs e)
    {
        dbGrid_status.PageIndex = e.NewPageIndex;
        BuildDataSource();

    }

    protected void btnShowAll_Click(object sender, System.EventArgs e)
    {
        ViewState["bNoRecords"] = false;
        txt_statuscode.Text = "";
        txt_description.Text = "";


        Session["dbGrid_status_CurrentPageIndex"] = 0;
        Session["dbGrid_status_SearchSQL"] = null;
        Session["dbGrid_status_SearchParams"] = null;
        Session["dbGrid_status_AdvSearch"] = null;
        Session["dbGrid_status_AdvParam"] = null;
        Session["htPeramstatus"] = null;



        Session["htPeramstatus"] = null;
        Session["status_list_index"] = null;

        BuildDataSource();
    }





    protected void hlkBackToMenu_Click(object sender, EventArgs e)
    {
        string sMenuURL = ConfigurationManager.AppSettings["MenuFile"];
        if (sMenuURL == String.Empty)
        {
            Response.Write("<script language=javascript>alert('Menu page isn't set');</script>");
            return;
        }

        ClearSession();
        Response.Redirect(sMenuURL);
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



    protected void dbGrid_status_SelectedIndexChanged(object sender, EventArgs e)
    {
        Session["status_list_index"] = dbGrid_status.SelectedIndex;
        Session["status_list_code"] = dbGrid_status.SelectedRow.Cells[1].Text;

    }





    protected void ddl_display_TextChanged(object sender, EventArgs e)
    {
        TextBox ddl_display = (TextBox)FormView2.FindControl("aLineLabel");
        Session["gridsize"] = ddl_display.Text;
        //ddl_display.Text = Convert.ToString(Session["gridsize"]);
        ObjectDataSource2.SelectParameters["vLine"].DefaultValue = Convert.ToString(Session["gridsize"]);
        BuildDataSource();


    }

    protected void lnk_status_list_Click(object sender, EventArgs e)
    {
        Response.Redirect("status_list.aspx");

    }
    protected void lnk_status_view_Click(object sender, EventArgs e)
    {
        Response.Redirect("status_viewlist.aspx");

    }
    public SortDirection GridViewSortDirection
    {

        get
        {

            if (ViewState["sortDirection"] == null)

                ViewState["sortDirection"] = SortDirection.Ascending;

            return (SortDirection)ViewState["sortDirection"];

        }

        set { ViewState["sortDirection"] = value; }

    }
    protected void GridView1_Sorting(object sender, GridViewSortEventArgs e)
    {

        string sortExpression = e.SortExpression;

        if (GridViewSortDirection == SortDirection.Ascending)
        {

            GridViewSortDirection = SortDirection.Descending;

            SortGridView(sortExpression, " DESC");

        }

        else
        {

            GridViewSortDirection = SortDirection.Ascending;

            SortGridView(sortExpression, " ASC");

        }

    }

    private void SortGridView(string sortExpression, string direction)
    {
        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();
            string cmd = "select status_code as 'Status Code', description as 'Description' from  status";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            dbGrid_status.DataSource = ds;
            dbGrid_status.DataBind();
            DataTable dt = ds.Tables[0];

            DataView dv = new DataView(dt);

            dv.Sort = sortExpression + direction;

            dbGrid_status.DataSource = dv;

            dbGrid_status.DataBind();


            conn.Close();

        }



        catch { return; }

    }
}
