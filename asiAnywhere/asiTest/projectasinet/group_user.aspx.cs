
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

public partial class Group_user_list : System.Web.UI.Page
{

    protected void Page_Load(object sender, System.EventArgs e)
    {
        Session["user_master_company"] = null;

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "group_user.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            //lblComp.Text = PrmComp;
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

        //Session["user_master_company_index"] = null;
        //Session["user_master_location_index"] = null;

        //lblMessage.Text = "";


        if (!Page.IsPostBack)
        {

            if (Session["User"] != null)
            {
                lblUser.Text = UserLogin.UserName;

            }

            //BuildDataSource();
        }
        //Session["Rowuser"] = UserLogin.UserName;
        //BuildDataSource();
        //try
        //{

        //    TextBox ddl_display = (TextBox)FormView1.FindControl("aLineLabel");
        //    if (ddl_display.Text == "")
        //        ddl_display.Text = "0";

        //    Session["size"] = Convert.ToInt32(ddl_display.Text);
        //    dbGrid_user_master.PageSize = Convert.ToInt32(Session["size"]);
        //}


        //catch
        //{
        //}

        //dbGrid_user_master.SelectedIndex = Convert.ToInt32(Session["user_master_index"]);
        //try
        //{
        //    Session["tot_user_master_row"] = dbGrid_user_master.Rows.Count;

        //    if (Session["user_master_index"] == null)
        //    {
        //        dbGrid_user_master.SelectedIndex = 0;
        //        Session["user_master_id"] = ((Label)dbGrid_user_master.SelectedRow.FindControl("Label1")).Text;
        //    }
        //}
        //catch
        //{
        //    //return;
        //}

    }

    private void BuildDataSource()
    {
        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();
            string cmd = "select *  from user_master";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            dbGrid_user_master.DataSource = ds;
            dbGrid_user_master.DataBind();

            conn.Close();


        }
        catch
        {
            //return; 
        }
        finally
        {
            conn.Close();
        }
    }

    protected void dbGrid_user_master_PageIndexChanging(object sender, GridViewPageEventArgs e)
    {
        dbGrid_user_master.PageIndex = e.NewPageIndex;
        BuildDataSource();
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

    protected void dbGrid_user_master_Sorting(object sender, GridViewSortEventArgs e)
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
            string cmd = "select *  from user_master";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            dbGrid_user_master.DataSource = ds;
            dbGrid_user_master.DataBind();
            DataTable dt = ds.Tables[0];

            DataView dv = new DataView(dt);

            dv.Sort = sortExpression + direction;

            dbGrid_user_master.DataSource = dv;

            dbGrid_user_master.DataBind();


            conn.Close();

        }



        catch { return; }

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
            
    protected void hlkBackToMenu_Click(object sender, EventArgs e)
    {
        string sMenuURL = ConfigurationManager.AppSettings["MenuFile"];
        if (sMenuURL == String.Empty)
        {
            Response.Write("<script language=javascript>alert('Menu page isn't set');</script>");
            return;
        }

        // ClearSession();
        Response.Redirect(sMenuURL);
    }
    
    protected void dbGrid_user_master_SelectedIndexChanged(object sender, EventArgs e)
    {
        //try
        //{
        Session["user_master_index"] = dbGrid_user_master.SelectedIndex;
        BuildDataSource();
        // Session["user_master_code"] = dbGrid_user_master.SelectedRow.Cells[2].Text;
        Session["user_master_id"] = ((Label)dbGrid_user_master.SelectedRow.FindControl("Label1")).Text;
        Session["comp"] = dbGrid_user_master.SelectedRow.Cells[2].Text;

        //}
        //catch { }
    }    

    protected void lnk_listgroup_Click(object sender, EventArgs e)
    {
        Response.Redirect("usergroup_list.aspx");
    }
    protected void lnk_viewgroup_Click(object sender, EventArgs e)
    {
        Response.Redirect("usergroup_viewlist.aspx");
    }
    protected void lnk_groupuser_Click(object sender, EventArgs e)
    {
        Response.Redirect("group_user.aspx");
    }
}
