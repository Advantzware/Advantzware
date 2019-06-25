using System;
using System.Data;
using System.Configuration;
using System.Collections;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;

public partial class brwsjobs : System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {
        if (Session["view_order_entry_pages_with_estimate"] == null)
        {
            LinkButton2.Visible = false;
        }
        else
        {
            LinkButton1.Visible = false;
        }
        if (!Page.IsPostBack)
        {
            CheckBox1.Checked = true;
            CheckBox2.Checked = false;
            if (CheckBox1.Checked == true)
            {
                openvalue.Value = "Yes";
            }
            else
            {
                openvalue.Value = "NO";
            }
            if (CheckBox2.Checked == true)
            {
                closedvalue.Value = "No";
            }
            else
            {
                closedvalue.Value = "Yes";
            }
            Session["brwsjobs_open"] = openvalue.Value;
            Session["brwsjobs_close"] = closedvalue.Value.Trim();
            
            ddl_order.Text = Convert.ToString(Session["brwsjobs_order_no"]);
            txt_fgitem.Text = Convert.ToString(Session["brwsjobs_item_num"]);
            txt_job1.Text = Convert.ToString(Session["brwsjobs_job_no"]);
            txt_job2.Text = Convert.ToString(Session["brwsjobs_job_no2"]);
            txt_customer.Text = Convert.ToString(Session["brwsjobs_cust_no"]);
            txt_estimate.Text = Convert.ToString(Session["brwsjobs_est_no"]);
            openvalue.Value = Convert.ToString(Session["brwsjobs_open"]);
            closedvalue.Value = Convert.ToString(Session["brwsjobs_close"]);
        }
        if (Session["User"] != null)
        {
            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;

            string vUserId = UserLogin.UserName;
            string vPage = "brwsjobs.aspx";
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
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }
        Label name = (Label)Master.FindControl("lbl_page");
        name.Text = "Job Variance Inquiry";
        /*ImageButton brwsjobs = (ImageButton)Master.FindControl("brwsjobs");
        brwsjobs.ImageUrl = "~/Images/brws jobs 1.jpg";*/

        if (CheckBox1.Checked == true)
        {
            openvalue.Value = "Yes";
        }
        else
        {
            openvalue.Value = "NO";
        }
        if (CheckBox2.Checked == true)
        {
            closedvalue.Value = "No";
        }
        else
        {
            closedvalue.Value = "Yes";
        }
        Session["brwsjobs_open"] = openvalue.Value;
        Session["brwsjobs_close"] = closedvalue.Value.Trim();
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Select";


        
        GridView1.SelectedIndex = Convert.ToInt32(Session["brws_jobs_index"]);
        try
        {
            if (Session["brws_jobs_index"] == null)
            {
                GridView1.SelectedIndex = 0;
                Session["order"] = GridView1.SelectedRow.Cells[1].Text;
            }
            
        }
        catch
        {
            return;
        }

        ddl_order.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_fgitem.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_job1.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_job2.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_customer.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_estimate.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");

       
    }

    protected void btnSearch_Click(object sender, EventArgs e)
    {
        if (CheckBox1.Checked == true)
        {
            openvalue.Value = "Yes";
        }
        else
        {
            openvalue.Value = "NO";
        }
        if (CheckBox2.Checked == true)
        {
            closedvalue.Value = "No";
        }
        else
        {
            closedvalue.Value = "Yes";
        }
        Session["brwsjobs_order_no"] = ddl_order.Text.Trim();
        Session["brwsjobs_item_num"] = txt_fgitem.Text.Trim();
        Session["brwsjobs_job_no"] = txt_job1.Text.Trim();
        Session["brwsjobs_job_no2"] = txt_job2.Text.Trim();
        Session["brwsjobs_cust_no"] = txt_customer.Text.Trim();
        Session["brwsjobs_est_no"] = txt_estimate.Text.Trim();
        Session["brwsjobs_open"] = openvalue.Value;
        Session["brwsjobs_close"] = closedvalue.Value.Trim();

        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Select";
        ObjectDataSource1.SelectParameters["prmOrderNum"].DefaultValue = ddl_order.Text.Trim();
        ObjectDataSource1.SelectParameters["prmItemNum"].DefaultValue = txt_fgitem.Text.Trim();
        ObjectDataSource1.SelectParameters["prmJob"].DefaultValue = txt_job1.Text.Trim();
        ObjectDataSource1.SelectParameters["prmJob2"].DefaultValue = txt_job2.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCustomer"].DefaultValue = txt_customer.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEst"].DefaultValue = txt_estimate.Text.Trim();
        ObjectDataSource1.SelectParameters["prmOpen"].DefaultValue = openvalue.Value;
        ObjectDataSource1.SelectParameters["prmClosed"].DefaultValue = closedvalue.Value.Trim();
        
    }
    protected void btn_reset_Click(object sender, EventArgs e)
    {        
        string str = "";
        ddl_order.Text = str.ToString();
        txt_customer.Text = str.ToString();
        txt_estimate.Text = str.ToString();
        txt_fgitem.Text = str.ToString();
        txt_job1.Text = str.ToString();
        txt_job2.Text = str.ToString();
        CheckBox1.Checked = true;
        CheckBox2.Checked = false;
        if (CheckBox1.Checked == true)
        {
            openvalue.Value = "Yes";
        }
        else
        {
            openvalue.Value = "NO";
        }
        if (CheckBox2.Checked == true)
        {
            closedvalue.Value = "No";
        }
        else
        {
            closedvalue.Value = "Yes";
        }
        Session["brwsjobs_order_no"] = ddl_order.Text.Trim();
        Session["brwsjobs_item_num"] = txt_fgitem.Text.Trim();
        Session["brwsjobs_job_no"] = txt_job1.Text.Trim();
        Session["brwsjobs_job_no2"] = txt_job2.Text.Trim();
        Session["brwsjobs_cust_no"] = txt_customer.Text.Trim();
        Session["brwsjobs_est_no"] = txt_estimate.Text.Trim();
        Session["brwsjobs_open"] = openvalue.Value;
        Session["brwsjobs_close"] = closedvalue.Value.Trim();

        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Select";
        ObjectDataSource1.SelectParameters["prmOrderNum"].DefaultValue = ddl_order.Text.Trim();
        ObjectDataSource1.SelectParameters["prmItemNum"].DefaultValue = txt_fgitem.Text.Trim();
        ObjectDataSource1.SelectParameters["prmJob"].DefaultValue = txt_job1.Text.Trim();
        ObjectDataSource1.SelectParameters["prmJob2"].DefaultValue = txt_job2.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCustomer"].DefaultValue = txt_customer.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEst"].DefaultValue = txt_estimate.Text.Trim();
        ObjectDataSource1.SelectParameters["prmOpen"].DefaultValue = openvalue.Value;
        ObjectDataSource1.SelectParameters["prmClosed"].DefaultValue = closedvalue.Value.Trim();

    }
    protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
    {
        Session["brws_jobs_index"] = GridView1.SelectedIndex;
        Session["order"] = GridView1.SelectedRow.Cells[1].Text;
        //Response.Write(Session["order"]);
    }
    protected void LinkButton1_Click(object sender, EventArgs e)
    {
        Response.Redirect("order_inquiry.aspx");

    }
    protected void LinkButton2_Click(object sender, EventArgs e)
    {
        Response.Redirect("order_estimate.aspx");
    }
}
